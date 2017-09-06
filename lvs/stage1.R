
rm(list=ls())

load("C:/Users/admin/Documents/WeChat Files/wensensei/Files/raw_salesdata_prepare_for_model_20170904.rdata")
library(dplyr)
library(data.table)

##########################
#Function
##########################

mlog <- function(x){
  log(x/mean(x))
}

controlpe <- function(x,max=-0.5,min=-5){
  x <- ifelse(x>max,max,x)
  x <- ifelse(x<min,min,x)
  x
}

bprice <- function(x){
  x2 <- x
  for(i in 2:length(x)){
    x2[i] <- sort(x[max(1,i-3):min(length(x2),i+2)],T)[2]
  }
  # plot.ts(x); lines(x2,col=2)
  return(x2)
}

#Model1 for roughly split base sales and promo sales
model1 <- function(x){
  # x <- data9[[1]]
  tpr <- coef(lm(vol~bprice+tpr+ss,data=x))[3] * (x$tpr-1)
  tpr <- ifelse(tpr>0,tpr,0)
  data.frame(bsales=x$vol-tpr,psales=tpr)
}

model2 <- function(x){}

model3 <- function(x){
  # x <- data9[[1]]
  xm <- mean(x$vol)
  x <- data.frame(vol=mlog(x$vol),
                  bprice=mlog(x$bprice),
                  tpr=mlog(x$tpr),
                  ss=x$ss,
                  clift=x$lift)
  m <- lm(vol~-1+.,data=x[,1:5])
  return(m)
}

##########################
#Processing data
##########################

#data selection
data <- select(
  filter(salesdata,stunit>0&unitvaldisc>0),
  segment,week=fisweek,weeklen,pc5=style,pc9=pc5desc,vol=stunit,val=unitvaldisc,tag_price
) %>% mutate(pf=7/weeklen)
#project to weeklen = 7
data <- mutate(data,vol=vol*pf,val=val*pf)

#seasonality total volume by week
ss <- data %>% group_by(week) %>% summarise(ss=sum(vol))
#aggregate data into pc5_tag_tagprice level
data$pc9 <- tolower(data$pc9)
data$pc9 <- paste(data$pc9,data$tag_price,sep=',')
data <- data %>% group_by(week,weeklen,pc5,pc9,tag_price,segment) %>%
  summarise(vol=sum(vol),val=sum(val)) %>% mutate(price=val/vol)
#filter the items with over 100 week sales only(for demo)
sel <- names(which(table(data$pc9)>100))
data <- filter(data,pc9%in%sel)

#calculate base price for each pc5_tag_tagprice
data9 <- lapply(unique(data$pc9),function(x){
  x <- filter(data,pc9==x)
  x$bprice <- bprice(x$price)
  x
})
data <- do.call(rbind,data9)
#base value and tag value calculated by base price, tag price multi vol
data$bval <- data$bprice * data$vol
data$tval <- data$tag_price * data$vol

#process data to pc4_tag level
data$pc9 <- substr(data$pc9,1,regexpr(',',data$pc9)-1)
data9 <- data %>% group_by(week,weeklen,pc9,segment) %>%
  summarise(vol=sum(vol),val=sum(val),bval=sum(bval),tval=sum(tval),
            price=val/vol,bprice=bval/vol,
            tpr=ifelse(price/bprice<0.95,price/bprice,1))

#calculate pdata 
data9 <- lapply(unique(data9$pc9),function(x){
  x <- filter(data9,pc9==x)
  x$ss <- ss$ss[match(x$week,ss$week)]
  x$pvol <- model1(x)$psales
  x$pval <- x$pvol * x$price
  x
})
data <- do.call(rbind,data9)

#canniblization data in segment level
seg <- data %>% group_by(week,segment) %>% 
  summarise(segval=sum(val),segvol=sum(vol),segpval=sum(pval),segpvol=sum(pvol),segprice=segval/segvol)
data <- data.table(data,
                   seg[match(paste(data$week,data$segment),
                             paste(seg$week,seg$segment)),-1:-2]
                   ) %>% mutate(canpvol=segpvol-pvol,
                                canpval=segpval-pval,
                                lift=canpvol/(segvol-pvol))
data9 <- lapply(unique(data$pc9),function(x){
  filter(data,pc9==x)
})

##########################
#Modeling at Segment Level
##########################

mfile_segment <- do.call(rbind,
                    lapply(data9,function(x){
                      x <- mutate(x,
                                  vol=vol/mean(vol),bprice=bprice/mean(bprice),ss=ss/mean(ss),
                                  tpr=tpr/mean(tpr))
                    })
                    )
mout_segment <- lapply(unique(mfile_segment$segment),function(x){
  x <- filter(mfile_segment,segment==x)
  mout_s1 <- lm(mlog(vol)~mlog(bprice)+mlog(tpr)+ss+lift,data=x)
  mout_s1
})
matrix(sapply(mout_segment,coef),5,5,
       dimnames=list(rownames(sapply(mout_segment,coef)),
                     unique(mfile_segment$segment)))

##########################
#Modeling at PC5_tag level_stage1
##########################

mfile_pc9 <- data.table(mfile_segment,
                        res=unlist(lapply(mout_segment,function(x){as.numeric(x$residual)}))
                        )
mfile_pc9 <- lapply(unique(mfile_pc9$pc9),function(x){
  filter(mfile_pc9,pc9==x)
})
mout_pc9 <- lapply(mfile_pc9,function(x){
  lm(mlog(vol)~mlog(bprice)+mlog(tpr)+ss+lift,data=x)
})

mcoef_pc9 <- t(sapply(mout_pc9,coef))
mcoef_pc9 <- data.table(
  t(sapply(mfile_pc9,function(x){c(x[1][[3]],x[1][[4]])})),mcoef_pc9
)
colnames(mcoef_pc9) <- c('sku','segment','constant','rpe','ppe','season','canib')
mcoef_pc9$rpe <- controlpe(mcoef_pc9$rpe)
mcoef_pc9$ppe <- controlpe(mcoef_pc9$ppe)
mcoef_pc9$cannib <- controlpe(mcoef_pc9$canib,0,-Inf)

as.data.frame(
  mcoef_pc9 %>% 
    group_by(segment) %>% 
    summarise(mean(rpe),mean(ppe),mean(canib),
              median(rpe),median(ppe),median(canib))
)
