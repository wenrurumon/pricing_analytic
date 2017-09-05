
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

mfile_s1 <- do.call(rbind,
                    lapply(data9,function(x){
                      x <- mutate(x,
                                  vol=vol/mean(vol),bprice=bprice/mean(bprice),ss=ss/mean(ss),
                                  tpr=tpr/mean(tpr))
                    })
                    )
mout_s1 <- lm(log(vol)~log(bprice)+log(tpr)+ss+lift,data=mfile_s1)
summary(mout_s1)

##########################
#Modeling
##########################

#PE calculation in PC5_tag level
pe_pc9 <- t(sapply(data9,function(x){coef(model2(x))}))
  pe_pc9[,1] <- controlpe(pe_pc9[,1],0,-5)
  pe_pc9[,2] <- controlpe(pe_pc9[,2],0,-10)
  pe_pc9[,4] <- controlpe(pe_pc9[,4],0,-Inf)
