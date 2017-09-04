
rm(list=ls())

load("C:/Users/admin/Documents/WeChat Files/wensensei/Files/raw_salesdata_prepare_for_model_20170904.rdata")
library(dplyr)
library(data.table)

#Function

bprice <- function(x){
  x2 <- x
  for(i in 2:length(x)){
    x2[i] <- sort(x[max(1,i-3):min(length(x2),i+2)],T)[2]
  }
  # plot.ts(x); lines(x2,col=2)
  return(x2)
}

#Processing data

data <- select(
  filter(salesdata,stunit>0),
  segment,week=fisweek,weeklen,pc5=style,pc9=pc5desc,vol=stunit,val=unitvaldisc,tag_price
)
data$pc9 <- tolower(data$pc9)
data$pc9 <- paste(data$pc9,data$tag_price,sep=',')
data <- data %>% group_by(week,weeklen,pc5,pc9,tag_price,segment) %>%
  summarise(vol=sum(vol),val=sum(val)) %>% mutate(price=val/vol)
data <- filter(data,pc9%in%names(which(table(data$pc9)>130)))

data9 <- lapply(unique(data$pc9)[1:10],function(x){
  x <- filter(data,pc9==x)
  x$bprice <- bprice(x$price)
  x
})

data <- do.call(rbind,data9)
data$bval <- data$bprice * data$vol
data$tval <- data$tag_price * data$vol

data$pc9 <- substr(data$pc9,1,regexpr(',',data$pc9)-1)
data9 <- data %>% group_by(week,weeklen,pc9) %>%
  summarise(vol=sum(vol),val=sum(val),bval=sum(bval),tval=sum(tval),
            price=val/vol,bprice=bval/vol,
            tpr=ifelse(price/bprice<0.95,price/bprice,1))




