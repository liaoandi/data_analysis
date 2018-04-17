load("weather2.Rdata")

#提取AQI/PM2.5/PM2.5_24h
AQI = subset(weather2, type == "AQI", select = date:DongSiHuan)
PM2.5 = subset(weather2, type == "PM2.5", select = date:DongSiHuan)
PM2.5_24h = subset(weather2, type == "PM2.5_24h", select = date:DongSiHuan) 

#寻找缺失数据，缺失值在14h和19h,20150217,20160724,20160725
library(dplyr)
a = AQI %>% group_by(hour)%>% summarize(counts=n())
b = PM2.5 %>% group_by(hour)%>% summarize(counts=n())
c = PM2.5_24h %>% group_by(hour)%>% summarize(counts=n())
d = AQI %>% group_by(date)%>% summarize(counts=n())
e = PM2.5 %>% group_by(date)%>% summarize(counts=n()) 
f = PM2.5_24h %>% group_by(date)%>% summarize(counts=n()) 

#删除20150217,20160724,20160725三日数据
AQI_DEL = filter(AQI, date!= "20150217", date!= "20160724", date!= "20160725")
PM2.5_DEL = filter(PM2.5, date!= "20150217", date!= "20160724", date!= "20160725")
PM2.5_24h_DEL = filter(PM2.5_24h, date!= "20150217", date!= "20160724", date!= "20160725")

#PM2.5/PM2.5_24h的空气质量分指数计算
PM2.5_AQI = function(x){
  if (x < 35){
    PM2.5_AQI = round(50*x/35)}
  else if(x < 75){
    PM2.5_AQI = round(50*(x-35)/40+50)}
  else if (x < 115){
    PM2.5_AQI = round(50*(x-75)/40+100)}
  else if (x < 150){
    PM2.5_AQI = round(50*(x-115)/35+150)}
  else if (x < 250){
    PM2.5_AQI = round(100*(x-150)/100+200)}
  else if (x < 350){
    PM2.5_AQI = round(100*(x-250)/100+300)}
  else if (x < 500){
    PM2.5_AQI = round(100*(x-350)/150+400)}
  else  PM2.5_AQI = 0
}

#使用PM2.5进行AQI计算
dat1 = lapply(list(PM2.5_DEL[,-c(1:3)]), PM2.5_AQI)
AQI_PM2.5_DEL = data.frame(matrix(unlist(dat1), nrow=25109, byrow = F))

#使用PM2.5_24h进行AQI计算
dat2 = lapply(list(PM2.5_24h_DEL[,-c(1:3)]), PM2.5_AQI)
AQI_PM2.5_24h_DEL = data.frame(matrix(unlist(dat2), nrow=25109, byrow = F))  

#整理数据格式供比较
library(tidyr)
dat3 =gather(AQI_DEL,place, value, -c(date,hour,type))
dat4 =gather(AQI_PM2.5_DEL, place, value)
dat5 =gather(AQI_PM2.5_24h_DEL, place, value)

#比较数据集中的AQI与使用PM2.5/PM2.5_24h计算所得的AQI之间的差异
compare_PM2.5 = cbind(dat3[,c(1,2,4)], AQI =dat3[,5], 
                      AQI_PM2.5 = dat4[,2], AQI_PM2.5_24h = dat5[,2], 
                      diff1 = dat3[,5] - dat4[,2],
                      diff2 = dat3[,5] - dat5[,2],
                      diff3 = dat4[,2] - dat5[,2])

#去除异常值20140116数据
compare_PM2.5_NEW = filter(compare_PM2.5, date!= "20140116")

#绘制AQI,AQI_PM2.5,AQI_PM2.5_24h的概率密度图
library(ggplot2)
library(gridExtra)
compare_PM2.5_NEW_PLOT = gather(compare_PM2.5_NEW,"diff_AQI","value",AQI:AQI_PM2.5_24h)
g1 = ggplot(compare_PM2.5_NEW_PLOT,aes(x = value,fill=diff_AQI)) + geom_density(alpha = 0.6) + theme(legend.position = "top") + guides(fill=guide_legend(title=NULL))
g2 = ggplot(compare_PM2.5_NEW_PLOT,aes(x = value,fill=diff_AQI)) + geom_density(alpha = 0.6) + scale_x_continuous(limits = c(0,500)) + theme(legend.position = "top") + guides(fill=guide_legend(title=NULL))
g3 = ggplot(compare_PM2.5_NEW_PLOT,aes(x = value,fill=diff_AQI)) + geom_density(alpha = 0.6) + scale_x_continuous(limits = c(500,1200)) + theme(legend.position = "top") + guides(fill=guide_legend(title=NULL))
grid.arrange(g1, g2, g3, ncol=3, nrow=1)

#样本量足够大，可视为正态分布考察diff1及diff2
m1 = mean(compare_PM2.5_NEW$diff1,na.rm=TRUE)
m2 = mean(compare_PM2.5_NEW$diff2,na.rm=TRUE)
sd1 = sd(compare_PM2.5_NEW$diff1,na.rm=TRUE)
sd2 = sd(compare_PM2.5_NEW$diff2,na.rm=TRUE)
print(c(m1,m2,sd1,sd2))
#结果：4.643353  5.270638 66.834715 19.464041

#绘制diff1及diff2的概率密度图
g4 = ggplot(compare_PM2.5_NEW,aes(x = diff1)) + geom_density() + scale_x_continuous(limits = c(-300,300))
g5 = ggplot(compare_PM2.5_NEW,aes(x = diff2)) + geom_density() + scale_x_continuous(limits = c(-50,50))
grid.arrange(g4, g5, ncol=2, nrow=1)

#将结果误差在+-4内均视为正确匹配，则有
sum1 = count(filter(compare_PM2.5_NEW, diff1>-1, diff1<1))
sum2 = count(filter(compare_PM2.5_NEW, diff2>-1, diff2<1))
percent1 = sum1/count(compare_PM2.5_NEW)
percent2 = sum2/count(compare_PM2.5_NEW)
print(c(sum1,sum2,percent1,percent2))

sum1 = count(filter(compare_PM2.5_NEW, diff1>-2, diff1<2))
sum2 = count(filter(compare_PM2.5_NEW, diff2>-2, diff2<2))
percent1 = sum1/count(compare_PM2.5_NEW)
percent2 = sum2/count(compare_PM2.5_NEW)
print(c(sum1,sum2,percent1,percent2))

sum1 = count(filter(compare_PM2.5_NEW, diff1>-3, diff1<3))
sum2 = count(filter(compare_PM2.5_NEW, diff2>-3, diff2<3))
percent1 = sum1/count(compare_PM2.5_NEW)
percent2 = sum2/count(compare_PM2.5_NEW)
print(c(sum1,sum2,percent1,percent2))

sum1 = count(filter(compare_PM2.5_NEW, diff1>-4, diff1<4))
sum2 = count(filter(compare_PM2.5_NEW, diff2>-4, diff2<4))
percent1 = sum1/count(compare_PM2.5_NEW)
percent2 = sum2/count(compare_PM2.5_NEW)
print(c(sum1,sum2,percent1,percent2))

sum1 = count(filter(compare_PM2.5_NEW, diff1>-5, diff1<5))
sum2 = count(filter(compare_PM2.5_NEW, diff2>-5, diff2<5))
percent1 = sum1/count(compare_PM2.5_NEW)
percent2 = sum2/count(compare_PM2.5_NEW)
print(c(sum1,sum2,percent1,percent2))

sum1 = count(filter(compare_PM2.5_NEW, diff1>-6, diff1<6))
sum2 = count(filter(compare_PM2.5_NEW, diff2>-6, diff2<6))
percent1 = sum1/count(compare_PM2.5_NEW)
percent2 = sum2/count(compare_PM2.5_NEW)
print(c(sum1,sum2,percent1,percent2))

#标准为0：  8955  184807 0.01019961 0.2104923
#标准为+-1: 26197 367230 0.02983798 0.4182693
#标准为+-2: 43470 413862 0.04951166 0.4713824
#标准为+-3: 60704 453806 0.06914092 0.5168780
#标准为+-4: 77764 487533 0.08857200 0.5552926
#标准为+-5: 94650 518889 0.10780490 0.5910066
-----------------------------------------------------------

#提取PM10/PM10_24h
PM10 = subset(weather2, type == "PM10", select = date:DongSiHuan)
PM10_24h = subset(weather2, type == "PM10_24h", select = date:DongSiHuan) 

#寻找缺失数据，没有缺失值
h = PM10 %>% group_by(hour)%>% summarize(counts=n())
i = PM10_24h %>% group_by(hour)%>% summarize(counts=n())
j = PM10 %>% group_by(date)%>% summarize(counts=n()) 
k = PM10_24h %>% group_by(date)%>% summarize(counts=n()) 

#为统一标准，删除20150217,20160724,20160725三日数据
AQI_DEL = filter(AQI, date!= "20150217", date!= "20160724", date!= "20160725")
PM10_DEL = filter(PM10, date!= "20150217", date!= "20160724", date!= "20160725")
PM10_24h_DEL = filter(PM10_24h, date!= "20150217", date!= "20160724", date!= "20160725")

#PM10/PM10_24h的空气质量分指数计算
PM10_AQI = function(x){
  if (x < 50){
    PM10_AQI = round(50*x/50)}
  else if(x < 150){
    PM10_AQI = round(50*(x-50)/100+50)}
  else if (x < 250){
    PM10_AQI = round(50*(x-150)/100+100)}
  else if (x < 350){
    PM10_AQI = round(50*(x-250)/100+150)}
  else if (x < 420){
    PM10_AQI = round(100*(x-350)/70+200)}
  else if (x < 500){
    PM10_AQI = round(100*(x-420)/80+300)}
  else if (x < 600){
    PM10_AQI = round(100*(x-500)/100+400)}
  else  PM10_AQI = 0
}

#使用PM10进行AQI计算
dat6 = lapply(list(PM10_DEL[,-c(1:3)]), PM10_AQI)
AQI_PM10_DEL = data.frame(matrix(unlist(dat6), nrow=25109, byrow = F))

#使用PM10_24h进行AQI计算
dat7 = lapply(list(PM10_24h_DEL[,-c(1:3)]), PM10_AQI)
AQI_PM10_24h_DEL = data.frame(matrix(unlist(dat7), nrow=25109, byrow = F))  

#整理数据格式供比较
dat8 =gather(AQI_PM10_DEL, place, value)
dat9 =gather(AQI_PM10_24h_DEL, place, value)

#比较数据集中的AQI与使用PM10/PM10_24h计算所得的AQI之间的差异
compare_PM10 = cbind(dat3[,c(1,2,4)], AQI =dat3[,5], 
                    AQI_PM10 = dat8[,2], AQI_PM10_24h = dat9[,2], 
                    diff4 = dat3[,5] - dat8[,2],
                    diff5 = dat3[,5] - dat9[,2],
                    diff6 = dat8[,2] - dat9[,2])

#绘制AQI,AQI_PM10,AQI_PM10_24h的概率密度图
compare_PM10_PLOT = gather(compare_PM10,"diff_AQI","value",AQI:AQI_PM10_24h)
g6 = ggplot(compare_PM10_PLOT,aes(x = value,fill=diff_AQI)) + geom_density(alpha = 0.6) + theme(legend.position = "top") + guides(fill=guide_legend(title=NULL))

#样本量足够大，可视为正态分布考察diff4及diff5
m3 = mean(compare_PM10$diff4,na.rm=TRUE)
m4 = mean(compare_PM10$diff5,na.rm=TRUE)
sd3 = sd(compare_PM10$diff4,na.rm=TRUE)
sd4 = sd(compare_PM10$diff5,na.rm=TRUE)
print(c(m3,m4,sd3,sd4))
#结果：22.42521 26.28407 54.11688 41.62776

#绘制diff4及diff5的概率密度图
g7 = ggplot(compare_PM10,aes(x = diff4)) + geom_density() + scale_x_continuous(limits = c(-250,250))
g8 = ggplot(compare_PM10,aes(x = diff5)) + geom_density() + scale_x_continuous(limits = c(-250,250))
grid.arrange(g7, g8, ncol=2, nrow=1)
+ scale_x_continuous(limits = c(-250,250))

#将结果误差在+-4内均视为正确匹配，则有
sum3 = count(filter(compare_PM10, diff4>-1, diff4<1))
sum4 = count(filter(compare_PM10, diff5>-1, diff5<1))
percent3 = sum3/count(compare_PM10)
percent4 = sum4/count(compare_PM10)
print(c(sum3,sum4,percent3,percent4))

sum3 = count(filter(compare_PM10, diff4>-2, diff4<2))
sum4 = count(filter(compare_PM10, diff5>-2, diff5<2))
percent3 = sum3/count(compare_PM10)
percent4 = sum4/count(compare_PM10)
print(c(sum3,sum4,percent3,percent4))

sum3 = count(filter(compare_PM10, diff4>-3, diff4<3))
sum4 = count(filter(compare_PM10, diff5>-3, diff5<3))
percent3 = sum3/count(compare_PM10)
percent4 = sum4/count(compare_PM10)
print(c(sum3,sum4,percent3,percent4))

sum3 = count(filter(compare_PM10, diff4>-4, diff4<4))
sum4 = count(filter(compare_PM10, diff5>-4, diff5<4))
percent3 = sum3/count(compare_PM10)
percent4 = sum4/count(compare_PM10)
print(c(sum3,sum4,percent3,percent4))

sum3 = count(filter(compare_PM10, diff4>-5, diff4<5))
sum4 = count(filter(compare_PM10, diff5>-5, diff5<5))
percent3 = sum3/count(compare_PM10)
percent4 = sum4/count(compare_PM10)
print(c(sum3,sum4,percent3,percent4))

sum3 = count(filter(compare_PM10, diff4>-6, diff4<6))
sum4 = count(filter(compare_PM10, diff5>-6, diff5<6))
percent3 = sum3/count(compare_PM10)
percent4 = sum4/count(compare_PM10)
print(c(sum3,sum4,percent3,percent4))

#标准为0：  7513  86795  0.008549012 0.09876368
#标准为+-1: 22561 171380 0.02567207  0.1950126
#标准为+-2: 37656 186999 0.04284861  0.2127854
#标准为+-3: 52350 202960 0.05956885  0.2309474
#标准为+-4: 67291 218177 0.07657015  0.2482627
#标准为+-5: 81933 233414 0.09323123  0.2656008