library(readxl)
library("ggplot2")
MRT <- read_excel("C:/Users/wilso/Desktop/MRT/MRT.xlsx")
people <- read_excel("C:/Users/wilso/Desktop/population.xlsx")
parking <- read_excel("C:/Users/wilso/Desktop/parking.xlsx")

MRT_season <- read_excel("C:/Users/wilso/Desktop/MRT_season.xlsx")
aging_index <- read_excel("C:/Users/wilso/Desktop/aging_index.xlsx")
density <- read_excel("C:/Users/wilso/Desktop/density.xlsx")

MRT_half <- read_excel("C:/Users/wilso/Desktop/MRT_half.xlsx")
business <- read_excel("C:/Users/wilso/Desktop/business.xlsx")
elderly <- read_excel("C:/Users/wilso/Desktop/elderly.xlsx")
hospital <- read_excel("C:/Users/wilso/Desktop/hospital.xlsx")

#跑折線圖
k = ggplot() + 
  geom_line(data = MRT, aes(x = time, y = LS4,group = 1, color = "龍山寺"),size=2) +
  geom_line(data = MRT, aes(x = time, y = XM,group = 1, color = "西門"),size=2) +
  geom_line(data = MRT, aes(x = time, y = XS,group = 1, color = "象山"),size=2) +
  geom_line(data = MRT, aes(x = time, y = UT,group = 1, color = "永春"),size=2) +
  geom_line(data = MRT, aes(x = time, y = city,group = 1, color = "市政府"),size=2) +
  geom_line(data = MRT, aes(x = time, y = E0E,group = 1, color = "`台北101世貿`"),size=2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab('people')+
  ggtitle('捷運流量比較圖')
k

h = ggplot() + 
  geom_line(data = MRT, aes(x = time, y = XingE,group = 1, color = "信義區平均"),size=2) +
  geom_line(data = MRT, aes(x = time, y = WangH,group = 1, color = "萬華區平均"),size=2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab('people')+
  ggtitle('捷運區域流量比較圖')
h

#相關性
x=data.frame(MRT$XingE,people$XingE,parking$XingE)
y=data.frame(MRT_season$XingE,aging_index$XingE,density$XingE)
z=data.frame(MRT_half$XingE,business$XingE,elderly$XingE,hospital$XingE)
r=data.frame(MRT$WangH,people$WangH,parking$WangH)
p=data.frame(MRT_season$WangH,aging_index$WangH,density$WangH)
q=data.frame(MRT_half$WangH,business$WangH,elderly$WangH,hospital$WangH)
#Pearson檢定
cor.test(MRT$XingE,people$XingE)
cor.test(MRT$XingE,parking$XingE)
cor.test(MRT_season$XingE,aging_index$XingE)
cor.test(MRT_season$XingE,density$XingE)
cor.test(MRT_half$XingE,business$XingE)
cor.test(MRT_half$XingE,elderly$XingE)
cor.test(MRT_half$XingE,hospital$XingE)

cor.test(MRT$WangH,people$WangH)
cor.test(MRT$WangH,parking$WangH)
cor.test(MRT_season$WangH,aging_index$WangH)
cor.test(MRT_season$WangH,density$WangH)
cor.test(MRT_half$WangH,business$WangH)
cor.test(MRT_half$WangH,elderly$WangH)
cor.test(MRT_half$WangH,hospital$WangH)
#rcorr function

library("Hmisc")
mydata.rcorr = rcorr(as.matrix(x)) #信義
mydata.rcorr
mydata.rcorr1 = rcorr(as.matrix(y))
mydata.rcorr1
mydata.rcorr2 = rcorr(as.matrix(z))
mydata.rcorr2

mydata.rcorr4 = rcorr(as.matrix(r)) #萬華
mydata.rcorr4
mydata.rcorr5 = rcorr(as.matrix(p))
mydata.rcorr5
mydata.rcorr6 = rcorr(as.matrix(q))
mydata.rcorr6
#相關性散佈圖 #信義
plot(MRT$XingE,people$XingE)
abline(lm(people$XingE~MRT$XingE))
plot(MRT$XingE,parking$XingE)
abline(lm(parking$XingE~MRT$XingE))

plot(MRT_season$XingE,aging_index$XingE)
abline(lm(aging_index$XingE~MRT_season$XingE))
plot(MRT_season$XingE,density$XingE)
abline(lm(density$XingE~MRT_season$XingE))

plot(MRT_half$XingE,business$XingE)
abline(lm(business$XingE~MRT_half$XingE))
plot(MRT_half$XingE,elderly$XingE)
abline(lm(elderly$XingE~MRT_half$XingE))
plot(MRT_half$XingE,hospital$XingE)
abline(lm(hospital$XingE~MRT_half$XingE))
#萬華
plot(MRT$WangH,people$WangH)
abline(lm(people$WangH~MRT$WangH))
plot(MRT$WangH,parking$WangH)
abline(lm(parking$WangH~MRT$WangH))

plot(MRT_season$WangH,aging_index$WangH)
abline(lm(aging_index$WangH~MRT_season$WangH))
plot(MRT_season$WangH,density$WangH)
abline(lm(density$WangH~MRT_season$WangH))

plot(MRT_half$WangH,business$WangH)
abline(lm(business$WangH~MRT_half$WangH))
plot(MRT_half$WangH,elderly$WangH)
abline(lm(elderly$WangH~MRT_half$WangH))
plot(MRT_half$WangH,hospital$WangH)
abline(lm(hospital$WangH~MRT_half$WangH))
#迴歸分析
#信義
summary(lm(MRT$XingE~people$XingE+parking$XingE)) #MRT是捷運流量 #people是區域人數 #parking是停車位
summary(lm(MRT_season$XingE~aging_index$XingE+density$XingE)) #aging是老化指數 #density是人口密度
summary(lm(MRT_half$XingE~business$XingE+elderly$XingE+hospital$XingE)) #business是工商業家數 #elderly是老年人數 #hospital是醫療場所數
#萬華
summary(lm(MRT$WangH~people$WangH+parking$WangH))
summary(lm(MRT_season$WangH~aging_index$WangH+density$WangH))
summary(lm(MRT_half$WangH~business$WangH+elderly$WangH+hospital$WangH))
