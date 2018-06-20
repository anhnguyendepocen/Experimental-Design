

# Exerciese 13.2 ----------------------------------------------------------


library(dplyr)
Ph = rep(c("PhLow","PhHigh"), time = 6) 
Sugar = rep(c("SugarLow","SugarHigh"),each = 2,time = 3)
Temp <- rep(c("1","2","3"),each = 4)%>% as.factor()
Y <- c(21, 15, 13.5, 4.5, 23, 15.5, 14.5, 6, 20, 18.5, 16.5, 9.5)
data <- cbind.data.frame(Ph, Sugar, Temp,Y)
str(data)
la <- aov(Y~Ph+Sugar+Temp,data)
anova(la)

model.tables(la)


# Exercise 13.3 -----------------------------------------------------------

# L1
coll <- rep(c("C1","C2","C3","C4"),times = 4)
roww <- rep(c("R1","R2","R3","R4"),each = 4)
temp1 <-c("D","B","C","A","C","A","D","B","B","D","A","C","A","C","B","D")
Y1 <- c(44,26,67,77,39,45,71,74,52,49,81,88,73,58,76,100)
square1 <- rep(1,16) %>% as.factor()
data1 <- cbind.data.frame(coll,roww,temp1,Y1,square1)
colnames(data1) <- c("Column","Row","Temp","Y","square")
#L2
coll <- rep(c("C5","C6","C7","C8"),times = 4)
roww <- rep(c("R1","R2","R3","R4"),each= 4)
temp2 <- c("B","D","A","C","C","A","D","B","A","C","B","D","D","B","C","A")
Y2 <- c(51,62,71,49,63,74,67,47,74,75,60,58,82,79,74,68)
square2 <- rep(2,16) %>% as.factor()
data2 <- cbind.data.frame(coll,roww,temp2,Y2,square2)
colnames(data2) <- c("Column","Row","Temp","Y","square")
data <- rbind.data.frame(data1,data2)

temp3 <- lm(Y~Row+square+Temp+square:Column,data)
anova(temp3)
aov.model <- aov(Y~Row+Temp+square:Column+square,data)

sss <- TukeyHSD(aov.model,conf.level = 0.99)

plot(sss)
options(scipen=999)
sss$Temp

