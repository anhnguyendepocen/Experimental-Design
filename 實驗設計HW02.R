library(ggplot2)
# 實驗設計 Exercise 2.4#
x1<- c(256,159,149)
x2 <- c(54,123,248)
x <- c(x1,x2)

#observation#
ob <- (sum(x1) - sum(x2))/3

# simulation
n = 1; pv = NULL
while(n <= 1000){
sumd <- NULL
for(i in 1:10000){
y <- sample(x,3,F)
tmp <- sum(x) - sum(y)
sumd[i] <-  (sum(y) - tmp)/3
}

p <- sum(sumd > ob)/10000
pv <- c(pv,p)
n = n + 1}
# 雙尾檢定 P value #
P_value <-mean(pv) * 2; sd(pv)
P_value

# 繪圖 
sumd <- as.data.frame(sumd)
sumd <- mutate(sumd,cc = (sumd >= ob) *1 )
sumd$cc[sumd$cc == 1] <- '>= red line'
sumd$cc[sumd$cc == 0] <- '< red line' 
sumd$cc <- sumd$cc %>% as.factor()
tmp <- ggplot(sumd,aes(x = sumd, fill=cc, color = cc))+ geom_histogram(position="identity", alpha = 0.5, binwidth = 30)
tmp+theme(plot.title = element_text(colour = "black", face = "bold", size = 20, vjust = 0.3)) + 
  geom_vline(aes(xintercept=ob),linetype="dashed",color="red")+
  scale_color_manual(values=c("skyblue", "darkblue","black"))+
  scale_fill_manual(values=c("skyblue", "darkblue","black"))+
  labs(title="Randomization ",x="Simulation of statistic", y = "Count")

  

