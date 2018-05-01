
# Exercise 01 -------------------------------------------------------------
# Exercise 7.1
# H0: 6個組別的mean都一樣
# H1: 6個組別的mean存在不一樣
# 假設每一組的樣本數都為n，alpha = 0.05，ζ = 4n. 找個最小n使得power >= 0.7 
## Design a power function

E1 <- function(n){
tmp <- 1 - pf(qf(1-0.05, 6-1, 6*n - 6),6-1, 6*n-6, ncp = 4*n)
return(tmp) 
}

## Show the power when giving different n
temp <- NULL
for( i in 2:8){
  temp <- c(temp, E1(i))
}
temp <- cbind(c(2:8),temp)
colnames(temp) <- c("n","Power")
temp
# 由表可知n至少要4，power才會來到0.7以上，總樣本數為24

# Exercise 02 -------------------------------------------------------------
# Exercise 7.3
#What is the probability of rejecting the null hypothesis when there are four groups,
#the sum of the squared treatment effects is 6, 
#the error variance is 3, the group sample sizes are 4, and E is .01?

g <- 4; n <- 4
SSTR <- 6
MSE <- 3
zeta = 6 * 4 / 3
beta <- pf(qf(1-0.01, g - 1, g*n - g), g - 1, g*n - g, ncp = zeta)
power <- 1 - beta
power
# power is 0.2261


# Exercise 03 -------------------------------------------------------------
# Problem 7.2 
#Nondigestible carbohydrates can be used in diet foods,
#but they may have effects on colonic hydrogen production in humans. 
#We want to test to see if inulin, fructooligosaccharide, 
#and lactulose are equivalent in their hydrogen production. 
#Preliminary data suggest that the treatment means could be about 45, 32, and 60 respectively, 
#with the error variance conservatively estimated at 35. 
#How many subjects do we need to have power .95 for this situation when testing at the EI = .01 level?
g <- 3
x_bar <- c(45, 32, 60)
x_bar2 <- mean(x_bar)
SSTR <- sum((x_bar - x_bar2)^2)
MSE <- 35
P2 <- function(n){
  zeta = n * SSTR/35
  temp <- qf((1 - 0.01),(g-1),(n*g-g))
  power <- 1 - pf(temp,g-1,n*g-g,ncp = zeta)
  return(power)}  
n = 2
while(P2(n)<0.95){
  n = n+1}
n 
P2(4)
# If n = 4, then power will be 0.9808418, so total # of sample is 12.


# Exercise 04 -------------------------------------------------------------
# Exercise 6.3
library(MASS)
library(car)
library(multcomp)
#In order to determine the efficacy and lethal dosage of cardiac relaxants, 
#anesthetized guinea pigs are infused with a drug (the treatment) till death occurs. 
#The total dosage required for death is the response; smaller lethal doses are considered more effective. 
#There are four drugs, and ten guinea pigs are chosen at random for each drug. Lethal dosages follow.

#Determine which drugs are equivalent, which are more effective, and which less effective.
y1 <- c(18.2, 16.4, 10.0, 13.5, 13.5, 6.7, 12.2, 18.2, 13.5, 16.4)
y2 <- c(5.5, 12.2, 11.0, 6.7, 16.4, 8.2, 7.4, 12.2, 6.7, 11.0)
y3 <- c(5.5, 5.0, 8.2, 9.0, 10.0, 6.0, 7.4, 5.5, 12.2, 8.2)
y4 <- c(6.0, 7.4, 12.2, 11.0, 5.0, 7.4, 7.4, 5.5, 6.7, 5.5)
y <- c(y1, y2, y3, y4)
A <- as.factor(rep(c("drug1","drug2","drug3","drug4"), each = 10))
temp <- lm(y~A)
boxplot(y~A)
summary(aov(temp))

# 殘差的常態性、變異數均齊性以及獨立性
par(mfrow = c(2,2))
plot(temp)
resid <- (temp$residuals)

# 常態性檢定
shapiro.test(resid )

# 均齊性
ncvTest(temp)

# 殘差獨立性檢定
durbinWatsonTest(temp)

# 離群值檢定
outlierTest(temp)

# ANOVA檢定
aov(temp)

# 多重比較
TukeyHSD(aov(temp))
plot(TukeyHSD(aov(temp)))
library(multcomp)
temp1 <- glht(aov(temp), linfct = mcp(A = "Tukey"))
old.par <- par(mai=c(1,1.5,1,1))
plot(temp1)

# 檢測藥物對 Lethal dosages 的影響最大及最小
model.tables(aov(temp),type="effects")













