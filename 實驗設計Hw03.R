library(dplyr)
library(knitr)
library(kableExtra)
# Problem 3.1
x1 <- c(5.60, 6.80, 8.32, 8.70, 7.64, 7.44, 7.48, 7.80, 7.72, 8.40, 6.98, 8.00)
x2 <- c(5.04, 7.38, 5.56, 6.96, 8.30, 6.86, 5.62, 7.22, 5.72, 6.40, 7.54, 7.50)
x3 <- c(8.36, 7.04, 6.92, 8.18, 6.20, 6.10, 2.75, 8.14, 9.00, 8.64, 6.60, 8.18)
x4 <- c(8.30, 8.54, 7.68, 8.92, 8.46, 7.38, 8.08, 8.12, 8.68, 8.24, 8.09, 8.06)
x <- c(x1, x2, x3, x4)
operators <- as.factor(rep(c("Operators \nJohnson", "Operators \nGina", "Operators \nCharles", "Operators \nEva"), each = 12))
y <- data.frame(ww = x, op = operators)
ggplot(y) + geom_boxplot(aes(x = op,y = ww), fill = c("#FF8888","#9999FF","#FFDD55","#99FF33"))+ 
  xlab("Operators")+
  ylab("shear strengths")
lm(x ~ operators)
summary(lm(x ~ operators)) 
p31 <- anova(lm(x ~ operators))
p31 %>% kable(., "html") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)

# Problem 3.2
N<- c(35,37,49,46,63,39,46,56,63,65,56,65,70,63,65,70,77,81,86,70,70,77,77,81,77)
p1 <- c(40,37,44,47,47,47,68,47,54,61,71,75,89,58,59,62,79,96,58,62,70,72,75,96,75)
v1 <- c(46,42,65,46,58,42,48,58,50,80,63,65,70,70,72,97,46,56,70,70,72,76,90,76,92)
p8 <- c(21,40,44,54,36,40,56,60,48,53,60,60,65,68,60,81,81,48,48,56,68,75,81,48,68)
v8 <- c(16,19,19,32,33,33,30,42,42,33,26,30,40,54,34,34,47,47,42,47,54,54,56,60,44)
x <- c(N,p1,v1,p8,v8)
flys <- as.factor(rep(c("None", "1 pregnant", "1 virgin", "8 pregnant", "8 virgin"), each = 25))
y <- data.frame(ww = x, op = flys)
ggplot(y) + geom_boxplot(aes(x = op,y = ww), fill = c("#FF8888","#9999FF","#FFDD55","#99FF33","#D28EFF"))+ 
  xlab("Companions")+
  ylab("Longevity")
lm(x ~ flys)
summary(lm(x ~ flys))
anova(lm(x ~ flys)) %>% kable(., "html") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)

# Exercise 4.3
# Refer to the data in Problem 3.1. Workers 1 and 2 were experienced, whereas workers 3 and 4 were novices. 
# Find a contrast to compare the experienced and novice workers
# and test the null hypothesis that experienced and novice works produce the same average shear strength.
x1 <- c(5.60, 6.80, 8.32, 8.70, 7.64, 7.44, 7.48, 7.80, 7.72, 8.40, 6.98, 8.00)
x2 <- c(5.04, 7.38, 5.56, 6.96, 8.30, 6.86, 5.62, 7.22, 5.72, 6.40, 7.54, 7.50)
x3 <- c(8.36, 7.04, 6.92, 8.18, 6.20, 6.10, 2.75, 8.14, 9.00, 8.64, 6.60, 8.18)
x4 <- c(8.30, 8.54, 7.68, 8.92, 8.46, 7.38, 8.08, 8.12, 8.68, 8.24, 8.09, 8.06)
x <- c(x1, x2, x3, x4)
operators <- as.factor(rep(c("Johnson", "Gina", "Charles", "Eva"), each = 12))
boxplot(x ~ operators, ylab = "Strength of the pins", xlab = "Operators")
lm(x ~ operators)
summary(lm(x ~ operators)) 
p31 <- anova(lm(x ~ operators))
w <- c(1, 1, -1, -1)
mse <- p31$`Mean Sq`[2]
x <- matrix(x,ncol=12,byrow = T) %>% apply(.,1,mean)
t0 <- (w %*% x)/sqrt(mse*sum(w^2/12))
t0
quantt <- qt(0.975,48-4)
quantt
# test 
abs(t0) >= quantt
# do not reject

# Problem 4.2
## (a) 1 v 8 female fly
p1 <- c(40,37,44,47,47,47,68,47,54,61,71,75,89,58,59,62,79,96,58,62,70,72,75,96,75)
v1 <- c(46,42,65,46,58,42,48,58,50,80,63,65,70,70,72,97,46,56,70,70,72,76,90,76,92)
p8 <- c(21,40,44,54,36,40,56,60,48,53,60,60,65,68,60,81,81,48,48,56,68,75,81,48,68)
v8 <- c(16,19,19,32,33,33,30,42,42,33,26,30,40,54,34,34,47,47,42,47,54,54,56,60,44)
x <- c(p1,v1,p8,v8)
flys <- as.factor(rep(c("1 pregnant", "1 virgin", "8 pregnant", "8 virgin"), each = 25))
boxplot(x ~ flys, ylab = "Longevity (days)", xlab = "Companions")
lm(x ~ flys)
summary(lm(x ~ flys))
pfly <- anova(lm(x ~ flys)) 
pfly%>% kable(., "html") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
w <- c(1, 1, -1, -1)
mse <- pfly$`Mean Sq`[2]
x <- matrix(x,ncol=25,byrow = T) %>% apply(.,1,mean)
t0 <- (w %*% x)/sqrt(mse*sum(w^2/25))
t0
quantt <- qt(0.975,100-4)
quantt
# test 
abs(t0) >= quantt
# reject

## (b) virgin v pregant
p1 <- c(40,37,44,47,47,47,68,47,54,61,71,75,89,58,59,62,79,96,58,62,70,72,75,96,75)
v1 <- c(46,42,65,46,58,42,48,58,50,80,63,65,70,70,72,97,46,56,70,70,72,76,90,76,92)
p8 <- c(21,40,44,54,36,40,56,60,48,53,60,60,65,68,60,81,81,48,48,56,68,75,81,48,68)
v8 <- c(16,19,19,32,33,33,30,42,42,33,26,30,40,54,34,34,47,47,42,47,54,54,56,60,44)
x <- c(p1,v1,p8,v8)
flys <- as.factor(rep(c("1 pregnant", "1 virgin", "8 pregnant", "8 virgin"), each = 25))
boxplot(x ~ flys, ylab = "Longevity (days)", xlab = "Companions")
lm(x ~ flys)
summary(lm(x ~ flys))
pfly <- anova(lm(x ~ flys)) 
pfly%>% kable(., "html") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
w <- c(1, -1, 1, -1)
mse <- pfly$`Mean Sq`[2]
x <- matrix(x,ncol=25,byrow = T) %>% apply(.,1,mean)
t0 <- (w %*% x)/sqrt(mse*sum(w^2/25))
t0
quantt <- qt(0.975,100-4)
quantt
# test 
abs(t0) >= quantt
# reject

## (c) non v more
N <- c(35,37,49,46,63,39,46,56,63,65,56,65,70,63,65,70,77,81,86,70,70,77,77,81,77)
p1 <- c(40,37,44,47,47,47,68,47,54,61,71,75,89,58,59,62,79,96,58,62,70,72,75,96,75)
v1 <- c(46,42,65,46,58,42,48,58,50,80,63,65,70,70,72,97,46,56,70,70,72,76,90,76,92)
p8 <- c(21,40,44,54,36,40,56,60,48,53,60,60,65,68,60,81,81,48,48,56,68,75,81,48,68)
v8 <- c(16,19,19,32,33,33,30,42,42,33,26,30,40,54,34,34,47,47,42,47,54,54,56,60,44)
x <- c(N,p1,v1,p8,v8)
flys <- as.factor(rep(c("None", "1 pregnant", "1 virgin", "8 pregnant", "8 virgin"), each = 25))
boxplot(x ~ flys, ylab = "Longevity (days)", xlab = "Companions")
lm(x ~ flys)
summary(lm(x ~ flys))
pfly <- anova(lm(x ~ flys)) 
pfly%>% kable(., "html") %>%
  kable_styling(bootstrap_options = "striped", full_width = F)
w <- c(1,-1/4,-1/4,-1/4,-1/4)
mse <- pfly$`Mean Sq`[2]
x <- matrix(x,ncol=25,byrow = T) %>% apply(.,1,mean)
t0 <- (w %*% x)/sqrt(mse*sum(w^2/25))
t0
quantt <- qt(0.975,125-5)
quantt
# test 
abs(t0) >= quantt
# reject
