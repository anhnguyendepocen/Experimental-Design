library(dplyr)

# Exercise 2.5 蟑螂 #
# H0 : The chlordane treatment has not affected the Na+-K+ATPas activity. #
# H1 : The chlordane treatment has affected the Na+-K+ATPas activity. # 
pv = NULL
for (j in 1:100){
# Data
d <- c(15.3, -31.8, -35.6, -14.5, 3.1, -24.5)
# Number of simulation #
N <- 10000
# Define function #
ttest <- function(d1){
  d_sum <- sum(d1)
  ds <- sd(d1)
  t <- d_sum/ds*sqrt(6)
  return(t)}

temp <- NULL
for( i in 1:N){
  number <- sample(0:6,1,T,prob = c(1,6,15,20,15,6,1)/64)
  if(number != 0 ){
    sign <- sample(1:6,number,F)
    d1 <- c(d[sign]*-1,d[-sign])}
  else{d1 <- d}
  temp <- c(temp,ttest(d1))}

# crititcal point #
P_value <- sum(ttest(d) >= temp)/N
pv <- c(pv, P_value)}

# p value and it's standaed error
P_value <- mean(pv)*2
P_value
sd <- sd(pv)
sd
# Under alpha = 0.05 , P value < alpha,  Reject H0 #

# Histogram #
# Red line is critical value #
hist(temp,100, main = 'Simulation of Experiment', xlab = 'Simulation value of Statistic')
abline( v = ttest(d), col = 'red')

########################################################################

# Exercise Promblem 2.1 #
# H0 : The null hypothesis is that Visiplume reads equal the standard.
# H1 : The alternative hypothesis is that Visiplume reads higher than the standard.
x <- c(0.95, 0.978, 0.762, 0.733, 0.823, 1.011)
N <- 10000
pv <- NULL
for(i in 1:100){
  temp <- NULL
  for(j in 1:N){
    y <- NULL
    sign <- sample(c(1,-1),6,prob = c(0.5,0.5),T)
      for(i in 1:6){
        if(sign[i] == -1){
          y[i] <-  1/x[i]}
          else{y[i] <-  x[i]}}
   temp <- c(temp,sum(y))}
  P_value <- sum(sum(x) <= temp)/N
  pv <- c(pv,P_value)}

# p value and it's standaed error
P_value <- mean(pv)
P_value
sd <- sd(pv)
sd
# Under alpha = 0.05 , P value > alpha,  Do not reject H0 #

# Histogram #
# Red line is critical value #
hist(temp,100, main = 'Simulation of Experiment', xlab = 'Simulation value of Statistic')
abline( v = sum(x), col = 'red')


