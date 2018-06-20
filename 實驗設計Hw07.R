library(dplyr)
Y = c(0.445,0.624,0.360,0.493,0.513,0.693,0.534,0.675,0.578,0.786,0.622,0.755,0.814,0.902,0.869,0.902)
C <- factor(rep(c("1","2"), each = 8))
A <- factor(rep(c("1","2"), each = 4, times = 2))
L <- factor(rep(c("1","2"), each = 2, times = 4))
M <- factor(rep(c("1","2"), times = 8))
temp <- cbind(C,A,L,M,Y) %>% as.matrix()

# Anova table
lm.result1 <- lm(Y ~ C*A*L*M-C:A:L:M)
anova(lm.result)

# delete non-significant factor
#Anova table
lm.result2 <- lm(Y~C*A*L*M-C:A:L:M-C:L:M-A:L:M)
anova(lm.result2)
lm.result2$coefficients
