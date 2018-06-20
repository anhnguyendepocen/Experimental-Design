library(lmerTest)

# Input data
Block <- rep(c("Wash","Minn","Berk"),each = 3,times = 2)
Exam <- rep(rep(c("1","2","3"),times = 3),times = 2)
Text <- rep(c("A","B"),each = 9)
score <- c(81,79,70,84,81,83,87,82,86,87,85,78,82,81,84,98,93,90)
data.score <- cbind.data.frame(Block,Exam,Text,score)

str(data.score)

temp <- lmerTest::lmer(score~Block+Exam*Text+(1|Block:Exam),data = data.score)
anova(temp)
