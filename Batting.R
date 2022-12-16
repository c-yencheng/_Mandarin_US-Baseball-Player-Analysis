###2019batting###
library(tidyverse)
b2019 <- read_csv("2019_batting.csv")

b2019_10w <- b2019 %>%
  filter(salary < 1000000)
b2019_100w <- b2019 %>%
  filter(salary >= 1000000 & salary < 10000000)
b2019_1000w <- b2019 %>%
  filter(salary >= 10000000)

library(corrplot)
cor2019b_10 = cor(b2019_10w[,c(4,5:16)])
corrplot(cor2019b_10,method = "number")
cor2019b_100 = cor(b2019_100w[,c(4,5:16)])
corrplot(cor2019b_100,method = "number")
cor2019b_1000 = cor(b2019_1000w[,c(4,5:16)])
corrplot(cor2019b_1000,method = "number")
cor2019b = cor(b2019[,c(4,5:16)])
corrplot(cor2019b,method = "number")

library(broom)
reg2019b_10w <- lm(salary ~ AB + R + RBI + SB + CS + BB + SO + AVG + OBP + SLG + WAR + age, data = b2019_10w)
summary(reg2019b_10w)
reg2019b_100w <- lm(salary ~ AB + R + RBI + SB + CS + BB + SO + AVG + OBP + SLG + WAR + age, data = b2019_100w)
summary(reg2019b_100w)
reg2019b_1000w <- lm(salary ~ AB + R + RBI + SB + CS + BB + SO + AVG + OBP + SLG + WAR + age, data = b2019_1000w)
summary(reg2019b_1000w)
reg2019b <- lm(salary ~ AB + R + RBI + SB + CS + BB + SO + AVG + OBP + SLG + WAR + age, data = b2019)
summary(reg2019b)

pca2019b_10w = prcomp(x=b2019_10w[,-c(1:4)])
var_2019b_10w=(pca2019b_10w$sdev)^2/sum((pca2019b_10w$sdev)^2)
plot(cumsum(var_2019b_10w), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)
pca2019b_100w = prcomp(x=b2019_100w[,-c(1:4)])
var_2019b_100w=(pca2019b_100w$sdev)^2/sum((pca2019b_100w$sdev)^2)
plot(cumsum(var_2019b_100w), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)
pca2019b_1000w = prcomp(x=b2019_1000w[,-c(1:4)])
var_2019b_1000w=(pca2019b_1000w$sdev)^2/sum((pca2019b_1000w$sdev)^2)
plot(cumsum(var_2019b_1000w), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)
pca2019b = prcomp(x=b2019[,-c(1:4)])
var_2019b=(pca2019b$sdev)^2/sum((pca2019b$sdev)^2)
plot(cumsum(var_2019b), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)

pca2019b_10w = prcomp(b2019_10w[,-c(1:4)], center = TRUE, scale = TRUE)
pca2019b_10w$rotation#Loadings
biplot(pca2019b_10w)
pca2019b_100w = prcomp(b2019_100w[,-c(1:4)], center = TRUE, scale = TRUE)
pca2019b_100w$rotation
biplot(pca2019b_100w)
pca2019b_1000w = prcomp(b2019_1000w[,-c(1:4)], center = TRUE, scale = TRUE)
pca2019b_1000w$rotation
biplot(pca2019b_1000w)
pca2019b = prcomp(b2019[,-c(1:4)], center = TRUE, scale = TRUE)
pca2019b$rotation
biplot(pca2019b)

top3pc2019b_10w = pca2019b_10w$rotation[,1:3]
pc_var2019b_10w = list()
for(i in 1:3){
  order = order(abs(top3pc2019b_10w[,i]),decreasing = TRUE)
  pc_var2019b_10w[[i]] = top3pc2019b_10w[,i][order][1:5]
}
top3pc2019b_100w = pca2019b_100w$rotation[,1:3]
pc_var2019b_100w = list()
for(i in 1:3){
  order = order(abs(top3pc2019b_100w[,i]),decreasing = TRUE)
  pc_var2019b_100w[[i]] = top3pc2019b_100w[,i][order][1:5]
}
top3pc2019b_1000w = pca2019b_1000w$rotation[,1:3]
pc_var2019b_1000w = list()
for(i in 1:3){
  order = order(abs(top3pc2019b_1000w[,i]),decreasing = TRUE)
  pc_var2019b_1000w[[i]] = top3pc2019b_1000w[,i][order][1:5]
}
top3pc2019b = pca2019b$rotation[,1:3]
pc_var2019b = list()
for(i in 1:3){
  order = order(abs(top3pc2019b[,i]),decreasing = TRUE)
  pc_var2019b[[i]] = top3pc2019b[,i][order][1:5]
}
#pc_var2019b_10w
pc_var2019b_10w[[1]]
pc_var2019b_10w[[2]]
#pc_var2019b_100w
pc_var2019b_100w[[1]]
pc_var2019b_100w[[2]]
#pc_var2019b_1000w
pc_var2019b_1000w[[1]]
pc_var2019b_1000w[[2]]
#pc_var2019b
pc_var2019b[[1]]
pc_var2019b[[2]]

#------------------------------------------------------------------------#
###2018batting###
b2018 <- read_csv("2018_batting.csv")

b2018_10w <- b2018 %>%
  filter(salary < 1000000)
b2018_100w <- b2018 %>%
  filter(salary >= 1000000 & salary < 10000000)
b2018_1000w <- b2018 %>%
  filter(salary >= 10000000)

library(corrplot)
cor2018b_10 = cor(b2018_10w[,c(4,5:16)])
corrplot(cor2018b_10,method = "number")
cor2018b_100 = cor(b2018_100w[,c(4,5:16)])
corrplot(cor2018b_100,method = "number")
cor2018b_1000 = cor(b2018_1000w[,c(4,5:16)])
corrplot(cor2018b_1000,method = "number")
cor2018b = cor(b2018[,c(4,5:16)])
corrplot(cor2018b,method = "number")

library(broom)
reg2018b_10w <- lm(salary ~ AB + R + RBI + SB + CS + BB + SO + AVG + OBP + SLG + WAR + age, data = b2018_10w)
summary(reg2018b_10w)
reg2018b_100w <- lm(salary ~ AB + R + RBI + SB + CS + BB + SO + AVG + OBP + SLG + WAR + age, data = b2018_100w)
summary(reg2018b_100w)
reg2018b_1000w <- lm(salary ~ AB + R + RBI + SB + CS + BB + SO + AVG + OBP + SLG + WAR + age, data = b2018_1000w)
summary(reg2018b_1000w)
reg2018b <- lm(salary ~ AB + R + RBI + SB + CS + BB + SO + AVG + OBP + SLG + WAR + age, data = b2018)
summary(reg2018b)

pca2018b_10w = prcomp(x=b2018_10w[,-c(1:4)])
var_2018b_10w=(pca2018b_10w$sdev)^2/sum((pca2018b_10w$sdev)^2)
plot(cumsum(var_2018b_10w), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)
pca2018b_100w = prcomp(x=b2018_100w[,-c(1:4)])
var_2018b_100w=(pca2018b_100w$sdev)^2/sum((pca2018b_100w$sdev)^2)
plot(cumsum(var_2018b_100w), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)
pca2018b_1000w = prcomp(x=b2018_1000w[,-c(1:4)])
var_2018b_1000w=(pca2018b_1000w$sdev)^2/sum((pca2018b_1000w$sdev)^2)
plot(cumsum(var_2018b_1000w), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)
pca2019b = prcomp(x=b2018[,-c(1:4)])
var_2019b=(pca2018b$sdev)^2/sum((pca2018b$sdev)^2)
plot(cumsum(var_2019b), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)

pca2018b_10w = prcomp(b2018_10w[,-c(1:4)], center = TRUE, scale = TRUE)
pca2018b_10w$rotation#Loadings
biplot(pca2018b_10w)
pca2018b_100w = prcomp(b2018_100w[,-c(1:4)], center = TRUE, scale = TRUE)
pca2018b_100w$rotation
biplot(pca2018b_100w)
pca2018b_1000w = prcomp(b2018_1000w[,-c(1:4)], center = TRUE, scale = TRUE)
pca2018b_1000w$rotation
biplot(pca2018b_1000w)
pca2018b = prcomp(b2018[,-c(1:4)], center = TRUE, scale = TRUE)
pca2018b$rotation
biplot(pca2018b)

top3pc2018b_10w = pca2018b_10w$rotation[,1:3]
pc_var2018b_10w = list()
for(i in 1:3){
  order = order(abs(top3pc2018b_10w[,i]),decreasing = TRUE)
  pc_var2018b_10w[[i]] = top3pc2018b_10w[,i][order][1:5]
}
top3pc2018b_100w = pca2018b_100w$rotation[,1:3]
pc_var2018b_100w = list()
for(i in 1:3){
  order = order(abs(top3pc2018b_100w[,i]),decreasing = TRUE)
  pc_var2018b_100w[[i]] = top3pc2018b_100w[,i][order][1:5]
}
top3pc2018b_1000w = pca2018b_1000w$rotation[,1:3]
pc_var2018b_1000w = list()
for(i in 1:3){
  order = order(abs(top3pc2018b_1000w[,i]),decreasing = TRUE)
  pc_var2018b_1000w[[i]] = top3pc2018b_1000w[,i][order][1:5]
}
top3pc2018b = pca2018b$rotation[,1:3]
pc_var2018b = list()
for(i in 1:3){
  order = order(abs(top3pc2018b[,i]),decreasing = TRUE)
  pc_var2018b[[i]] = top3pc2018b[,i][order][1:5]
}
#pc_var2018b_10w
pc_var2018b_10w[[1]]
pc_var2018b_10w[[2]]
#pc_var2018b_100w
pc_var2018b_100w[[1]]
pc_var2018b_100w[[2]]
#pc_var2018b_1000w
pc_var2018b_1000w[[1]]
pc_var2018b_1000w[[2]]
#pc_var2018b
pc_var2018b[[1]]
pc_var2018b[[2]]

#------------------------------------------------------------------------#
###2017batting###
b2017 <- read_csv("2017_batting.csv")

b2017_10w <- b2017 %>%
  filter(salary < 1000000)
b2017_100w <- b2017 %>%
  filter(salary >= 1000000 & salary < 10000000)
b2017_1000w <- b2017 %>%
  filter(salary >= 10000000)

library(corrplot)
cor2017b_10 = cor(b2017_10w[,c(4,5:16)])
corrplot(cor2017b_10,method = "number")
cor2017b_100 = cor(b2017_100w[,c(4,5:16)])
corrplot(cor2017b_100,method = "number")
cor2017b_1000 = cor(b2017_1000w[,c(4,5:16)])
corrplot(cor2017b_1000,method = "number")
cor2017b = cor(b2017[,c(4,5:16)])
corrplot(cor2017b,method = "number")

library(broom)
library(car)
reg2017b_10w <- lm(salary ~ AB + R + RBI + SB + CS + BB + SO + AVG + OBP + SLG + WAR + age, data = b2017_10w)
summary(reg2017b_10w)
reg2017b_100w <- lm(salary ~ AB + R + RBI + SB + CS + BB + SO + AVG + OBP + SLG + WAR + age, data = b2017_100w)
summary(reg2017b_100w)
reg2017b_1000w <- lm(salary ~ AB + R + RBI + SB + CS + BB + SO + AVG + OBP + SLG + WAR + age, data = b2017_1000w)
summary(reg2017b_1000w)
reg2017b <- lm(salary ~ AB + R + RBI + SB + CS + BB + SO + AVG + OBP + SLG + WAR + age, data = b2017)
summary(reg2017b)

pca2017b_10w = prcomp(x=b2017_10w[,-c(1:4)])
var_2017b_10w=(pca2017b_10w$sdev)^2/sum((pca2017b_10w$sdev)^2)
plot(cumsum(var_2017b_10w), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)
pca2017b_100w = prcomp(x=b2017_100w[,-c(1:4)])
var_2017b_100w=(pca2017b_100w$sdev)^2/sum((pca2017b_100w$sdev)^2)
plot(cumsum(var_2017b_100w), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)
pca2017b_1000w = prcomp(x=b2017_1000w[,-c(1:4)])
var_2017b_1000w=(pca2017b_1000w$sdev)^2/sum((pca2017b_1000w$sdev)^2)
plot(cumsum(var_2017b_1000w), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)
pca2019b = prcomp(x=b2017[,-c(1:4)])
var_2019b=(pca2017b$sdev)^2/sum((pca2017b$sdev)^2)
plot(cumsum(var_2019b), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)

pca2017b_10w = prcomp(b2017_10w[,-c(1:4)], center = TRUE, scale = TRUE)
pca2017b_10w$rotation#Loadings
biplot(pca2017b_10w)
pca2017b_100w = prcomp(b2017_100w[,-c(1:4)], center = TRUE, scale = TRUE)
pca2017b_100w$rotation
biplot(pca2017b_100w)
pca2017b_1000w = prcomp(b2017_1000w[,-c(1:4)], center = TRUE, scale = TRUE)
pca2017b_1000w$rotation
biplot(pca2017b_1000w)
pca2017b = prcomp(b2017[,-c(1:4)], center = TRUE, scale = TRUE)
ca2017b$rotation
biplot(pca2017b)

top3pc2017b_10w = pca2017b_10w$rotation[,1:3]
pc_var2017b_10w = list()
for(i in 1:3){
  order = order(abs(top3pc2017b_10w[,i]),decreasing = TRUE)
  pc_var2017b_10w[[i]] = top3pc2017b_10w[,i][order][1:5]
}
top3pc2017b_100w = pca2017b_100w$rotation[,1:3]
pc_var2017b_100w = list()
for(i in 1:3){
  order = order(abs(top3pc2017b_100w[,i]),decreasing = TRUE)
  pc_var2017b_100w[[i]] = top3pc2017b_100w[,i][order][1:5]
}
top3pc2017b_1000w = pca2017b_1000w$rotation[,1:3]
pc_var2017b_1000w = list()
for(i in 1:3){
  order = order(abs(top3pc2017b_1000w[,i]),decreasing = TRUE)
  pc_var2017b_1000w[[i]] = top3pc2017b_1000w[,i][order][1:5]
}
top3pc2017b = pca2017b$rotation[,1:3]
pc_var2017b = list()
for(i in 1:3){
  order = order(abs(top3pc2017b[,i]),decreasing = TRUE)
  pc_var2017b[[i]] = top3pc2017b[,i][order][1:5]
}
#pc_var2017b_10w
pc_var2017b_10w[[1]]
pc_var2017b_10w[[2]]
#pc_var2017b_100w
pc_var2017b_100w[[1]]
pc_var2017b_100w[[2]]
#pc_var2017b_1000w
pc_var2017b_1000w[[1]]
pc_var2017b_1000w[[2]]
#pc_var2017b
pc_var2017b[[1]]
pc_var2017b[[2]]

