###2019pitching###
library(tidyverse)
p2019 <- read_csv("2019_pitching.csv")

p2019_10w <- p2019 %>%
  filter(salary < 1000000)
p2019_100w <- p2019 %>%
  filter(salary >= 1000000 & salary < 10000000)
p2019_1000w <- p2019 %>%
  filter(salary >= 10000000)

library(corrplot)
cor2019p_10 = cor(p2019_10w[,c(4,5:14)])
corrplot(cor2019p_10,method = "number")
cor2019p_100 = cor(p2019_100w[,c(4,5:14)])
corrplot(cor2019p_100,method = "number")
cor2019p_1000 = cor(p2019_1000w[,c(4,5:14)])
corrplot(cor2019p_1000,method = "number")
cor2019p = cor(p2019[,c(4,5:14)])
corrplot(cor2019p,method = "number")

library(broom)
reg2019p_10w <- lm(salary ~ GP + IP + H + BB + SO + W + L + WAR + ERA + age, data = p2019_10w)
summary(reg2019p_10w)
reg2019p_100w <- lm(salary ~ GP +IP + H + BB + SO + W + L + WAR + ERA + age, data = p2019_100w)
summary(reg2019p_100w)
reg2019p_1000w <- lm(salary ~ GP + IP + H + BB + SO + W + L + WAR + ERA + age, data = p2019_1000w)
summary(reg2019p_1000w)
reg2019p <- lm(salary ~ GP + IP + H + BB + SO + W + L + WAR + ERA + age, data = p2019)
summary(reg2019p)

pca2019p_10w = prcomp(x=p2019_10w[,-c(1:4)])
var_2019p_10w =(pca2019p_10w$sdev)^2/sum((pca2019p_10w$sdev)^2)
plot(cumsum(var_2019p_10w), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)
pca2019p_100w = prcomp(x=p2019_100w[,-c(1:4)])
var_2019p_100w=(pca2019p_100w$sdev)^2/sum((pca2019p_100w$sdev)^2)
plot(cumsum(var_2019p_100w), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)
pca2019p_1000w = prcomp(x=p2019_1000w[,-c(1:4)])
var_2019p_1000w=(pca2019p_1000w$sdev)^2/sum((pca2019p_1000w$sdev)^2)
plot(cumsum(var_pca2019p_1000w), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)
pca2019p = prcomp(x=p2019[,-c(1:4)])
var_2019p=(pca2019p$sdev)^2/sum((pca2019p$sdev)^2)
plot(cumsum(var_2019p), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)

pca2019p_10w = prcomp(p2019_10w[,-c(1:4)], center = TRUE, scale = TRUE)
pca2019p_10w$rotation#Loadings
biplot(pca2019p_10w)
pca2019p_100w = prcomp(p2019_100w[,-c(1:4)], center = TRUE, scale = TRUE)
pca2019p_100w$rotation
biplot(pca2019p_100w)
pca2019p_1000w = prcomp(p2019_1000w[,-c(1:4)], center = TRUE, scale = TRUE)
pca2019p_1000w$rotation
biplot(pca2019p_1000w)
pca2019p = prcomp(p2019[,-c(1:4)], center = TRUE, scale = TRUE)
pca2019p$rotation
biplot(pca2019p)

top3pc2019p_10w = pca2019p_10w$rotation[,1:3]
pc_var2019p_10w = list()
for(i in 1:3){
  order = order(abs(top3pc2019p_10w[,i]),decreasing = TRUE)
  pc_var2019p_10w[[i]] = top3pc2019p_10w[,i][order][1:5]
}
top3pc2019p_100w = pca2019p_100w$rotation[,1:3]
pc_var2019p_100w = list()
for(i in 1:3){
  order = order(abs(top3pc2019p_100w[,i]),decreasing = TRUE)
  pc_var2019p_100w[[i]] = top3pc2019p_100w[,i][order][1:5]
}
top3pc2019p_1000w = pca2019p_1000w$rotation[,1:3]
pc_var2019p_1000w = list()
for(i in 1:3){
  order = order(abs(top3pc2019p_1000w[,i]),decreasing = TRUE)
  pc_var2019p_1000w[[i]] = top3pc2019p_1000w[,i][order][1:5]
}
top3pc2019p = pca2019p$rotation[,1:3]
pc_var2019p = list()
for(i in 1:3){
  order = order(abs(top3pc2019p[,i]),decreasing = TRUE)
  pc_var2019p[[i]] = top3pc2019p[,i][order][1:5]
}
#pc_var2019p_10w
pc_var2019p_10w[[1]]
pc_var2019p_10w[[2]]
#pc_var2019p_100w
pc_var2019p_100w[[1]]
pc_var2019p_100w[[2]]
#pc_var2019p_1000w
pc_var2019p_1000w[[1]]
pc_var2019p_1000w[[2]]
#pc_var2019p
pc_var2019p[[1]]
pc_var2019p[[2]]

#------------------------------------------------------------------------#
###2018pitching###
p2018 <- read_csv("2018_pitching.csv")

p2018_10w <- p2018 %>%
  filter(salary < 1000000)
p2018_100w <- p2018 %>%
  filter(salary >= 1000000 & salary < 10000000)
p2018_1000w <- p2018 %>%
  filter(salary >= 10000000)

library(corrplot)
cor2018p_10 = cor(p2018_10w[,c(4,5:14)])
corrplot(cor2018p_10,method = "number")
cor2018p_100 = cor(p2018_100w[,c(4,5:14)])
corrplot(cor2018p_100,method = "number")
cor2018p_1000 = cor(p2018_1000w[,c(4,5:14)])
corrplot(cor2018p_1000,method = "number")
cor2018p = cor(p2018[,c(4,5:14)])
corrplot(cor2018p,method = "number")

library(broom)
reg2018p_10w <- lm(salary ~ GP + IP + H + BB + SO + W + L + WAR + ERA + age, data = p2018_10w)
summary(reg2018p_10w)
reg2018p_100w <- lm(salary ~ GP +IP + H + BB + SO + W + L + WAR + ERA + age, data = p2018_100w)
summary(reg2018p_100w)
reg2018p_1000w <- lm(salary ~ GP + IP + H + BB + SO + W + L + WAR + ERA + age, data = p2018_1000w)
summary(reg2018p_1000w)
reg2018p <- lm(salary ~ GP + IP + H + BB + SO + W + L + WAR + ERA + age, data = p2018)
summary(reg2018p)

pca2018p_10w = prcomp(x=p2018_10w[,-c(1:4)])
var_2018p_10w=(pca2018p_10w$sdev)^2/sum((pca2018p_10w$sdev)^2)
plot(cumsum(var_2018p_10w), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)
pca2018p_100w = prcomp(x=p2018_100w[,-c(1:4)])
var_2018p_100w=(pca2018p_100w$sdev)^2/sum((pca2018p_100w$sdev)^2)
plot(cumsum(var_2018p_100w), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)
pca2018p_1000w = prcomp(x=p2018_1000w[,-c(1:4)])
var_2018p_1000w=(pca2018p_1000w$sdev)^2/sum((pca2018p_1000w$sdev)^2)
plot(cumsum(var_2018p_1000w), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)
pca2018p = prcomp(x=p2018[,-c(1:4)])
var_2018p=(pca2018p$sdev)^2/sum((pca2018p$sdev)^2)
plot(cumsum(var_2018p), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)

pca2018p_10w = prcomp(p2018_10w[,-c(1:4)], center = TRUE, scale = TRUE)
pca2018p_10w$rotation#Loadings
biplot(pca2018p_10w)
pca2018p_100w = prcomp(p2018_100w[,-c(1:4)], center = TRUE, scale = TRUE)
pca2018p_100w$rotation
biplot(pca2018p_100w)
pca2018p_1000w = prcomp(p2018_1000w[,-c(1:4)], center = TRUE, scale = TRUE)
pca2018p_1000w$rotation
biplot(pca2018p_1000w)
pca2018p = prcomp(p2018[,-c(1:4)], center = TRUE, scale = TRUE)
pca2018p$rotation
biplot(pca2018p)

top3pc2018p_10w = pca2018p_10w$rotation[,1:3]
pc_var2018p_10w = list()
for(i in 1:3){
  order = order(abs(top3pc2018p_10w[,i]),decreasing = TRUE)
  pc_var2018p_10w[[i]] = top3pc2018p_10w[,i][order][1:5]
}
top3pc2018p_100w = pca2018p_100w$rotation[,1:3]
pc_var2018p_100w = list()
for(i in 1:3){
  order = order(abs(top3pc2018p_100w[,i]),decreasing = TRUE)
  pc_var2018p_100w[[i]] = top3pc2018p_100w[,i][order][1:5]
}
top3pc2018p_1000w = pca2018p_1000w$rotation[,1:3]
pc_var2018p_1000w = list()
for(i in 1:3){
  order = order(abs(top3pc2018p_1000w[,i]),decreasing = TRUE)
  pc_var2018p_1000w[[i]] = top3pc2018p_1000w[,i][order][1:5]
}
top3pc2018p = pca2018p$rotation[,1:3]
pc_var2018p = list()
for(i in 1:3){
  order = order(abs(top3pc2018p[,i]),decreasing = TRUE)
  pc_var2018p[[i]] = top3pc2018p[,i][order][1:5]
}
#pc_var2018p_10w
pc_var2018p_10w[[1]]
pc_var2018p_10w[[2]]
#pc_var2018p_100w
pc_var2018p_100w[[1]]
pc_var2018p_100w[[2]]
#pc_var2018p_1000w
pc_var2018p_1000w[[1]]
pc_var2018p_1000w[[2]]
#pc_var2018p
pc_var2018p[[1]]
pc_var2018p[[2]]


#------------------------------------------------------------------------#
###2017pitching###
p2017 <- read_csv("2017_pitching.csv")

p2017_10w <- p2017 %>%
  filter(salary < 1000000)
p2017_100w <- p2017 %>%
  filter(salary >= 1000000 & salary < 10000000)
p2017_1000w <- p2017 %>%
  filter(salary >= 10000000)

library(corrplot)
cor2017p_10 = cor(p2017_10w[,c(4,5:14)])
corrplot(cor2017p_10,method = "number")
cor2017p_100 = cor(p2017_100w[,c(4,5:14)])
corrplot(cor2017p_100,method = "number")
cor2017p_1000 = cor(p2017_1000w[,c(4,5:14)])
corrplot(cor2017p_1000,method = "number")
cor2017p = cor(p2017[,c(4,5:14)])
corrplot(cor2017p,method = "number")

library(broom)
reg2017p_10w <- lm(salary ~ GP + IP + H + BB + SO + W + L + WAR + age, data = p2017_10w)
summary(reg2017p_10w)
reg2017p_100w <- lm(salary ~ GP +IP + H + BB + SO + W + L + WAR + ERA + age, data = p2017_100w)
summary(reg2017p_100w)
reg2017p_1000w <- lm(salary ~ GP + IP + H + BB + SO + W + L + WAR + ERA + age, data = p2017_1000w)
summary(reg2017p_1000w)
reg2017p <- lm(salary ~ GP + IP + H + BB + SO + W + L + WAR + ERA + age, data = p2017)
summary(reg2017p)

pca2017p_10w = prcomp(x=p2017_10w[,-c(1:4)])
var_2017p_10w=(pca2017p_10w$sdev)^2/sum((pca2017p_10w$sdev)^2)
plot(cumsum(var_2017p_10w), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)
pca2017p_100w = prcomp(x=p2017_100w[,-c(1:4)])
var_2017p_100w=(pca2017p_100w$sdev)^2/sum((pca2017p_100w$sdev)^2)
plot(cumsum(var_2017p_100w), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)
pca2017p_1000w = prcomp(x=p2017_1000w[,-c(1:4)])
var_2017p_1000w=(pca2017p_1000w$sdev)^2/sum((pca2017p_1000w$sdev)^2)
plot(cumsum(var_2017p_1000w), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)
pca2017p = prcomp(x=p2017[,-c(1:4)])
var_2017p=(pca2017p$sdev)^2/sum((pca2017p$sdev)^2)
plot(cumsum(var_2017p), type="line", xlab="pc_num", ylab="cumulative_variance_percentage", xaxt="n")
axis(1, at=seq(1,11,by=1), las=2)
abline(h=0.8)

pca2017p_10w = prcomp(p2017_10w[,-c(1:4)], center = TRUE, scale = TRUE)
pca2017p_10w$rotation#Loadings
biplot(pca2017p_10w)
pca2017p_100w = prcomp(p2017_100w[,-c(1:4)], center = TRUE, scale = TRUE)
pca2017p_100w$rotation
biplot(pca2017p_100w)
pca2017p_1000w = prcomp(p2017_1000w[,-c(1:4)], center = TRUE, scale = TRUE)
pca2017p_1000w$rotation
biplot(pca2017p_1000w)
pca2017p = prcomp(p2017[,-c(1:4)], center = TRUE, scale = TRUE)
pca2017p$rotation
biplot(pca2017p)

top3pc2017p_10w = pca2017p_10w$rotation[,1:3]
pc_var2017p_10w = list()
for(i in 1:3){
  order = order(abs(top3pc2017p_10w[,i]),decreasing = TRUE)
  pc_var2017p_10w[[i]] = top3pc2017p_10w[,i][order][1:5]
}
top3pc2017p_100w = pca2017p_100w$rotation[,1:3]
pc_var2017p_100w = list()
for(i in 1:3){
  order = order(abs(top3pc2017p_100w[,i]),decreasing = TRUE)
  pc_var2017p_100w[[i]] = top3pc2017p_100w[,i][order][1:5]
}
top3pc2017p_1000w = pca2017p_1000w$rotation[,1:3]
pc_var2017p_1000w = list()
for(i in 1:3){
  order = order(abs(top3pc2017p_1000w[,i]),decreasing = TRUE)
  pc_var2017p_1000w[[i]] = top3pc2017p_1000w[,i][order][1:5]
}
top3pc2017p = pca2017p$rotation[,1:3]
pc_var2017p = list()
for(i in 1:3){
  order = order(abs(top3pc2017p[,i]),decreasing = TRUE)
  pc_var2017p[[i]] = top3pc2017p[,i][order][1:5]
}
#pc_var2017p_10w
pc_var2017p_10w[[1]]
pc_var2017p_10w[[2]]
#pc_var2017p_100w
pc_var2017p_100w[[1]]
pc_var2017p_100w[[2]]
#pc_var2017p_1000w
pc_var2017p_1000w[[1]]
pc_var2017p_1000w[[2]]
#pc_var2017p
pc_var2017p[[1]]
pc_var2017p[[2]]
