#STAT 6021: Project 1
#Due March 23, 2020
#Team:
# Brad Howlett (bth2g)
# Monish Dadlani
# Sarah Adams
# Brittany Durkin
#--------------------------------------------
data <- read.table('mileage.txt', header = TRUE, sep ='')
attach(data)
data

library(MASS)
library(faraway)

#--------------------------------------------
#1 Initial Model Considered

######### VISUALLY REVIEW ###############
ordered_data <- data[order(-y),] #sort by highest mpg, visually review results
ordered_data

pairs(data, lower.panel = NULL) #graphically view correlation

# x1,x2,x3 appear highly inversely related to y, highly correlated with each other
# x8,x9,x10 appear highly inversely related to y, highly correlated with each other
# x6,x7,x11 categorical, x11 transmission seems highly correlated

is.factor(x11) #yes
x6 <- factor(x6) #based on work below need to change this to a factor
x7 <- factor(x7) #based on work below need to change this to a factor

result_full <- lm(y~.,data=data)
result_full

######### TESTING CORRELATION ###############
cor_var_engine <- cbind(x1,x2,x3)
cor_var_size <- cbind(x8,x9,x10)

round(cor(cor_var_engine),2)
round(cor(cor_var_size),2)

#highest overall is x3 (torque), might include x3 + x10 for different characteristics / need to test for fit against Y not just cor with each other
vif(cor_var_engine)
vif(cor_var_size)

test_result <- lm(y~x1) #higher F stat for x1 than x3 suggesting better fit as a predictor
summary(test_result)

#initial selection of x1, x10, x11

######### INITAL INTUITIVE MODEL ###############
result <- lm(y~x2+x10*x11) #proposed model based on features; interaction is stronger than just additive 
summary(result) #F-statistic: 46.36; Adj R-square: 0.85
anova(result)
BIC(result) #very high - unclear if calculation with interaction is correct
boxcox(result) #1 is in range so no need to transfrom

plot(result$fitted.values, result$residuals)
abline(h=0, col = "red")
acf(result$residuals) #violation at lag of 7/8
qqnorm(result$residuals) 
qqline(result$residuals)

#retesting the ACF violation above
rows <- sample(nrow(data))
data_random <- data[rows,]
data_random

result_random <- lm(data_random$y~data_random$x2+data_random$x10*data_random$x11) 
acf(result_random$residuals) #if you iterate enough you arive at models that satisfy this test


######### BASED ON FORWARD/STEP AUTOMATED SEARCH MODEL ###############

y_log <- log(y) #0 was in range, 1 was not in range of reduced model

result_reduced <- lm(y_log~x1) #proposed model based on features
summary(result_reduced)
anova(result_reduced)
BIC(result_reduced)

plot(result_reduced$fitted.values, result_reduced$residuals)
acf(result_reduced$residuals)
qqnorm(result_reduced$residuals)
qqline(result_reduced$residuals)


######### INCREMENTAL MODEL OUTPUTs ###############

library(leaps)
allreg <- regsubsets(log(y) ~., data=data, nbest=9)

best <- as.data.frame(summary(allreg)$outmat)
best$p <- as.numeric(substr(rownames(best),1,1))+1
best$r2 <- summary(allreg)$rsq
best$adjr2 <- summary(allreg)$adjr2
best$mse <- (summary(allreg)$rss)/(dim(data)[1]-best$p)
best$cp <- summary(allreg)$cp
best$bic <- summary(allreg)$bic
best

best[order(best$r2),]
best[order(-best$adjr2),]
best[order(-best$mse),]
best[order(best$cp),]
best[order(best$bic),]


######### AUTOMATIC SEARCH PROCEDURES FOR MODEL REGRESSORS ###############

new_data <- data
new_data$x6 <- factor(new_data$x6)
new_data$x7 <- factor(new_data$x7)
is.factor(new_data$x7)

regnull_new <- lm(log(y)~1, data=new_data)
regfull_new <- lm(log(y)~., data=new_data)

step(regnull_new, scope=list(lower=regnull_new, upper=regfull_new), direction="forward") #x1
step(regfull_new, scope=list(lower=regnull_new, upper=regfull_new), direction="backward") #x1,x8,x10
step(regnull_new, scope=list(lower=regnull_new, upper=regfull_new), direction="both")


######### GRAPHING CATEGORICAL VALUES ###############

contrasts(x11)
a1<-subset(data,x11=="automatic") 
a2<-subset(data,x11=="manual") 

reg1<-lm(y~x1,data=a1)
reg2<-lm(y~x1,data=a2)

plot(x1,y)
points(a2$x1, a2$y, pch=2, col="red") 
abline(reg1,lty=1)
abline(reg2,lty=2, col="red") 

