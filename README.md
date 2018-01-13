# Tookitaki test question

```

library(randomForest)  #classification model
library(tree)  #classification model
library(pROC)  #AUC
library(gains)  #variable comparison
library(lattice)  #visualisation
library(gbm)  #classification model

###

train <- read.csv('...train70.csv',header=T)  #csv doc with only variables of interest, training set
#names(train)
#head(train)
n <- nrow(train)  #number of observations

y <- train[,2]
#table(y)
variable <- train[,3:ncol(train)]
p <- ncol(variable)  #number of variables
#names(variable)
#head(variable)

test <- read.csv('...test30.csv',header=T)  #csv doc with only variables of interest, test set
m <- nrow(test)

test_y <- test[,2]
test_var <- test[,3:ncol(train)]
q <- ncol(test_var)

for (j in 1:length(variable)) {   #ensure all variables are continuous 
  variable[,j] <- as.numeric(variable[,j])
  test_var[,j] <- as.numeric(test_var[,j])
  }
  
### 

#exploratory

#correlation plot
cor_mat_covariates = cor(variable)
rgb.palette <- colorRampPalette(c("red", "white", "blue"), space = "rgb")
levelplot(cor_mat_covariates,xlab="",ylab="",col.regions=rgb.palette(200), cuts=100, at=seq(-1,1,0.01))

plot(variable[,5],variable[,6],xlab='mean credit limit',ylab='mean cash limit')
lines(lowess(variable[,5],variable[,6]))

plot(variable[,16],variable[,17],xlab='enquiry recency 365',ylab='enquiry recency 90')
lines(lowess(variable[,16],variable[,17]))

#boxplots
par(mfrow=c(3,3))
for (k in 1:9) {
  boxplot(variable[,k],main=names(variable)[k])
  }
for (k in 10:17) {
  boxplot(variable[,k],main=names(variable)[k])
  }

### 

#selection

#computing gain of each variable
gain <- data.frame(var_index=1:p,var_gain=0)
for (i in 1:p) {
  x <- variable[,i]
  test_x <- test_var[,i]
  fit <- randomForest(y~x,ntree=1000)
  fit.pred <- predict(fit,newdata=data.frame(x=test_x))
  fit.gain <- gains(test_y,fit.pred,groups=10)
  x_gain <- fit.gain$cume.lift[1]/(n/10)
  gain[i,2] <- x_gain
  }

gain[order(gain$var_gain,decreasing=T),]

###

#model fitting

#remove unwanted variable
var_s <- data.frame(variable[,-3])
#names(var_s)
#head(var_s)
test_var <- test_var[,-3]

#random forest
myfit <- randomForest(y~.,data=var_s,ntree=1000,importance=TRUE)
mypred <- predict(myfit,newdata=test_var)

p <- plot.roc(test_y,mypred,lwd=3,print.auc=TRUE,print.auc.y=0.3,main='AUC')
gini <- p$auc*2-1

#gradient boosting
myfit2 <- gbm(y~.,data=var_s,n.trees=100,interaction.depth=4,cv.folds=5,shrinkage=0.005)
mypred2 <- predict(myfit2,newdata=test_var,type='response')
p2 <- plot.roc(test_y,mypred2,lwd=3,print.auc=TRUE,print.auc.y=0.25,col=2,add=T)
gini2 <- p2$auc*2-1
legend(0.95,0.9,legend=c('random forest','gradient boosting'),col=c(1,2),lty=c(1,1))

###

prob_order <- order(mypred2,decreasing=T)  #sort customers' bad indicator by probabilities
test_y_ordered <- test_y[prob_order]
m/10

#proportion of bad in each decile
mean(test_y_ordered[1:1024])
mean(test_y_ordered[1025:2048])
mean(test_y_ordered[2049:3072])
mean(test_y_ordered[3073:4096])
mean(test_y_ordered[4097:5120])
mean(test_y_ordered[5121:6144])
mean(test_y_ordered[6145:7168])
mean(test_y_ordered[7169:8192])
mean(test_y_ordered[8193:9216])
mean(test_y_ordered[9217:10240])

#end
