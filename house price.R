library(ggplot2)
library(Amelia)
library(caTools)
library(dplyr)
library(corrplot)
library(caret)
library(plotly)
library(reshape)
library(gbm)

data=read.csv("Property Prices in Tunisia.csv")
glimpse(data)

data=na.omit(data)
missmap(data,col=c("yellow",'black'),y.at=1,y.label='',legend =TRUE)

data$category=as.integer(as.factor(data$category))
glimpse(data$category)
unique(data[1])

data$room_count=as.integer(as.factor(data$room_count))
data$bathroom_count=as.integer(as.factor(data$bathroom_count))
data$size=as.integer(as.factor(data$size))
data$type=as.integer(as.factor(data$type))
data$price=as.integer(as.factor(data$price))
data$city=as.integer(as.factor(data$city))
data$region=as.integer(as.factor(data$region))
data$log_price=as.integer(as.factor(data$log_price))

corrplot(cor(data))
data%>% ggplot(aes(price))+stat_density()+theme_bw()
data %>%
  select(c(category, room_count, bathroom_count, size,type,price,city,region)) %>%
  melt(id.vars = "price") %>%
  ggplot(aes(x = value, y = price, colour = variable)) +
  geom_point(alpha = 0.7) +
  stat_smooth(aes(colour = "black")) +
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Variable Value", y = "Median House Price") +
  theme_minimal()

set.seed(123)
split=sample.split(data,SplitRatio = 0.7)
train=subset(data,split==TRUE)
test=subset(data,split==FALSE)
RMSE <- function(x,y){
  a <- sqrt(sum((log(x)-log(y))^2)/length(y))
  return(a)
}

model<-gbm(price ~.,data=train,distribution = "laplace",
           shrinkage = 0.05,interaction.depth = 5,
           bag.fraction = 0.66,n.minobsinnode = 1,
           cv.folds = 100,keep.data = F,verbose = F,
           n.trees = 300)
predict<-predict(model,test,n.trees=300)
result<-RMSE(predict,test$price)
result<-round(result,digits=3)*10

#rf
library(randomForest)
set.seed(101)
data[9]<-NULL
glimpse(data)
str(data)

index=sample(2,nrow(data),replace = TRUE,prob = c(0.7,0.3))

training=data[index==1,]
testing=data[index==2,]
rf=randomForest(price ~ .,data=training)
rf
plot(rf)
price_pred=predict(rf,testing)
testing$price_pred=round(price_pred)

confusionmat =table(testing$price,testing$price_pred)
accu= round(sum(diag(confusionmat)/sum(confusionmat)),digits=3)
accu=accu*100
accu
