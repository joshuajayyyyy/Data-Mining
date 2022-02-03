library(corrgram)
library(ggplot2)
library(ggpubr)
library(forecast)


fares<- read.csv("Airfares.csv", header = TRUE)
head(fares,10)

names(fares)

str(fares)

summary(fares)

fares<- fares[,-c(1:4)]
names(fares)

set.seed(444)

train_index <- sample(1:nrow(fares), 0.6 * nrow(fares)) 
valid_index <- setdiff(1:nrow(fares), train_index) 

train_df <- fares[train_index, ] 
valid_df <- fares[valid_index, ] 

nrow(train_df)

nrow(valid_df)

corrgram(train_df)

names(train_df)

ggplot(data = train_df) + aes(x = DISTANCE, y = FARE) + 
  geom_point() + 
  ggtitle("Scatter Plot of Fare vs Distance") + 
  geom_smooth(method=lm, se=TRUE) + 
  stat_cor(method = "pearson", label.x = 2000, label.y = 3.8) 

Fare_Model <- lm(FARE ~ ., data = train_df) 
summary(Fare_Model)

Fare_Model_pred <- predict(Fare_Model, valid_df) 
accuracy(Fare_Model_pred, valid_df$FARE)

sd(valid_df$FARE)

new_record <- data.frame(COUPON = 1.202, NEW = 3,  
                         VACATION = "No", SW = "No",  
                         HI = 4442.141, S_INCOME = 28760,  
                         E_INCOME = 27664, S_POP = 4557004,  
                         E_POP = 3195503, SLOT = "Free",  
                         GATE = "Free", PAX = 12782,  
                         DISTANCE = 1976) 
new_record_pred <- predict(Fare_Model, new_record) 

new_record_pred 
