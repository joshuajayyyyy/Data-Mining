library(rpart)
library(rpart.plot)
library(forecast)


toyota<- read.csv("ToyotaCorolla.csv")


train_index <-sample(1:nrow(toyota), 0.6*nrow(toyota))
valid_index <-setdiff(1:nrow(toyota), train_index)

train <-toyota[train_index, ]
valid <-toyota[valid_index, ]

nrow(train)
nrow(valid)

set.seed(444)

##Regression

names(toyota)


toyota <- toyota[ , c(3:4, 7:9, 12, 14, 17, 19, 21, 25:26, 28, 30, 34, 39)]
names(toyota)

regress_tr <- rpart(Price ~ Age_08_04 + KM 
                    + Fuel_Type + HP 
                    + Automatic + Doors 
                    + Quarterly_Tax 
                    + Mfr_Guarantee 
                    + Guarantee_Period 
                    + Airco + Automatic_airco
                    + CD_Player + Powered_Windows
                    + Sport_Model + Tow_Bar,
                    data = train, method = "anova", maxdepth = 20)
prp(regress_tr)

new_record <- data.frame(Age_08_04 = 77, 
                         KM = 117000, 
                         Fuel_Type = "Petrol", 
                         HP = 110, 
                         Automatic = 0, 
                         Doors = 5, 
                         Quarterly_Tax = 100, 
                         Mfr_Guarantee = 0, 
                         Guarantee_Period = 3,         
                         Airco = 1, 
                         Automatic_airco = 0, 
                         CD_Player = 0, 
                         Powered_Windows = 0, 
                         Sport_Model = 0, 
                         Tow_Bar = 1)



regress_tr_pred <- predict(regress_tr, newdata = new_record)
regress_tr_pred

## Classification

toyota$cat_price <- ifelse(toyota$Price <= mean(toyota$Price, na.rm = TRUE), 0, 1)


class_tr <- rpart(Price ~ Age_08_04 + KM 
                    + Fuel_Type + HP 
                    + Automatic + Doors 
                    + Quarterly_Tax 
                    + Mfr_Guarantee 
                    + Guarantee_Period 
                    + Airco + Automatic_airco
                    + CD_Player + Powered_Windows
                    + Sport_Model + Tow_Bar,
                    data = train, method = "class", maxdepth = 20)
prp(class_tr,cex=0.8, tweak=1)


new_recordclass <- data.frame(Age_08_04 = 77, 
                         KM = 117000, 
                         Fuel_Type = "Petrol", 
                         HP = 110, 
                         Automatic = 0, 
                         Doors = 5, 
                         Quarterly_Tax = 100, 
                         Mfr_Guarantee = 0, 
                         Guarantee_Period = 3,         
                         Airco = 1, 
                         Automatic_airco = 0, 
                         CD_Player = 0, 
                         Powered_Windows = 0, 
                         Sport_Model = 0, 
                         Tow_Bar = 1)

class_tr_pred <- predict(class_tr, newdata = new_recordclass)
class_tr_pred

