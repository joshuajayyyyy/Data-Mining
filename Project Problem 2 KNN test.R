library(caret)
library(ggplot2)
library(ROSE)
library(tidyr)

cred <- read.csv('credit_8.csv')
names(cred)

cred<- drop_na(cred)

cred_filter <- cred[,c(9,7 ,10:12, 15,3)]

str(cred_filter)

table(cred_filter$TARGET)
nrow(cred_filter)
cred_filter$AMT_INCOME_TOTAL <-as.factor(cred_filter$AMT_INCOME_TOTAL) 
cred_filter$FLAG_OWN_REALTY <-as.factor(cred_filter$FLAG_OWN_REALTY) 
cred_filter$AMT_CREDIT <-as.factor(cred_filter$AMT_CREDIT)
cred_filter$AMT_ANNUITY <-as.factor(cred_filter$AMT_ANNUITY)
cred_filter$AMT_GOODS_PRICE <-as.factor(cred_filter$AMT_GOODS_PRICE) 
cred_filter$NAME_EDUCATION_TYPE <-as.factor(cred_filter$NAME_EDUCATION_TYPE)

cred_filter$TARGET <- factor(cred_filter$TARGET,
                            levels = c('0','1'),
                            labels = c('Yes','No'))
cred_filter$FLAG_OWN_REALTY <- factor(cred_filter$FLAG_OWN_REALTY,
                             levels = c('Y','N'),
                             labels = c('Yes','No'))

table(cred_filter$TARGET)

set.seed(444)

train_index <-sample(1:nrow(cred_filter), 0.6*nrow(cred_filter))
valid_index <-setdiff(1:nrow(cred_filter), train_index)

train <-cred_filter[train_index, ]
valid <-cred_filter[valid_index, ]

t(t(names(train)))
t(t(names(valid)))

nrow(train)
nrow(valid)

NewCust <-data.frame(AMT_INCOME_TOTAL = 100000,
                     FLAG_OWN_REALTY = 'Yes',
                     AMT_CREDIT = 400000,
                     AMT_ANNUITY = 15000,
                     AMT_GOODS_PRICE = 400000,
                     NAME_EDUCATION_TYPE = "Higher education")

NewCust$AMT_INCOME_TOTAL <-as.factor(NewCust$AMT_INCOME_TOTAL) 
NewCust$FLAG_OWN_REALTY <-as.factor(NewCust$FLAG_OWN_REALTY) 
NewCust$AMT_CREDIT <-as.factor(NewCust$AMT_CREDIT)
NewCust$AMT_ANNUITY <-as.factor(NewCust$AMT_ANNUITY)
NewCust$AMT_GOODS_PRICE <-as.factor(NewCust$AMT_GOODS_PRICE) 
NewCust$NAME_EDUCATION_TYPE <-as.factor(NewCust$NAME_EDUCATION_TYPE)

NewCust

train_norm<- train
valid_norm<- valid











