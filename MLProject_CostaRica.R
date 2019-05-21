train = read.csv('train.csv',stringsAsFactors=F)
test = read.csv('test.csv',stringsAsFactors=F)

View(train)
nrow(train)
ncol(train)

head(train)
str(train)

library(dplyr)
library(tidyverse)           

#Majority of columns in our data are categorical variables with a few of them numerical.

#For ease of use, we change the labels of the target to their original labels.
#train$Target[train$Target == 1] = 'Extreme Poverty'
#train$Target[train$Target == 2] = 'Moderate Poverty'
#train$Target[train$Target == 3] = 'Vulnerable Household'
#train$Target[train$Target == 4] = 'Non-Vulnerable Household'

#Data Exploration:

#options(scipen = 99)
train[1:21] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "green")

#Remove hacapo, =1 Overcrowding by rooms
#Remove hacdor, =1 Overcrowding by bedrooms
#Remove v14a, =1 has bathroom in the household

train[22:41] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "green")

#Remove pareddes, =1 if predominant material on the outside wall is waste material
#Remove paredfibras, =1 if predominant material on the outside wall is natural fibers
#Remove paredmad, =1 if predominant material on the outside wall is wood
#Remove paredother, =1 if predominant material on the outside wall is other
#Remove paredzinc, =1 if predominant material on the outside wall is zink
#Remove paredzocalo, "=1 if predominant material on the outside wall is socket (wood, zinc or absbesto"
#Remove pisocemento, =1 if predominant material on the floor is cement
#Remove pisomadera, =1 if predominant material on the floor is wood
#Remove pisonatur, =1 if predominant material on the floor is natural material
#Remove pisonotiene, =1 if no floor at the household
#Remove pisoother, =1 if predominant material on the floor is other
#Remove techocane, =1 if predominant material on the roof is natural fibers
#Remove techoentrepiso, "=1 if predominant material on the roof is fiber cement, mezzanine"
#Remove techootro, =1 if predominant material on the roof is other
#Remove techozinc, =1 if predominant material on the roof is metal foil or zink

train[42:61] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "green")

#Remove abastaguadentro, =1 if water provision inside the dwelling
#Remove abastaguafuera, =1 if water provision outside the dwelling
#Remove abastaguano, =1 if no water provision
#Remove elimbasu2, =1 if rubbish disposal mainly by botan hollow or buried
#Remove elimbasu3, =1 if rubbish disposal mainly by burning
#Remove energcocinar1, =1 no main source of energy used for cooking (no kitchen)
#Remove energcocinar4, =1 main source of energy used for cooking wood charcoal
#Remove planpri, =1 electricity from private plant
#Remove noelec, =1 no electricity in the dwelling
#Remove sanitario1, =1 no toilet in the dwelling
#Remove sanitario5, =1 toilet connected to black hole or letrine
#Remove sanitario6, =1 toilet connected to other system

train[62:82] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "green")

#Remove dis, =1 if disable person
#Remove elimbasu4, =1 if rubbish disposal mainly by throwing in an unoccupied space
#Remove elimbasu5, "=1 if rubbish disposal mainly by throwing in river, creek or sea"
#Remove elimbasu6, =1 if rubbish disposal mainly other
#Remove epared1, =1 if walls are bad
#Remove estadocivil1, =1 if less than 10 years old
#Remove estadocivil2, =1 if free or coupled uunion
#Remove estadocivil4, =1 if divorced
#Remove estadocivil5, =1 if separated
#Remove estadocivil6, =1 if widow/er
#Remove etecho1, =1 if roof are bad
#Remove eviv1, =1 if floor are bad

train[83:102] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "green")

#Remove parentesco4, =1 if stepson/doughter
#Remove parentesco5, =1 if son/doughter in law
#Remove parentesco6, =1 if grandson/doughter
#Remove parentesco7, =1 if mother/father
#Remove parentesco8, =1 if father/mother in law
#Remove parentesco9, =1 if brother/sister
#Remove parentesco10, =1 if brother/sister in law
#Remove parentesco11, =1 if other family member
#Remove parentesco12, =1 if other non family member

train[102:121] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "green")

#Remove instlevel6, =1 incomplete technical secondary level
#Remove instlevel7, =1 complete technical secondary level
#Remove instlevel9, =1 postgraduate higher education
#Remove tipovivi4, =1 precarious
#Remove tipovivi5, "=1 other(assigned, borrowed)"

train[121:142] %>%
  keep(is.numeric) %>%
  gather() %>%                             
  ggplot(aes(x = value)) +                     
  facet_wrap(~ key, scales = "free") +  
  geom_histogram(fill = "green")

#Remove lugar2, =1 region Chorotega
#Remove lugar3, =1 region PacÃfÂfico central
#Remove lugar4, =1 region Brunca
#Remove lugar5, =1 region Huetar AtlÃfÂ¡ntica
#Remove lugar6, =1 region Huetar Norte
#Remove mobilephone, =1 if mobile phone
##############################################################################

#Remove not so important features and convert others into factor.
ncol(train)
library(dplyr)
train<-select(train,-hacdor,-hacapo,-v14a,-v18q1,-rez_esc)
test<-select(test,-hacdor,-hacapo,-v14a,-v18q1,-rez_esc)
#train$refrig<-as.factor(train$refrig)
#train$v18q<-as.factor(train$v18q)

train<-select(train,-pareddes,-paredfibras,-paredmad,-paredother,
                 -paredzinc,-paredzocalo,-pisocemento,-pisomadera,
                 -pisonatur,-pisonotiene,-pisoother,-techocane,
                 -techoentrepiso,-techootro,-techozinc)

test<-select(test,-pareddes,-paredfibras,-paredmad,-paredother,
              -paredzinc,-paredzocalo,-pisocemento,-pisomadera,
              -pisonatur,-pisonotiene,-pisoother,-techocane,
              -techoentrepiso,-techootro,-techozinc)
#train$paredblolad<-as.factor(train$paredblolad)
#train$paredpreb<-as.factor(train$paredpreb)
#train$pisomoscer<-as.factor(train$pisomoscer)

train<-select(train,-abastaguadentro,-abastaguafuera,-abastaguano,
                 -elimbasu2,-elimbasu3,-elimbasu3,-energcocinar1,-energcocinar4,
                 -planpri,-noelec,-sanitario1,-sanitario5,-sanitario6)

test<-select(test,-abastaguadentro,-abastaguafuera,-abastaguano,
              -elimbasu2,-elimbasu3,-elimbasu3,-energcocinar1,-energcocinar4,
              -planpri,-noelec,-sanitario1,-sanitario5,-sanitario6)
#train$cielorazo<-as.factor(train$cielorazo)
#train$coopele<-as.factor(train$coopele)
#train$elimbasu1<-as.factor(train$elimbasu1)
#train$energcocinar2<-as.factor(train$energcocinar2)
#train$energcocinar3<-as.factor(train$energcocinar3)
#train$public<-as.factor(train$public)
#train$sanitario2<-as.factor(train$sanitario2)
#train$sanitario3<-as.factor(train$sanitario3)

train<-select(train,-dis,-elimbasu4,-elimbasu5,-elimbasu6,
                 -epared1,-estadocivil1,-estadocivil2,
                 -estadocivil4,-estadocivil5,-estadocivil6,
                 -etecho1,-eviv1)

test<-select(test,-dis,-elimbasu4,-elimbasu5,-elimbasu6,
              -epared1,-estadocivil1,-estadocivil2,
              -estadocivil4,-estadocivil5,-estadocivil6,
              -etecho1,-eviv1)
#train$epared2<-as.factor(train$epared2)
#train$epared3<-as.factor(train$epared3)
#train$estadocivil3<-as.factor(train$estadocivil3)
#train$etecho2<-as.factor(train$etecho2)
#train$etecho3<-as.factor(train$etecho3)
#train$eviv2<-as.factor(train$eviv2)
#train$eviv3<-as.factor(train$eviv3)
#train$male<-as.factor(train$male)
#train$female<-as.factor(train$female)

train<-select(train,-parentesco4,-parentesco5,-parentesco6,
                 -parentesco7,-parentesco8,-parentesco9,
                 -parentesco10,-parentesco11,-parentesco12)


test<-select(test,-parentesco4,-parentesco5,-parentesco6,
              -parentesco7,-parentesco8,-parentesco9,
              -parentesco10,-parentesco11,-parentesco12)
#train$estadocivil7<-as.factor(train$estadocivil7)
#train$parentesco1<-as.factor(train$parentesco1)
#train$parentesco2<-as.factor(train$parentesco2)
#train$parentesco3<-as.factor(train$parentesco3)

train<-select(train,-instlevel6,-instlevel7,-instlevel9,
                 -tipovivi4,-tipovivi5)

test<-select(test,-instlevel6,-instlevel7,-instlevel9,
              -tipovivi4,-tipovivi5)
#train$computer<-as.factor(train$computer)
#train$instlevel1<-as.factor(train$instlevel1)
#train$instlevel2<-as.factor(train$instlevel2)
#train$instlevel3<-as.factor(train$instlevel3)
#train$instlevel4<-as.factor(train$instlevel4)
#train$instlevel5<-as.factor(train$instlevel5)
#train$instlevel8<-as.factor(train$instlevel8)
#train$tipovivi1<-as.factor(train$tipovivi1)
#train$tipovivi2<-as.factor(train$tipovivi2)
#train$tipovivi3<-as.factor(train$tipovivi3)

train<-select(train,-lugar2,-lugar3,-lugar4,-lugar5,
                 -lugar6,-mobilephone)

test<-select(test,-lugar2,-lugar3,-lugar4,-lugar5,
              -lugar6,-mobilephone)
#train$area1<-as.factor(train$area1)
#train$area2<-as.factor(train$area2)
#train$lugar1<-as.factor(train$lugar1)
#train$television<-as.factor(train$television)
ncol(train)

train %>%
  ggplot(aes(Target))+
  geom_bar(color = 'black', fill = 'tomato')+
  xlab("Target Classes")+
  ylab("Target Classes Count")

#It seems like majority of people in our train come from non-vulnerable households. 
#And the least number comes from extreme poverty households.

#1. Monthly Rent - v2a1
#Logically, the monthly rent of the household can give us indications of the poverty
#of the household. 
#theme - axis.text.t is for axis values
#theme - strip.text -is for strip values(1,2,3,4)
train %>%
  ggplot(aes(x = log1p(v2a1)))+
  geom_histogram(colour = "grey19", fill = "tomato3", bins = 50)+ 
  facet_wrap(~Target, scales = "free", ncol = 3)+ 
  theme(axis.text.x = element_text(hjust = 1, size = 12))+
  theme(strip.text = element_text(size = 10, face = "bold"))+
  labs(title =" Monthly Rent Payment by Target", x = "Monthly Rent", 
       y = "Number of Instances", size = 15)

#Seems like the households in extreme and moderate poverty also pay a high montly 
#rent and infact similar to non-vulnerable households.

##########################################################################
#3. Number of rooms - rooms

train %>%
  ggplot(aes(x = as.factor(rooms)))+
  geom_bar(colour = "grey19", fill = "yellow")+
  facet_wrap(~Target, scales = "free", ncol = 3)+ 
  theme(axis.text.x = element_text(hjust = 1, size = 12))+
  theme(strip.text = element_text(size = 9, face = "bold"))+
  labs(x = "Number of rooms", y = "Count")

#We see that the number of rooms in a household varies with the income bracket 
#of the household. In non-vulnerable households, the highest number of rooms 
#if 5 whereas in extreme and moderate poverty households the number of rooms 
#is maximum 4.

#########################################################################
#4. Refrigerator - refrig

train %>%
  ggplot(aes(x = as.factor(refrig)))+
  geom_bar(colour = "grey19", fill = "orange")+
  facet_wrap(~Target, scales = "free", ncol = 3)+ 
  theme(axis.text.x = element_text(hjust = 1, size = 10))+
  theme(strip.text = element_text(size = 9, face = "bold"))+
  labs(x = "Owns a refrigerator or not?", y = "Count")+
  scale_x_discrete(labels = c("No", "Yes"))

#Interesting! It seems that in low income households too there is a refrigerator.
##############################################################################
#5. Owning a tablet - v18q

train %>%
  filter(!is.na(v18q)) %>%
  ggplot(aes(as.factor(v18q)))+
  geom_bar(aes(fill = as.factor(Target)), position = "dodge", color = "black")+
  labs(x = "Owns a tablet or not?", y = "Count")+
  guides(fill = guide_legend('Household Type'))+
  theme(axis.text.x = element_text(hjust = 1, size = 9))+
  scale_x_discrete(labels = c("No", "Yes"))

#The most interesting thing is that households in extreme poverty own tablets!
###############################################################################
#6. Male distribution - r4h1, r4h2, r4h3
#geom_bar - aplha is for color darkness.

train %>%
  ggplot(aes(as.factor(r4h1)))+
  geom_bar(aes(fill = as.factor(Target)), position = "dodge", color = "black", alpha = 0.7)+
  labs(x = "Number of males younger than 12", y = "Count")+
  guides(fill = guide_legend('Household Type'))+
  theme(axis.text.x = element_text(hjust = 1, size = 9))

#We see that the number of males younger than 12 are high in extreme poverty 
#and non-vulnerable households. 

train %>%
  ggplot(aes(as.factor(r4h2)))+
  geom_bar(aes(fill = as.factor(Target)), position = "dodge", color = "black", alpha = 0.6)+
  labs(x = "Number of males older than 12", y = "Count")+
  guides(fill = guide_legend('Household Type'))+
  theme(axis.text.x = element_text(hjust = 1, size = 9))

#We see that the number of males older than 12 are less in extreme poverty
#bread earners are less in extreme poverty as compare to non vulnerable class.

#Impute all numeric/integer fields with mean values
train$SQBmeaned[is.na(train$SQBmeaned)]<-mean(train$SQBmeaned,na.rm=T)
train$meaneduc[is.na(train$meaneduc)]<-mean(train$meaneduc,na.rm=T)
train$v2a1[is.na(train$v2a1)]<-mean(train$v2a1,na.rm=T)


boxplot(meaneduc~Target, data=train,main="Mean Education Distribution", 
        xlab="Target", ylab="Mean Education")
#This shows that families the least at risk for poverty - non-vulnerable
#tend to have higher average education levels than those most at risk.


boxplot(overcrowding~Target, data=train,main="Occupancy in Rooms Distribution", 
        xlab="Target", ylab="Overcrowding")
#households with more people per room tend to have greater levels of poverty. 


boxplot(age~Target, data=train,main="Age Distribution", 
        xlab="Target", ylab="Age")
#Extreme Poverty households have younger people as compare to Non vulnerable. 

###############################################################################
#7. Female distribution - r4m1, r4m2, r4m3
#8. Person distribution - r4t1, r4t2, r4t3 
# Total number of person younger than 12 are more in extreme and moderate poverty.
################################################################################
#9. Schooling - escolari,
#Coord_polar - pie chart

train %>%
  #filter(Target == 1) %>%
  ggplot(aes(x = as.factor(Target), stat = 'bin', fill = as.factor(escolari)))+
  geom_bar(position = 'fill', color = 'black')+
  coord_polar("y")+
  labs(x = "", y = "")+
  guides(fill = guide_legend("Number of years of schooling"))

# the majority of people in extreme poverty households have 0 years in schooling.
#As we gradually progress towards non-vulnerable household, this pattern reverses.

##################################################################################
#Imputation of values

train$idhogar<-as.numeric(as.factor(train$idhogar))
train$dependency<-as.numeric(as.factor(train$dependency))
train$edjefe<-as.numeric(as.factor(train$edjefe))
train$edjefa<-as.numeric(as.factor(train$edjefa))

test$idhogar<-as.numeric(as.factor(test$idhogar))
test$dependency<-as.numeric(as.factor(test$dependency))
test$edjefe<-as.numeric(as.factor(test$edjefe))
test$edjefa<-as.numeric(as.factor(test$edjefa))

#Impute all numeric/integer fields with mean values
test$SQBmeaned[is.na(test$SQBmeaned)]<-mean(test$SQBmeaned,na.rm=T)
test$meaneduc[is.na(test$meaneduc)]<-mean(test$meaneduc,na.rm=T)
test$v2a1[is.na(test$v2a1)]<-mean(test$v2a1,na.rm=T)
sum(is.na(test))
summary(test)
################################################################################
library(randomForest)
library(caret)

##Splitting train dataset into train and valid dataset.
set.seed(4646)
train_valid.ids <- train %>% select(Id)
train_valid.ids <- sample_frac(train_valid.ids,0.08)
nrow(train_valid.ids)

valid <- train %>% inner_join(train_valid.ids)
train <- train %>% anti_join(train_valid.ids)

nrow(valid)
nrow(train)
train.labels <- train$Target
valid.labels <- valid$Target

train <- train %>% select(-Id,-idhogar,-dependency,-edjefe,-edjefa)
train <- train %>% replace(., is.na(.), -1)
summary(train)
ncol(train)
nrow(train)
str(train)

valid <- valid %>% select(-Id,-idhogar,-dependency,-edjefe,-edjefa)
valid <- valid %>% replace(., is.na(.), -1)
ncol(valid)
nrow(valid)

test <- test %>% select(-Id,-idhogar,-dependency,-edjefe,-edjefa)
test <- test %>% replace(., is.na(.), -1)

#### KNN ##################################################
set.seed(44434)

# Featuring scaling is taken care by algo. It determines the best k value.
trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
knn_fit <- train(Target ~., data = train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

knn_fit
predictions.valid <- round(predict(knn_fit, newdata=valid))
CM<-table(predictions.valid,valid.labels)
CM
accuracy = (sum(diag(CM)))/sum(CM)
print(accuracy)
#0.6352941
Metrics::mae(predictions.valid,valid.labels)
#0.4156863

predictions.test <- predict(knn_fit, test)
head(predictions.test)

sub <- read.csv("sample_submission.csv", stringsAsFactors=F)
sub$Target <- predictions.test
write.csv(sub,"benchmark_knn.csv", row.names=F)


#KNN method with standardized data #####################
library(class)

#Normalizing the data using standardized method. 
data_norm<-function(x) {((x -min(x))/ (max(x) - min(x)))}
train_norm<- as.data.frame(lapply(train[-74],data_norm))
summary(train_norm[,1:4])


valid_norm<- as.data.frame(lapply(valid[-74],data_norm))
summary(valid_norm[,1:4])

test_norm<- as.data.frame(lapply(test,data_norm))
summary(test_norm[,1:4])

knn_model<- knn(train_norm,valid_norm,train.labels,k=5)
knn_model
CM<-table(knn_model,valid.labels)
CM
accuracy = (sum(diag(CM)))/sum(CM)
print(accuracy)
#0.7137255

prediction_test_knn<- knn(train_norm,test_norm,train.labels,k=5)

sub1 <- read.csv("sample_submission.csv", stringsAsFactors=F)
sub1$Target <- prediction_test_knn
write.csv(sub,"benchmark_knn_test.csv", row.names=F)

######### K=88 gives 0.64 accuracy. K=5 gives 0.71 accuracy ##############

### Knn method non standard data ####################
knn_model<- knn(train,valid,train.labels,k=5)
CM<-table(knn_model,valid.labels)
CM
accuracy = (sum(diag(CM)))/sum(CM)
print(accuracy)
#### Less accuracy - around 0.58 for k=5

##### Random Forest #########################################
rnd_fit <- randomForest(Target ~ .,
                        data=train, 
                        importance=TRUE, 
                        ntree=1001)


importance(rnd_fit)
varImpPlot(rnd_fit,pch=18,col='red',cex=1.5)
predictions.valid.rf <- round(predict(rnd_fit, valid))

CM<-table(predictions.valid.rf,valid.labels)
CM
accuracy = (sum(diag(CM)))/sum(CM)
print(accuracy)
Metrics::mae(predictions.valid.rf,valid.labels)



predictions.test.rf <- round(predict(rnd_fit,test))
sub <- read.csv("sample_submission.csv", stringsAsFactors=F)
sub$Target <- predictions.test.rf
write.csv(sub,"benchmark_rf.csv", row.names=F)

############## XGBoost #########################
library(xgboost)
train_x <- train[,-74] 
valid_x <- valid[,-74] 

train_dm_freq <- xgb.DMatrix(
  data = as.matrix(train_x), 
  label = train[,"Target"]-1,
  missing = "NAN")

valid_dm_freq <- xgb.DMatrix(
  data = as.matrix(valid_x), 
  label = valid[,"Target"]-1,
  missing = "NAN")

test_dm_freq <- xgb.DMatrix(
  data = as.matrix(test), 
  missing = "NAN")


# Train XGBoost model 
nc<-length(unique(train[,"Target"]))

set.seed(1234)
xgb1_freq <- xgb.train(
  data = train_dm_freq, 
  watchlist = list( train = train_dm_freq, valid = valid_dm_freq), 
  eval_metric = "mlogloss",
  num_class=4,
  objective = "multi:softmax",
  eta = 0.2,
  max_depth = 8,
  subsample = 0.8,
  colsample_bytree = 0.8,
  nrounds = 200,
  early.stop.round = 50,
  print_every_n = 50
)
e<-data.frame(xgb1_freq$evaluation_log)
plot(e$iter,e$train_mlogloss,col='blue')
lines(e$iter,e$valid_mlogloss,col='red')

#to know which iteration has minimum error
min(e$valid_mlogloss)

#feature importance 
xgb_imp_freq <- xgb.importance(feature_names = colnames(train_x), model = xgb1_freq)
xgb.plot.importance(xgb_imp_freq)
print(head(xgb_imp_freq,30))

#prediction
predictions_xgb_valid <- predict(xgb1_freq, newdata=valid_dm_freq)
CM<-table(predictions_xgb_valid+1,valid[,"Target"])
CM
accuracy = (sum(diag(CM)))/sum(CM)
print(accuracy)
Metrics::mae(predictions_xgb_valid+1,valid[,"Target"])

predictions_xgb_test <- predict(xgb1_freq, newdata=test_dm_freq)
sub <- read.csv("sample_submission.csv", stringsAsFactors=F)
sub$Target <- predictions_xgb_test
write.csv(sub,"benchmark_xgboost.csv", row.names=F)
