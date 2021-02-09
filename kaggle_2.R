#Sourabh_Kaggle_model

library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(psych)
library(formattable)
library(moments)
library(glmnet)

house_train <- read.csv("C:\\SNM\\MMA\\MMA867\\Assignment2_Predictive Modeling - MMA 867 - 8232019 - 258 PM\\house-prices-advanced-regression-techniques\\train.csv",
                        header = TRUE, sep = ",", stringsAsFactors= FALSE)
house_test <- read.csv("C:\\SNM\\MMA\\MMA867\\Assignment2_Predictive Modeling - MMA 867 - 8232019 - 258 PM\\house-prices-advanced-regression-techniques\\test.csv",
                       header = TRUE, sep = ",", stringsAsFactors= FALSE)


#get rid of the id column from train and test data sets
test_id <- house_test$Id
house_test$Id <- NULL
house_train$Id <- NULL

#create a master data buy combining training and test data sets
house_test$SalePrice <- NA
master_data <- rbind(house_train, house_test)
all <- read.csv("C:\\SNM\\MMA\\MMA867\\Assignment2_Predictive Modeling - MMA 867 - 8232019 - 258 PM\\house-prices-advanced-regression-techniques\\major_master.csv")

#explore and understand salesprice data
ggplot(data=master_data[!is.na(master_data$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="red", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

ggplot(data=master_data[!is.na(master_data$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
  geom_point(col='red') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(master_data$GrLivArea[!is.na(master_data$SalePrice)]>4500, rownames(master_data), '')))

#understand Correlations with numeric predictors
numeric_predictors <- which(sapply(master_data, is.numeric))
numericVarN <- names(numeric_predictors)
num_predict_data <- master_data[, numeric_predictors]
correlation_num <- cor(num_predict_data, use="pairwise.complete.obs")
sort_cor <- as.matrix(sort(correlation_num[,'SalePrice'], decreasing = TRUE))
Highly_cor <- names(which(apply(sort_cor, 1, function(x) abs(x)>0.5)))
correlation_num <- correlation_num [Highly_cor, Highly_cor]
corrplot.mixed(correlation_num, tl.col="red", tl.pos = "lt")

#Missing data, label encoding, and factorizing variables

##Completeness of the data

NAcol <- which(colSums(is.na(master_data)) > 0)
sort(colSums(sapply(master_data[NAcol], is.na)), decreasing = TRUE)

#check for the missing data
missing_data = master_data %>% 
  sapply(is.na) %>% 
  colSums() %>% 
  sort(decreasing = TRUE)

missing_data = (missing_data[missing_data > 0] / dim(master_data)[1]) %>% stack()
missing_data$values = percent(missing_data$values)
colnames(missing_data) = c('Proportion of NA', 'Variables')

missing_data %>% 
  ggplot(data=., aes(y=`Proportion of NA`, x=`Variables`)) + 
  geom_bar(stat="identity") +
  coord_flip()

master_data$PoolQC[is.na(master_data$PoolQC)] <- 'None'

Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

master_data$PoolQC<-as.integer(revalue(master_data$PoolQC, Qualities))
table(master_data$PoolQC)

master_data[master_data$PoolArea>0 & master_data$PoolQC==0, c('PoolArea', 'PoolQC', 'OverallQual')]

master_data$PoolQC[2421] <- 2
master_data$PoolQC[2504] <- 3
master_data$PoolQC[2600] <- 2

master_data$MiscFeature[is.na(master_data$MiscFeature)] <- 'None'
master_data$MiscFeature <- as.factor(master_data$MiscFeature)

master_data$Alley[is.na(master_data$Alley)] <- 'None'
master_data$Alley <- as.factor(master_data$Alley)


master_data$Fence[is.na(master_data$Fence)] <- 'None'
table(master_data$Fence)
master_data[!is.na(master_data$SalePrice),] %>% group_by(Fence) %>% summarise(median = median(SalePrice), counts=n())

master_data$Fence <- as.factor(master_data$Fence)

master_data$FireplaceQu[is.na(master_data$FireplaceQu)] <- 'None'
master_data$FireplaceQu<-as.integer(revalue(master_data$FireplaceQu, Qualities))

for (i in 1:nrow(master_data)){
  if(is.na(master_data$LotFrontage[i])){
    master_data$LotFrontage[i] <- as.integer(median(master_data$LotFrontage[master_data$Neighborhood==master_data$Neighborhood[i]], na.rm=TRUE)) 
  }
}

master_data$LotShape<-as.integer(revalue(master_data$LotShape, c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3)))

master_data$LotConfig <- as.factor(master_data$LotConfig)

master_data$GarageYrBlt[is.na(master_data$GarageYrBlt)] <- master_data$YearBuilt[is.na(master_data$GarageYrBlt)]

#Imputing modes.
master_data$GarageCond[2127] <- names(sort(-table(master_data$GarageCond)))[1]
master_data$GarageQual[2127] <- names(sort(-table(master_data$GarageQual)))[1]
master_data$GarageFinish[2127] <- names(sort(-table(master_data$GarageFinish)))[1]
}
#fixing 3 values for house 2577
master_data$GarageCars[2577] <- 0
master_data$GarageArea[2577] <- 0
master_data$GarageType[2577] <- NA

master_data$GarageType[is.na(master_data$GarageType)] <- 'No Garage'
master_data$GarageType <- as.factor(master_data$GarageType)
table(all$GarageType)

master_data$GarageFinish[is.na(master_data$GarageFinish)] <- 'None'
Finish <- c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)

master_data$GarageFinish<-as.integer(revalue(master_data$GarageFinish, Finish))

master_data$GarageQual[is.na(master_data$GarageQual)] <- 'None'
master_data$GarageQual<-as.integer(revalue(master_data$GarageQual, Qualities))

master_data$GarageCond[is.na(master_data$GarageCond)] <- 'None'
master_data$GarageCond<-as.integer(revalue(master_data$GarageCond, Qualities))

#Imputing modes.
master_data$BsmtFinType2[333] <- names(sort(-table(master_data$BsmtFinType2)))[1]
master_data$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(master_data$BsmtExposure)))[1]
master_data$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(master_data$BsmtCond)))[1]
master_data$BsmtQual[c(2218, 2219)] <- names(sort(-table(master_data$BsmtQual)))[1]

         master_data$BsmtQual[is.na(master_data$BsmtQual)] <- 'None'
         master_data$BsmtQual<-as.integer(revalue(master_data$BsmtQual, Qualities))
       
         master_data$BsmtCond[is.na(master_data$BsmtCond)] <- 'None'
         master_data$BsmtCond<-as.integer(revalue(master_data$BsmtCond, Qualities))
      
         master_data$BsmtExposure[is.na(master_data$BsmtExposure)] <- 'None'
         Exposure <- c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)
         
         master_data$BsmtExposure<-as.integer(revalue(master_data$BsmtExposure, Exposure))
     
         master_data$BsmtFinType1[is.na(master_data$BsmtFinType1)] <- 'None'
         FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
         
         master_data$BsmtFinType1<-as.integer(revalue(master_data$BsmtFinType1, FinType))
         
         master_data$BsmtFinType2[is.na(master_data$BsmtFinType2)] <- 'None'
         FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)
         
         master_data$BsmtFinType2<-as.integer(revalue(master_data$BsmtFinType2, FinType))
      
         master_data$BsmtFullBath[is.na(master_data$BsmtFullBath)] <-0
       
         master_data$BsmtHalfBath[is.na(master_data$BsmtHalfBath)] <-0
       
         master_data$BsmtFinSF1[is.na(master_data$BsmtFinSF1)] <-0
       
         master_data$BsmtFinSF2[is.na(master_data$BsmtFinSF2)] <-0
       
         master_data$BsmtUnfSF[is.na(master_data$BsmtUnfSF)] <-0
       
         master_data$TotalBsmtSF[is.na(master_data$TotalBsmtSF)] <-0
    

#fix this veneer type by imputing the mode
master_data$MasVnrType[2611] <- names(sort(-table(master_data$MasVnrType)))[2] #taking the 2nd value as the 1st is 'none'
master_data[2611, c('MasVnrType', 'MasVnrArea')]

master_data$MasVnrType[is.na(master_data$MasVnrType)] <- 'None'

Masonry <- c('None'=0, 'BrkCmn'=0, 'BrkFace'=1, 'Stone'=2)
master_data$MasVnrType<-as.integer(revalue(master_data$MasVnrType, Masonry))

master_data$MasVnrArea[is.na(master_data$MasVnrArea)] <-0

#imputing the mode
master_data$MSZoning[is.na(master_data$MSZoning)] <- names(sort(-table(master_data$MSZoning)))[1]
master_data$MSZoning <- as.factor(master_data$MSZoning)

master_data$KitchenQual[is.na(master_data$KitchenQual)] <- 'TA'
master_data$KitchenQual<-as.integer(revalue(master_data$KitchenQual, Qualities))

master_data$Utilities <- NULL

master_data$Functional[is.na(master_data$Functional)] <- names(sort(-table(master_data$Functional)))[1]

master_data$Functional <- as.integer(revalue(master_data$Functional, c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))

master_data$Exterior1st[is.na(master_data$Exterior1st)] <- names(sort(-table(master_data$Exterior1st)))[1]

master_data$Exterior1st <- as.factor(master_data$Exterior1st)

#imputing mode
master_data$Exterior2nd[is.na(master_data$Exterior2nd)] <- names(sort(-table(master_data$Exterior2nd)))[1]

master_data$Exterior2nd <- as.factor(master_data$Exterior2nd)

master_data$ExterQual<-as.integer(revalue(master_data$ExterQual, Qualities))

master_data$ExterCond<-as.integer(revalue(master_data$ExterCond, Qualities))

#imputing mode
master_data$Electrical[is.na(master_data$Electrical)] <- names(sort(-table(master_data$Electrical)))[1]

master_data$Electrical <- as.factor(master_data$Electrical)


master_dataSaleType[is.na(master_data$SaleType)] <- names(sort(-table(master_data$SaleType)))[1]

master_data$SaleType <- as.factor(master_data$SaleType)

master_data$SaleCondition <- as.factor(master_data$SaleCondition)

Charcol <- names(master_data[,sapply(master_data, is.character)])

master_data$Foundation <- as.factor(master_data$Foundation)

master_data$Heating <- as.factor(master_data$Heating)

master_data$HeatingQC<-as.integer(revalue(master_data$HeatingQC, Qualities))

master_data$CentralAir<-as.integer(revalue(master_data$CentralAir, c('N'=0, 'Y'=1)))

master_data$RoofStyle <- as.factor(master_data$RoofStyle)

master_data$RoofMatl <- as.factor(master_data$RoofMatl)

master_data$LandContour <- as.factor(master_data$LandContour)
  
master_data$LandSlope<-as.integer(revalue(master_data$LandSlope, c('Sev'=0, 'Mod'=1, 'Gtl'=2)))

master_data$BldgType <- as.factor(master_data$BldgType)
      
master_data$HouseStyle <- as.factor(master_data$HouseStyle)
       
master_data$Neighborhood <- as.factor(master_data$Neighborhood)
     
master_data$Condition1 <- as.factor(master_data$Condition1)
        
master_data$Condition2 <- as.factor(master_data$Condition2)
     
master_data$Street<-as.integer(revalue(master_data$Street, c('Grvl'=0, 'Pave'=1)))
        
master_data$PavedDrive<-as.integer(revalue(master_data$PavedDrive, c('N'=0, 'P'=1, 'Y'=2)))
         
master_data$MoSold <- as.factor(master_data$MoSold)
         
master_data$MSSubClass <- as.factor(master_data$MSSubClass)
         
master_data$MSSubClass<-revalue(master_data$MSSubClass, c('20'='1 story 1946+', '30'='1 story 1945-', '40'='1 story unf attic', '45'='1,5 story unf', '50'='1,5 story fin', '60'='2 story 1946+', '70'='2 story 1945-', '75'='2,5 story all ages', '80'='split/multi level', '85'='split foyer', '90'='duplex all style/age', '120'='1 story PUD 1946+', '150'='1,5 story PUD all', '160'='2 story PUD 1946+', '180'='PUD multilevel', '190'='2 family conversion'))

         #Visualization
     
         numericVars <- which(sapply(master_data, is.numeric))
         factorVars <- which(sapply(master_data, is.factor))
         
         ##Correlations
         
         all_numVar <- master_data[, numericVars]
         cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") 
         
         cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
        
         CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
         cor_numVar <- cor_numVar[CorHigh, CorHigh]
         
         corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)
         
     

         master_data$TotBathrooms <- master_data$FullBath + (master_data$HalfBath*0.5) + master_data$BsmtFullBath + (master_data$BsmtHalfBath*0.5)




         master_data$Remod <- ifelse(master_data$YearBuilt==master_data$YearRemodAdd, 0, 1)
         master_data$Age <- as.numeric(master_data$YrSold)-master_data$YearRemodAdd


         master_data$IsNew <- ifelse(master_data$YrSold==master_data$YearBuilt, 1, 0)

  master_data$YrSold <- as.factor(master_data$YrSold) 


  master_data$NeighRich[master_data$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
  master_data$NeighRich[!master_data$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
  master_data$NeighRich[master_data$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0

  master_data$TotalSqFeet <- master_data$GrLivArea + master_data$TotalBsmtSF

  master_data$TotalPorchSF <- master_data$OpenPorchSF + master_data$EnclosedPorch + master_data$X3SsnPorch + master_data$ScreenPorch

#Drop highly correlated variables

dropVars <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 'TotalRmsAbvGrd', 'BsmtFinSF1')

master_data <- master_data[,!(names(master_data) %in% dropVars)]

#Removing outliers

master_data <- master_data[-c(524, 1299),]

numericVarNames <- numericVarNames[!(numericVarNames %in% c('MSSubClass', 'MoSold', 'YrSold', 'SalePrice', 'OverallQual', 'OverallCond'))] #numericVarNames was created before having done anything
numericVarNames <- append(numericVarNames, c('Age', 'TotalPorchSF', 'TotBathrooms', 'TotalSqFeet'))

DFnumeric <- all[, names(all) %in% numericVarNames]

DFfactors <- all[, !(names(all) %in% numericVarNames)]
DFfactors <- DFfactors[, names(DFfactors) != 'SalePrice']

cat('There are', length(DFnumeric), 'numeric variables, and', length(DFfactors), 'factor variables')

###Skewness and normalizing of the numeric predictors

for(i in 1:ncol(DFnumeric)){
  if (abs(skew(DFnumeric[,i]))>0.8){
    DFnumeric[,i] <- log(DFnumeric[,i] +1)
  }
}

PreNum <- preProcess(DFnumeric, method=c("center", "scale"))
print(PreNum)

DFnorm <- predict(PreNum, DFnumeric)
dim(DFnorm)

DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors))
dim(DFdummies)

ZerocolTest <- which(colSums(DFdummies[(nrow(all[!is.na(all$SalePrice),])+1):nrow(all),])==0)
colnames(DFdummies[ZerocolTest])
DFdummies <- DFdummies[,-ZerocolTest] 

ZerocolTrain <- which(colSums(DFdummies[1:nrow(all[!is.na(all$SalePrice),]),])==0)
colnames(DFdummies[ZerocolTrain])
DFdummies <- DFdummies[,-ZerocolTrain]

fewOnes <- which(colSums(DFdummies[1:nrow(all[!is.na(all$SalePrice),]),])<10)
colnames(DFdummies[fewOnes])
DFdummies <- DFdummies[,-fewOnes] 
dim(DFdummies)

combined <- cbind(DFnorm, DFdummies) 

master_data$SalePrice <- log(master_data$SalePrice)

house_train1 <- combined[!is.na(all$SalePrice),]
house_test1 <- combined[is.na(all$SalePrice),]
dim(house_train1)
dim(house_test1)

dim(master_data)
dim(all)

#Modeling

##Lasso regression model

set.seed(27042019)
my_cont <-trainControl(method="cv", number=5)
lassoGrid1 <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))

lasso_mode <- train(x=house_train1, y=all$SalePrice[!is.na(all$SalePrice)], method='glmnet', trControl= my_cont, tuneGrid=lassoGrid1) 
lasso_mode$bestTune
min(lasso_mode$results$RMSE)

#prediction
LassoPredict <- predict(lasso_mode, house_test1)
predict_lasso <- exp(LassoPredict)
head(predict_lasso)

write.csv(predict_lasso,"C:\\SNM\\MMA\\MMA867\\Assignment2_Predictive Modeling - MMA 867 - 8232019 - 258 PM\\house-prices-advanced-regression-techniques\\ans.csv")

#install.packages("glmnetUtils")
library(glmnetUtils)

# Final model with Ridge
RidgeGrid1 <- expand.grid(alpha = 0, lambda = seq(0.001,0.1,by = 0.0005))
ridge_mode <- train(x=house_train1, y=all$SalePrice[!is.na(all$SalePrice)], method='glmnet', trControl= my_cont, tuneGrid=RidgeGrid1) 

ridge_mode$bestTune
min(ridge_mode$results$RMSE)

#prediction Ridge
ridgePredict <- predict(ridge_mode, house_test1)
predict_ridge <- exp(ridgePredict)
head(predict_ridge)

write.csv(predict_lasso,"C:\\SNM\\MMA\\MMA867\\Assignment2_Predictive Modeling - MMA 867 - 8232019 - 258 PM\\house-prices-advanced-regression-techniques\\ans_ridge.csv")
