#Installing and loading necessary packages for Analysis
#install.packages("knitr")
#install.packages("gridExtra")
#install.packages("scales")
#install.packages("ggrepel")
#install.packages("Rmisc")
#install.packages("moments")
#install.packages("formattable")
#install.packages("psych")
library(knitr)
library(tidyverse)
library(plyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(glmnet)
library(formattable)
library(moments)
library(psych)

#import training and test data
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

#explore and understand salesprice data
ggplot(data=master_data[!is.na(master_data$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="red", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

#understand Correlations with numeric predictors
numeric_predictors <- which(sapply(master_data, is.numeric))
numericVarN <- names(numeric_predictors)
num_predict_data <- master_data[, numeric_predictors]
correlation_num <- cor(num_predict_data, use="pairwise.complete.obs")
sort_cor <- as.matrix(sort(correlation_num[,'SalePrice'], decreasing = TRUE))
Highly_cor <- names(which(apply(sort_cor, 1, function(x) abs(x)>0.5)))
correlation_num <- correlation_num [Highly_cor, Highly_cor]
corrplot.mixed(correlation_num, tl.col="red", tl.pos = "lt")

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

#imputing missing values
master_data$LotFrontage[is.na(master_data$LotFrontage)] = mean(master_data$LotFrontage, na.rm = TRUE) 

mode_sub <- function(x) {
  un <- unique(x)
  un[which.max(tabulate(match(x, un)))]
}

mode_variables = c('MSZoning', 'Utilities', 'Functional', 'Electrical', 'KitchenQual', 'Exterior1st', 'Exterior2nd', 'SaleType')

for (v in mode_variables){
  master_data[v][is.na(master_data[v])] = mode_sub(na.omit(master_data[v][[1]]))
}

none_sub = c('PoolQC', 'MiscFeature','Alley', 'Fence', 'FireplaceQu',
                  'GarageType', 'GarageFinish', 'GarageQual', 'GarageCond', 
                  'GarageYrBlt', 'GarageArea', 'GarageCars', 'BsmtFinSF1', 
                  'BsmtFinSF2', 'BsmtUnfSF','TotalBsmtSF', 'BsmtFullBath', 
                  'BsmtHalfBath', 'BsmtQual', 'BsmtCond', 'BsmtExposure', 
                  'BsmtFinType1', 'BsmtFinType2', 'MasVnrType', 'MasVnrArea')
for (v in none_sub){
  master_data[v][is.na(master_data[v])] = 'None'
}

#lable encoding and conversting some charecters into factors
map = c('Ex'=5,'Gd'=4,'TA'=3,'Fa'=2,'Po'=1,'None'=0)
ordinal_variables = c('BsmtQual', 'BsmtCond', 'GarageQual', 'GarageCond', 'ExterQual', 'ExterCond', 'HeatingQC', 'PoolQC', 'KitchenQual')

for (v in ordinal_variables){
  master_data[v] = c('Ex'=5,'Gd'=4,'TA'=3,'Fa'=2,'Po'=1,'None'=0)[master_data[v][[1]]]
}
master_data['BsmtFinType1'] = c('GLQ'=6,'ALQ'=5,'BLQ'=4,'Rec'=3,'LwQ'=2,'Unf'=1,'None'=0)[master_data['BsmtFinType1'][[1]]]
master_data['BsmtFinType2'] = c('GLQ'=6,'ALQ'=5,'BLQ'=4,'Rec'=3,'LwQ'=2,'Unf'=1,'None'=0)[master_data['BsmtFinType2'][[1]]]
master_data['Functional'] = c('Typ'=7,'Min1'=6,'Min2'=5,'Mod'=4,'Maj1'=3,'Maj2'=2,'Sev'=1,'Sal'=0)[master_data['Functional'][[1]]]
master_data['Fence'] = c('GdPrv'=4,'MnPrv'=3,'GdWo'=2,'MnWw'=1,'None'=0)[master_data['Fence'][[1]]]
master_data['BsmtExposure'] = c('Gd'=4,'Av'=3,'Mn'=2,'No'=1,'None'=0)[master_data['BsmtExposure'][[1]]]
master_data['GarageFinish'] = c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)[master_data['GarageFinish'][[1]]]
master_data['LandSlope'] = c('Sev'=1, 'Mod'=2, 'Gtl'=3)[master_data['LandSlope'][[1]]]
master_data['LotShape'] = c('IR3'=1, 'IR2'=2, 'IR1'=3, 'Reg'=4)[master_data['LotShape'][[1]]]
master_data['PavedDrive'] = c('N'=1, 'P'=2, 'Y'=3)[master_data['PavedDrive'][[1]]]
master_data['Street'] = c('Grvl'=1, 'Pave'=2)[master_data['Street'][[1]]]
master_data['Alley'] = c('None'=0, 'Grvl'=1, 'Pave'=2)[master_data['Alley'][[1]]]
master_data['CentralAir'] = c('N'=0, 'Y'=1)[master_data['CentralAir'][[1]]]

master_data[master_data == 'None'] = 0
for (v in colnames(master_data)){
  x = master_data[v][[1]]
  master_data[v] = ifelse(is.na(as.numeric(x)), x, as.numeric(x))
}

master_data$PoolQC[2421] <- 2
master_data$PoolQC[2504] <- 3
master_data$PoolQC[2600] <- 2
master_data$MiscFeature <- as.factor(master_data$MiscFeature)
master_data$Alley <- as.factor(master_data$Alley)
master_data$Fence <- as.factor(master_data$Fence)
for (i in 1:nrow(master_data)){
  if(is.na(master_data$LotFrontage[i])){
    master_data$LotFrontage[i] <- as.integer(median(master_data$LotFrontage[master_data$Neighborhood==master_data$Neighborhood[i]], na.rm=TRUE)) 
  }
}

master_data$LotConfig <- as.factor(master_data$LotConfig)
master_data$GarageCond[2127] <- names(sort(-table(master_data$GarageCond)))[1]
master_data$GarageQual[2127] <- names(sort(-table(master_data$GarageQual)))[1]
master_data$GarageFinish[2127] <- names(sort(-table(master_data$GarageFinish)))[1]
master_data$GarageCars[2577] <- 0
master_data$GarageArea[2577] <- 0
master_data$GarageType[2577] <- NA
master_data$GarageType <- as.factor(master_data$GarageType)

master_data$BsmtFinType2[333] <- names(sort(-table(master_data$BsmtFinType2)))[1]
master_data$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(master_data$BsmtExposure)))[1]
master_data$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(master_data$BsmtCond)))[1]
master_data$BsmtQual[c(2218, 2219)] <- names(sort(-table(master_data$BsmtQual)))[1]
master_data$BsmtFullBath[is.na(master_data$BsmtFullBath)] <-0
master_data$BsmtHalfBath[is.na(master_data$BsmtHalfBath)] <-0
master_data$BsmtFinSF1[is.na(master_data$BsmtFinSF1)] <-0
master_data$BsmtFinSF2[is.na(master_data$BsmtFinSF2)] <-0
master_data$BsmtUnfSF[is.na(master_data$BsmtUnfSF)] <-0
master_data$TotalBsmtSF[is.na(master_data$TotalBsmtSF)] <-0
master_data$MasVnrType[2611] <- names(sort(-table(master_data$MasVnrType)))[2]
master_data$MasVnrArea[is.na(master_data$MasVnrArea)] <-0
master_data$MSZoning[is.na(master_data$MSZoning)] <- names(sort(-table(master_data$MSZoning)))[1]
master_data$MSZoning <- as.factor(master_data$MSZoning)
master_data$Utilities <- NULL
master_data$Functional[is.na(master_data$Functional)] <- names(sort(-table(master_data$Functional)))[1]
master_data$Exterior1st[is.na(master_data$Exterior1st)] <- names(sort(-table(master_data$Exterior1st)))[1]

master_data$Exterior1st <- as.factor(master_data$Exterior1st)
master_data$Exterior2nd[is.na(master_data$Exterior2nd)] <- names(sort(-table(master_data$Exterior2nd)))[1]

master_data$Exterior2nd <- as.factor(master_data$Exterior2nd)
master_data$Electrical[is.na(master_data$Electrical)] <- names(sort(-table(master_data$Electrical)))[1]

master_data$Electrical <- as.factor(master_data$Electrical)
master_data$SaleType[is.na(master_data$SaleType)] <- names(sort(-table(master_data$SaleType)))[1]

master_data$SaleType <- as.factor(master_data$SaleType)
master_data$SaleCondition <- as.factor(master_data$SaleCondition)

master_data$Foundation <- as.factor(master_data$Foundation)
master_data$Heating <- as.factor(master_data$Heating)
master_data$RoofStyle <- as.factor(master_data$RoofStyle)
master_data$RoofMatl <- as.factor(master_data$RoofMatl)
master_data$LandContour <- as.factor(master_data$LandContour)
master_data$BldgType <- as.factor(master_data$BldgType)
master_data$HouseStyle <- as.factor(master_data$HouseStyle)
master_data$Neighborhood <- as.factor(master_data$Neighborhood)
master_data$Condition1 <- as.factor(master_data$Condition1)
master_data$Condition2 <- as.factor(master_data$Condition2)

master_data$MoSold <- as.factor(master_data$MoSold)
master_data$MSSubClass <- as.factor(master_data$MSSubClass)
master_data$MSSubClass<-revalue(master_data$MSSubClass, c('20'='1 story 1946+', '30'='1 story 1945-', 
                                          '40'='1 story unf attic', '45'='1,5 story unf', 
                                          '50'='1,5 story fin', '60'='2 story 1946+', '70'='2 story 1945-',
                                          '75'='2,5 story all ages', '80'='split/multi level', 
                                          '85'='split foyer', '90'='duplex all style/age', 
                                          '120'='1 story PUD 1946+', '150'='1,5 story PUD all', 
                                          '160'='2 story PUD 1946+', '180'='PUD multilevel', '190'='2 family conversion'))

#visualization 
numericV <- which(sapply(master_data, is.numeric)) 
factorV <- which(sapply(master_data, is.factor)) 
all_numV <- master_data[, numericV]
cor_numV <- cor(all_numV, use="pairwise.complete.obs")

cor_sort <- as.matrix(sort(cor_numV[,'SalePrice'], decreasing = TRUE))

CorHigh1 <- names(which(apply(cor_sort, 1, function(x) abs(x)>0.5)))
cor_numV <- cor_numV[CorHigh1, CorHigh1]

corrplot.mixed(cor_numV, tl.col="red", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)

master_data$GarageYrBlt[2593] <- 2007

#featuring engineering
master_data$TotalBathrooms <- master_data$FullBath + (master_data$HalfBath*0.5) + master_data$BsmtFullBath + (master_data$BsmtHalfBath*0.5)
master_data$Remod1 <- ifelse(master_data$YearBuilt==master_data$YearRemodAdd, 0, 1)
master_data$Age <- as.numeric(master_data$YrSold)-master_data$YearRemodAdd
master_data$Is_New <- ifelse(master_data$YrSold==master_data$YearBuilt, 1, 0)
master_data$YearSold <- as.factor(master_data$YrSold)
master_data$NeighorRich[master_data$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
master_data$NeighorRich[!master_data$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
master_data$NeighorRich[master_data$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0
master_data$T_SqFeet <- master_data$GrLivArea + master_data$TotalBsmtSF

master_data$TotalP_chSF <- master_data$OpenPorchSF + master_data$EnclosedPorch + master_data$X3SsnPorch + master_data$ScreenPorch

#dropping highly corelated variables and outliers
dropVariables <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond', 'TotalBsmtSF', 'TotalRmsAbvGrd', 'BsmtFinSF1')

master_data <- master_data[,!(names(master_data) %in% dropVariables)]
master_data <- master_data[-c(524, 1299),]

#correction of predictors
numericVarN <- numericVarN[!(numericVarN %in% c('MSSubClass', 'MoSold', 'YrSold', 'SalePrice', 'OverallQual', 'OverallCond'))]
numericVarN <- append(numericVarN, c('Age', 'TotalPorchSF', 'TotBathrooms', 'TotalSqFeet'))

DFnum <- master_data[, names(master_data) %in% numericVarN]
dim(DFnum)
DFfact <- master_data[, !(names(master_data) %in% numericVarN)]
DFfact <- DFfact[, names(DFfact) != 'SalePrice']
dim(DFfact)
for(i in 1:ncol(DFnum)){
  if (abs(skew(DFnum[,i]))>0.8){
    DFnum[,i] <- log(DFnum[,i] +1)
  }
}

#additional sets on data
PreN <- preProcess(DFnum, method=c("center", "scale"))
print(PreN)
DFnor <- predict(PreN, DFnum)
DFdummy <- as.data.frame(model.matrix(~.-1, DFfact))
head(DFdummy)
ZerocolT <- which(colSums(DFdummy[(nrow(master_data[!is.na(master_data$SalePrice),])+1):nrow(master_data),])==0)
DFdummy <- DFdummy[,-ZerocolT]
ZerocolTr <- which(colSums(DFdummy[1:nrow(master_data[!is.na(master_data$SalePrice),]),])==0)
DFdummy <- DFdummy[,-ZerocolTr]
fewO <- which(colSums(DFdummy[1:nrow(master_data[!is.na(master_data$SalePrice),]),])<10)
DFdummy <- DFdummy[,-fewO]
combine <- cbind(DFnor, DFdummy)
master_data$SalePrice <- log(master_data$SalePrice)

dim(DFdummy)
dim(DFnor)
write.csv(DFdummy,"C:\\SNM\\MMA\\MMA867\\Assignment2_Predictive Modeling - MMA 867 - 8232019 - 258 PM\\house-prices-advanced-regression-techniques\\dummy.csv")
write.csv(DFnor,"C:\\SNM\\MMA\\MMA867\\Assignment2_Predictive Modeling - MMA 867 - 8232019 - 258 PM\\house-prices-advanced-regression-techniques\\nor.csv")

#newdata set
train1 <- combined[!is.na(all$SalePrice),]
test1 <- combined[is.na(all$SalePrice),]