

#### XGBOOST test, to be run after NetworkAnalysis.R


# load libraries
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(dplyr)
library(tidyr)
library(plyr)
library(Matrix)

set.seed(1066)
path <- 'C:/Users/Manuel/Desktop/Southampton/MasterThesis/Data/FTSE/'
marketInfo <- read.csv(paste0(path, 'marketInfo.csv'))


# Creation of Input and Output datasets to train XGBoost
Input <- list()
Output <- list()
for (t in seq_along(info.stocks)) {
  Input[[t]] <- cbind(info.stocks[[t]], GainLossMatrix[,t], marketInfo[,t], marketInfo$MC)
  names(Input[[t]]) <- c(names(info.stocks[[t]]), 'avg.gainLoss', 'avg.StockValue', 'MarketCapitalisation')
}

InputMatrix <- Input[[1]]
OutputMatrix <- Input[[2]]
for (t in 2:(length(info.stocks)-1)) {
  InputMatrix <- data.frame(rbind(InputMatrix, Input[[t]]))
  OutputMatrix <- data.frame(rbind(OutputMatrix, Input[[t+1]]))
}
row.names(InputMatrix) <- c(1:nrow(InputMatrix))
row.names(OutputMatrix) <- c(1:nrow(OutputMatrix))


#### try to model avg.StockValue and avg.gainLoss
Output <- data.frame(OutputMatrix$avg.gainLoss)
Tags <- OutputMatrix$stocks

### Take out ids/labels (not variables), and sector.type (we will consider it by OHE the sectors categories)
InputMatrix_clean <- InputMatrix[,3:15]

### OHE sectors
ohe_feats = 'sectors'
dummies <- dummyVars(~ sectors, data = InputMatrix_clean)
df_all_ohe <- as.data.frame(predict(dummies, newdata = InputMatrix_clean))
InputMat_OHE <- cbind(InputMatrix_clean[,-c(which(colnames(InputMatrix_clean) %in% ohe_feats))],df_all_ohe)


InputMat_OHE <- InputMat_OHE[,-9]


### Training (75%) and Test sets
Input_train <- as.matrix(InputMat_OHE[1:ceiling(nrow(InputMat_OHE)*0.75),])
Output_train <- as.matrix((Output[1:ceiling(nrow(InputMat_OHE)*0.75),]))
Input_test <-  as.matrix(InputMat_OHE[ceiling((nrow(InputMat_OHE)*0.75)+1):nrow(InputMat_OHE),])
Output_test <- as.matrix((Output[ceiling((nrow(InputMat_OHE)*0.75)+1):nrow(InputMat_OHE),]))
  


  
# 
# train xgboost
xgb <- xgboost(data = Input_train, 
               label = Output_train, 
               eta = 0.1,
               max_depth = 9, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 12,
               nthread = 3
)

# predict values in test set
y_pred <- predict(xgb, Input_test)

ErrorTest <- (sum((y_pred-Output_test)^2))/length(y_pred)

prueba <- data.frame(cbind(y_pred, Output_test))

ggplot() + geom_line(data=prueba, aes(x=(1:nrow(y_pred)), y=Output_train, color='blue'))


# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #### add count of links: variable defining the proportion of edges going to a given sector
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 




