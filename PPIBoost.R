#################################
#
#    author: Ken Wang
#    e-mail:ken711198@hotmail.com
#
#################################
# reqirement: R package "xgboost"
library(xgboost)

# our PPIBoost prediction model
PPIBoost <- function(pos, neg, PositiveSamplingSize, NegativeSamplingSize){
# reqirement: R package "xgboost"
#  pos:positiveData, PPI interaction --- TRUE
#  neg:negativeData, PPI interaction --- FALSE
#  PositiveSamplingSize: The sampling size we need to extract from positiveData
#  NegativeSamplingSize: The sampling size we need to extract from negativeData
#  output is XGBoost function

y <- c(rep(1,nrow(pos)),rep(0,nrow(neg)))
dataset <- as.data.from(rbind(pos,neg))
dataset$Y <- y

index <- sample(2,nrow(dataset),replace=TRUE,c(0.8,0.2))
traindata <- dataset[index==1,]
testdata <- dataset[index==2,]


x_train <- as.matrix(traindata[,-ncol(traindata)])
output_vector <- as.numeric(as.matrix(traindata$Y))
output_vector[output_vector == 1] <- "TRUE"
output_vector[output_vector == 0] <- "FALSE"
output_vector <- as.logical(output_vector)

bst <- xgboost(data = x_train,
                    label = output_vector, 
					max.depth = 6,  
                    eta = 0.1, 
					nthread = 4, 
					nround = 50,
					objective = "binary:logistic",
					booster="gbtree",
					min_child_weight=1,
					lambda=2
)

bst
}