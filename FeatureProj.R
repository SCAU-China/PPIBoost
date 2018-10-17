#################################
#
#    author: Ken Wang
#    e-mail:ken711198@hotmail.com
#
#################################

###################################Feature Projection################################################
# reqirement: R package "stringr"


featureProj <- function(dataset){
# input is a dataset consistant of protein sequence A and protein sequence B (m*2)
# output is a dataset consistant of sequence descriptor vector A and B  (m*686)
library(stringr)
base <- c("V1.csv","V2.csv","V3.csv","V4.csv","V5.csv","V6.csv","V7.csv")
V1 <- c("A","G","V")           # classify amino acids into 7 classes
V2 <- c("I","L","F","P")
V3 <- c("Y","M","T","S")
V4 <- c("H","N","Q","W")
V5 <- c("R","K")
V6 <- c("D","E")
V7 <- c("C")
write.csv(V1,"V1.csv")
write.csv(V2,"V2.csv")
write.csv(V3,"V3.csv")
write.csv(V4,"V4.csv")
write.csv(V5,"V5.csv")
write.csv(V6,"V6.csv")
write.csv(V7,"V7.csv")


id <- 1
for(i in 1:length(base)){
  for(j in 1:length(base)){
    for(k in 1:length(base)){
	      b1 <- read.csv(base[i],header=T,row.names=1)
		  b2 <- read.csv(base[j],header=T,row.names=1)
		  b3 <- read.csv(base[k],header=T,row.names=1)
		  b1 <- as.character(as.matrix(b1))
		  b2 <- as.character(as.matrix(b2))
		  b3 <- as.character(as.matrix(b3))
		  baseFeature <- NULL
		  for(m in 1:length(b1)){
		    for(n in 1:length(b2)){
			  for(o in 1:length(b3)){
			    baseFeature <- c(baseFeature, paste(b1[m],b2[n],b3[o],sep=""))
				}
				}
				}
		  assign(paste("m",id,sep=""),baseFeature)
	      id <- id+1
		}
	}
}
Feature <- rep("m",343)       # generate a 343 length sequence descriptor vector, each "m*" variable means a type of amino acids triple unit
for(i in 1:length(Feature)){
   Feature[i] <- paste(Feature[i], i,sep="")
}


pair_left <- dataset[,1] 
pair_right <- dataset[,2]
left_matrix <- NULL
left <- NULL
for(k in 1:length(pair_left)){
seq <- as.character(as.matrix(pair_left[k]))
for(i in 1:length(Feature)){
eval(parse(text = paste("fea <- m",i,sep="")))
leftM <- t(as.matrix(str_count(seq, fea)))
left <- cbind(left,rowSums(leftM))
}
left_matrix <- rbind(left_matrix, left)
left <- NULL
}

right_matrix <- NULL
right <- NULL
for(k in 1:length(pair_right)){
seq <- as.character(as.matrix(pair_right[k]))
for(i in 1:length(Feature)){
eval(parse(text = paste("fea <- m",i,sep="")))
rightM <- t(as.matrix(str_count(seq, fea)))
right <- cbind(right,rowSums(rightM))
}
right_matrix <- rbind(right_matrix, right)
right <- NULL
}

# MIN-MAX normalization
for(i in 1:nrow(dataset)){
left_matrix[i,] <- (left_matrix[i,]-min(left_matrix[i,]))/(max(left_matrix[i,])-min(left_matrix[i,]))
right_matrix[i,] <- (right_matrix[i,]-min(right_matrix[i,]))/(max(right_matrix[i,])-min(right_matrix[i,]))
}
dataset <- cbind(left_matrix,right_matrix)

return dataset
}