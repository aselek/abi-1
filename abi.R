#install.packages("ROCR")
#install.packages("rminer")
#install.packages("digest")
library(rminer) 
library(ROCR)
library(digest)

# Read values
setwd('C:\\Users\\BlueMoon\\Desktop\\abi')
d=read.csv('train.csv', TRUE, ',')
t=read.csv('test.csv', TRUE, ',')

# Identify users by characteristics, add an ID
d$A_ID <- "0"
d$B_ID <- "0"
for(i in 1:length(d[[1]])){
	d$A_ID[i]=digest(d[i,c(2:12)])
	d$B_ID[i]=digest(d[i,c(13:23)])
}

# If A>B>C, then A>C


# Create AB's and BA's
mirrorData = d
for(i in 1:length(d[[1]])){
	row=d[i,]
	mirrorRow = row
	mirrorRow[1] = ifelse(row[1] == 1,0,1)
      for(i in 2:12){
		mirrorRow[i] = row[i+11]
		mirrorRow[i+11] = row[i]
	}
	mirrorData = rbind(mirrorData, mirrorRow)
}

# Create ratios or differences between followers/following

# Maybe normalize, discretize, divide my median/std, scale between 0 and 1

# Try GBM (GBDT), Elo System, etc

# Ignore sent retweets (feature selection)

# Random Forest 	76%
#d$Choice <- as.factor(d$Choice)
#RF=fit(Choice~.,d,model="randomforest") 
#PRFu=predict(RF,t)

# Boosting		78%
d$Choice <- as.factor(d$Choice)
t$Choice <- as.factor(c(rep(1, length(t[[1]])/2),rep(0,length(t[[1]])/2)))
RF=fit(Choice~.,d,model="boosting") 
PRFu=predict(RF,t)

# Convert RF probabilities to 0s and 1s
PRF=c(1:length(t[[1]]))
for(i in 1:length(PRFu[,1])) {
    PRF[i] <- ifelse(PRFu[i,1] < PRFu[i,2],1,0)
}

# Save to a CSV
P=data.frame(ID=c(1:length(t[[1]])),Choice=PRF)
write.csv(P,"results.csv", row.names=FALSE)

# ROC curve
#pred = prediction(predictions, true_labels);
#auc.tmp = performance(pred,"auc");
#auc = as.numeric(auc.tmp@y.values);
