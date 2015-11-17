#install.packages("ROCR")
#install.packages("rminer")
#install.packages("digest")
#install.packages("sna")
#install.packages("rgl")
library(rminer) 
library(ROCR)
library(digest)
library(sna)
library(rgl)

# Read values
setwd('C:\\Users\\BlueMoon\\Documents\\GitHub\\abi')
d=read.csv('train.csv', TRUE, ',')
t=read.csv('test.csv', TRUE, ',')

# Identify users by characteristics, add an ID
digestList=c()
d$A_ID <- "0"
d$B_ID <- "0"
for(i in 1:length(d[[1]])){
	d$A_ID[i]=digest(c(d[i,c(2:12)]))
	d$B_ID[i]=digest(c(d[i,c(13:23)]))
	if(!is.element(d$A_ID[i],digestList)){
		digestList=c(digestList,d$A_ID[i])
	}
	if(!is.element(d$B_ID[i],digestList)){
		digestList=c(digestList,d$B_ID[i])
	}
}

# Show graph in 2d or 3d
rmatrix  <-  matrix(rep(0, length(digestList)*length(digestList)), 
	ncol=length(digestList))
colnames(rmatrix) <- digestList
rownames(rmatrix) <- digestList
for(i in 1:length(d[[1]])){
	rmatrix[d$A_ID[i],d$B_ID[i]]=1
}
gplot(rmatrix)
#gplot3d(rmatrix)


# If A>B>C, then A>C
for(i in 2:length(d[[1]])){
	print(i)
	for(j in 1:i){
		if(d$A_ID[i]==d$A_ID[j] && d$Choice[i]!=d$Choice[j] && d$B_ID[i]!=d$B_ID[j]){
			# A>B and A<C, then B<C	
			row=d[i,]
			for(k in 2:12){
				row[k] = d[j,k+11]
			}
			row$A_ID=d[j,]$B_ID
			d=rbind(d, row)
		} else if(d$A_ID[i]==d$B_ID[j] && d$Choice[i]==d$Choice[j] && d$B_ID[i]!=d$A_ID[j]){
			# A>B and C>A, then C>B	
			row=d[i,]
			for(k in 2:12){
				row[k] = d[j,k]
			}
			row$A_ID=d[j,]$A_ID
			d=rbind(d, row)
		} else if(d$B_ID[i]==d$B_ID[j] && d$Choice[i]!=d$Choice[j] && d$A_ID[i]!=d$A_ID[j]){
			# A>B and C<B, then A>C	
			row=d[i,]
			for(k in 2:12){
				row[k+11] = d[j,k]
			}
			row$B_ID=d[j,]$A_ID
			d=rbind(d, row)
		} else if(d$B_ID[i]==d$A_ID[j] && d$Choice[i]==d$Choice[j] && d$A_ID[i]!=d$B_ID[j]){
			# A>B and B>C, then A>C	
			row=d[i,]
			for(k in 2:12){
				row[k+11] = d[j,k+11]
			}
			row$B_ID=d[j,]$B_ID
			d=rbind(d, row)
		}
	}
}

# Show graph again
for(i in 1:length(d[[1]])){
	rmatrix[d$A_ID[i],d$B_ID[i]]=1
}
gplot(rmatrix)
#gplot3d(rmatrix)


# Remove IDs
d$A_ID <- NULL
d$B_ID <- NULL

# Create ratios or differences between followers/following
# these can have infinite values (if following=0); fix
d$A_FollowersFollowing <- 0
d$B_FollowersFollowing <- 0
d$A_FollowersFollowing=d[,2]/d[,3]
d$B_FollowersFollowing=d[,13]/d[,14]
d$A_FollowersFollowing[is.infinite(d$A_FollowersFollowing)] <- d$A_follower_count[is.infinite(d$A_FollowersFollowing)]
d$B_FollowersFollowing[is.infinite(d$B_FollowersFollowing)] <- d$B_follower_count[is.infinite(d$B_FollowersFollowing)]
d <- cbind(d[,1:12],d[,24,drop=F],d[,13:23],d[,25,drop=F])

t$A_FollowersFollowing <- 0
t$B_FollowersFollowing <- 0
t$A_FollowersFollowing=t[,1]/t[,2]
t$B_FollowersFollowing=t[,12]/t[,13]
t$A_FollowersFollowing[is.infinite(t$A_FollowersFollowing)] <- t$A_follower_count[is.infinite(t$A_FollowersFollowing)]
t$B_FollowersFollowing[is.infinite(t$B_FollowersFollowing)] <- t$B_follower_count[is.infinite(t$B_FollowersFollowing)]
t <- cbind(t[,1:11],t[,23,drop=F],t[,12:22],t[,24,drop=F])

# Maybe normalize, discretize, divide my median/std, scale between 0 and 1

# instead of 0 and 1's, when opinions differ, we could set values in between

# instead of using the regular cols, use ratios between same cols of A and B

# Create AB's and BA's
mirrorData = d
for(i in 1:length(d[[1]])){
	row=d[i,]
	mirrorRow = row
	mirrorRow[1] = ifelse(row[1] == 1,0,1)
      for(i in 2:13){
		mirrorRow[i] = row[i+12]
		mirrorRow[i+12] = row[i]
	}
	mirrorData = rbind(mirrorData, mirrorRow)
}
d = mirrorData[,c(1:25)]

# Try GBM (GBDT), Elo System, etc

# Ignore sent retweets (feature selection)

predictorsBoth = c("naive", "ctree", "dt", "kknn", "mlp", "mlpe", "ksvm", "randomforest")
predictorsClass = c("bagging", "boosting", "lda", "lr", "naivebayes", "qda")
predictorsRegr = c("mr", "mars", "cubist", "pcr", "plsr", "cppls", "rvm")

tC = t
dC = d
dC$Choice <- as.factor(dC$Choice)
tC$Choice <- as.factor(c(rep(1, length(t[[1]])/2),rep(0,length(t[[1]])/2)))
t$Choice <- c(rep(1, length(t[[1]])/2),rep(0,length(t[[1]])/2))

for(j in 1:length(predictorsBoth)) {
	print(predictorsBoth[j])
	RF=fit(Choice~.,d,model=predictorsBoth[j]) 
	PRFu=predict(RF,t)

	# Convert RF probabilities to 0s and 1s
	PRF=c(1:length(t[[1]]))
	for(i in 1:length(PRFu)) {
    		PRF[i] <- ifelse(PRFu[i] > 0.5,1,0)
	}

	# Save to a CSV
	P=data.frame(ID=c(1:length(t[[1]])),Choice=PRF)
	write.csv(P,predictorsBoth[j], row.names=FALSE)
}

for(j in 1:length(predictorsRegr)) {
	rint(predictorsRegr[j])
	RF=fit(Choice~.,d,model=predictorsRegr[j]) 
	PRFu=predict(RF,t)

	# Convert RF probabilities to 0s and 1s
	PRF=c(1:length(t[[1]]))
	for(i in 1:length(PRFu)) {
    		PRF[i] <- ifelse(PRFu[i] > 0.5,1,0)
	}

	# Save to a CSV
	P=data.frame(ID=c(1:length(t[[1]])),Choice=PRF)
	write.csv(P,predictorsRegr[j], row.names=FALSE)

}

for(j in 1:length(predictorsBoth)) {
	print(predictorsBoth[j])
	RF=fit(Choice~.,dC,model=predictorsBoth[j]) 
	PRFu=predict(RF,tC)

	# Convert RF probabilities to 0s and 1s
	PRF=c(1:length(t[[1]]))
	for(i in 1:length(PRFu[,1])) {
    		PRF[i] <- ifelse(PRFu[i,1] < PRFu[i,2],1,0)
	}

	# Save to a CSV
	P=data.frame(ID=c(1:length(t[[1]])),Choice=PRF)
	write.csv(P,predictorsBoth[j], row.names=FALSE)

}

for(j in 1:length(predictorsClass)) {
	print(predictorsClass[j])
	RF=fit(Choice~.,dC,model=predictorsClass[j]) 
	PRFu=predict(RF,tC)

	# Convert RF probabilities to 0s and 1s
	PRF=c(1:length(t[[1]]))
	for(i in 1:length(PRFu[,1])) {
    		PRF[i] <- ifelse(PRFu[i,1] < PRFu[i,2],1,0)
	}

	# Save to a CSV
	P=data.frame(ID=c(1:length(t[[1]])),Choice=PRF)
	write.csv(P,predictorsClass[j], row.names=FALSE)
}

# Random Forest 	76%
#d$Choice <- as.factor(d$Choice)
#RF=fit(Choice~.,d,model="randomforest") 
#PRFu=predict(RF,t)

# Boosting		78%
#d$Choice <- as.factor(d$Choice)
#t$Choice <- as.factor(c(rep(1, length(t[[1]])/2),rep(0,length(t[[1]])/2)))
#RF=fit(Choice~.,d,model="boosting") 
#PRFu=predict(RF,t)

# Convert RF probabilities to 0s and 1s
#PRF=c(1:length(t[[1]]))
#for(i in 1:length(PRFu[,1])) {
#    PRF[i] <- ifelse(PRFu[i,1] < PRFu[i,2],1,0)
#}

# Save to a CSV
#P=data.frame(ID=c(1:length(t[[1]])),Choice=PRF)
#write.csv(P,"results.csv", row.names=FALSE)

# ROC curve
#pred = prediction(predictions, true_labels);
#auc.tmp = performance(pred,"auc");
#auc = as.numeric(auc.tmp@y.values);
