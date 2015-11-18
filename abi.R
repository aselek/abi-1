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

############# FUNCTIONS ############

# Show graph in 2d or 3d
showGraph <- function(digestList){
	rmatrix  <-  matrix(rep(0, length(digestList)*length(digestList)), 
		ncol=length(digestList))
	colnames(rmatrix) <- digestList
	rownames(rmatrix) <- digestList
	for(i in 1:length(d[[1]])){
		rmatrix[d$A_ID[i],d$B_ID[i]]=1
	}
	gplot(rmatrix)
	#gplot3d(rmatrix)
}

# Create ratios between all A and B columns
joinAandBcols <- function(d){
	d$follower_count = d$A_follower_count/d$B_follower_count
	d$follower_count[is.infinite(d$follower_count)] <- d$A_follower_count[is.infinite(d$follower_count)]
	d$A_follower_count=NULL
	d$B_follower_count=NULL
	d$following_count = d$A_following_count/d$B_following_count
	d$following_count[is.infinite(d$following_count)] <- d$A_following_count[is.infinite(d$following_count)]
	d$A_following_count=NULL
	d$B_following_count=NULL
	d$listed_count = d$A_listed_count/d$B_listed_count
	d$listed_count[is.infinite(d$listed_count)] <- d$A_listed_count[is.infinite(d$listed_count)]
	d$A_listed_count=NULL
	d$B_listed_count=NULL
	d$mentions_received = d$A_mentions_received/d$B_mentions_received
	d$mentions_received[is.infinite(d$mentions_received)] <- d$A_mentions_received[is.infinite(d$mentions_received)]
	d$A_mentions_received=NULL
	d$B_mentions_received=NULL
	d$retweets_received = d$A_retweets_received/d$B_retweets_received
	d$retweets_received[is.infinite(d$retweets_received)] <- d$A_retweets_received[is.infinite(d$retweets_received)]
	d$A_retweets_received=NULL
	d$B_retweets_received=NULL
	d$mentions_sent = d$A_mentions_sent/d$B_mentions_sent
	d$mentions_sent[is.infinite(d$mentions_sent)] <- d$A_mentions_sent[is.infinite(d$mentions_sent)]
	d$A_mentions_sent=NULL
	d$B_mentions_sent=NULL
	d$retweets_sent  = d$A_retweets_sent /d$B_retweets_sent
	d$retweets_sent[is.infinite(d$retweets_sent)] <- d$A_retweets_sent[is.infinite(d$retweets_sent)]
	d$A_retweets_sent =NULL
	d$B_retweets_sent =NULL
	d$posts = d$A_posts/d$B_posts
	d$posts[is.infinite(d$posts)] <- d$A_posts[is.infinite(d$posts)]
	d$A_posts=NULL
	d$B_posts=NULL
	d$network_feature_1 = d$A_network_feature_1/d$B_network_feature_1
	d$network_feature_1[is.infinite(d$network_feature_1)] <- d$A_network_feature_1[is.infinite(d$network_feature_1)]
	d$network_feature_1[is.na(d$network_feature_1)] <- 0
	d$A_network_feature_1=NULL
	d$B_network_feature_1=NULL
	d$network_feature_2 = d$A_network_feature_2/d$B_network_feature_2
	d$network_feature_2[is.infinite(d$network_feature_2)] <- d$A_network_feature_2[is.infinite(d$network_feature_2)]
	d$network_feature_2[is.na(d$network_feature_2)] <- 0
	d$A_network_feature_2=NULL
	d$B_network_feature_2=NULL
	d$network_feature_3 = d$A_network_feature_3/d$B_network_feature_3
	d$network_feature_3[is.infinite(d$network_feature_3)] <- d$A_network_feature_3[is.infinite(d$network_feature_3)]
	d$network_feature_3[is.na(d$network_feature_3)] <- 0
	d$A_network_feature_3=NULL
	d$B_network_feature_3=NULL

	if(!is.null(d$A_FollowersFollowing)){
		d$FollowersFollowing = d$A_FollowersFollowing /d$B_FollowersFollowing 
		d$FollowersFollowing [is.infinite(d$FollowersFollowing)] <- d$A_FollowersFollowing[is.infinite(d$FollowersFollowing )]
		d$A_FollowersFollowing=NULL
		d$B_FollowersFollowing=NULL
	}

	return(d)
}

# Create ratios or differences between followers/following
createFollowersFollowingRatios <- function(d){
	d$A_FollowersFollowing <- 0
	d$B_FollowersFollowing <- 0
	d$A_FollowersFollowing=d$A_follower_count/d$A_following_count
	d$B_FollowersFollowing=d$B_follower_count/d$B_following_count
	d$A_FollowersFollowing[is.infinite(d$A_FollowersFollowing)] <- d$A_follower_count[is.infinite(d$A_FollowersFollowing)]
	d$B_FollowersFollowing[is.infinite(d$B_FollowersFollowing)] <- d$B_follower_count[is.infinite(d$B_FollowersFollowing)]
	d <- cbind(d[,1:(length(d)-13)],d[,(length(d)-1),drop=F],
		d[,(length(d)-12):(length(d)-2)],d[,(length(d)),drop=F])
	return(d)
}

# Create AB's and BA's
mirrorTheData <- function(d){
	mirrorData=d
	for(i in 2:13){
		mirrorData[i] = d[i+12]
		mirrorData[i+12] = d[i]
	}
	mirrorData[1] = ifelse(mirrorData[1] == 1,0,1)
	mirrorData = rbind(mirrorData, d)
	return(mirrorData[,c(1:25)])
}

# Use RMiner to do classification on a list of algorithms
classifyAll <- function(predictorsClass, dC, tC){
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
		write.csv(P,paste(predictorsClass[j], "C", sep=""), row.names=FALSE)
	}
}

# Use RMiner to do regression on a list of algorithms
regressionAll <- function(predictorsRegr, d, t){
	for(j in 1:length(predictorsBoth)) {
		print(predictorsRegr[j])
		RF=fit(Choice~.,d,model=predictorsRegr[j]) 
		PRFu=predict(RF,t)

		# Convert RF probabilities to 0s and 1s
		PRF=c(1:length(t[[1]]))
		for(i in 1:length(PRFu)) {
    			PRF[i] <- ifelse(PRFu[i] > 0.5,1,0)
		}

		# Save to a CSV
		P=data.frame(ID=c(1:length(t[[1]])),Choice=PRF)
		write.csv(P,paste(predictorsRegr[j], "R", sep=""), row.names=FALSE)
	}
}

# If A>B>C, then A>C
associateABCasAC <- function(d){
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
return(d)
}

# Identify users by characteristics, add an ID
addID <- function(d){
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
	return(c(digestList,d))
}

# Remove IDs
remID <- function(d){
	d$A_ID <- NULL
	d$B_ID <- NULL
	return(d)
}

############### CODE ###############

# Read values
setwd('C:\\Users\\BlueMoon\\Documents\\GitHub\\abi')
d=read.csv('newData.csv', TRUE, ',')
t=read.csv('test.csv', TRUE, ',')

# Create IDs and associate A>B>C as A>C on the dataset. Then remove the IDs
# This takes hours to run; consider loading "newData.csv"
#d=addID(d)
#showGraph(digestList)
#result=associateABCasAC(d)
#digestList=result[1]
#d=result[2]
#showGraph(digestList)
d=remID(d)

d = createFollowersFollowingRatios(d)
t = createFollowersFollowingRatios(t)

# Maybe normalize, discretize, divide my median/std, scale between 0 and 1

# instead of 0 and 1's, when opinions differ, we could set values in between

# mirror the data
d = mirrorTheData(d)

# create ratios between A and B columns
d = joinAandBcols(d)
t = joinAandBcols(t)

# Try GBM (GBDT), Elo System, etc

# Ignore sent retweets (feature selection)

# List of RMiner's algorithms
predictorsBoth = c("naive", "ctree", "dt", "kknn", "mlp", "mlpe", "ksvm", "randomforest")
predictorsClass = c("bagging", "boosting", "lda", "lr", "naivebayes", "qda")
predictorsRegr = c("mr", "mars", "cubist", "pcr", "plsr", "cppls", "rvm")

# Prepare the data to be learned and fitted
tC = t
dC = d
dC$Choice <- as.factor(dC$Choice)
tC$Choice <- as.factor(c(rep(1, length(t[[1]])/2),rep(0,length(t[[1]])/2)))
t$Choice <- c(rep(1, length(t[[1]])/2),rep(0,length(t[[1]])/2))

# Try all algorithms
regressionAll(predictorsBoth, d, t)
regressionAll(predictorsRegr, d, t)
classifyAll(predictorsBoth, dC, tC)
classifyAll(predictorsClass, dC, tC)

# Boosting		78%
RF=fit(Choice~.,dC,model="boosting") 
PRFu=predict(RF,tC)

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
