#install.packages("ROCR")
#install.packages("rminer")
#install.packages("digest")
#install.packages("sna")
#install.packages("rgl")
#install.packages("gbm")
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

joinAandBcolsD <- function(d){
	d$follower_count = d$A_follower_count-d$B_follower_count
	d$follower_count[is.infinite(d$follower_count)] <- d$A_follower_count[is.infinite(d$follower_count)]
	d$A_follower_count=NULL
	d$B_follower_count=NULL
	d$following_count = d$A_following_count-d$B_following_count
	d$following_count[is.infinite(d$following_count)] <- d$A_following_count[is.infinite(d$following_count)]
	d$A_following_count=NULL
	d$B_following_count=NULL
	d$listed_count = d$A_listed_count-d$B_listed_count
	d$listed_count[is.infinite(d$listed_count)] <- d$A_listed_count[is.infinite(d$listed_count)]
	d$A_listed_count=NULL
	d$B_listed_count=NULL
	d$mentions_received = d$A_mentions_received-d$B_mentions_received
	d$mentions_received[is.infinite(d$mentions_received)] <- d$A_mentions_received[is.infinite(d$mentions_received)]
	d$A_mentions_received=NULL
	d$B_mentions_received=NULL
	d$retweets_received = d$A_retweets_received-d$B_retweets_received
	d$retweets_received[is.infinite(d$retweets_received)] <- d$A_retweets_received[is.infinite(d$retweets_received)]
	d$A_retweets_received=NULL
	d$B_retweets_received=NULL
	d$mentions_sent = d$A_mentions_sent-d$B_mentions_sent
	d$mentions_sent[is.infinite(d$mentions_sent)] <- d$A_mentions_sent[is.infinite(d$mentions_sent)]
	d$A_mentions_sent=NULL
	d$B_mentions_sent=NULL
	d$retweets_sent  = d$A_retweets_sent-d$B_retweets_sent
	d$retweets_sent[is.infinite(d$retweets_sent)] <- d$A_retweets_sent[is.infinite(d$retweets_sent)]
	d$A_retweets_sent =NULL
	d$B_retweets_sent =NULL
	d$posts = d$A_posts-d$B_posts
	d$posts[is.infinite(d$posts)] <- d$A_posts[is.infinite(d$posts)]
	d$A_posts=NULL
	d$B_posts=NULL
	d$network_feature_1 = d$A_network_feature_1-d$B_network_feature_1
	d$network_feature_1[is.infinite(d$network_feature_1)] <- d$A_network_feature_1[is.infinite(d$network_feature_1)]
	d$network_feature_1[is.na(d$network_feature_1)] <- 0
	d$A_network_feature_1=NULL
	d$B_network_feature_1=NULL
	d$network_feature_2 = d$A_network_feature_2-d$B_network_feature_2
	d$network_feature_2[is.infinite(d$network_feature_2)] <- d$A_network_feature_2[is.infinite(d$network_feature_2)]
	d$network_feature_2[is.na(d$network_feature_2)] <- 0
	d$A_network_feature_2=NULL
	d$B_network_feature_2=NULL
	d$network_feature_3 = d$A_network_feature_3-d$B_network_feature_3
	d$network_feature_3[is.infinite(d$network_feature_3)] <- d$A_network_feature_3[is.infinite(d$network_feature_3)]
	d$network_feature_3[is.na(d$network_feature_3)] <- 0
	d$A_network_feature_3=NULL
	d$B_network_feature_3=NULL

	if(!is.null(d$A_FollowersFollowing)){
		d$FollowersFollowing = d$A_FollowersFollowing-d$B_FollowersFollowing 
		d$FollowersFollowing [is.infinite(d$FollowersFollowing)] <- d$A_FollowersFollowing[is.infinite(d$FollowersFollowing )]
		d$A_FollowersFollowing=NULL
		d$B_FollowersFollowing=NULL
	}

	return(d)
}

# Create ratios between all A and B columns
joinAandBcolsWithDeltas <- function(d){
	d$follower_count = d$A_follower_count/d$B_follower_count
	d$follower_countD = d$A_follower_count-d$B_follower_count
	d$follower_count[is.infinite(d$follower_count)] <- d$A_follower_count[is.infinite(d$follower_count)]
	
	d$following_count = d$A_following_count/d$B_following_count
	d$following_countD= d$A_following_count-d$B_following_count
	d$following_count[is.infinite(d$following_count)] <- d$A_following_count[is.infinite(d$following_count)]
	
	d$listed_count = d$A_listed_count/d$B_listed_count
	d$listed_countD= d$A_listed_count-d$B_listed_count
	d$listed_count[is.infinite(d$listed_count)] <- d$A_listed_count[is.infinite(d$listed_count)]

	d$mentions_received = d$A_mentions_received/d$B_mentions_received
	d$mentions_receivedD= d$A_mentions_received-d$B_mentions_received
	d$mentions_received[is.infinite(d$mentions_received)] <- d$A_mentions_received[is.infinite(d$mentions_received)]

	d$retweets_received = d$A_retweets_received/d$B_retweets_received
	d$retweets_receivedD= d$A_retweets_received-d$B_retweets_received
	d$retweets_received[is.infinite(d$retweets_received)] <- d$A_retweets_received[is.infinite(d$retweets_received)]

	d$mentions_sent = d$A_mentions_sent/d$B_mentions_sent
	d$mentions_sentD= d$A_mentions_sent-d$B_mentions_sent
	d$mentions_sent[is.infinite(d$mentions_sent)] <- d$A_mentions_sent[is.infinite(d$mentions_sent)]

	d$retweets_sent  = d$A_retweets_sent/d$B_retweets_sent
	d$retweets_sentD = d$A_retweets_sent-d$B_retweets_sent
	d$retweets_sent[is.infinite(d$retweets_sent)] <- d$A_retweets_sent[is.infinite(d$retweets_sent)]

	d$posts = d$A_posts/d$B_posts
	d$postsD = d$A_posts-d$B_posts
	d$posts[is.infinite(d$posts)] <- d$A_posts[is.infinite(d$posts)]

	d$network_feature_1 = d$A_network_feature_1/d$B_network_feature_1
	d$network_feature_1D = d$A_network_feature_1-d$B_network_feature_1
	d$network_feature_1[is.infinite(d$network_feature_1)] <- d$A_network_feature_1[is.infinite(d$network_feature_1)]
	d$network_feature_1[is.na(d$network_feature_1)] <- 0

	d$network_feature_2 = d$A_network_feature_2/d$B_network_feature_2
	d$network_feature_2D = d$A_network_feature_2-d$B_network_feature_2
	d$network_feature_2[is.infinite(d$network_feature_2)] <- d$A_network_feature_2[is.infinite(d$network_feature_2)]
	d$network_feature_2[is.na(d$network_feature_2)] <- 0

	d$network_feature_3 = d$A_network_feature_3/d$B_network_feature_3
	d$network_feature_3D = d$A_network_feature_3-d$B_network_feature_3
	d$network_feature_3[is.infinite(d$network_feature_3)] <- d$A_network_feature_3[is.infinite(d$network_feature_3)]
	d$network_feature_3[is.na(d$network_feature_3)] <- 0

	if(!is.null(d$A_FollowersFollowing)){
		d$FollowersFollowing = d$A_FollowersFollowing /d$B_FollowersFollowing 
		d$FollowersFollowingD = d$A_FollowersFollowing-d$B_FollowersFollowing 
		d$FollowersFollowing [is.infinite(d$FollowersFollowing)] <- d$A_FollowersFollowing[is.infinite(d$FollowersFollowing )]
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
classifyBoth <- function(predictorsClass, dC, tC){
	for(j in 1:length(predictorsClass)) {
		print(predictorsClass[j])
		knnParams=mining(Choice~.,d,model=predictorsClass[j],Runs=5,method=c("kfold",3),search="heuristic5") #,f="s"
		RF=fit(Choice~.,dC,model=predictorsClass[j],search=knnParams$mpar) 
		PRFu=predict(RF,tC)
	
	k=0
	xVals=c(0)
	yVals=c(0)
	acc=-1
	#print(paste("(0,0)"))
	while(k<=1){
		# Convert RF probabilities to 0s and 1s
		PRF=c(1:length(tC[[1]]))
			for(i in 1:length(PRFu[,1])) {
   				PRF[i] <- ifelse((PRFu[i,1] - PRFu[i,2] + 1) / 2 < k,1,0)
		}

		pred = prediction(PRF, t$Choice)
		perf=performance(pred,"tpr","fpr")
		if(length(perf@x.values[[1]])==3){
			xVals=c(xVals,perf@x.values[[1]][2])
			yVals=c(yVals,perf@y.values[[1]][2])
			#print(paste("(",perf@x.values[[1]][2],",",perf@y.values[[1]][2],")"))
		}
		if(k==0.5){
			#print(paste("test acc: ",perf@y.values[[1]][2]))
			#acc=perf@y.values[[1]][2]
			acc.tmp = performance(pred,"auc");
			acc = as.numeric(acc.tmp@y.values);
		}
		k=k+0.1
	}
	#print(paste("(1,1)"))
	print(paste("Acc: ",acc))
	xVals=c(xVals,1)
	yVals=c(yVals,1)
	perf@x.values[[1]]=xVals
	perf@y.values[[1]]=yVals
	plot(perf, lwd=3, col="red",spread.estimate="stderror",plotCI.lwd=2)
	#print(xVals)
	#print(yVals)


		# Convert RF probabilities to 0s and 1s
		PRF=c(1:length(t[[1]]))
		for(i in 1:length(PRFu[,1])) {
    			PRF[i] <- ifelse(PRFu[i,1] < PRFu[i,2],1,0)
		}
	
		# Save to a CSV
		P=data.frame(ID=c(1:length(t[[1]])),Choice=PRF)
		write.csv(P,paste(predictorsClass[j], "C", sep=""), row.names=FALSE)

		# Save probabilites of prediction to CSV
		P=data.frame(ID=c(1:length(t[[1]])),Choice0=PRFu[,1], Choice1=PRFu[,2])
		write.csv(P,paste(predictorsClass[j], "Cprobs", sep=""), row.names=FALSE)
	}
}

# Use RMiner to do classification on a list of algorithms
classifyAll <- function(predictorsClass, dC, tC){
	for(j in 1:length(predictorsClass)) {
		print(predictorsClass[j])
		knnParams=mining(Choice~.,d,model=predictorsClass[j],Runs=5,method=c("kfold",3),search="heuristic5",f="s")
		RF=fit(Choice~.,dC,model=predictorsClass[j],search=knnParams$mpar) 
		PRFu=predict(RF,tC)
	
	k=0
	xVals=c(0)
	yVals=c(0)
	acc=-1
	print(paste("(0,0)"))
	while(k<=1){
		# Convert RF probabilities to 0s and 1s
		PRF=c(1:length(tC[[1]]))
			for(i in 1:length(PRFu[,1])) {
   				PRF[i] <- ifelse((PRFu[i,1] - PRFu[i,2] + 1) / 2 < k,1,0)
		}

		pred = prediction(PRF, t$Choice)
		perf=performance(pred,"tpr","fpr")
		if(length(perf@x.values[[1]])==3){
			xVals=c(xVals,perf@x.values[[1]][2])
			yVals=c(yVals,perf@y.values[[1]][2])
			print(paste("(",perf@x.values[[1]][2],",",perf@y.values[[1]][2],")"))
		}
		if(k==0.5){
			#print(paste("test acc: ",perf@y.values[[1]][2]))
			#acc=perf@y.values[[1]][2]
			acc.tmp = performance(pred,"auc");
			acc = as.numeric(acc.tmp@y.values);
		}
		k=k+0.1
	}
	print(paste("(1,1)"))
	
	print(paste("Acc: ",acc))
	xVals=c(xVals,1)
	yVals=c(yVals,1)
	perf@x.values[[1]]=xVals
	perf@y.values[[1]]=yVals
	plot(perf, lwd=3, col="red",spread.estimate="stderror",plotCI.lwd=2)
	#print(xVals)
	#print(yVals)


		# Convert RF probabilities to 0s and 1s
		PRF=c(1:length(t[[1]]))
		for(i in 1:length(PRFu[,1])) {
    			PRF[i] <- ifelse(PRFu[i,1] < PRFu[i,2],1,0)
		}
	
		# Save to a CSV
		P=data.frame(ID=c(1:length(t[[1]])),Choice=PRF)
		write.csv(P,paste(predictorsClass[j], "C", sep=""), row.names=FALSE)

		# Save probabilites of prediction to CSV
		P=data.frame(ID=c(1:length(t[[1]])),Choice0=PRFu[,1], Choice1=PRFu[,2])
		write.csv(P,paste(predictorsClass[j], "Cprobs", sep=""), row.names=FALSE)
	}
}



# Use RMiner to do classification on an algorithm
classify <- function(predictorsClass, dC, tC){
	RF=fit(Choice~.,dC,model=predictorsClass) 
	PRFu=predict(RF,tC) #change this back to tC

	j=0
	xVals=c(0)
	yVals=c(0)
	while(j<=1){
		# Convert RF probabilities to 0s and 1s
		PRF=c(1:length(tC[[1]]))
			for(i in 1:length(PRFu[,1])) {
   				PRF[i] <- ifelse((PRFu[i,1] - PRFu[i,2] + 1) / 2 < j,1,0)
		}

		pred = prediction(PRF, d$Choice)
		perf=performance(pred,"tpr","fpr")
		if(length(perf@x.values[[1]])==3){
			xVals=c(xVals,perf@x.values[[1]][2])
			yVals=c(yVals,perf@y.values[[1]][2])
		}

		j=j+0.1
	}
	xVals=c(xVals,1)
	yVals=c(yVals,1)
	perf@x.values[[1]]=xVals
	perf@y.values[[1]]=yVals
	plot(perf, lwd=3, col="red",spread.estimate="stderror",plotCI.lwd=2)
	print(xVals)
	print(yVals)

	# Convert RF probabilities to 0s and 1s
	PRF=c(1:length(t[[1]]))
		for(i in 1:length(PRFu[,1])) {
   			PRF[i] <- ifelse((PRFu[i,1] - PRFu[i,2] + 1) / 2 < 0.5,1,0)
	}

	# Save to a CSV
	P=data.frame(ID=c(1:length(t[[1]])),Choice=PRF)
	write.csv(P,paste(predictorsClass, "C", sep=""), row.names=FALSE)
	
	# Save probabilites of prediction to CSV
	P=data.frame(ID=c(1:length(tC[[1]])),Choice0=PRFu[,1], Choice1=PRFu[,2])
	write.csv(P,paste(predictorsClass, "Cprobs", sep=""), row.names=FALSE)

	return(perf)
}

# Use RMiner to do regression on a list of algorithms
regressionAll <- function(predictorsRegr, d, t){
	
	for(j in 1:length(predictorsRegr)) {
		print(predictorsRegr[j])
		knnParams=mining(Choice~.,d,model=predictorsRegr[j],Runs=5,method=c("kfold",3),search="heuristic5",f="s")
		RF=fit(Choice~.,d,model=predictorsRegr[j],search=knnParams$mpar) 
		PRFu=predict(RF,t)

	k=0
	xVals=c(0)
	yVals=c(0)
	acc=-1
	print(paste("(0,0)"))
	while(k<=1){
		# Convert RF probabilities to 0s and 1s
		PRF=c(1:length(t[[1]]))
			for(i in 1:length(PRFu)) {
   				PRF[i] <- ifelse(PRFu[i] > k,1,0)
		}

		pred = prediction(PRF, t$Choice)
		perf=performance(pred,"tpr","fpr")
		if(length(perf@x.values[[1]])==3){
			xVals=c(xVals,perf@x.values[[1]][2])
			yVals=c(yVals,perf@y.values[[1]][2])
			print(paste("(",perf@x.values[[1]][2],",",perf@y.values[[1]][2],")"))
		}
		if(k==0.5){
			#print(paste("test acc: ",perf@y.values[[1]][2]))
			#acc=perf@y.values[[1]][2]
			acc.tmp = performance(pred,"auc");
			acc = as.numeric(acc.tmp@y.values);
		}
		k=k+0.1
	}
	print(paste("(1,1)"))


		# Convert RF probabilities to 0s and 1s
		PRF=c(1:length(t[[1]]))
		for(i in 1:length(PRFu)) {
    			PRF[i] <- ifelse(PRFu[i] > 0.5,1,0)
		}

		pred = prediction(PRF, t$Choice)
		acc.tmp = performance(pred,"auc");
		print(paste(as.numeric(acc.tmp@y.values)));


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

# Identify users by characteristics, add an ID
turnFactorIntoRegression <- function(d, idA, idB){
	#if(length(idA)*2==length(d[[1]])){
	#	mirrorIdA=c(idA,idB)
	#	mirrorIdB=c(idB,idA)
	#	idA3=mirrorIdA
	#	idB3=mirrorIdB
	#}
	d$A_ID <- idA
	d$B_ID <- idB

	# now compare all IDs and get average
	# for example, if we had A>B, A<B, A>B, B<A, 
	# then we have 3 A>B and 1 B>A
	# which equates to A<>B with 0.75 and B<>A with 0.25
	
	return(d)
}

# Remove IDs
remID <- function(d){
	d$A_ID <- NULL
	d$B_ID <- NULL
	return(d)
}

gbmFunction <- function(d, t){

all <- rbind(d, t)
library(gbm)

set.seed(1234)
#  Now rework with binomial loss
fit.gbm3 <- gbm(Choice ~ ., data=d, dist="bernoulli", n.tree = 400,
    shrinkage = 1, train.fraction = 1)
#gbm.perf(fit.gbm3, method="test")

predictions=predict(fit.gbm3, t, n.trees = 255)

	k=-8
	xVals=c(0)
	yVals=c(0)
	acc=-1
	print(paste("(0,0)"))
	while(k<=8){
		# Convert RF probabilities to 0s and 1s
		PRF=c(1:length(t[[1]]))
			for(i in 1:length(predictions)) {
   				PRF[i] <- ifelse(predictions[i] > k,1,0)
		}

		pred = prediction(PRF, t$Choice)
		perf=performance(pred,"tpr","fpr")
		if(length(perf@x.values[[1]])==3){
			xVals=c(xVals,perf@x.values[[1]][2])
			yVals=c(yVals,perf@y.values[[1]][2])
			print(paste("(",perf@x.values[[1]][2],",",perf@y.values[[1]][2],")"))
		}
		if(k==0){
			#print(paste("test acc: ",perf@y.values[[1]][2]))
			#acc=perf@y.values[[1]][2]
			acc.tmp = performance(pred,"auc");
			acc = as.numeric(acc.tmp@y.values);
		}
		k=k+1
	}
	print(paste("(1,1)"))


fit.gbm3 <- gbm(Choice ~ ., data=d, dist="bernoulli", n.tree = 400,
    shrinkage = 1, train.fraction = 1)

predictions=predict(fit.gbm3, t, n.trees = 255)
PRF=c(1:length(t[[1]]))
for(i in 1:length(PRF)) {
    	PRF[i] <- ifelse(predictions[i] > 0,1,0)
}
pred = prediction(PRF, t$Choice)
acc.tmp = performance(pred,"auc");
acc = as.numeric(acc.tmp@y.values);
print(acc)

confusion(predict(fit.gbm3, t, n.trees = 255) > 0, test.data2$y > 0)

}

############### CODE ###############
set.seed(1234)
# Read values
setwd('C:\\Users\\BlueMoon\\Documents\\GitHub\\abi')
d=read.csv('train.csv', TRUE, ',')
t=read.csv('test.csv', TRUE, ',')

realVals=t$Choice
# Create IDs and associate A>B>C as A>C on the dataset. Then remove the IDs
# This takes hours to run; consider loading "newData.csv"
#d=addID(d)
idA=d$A_ID
idB=d$B_ID
#showGraph(digestList)
#result=associateABCasAC(d)
#digestList=result[1]
#d=result[2]
#showGraph(digestList)
d=remID(d)

d = createFollowersFollowingRatios(d)
t = createFollowersFollowingRatios(t)

# mirror the data
d = mirrorTheData(d)

# create ratios between A and B columns
d = joinAandBcolsWithDeltas(d)
t = joinAandBcolsWithDeltas(t)

# Normalize with X-min/(max-min), to scale between 0 and 1
#for(i in 2:length(d)){
#	max=max(d[[i]])
#	min=min(d[[i]])
#	d[[i]]=(d[[i]]-min)/(max-min)
#}
#for(i in 1:length(t)){
#	max=max(t[[i]])
#	min=min(t[[i]])
#	t[[i]]=(t[[i]]-min)/(max-min)
#}

# Try GBM (GBDT), Elo System, etc
# Ignore sent retweets (feature selection)

# List of RMiner's algorithms
predictorsBoth = c("ctree", "dt", "kknn", "mlp", "mlpe", "ksvm", "randomforest")
predictorsClass = c("bagging", "boosting", "lda", "lr", "naivebayes", "qda")
predictorsRegr = c("mr", "mars", "cubist", "pcr", "plsr", "cppls")

# Prepare the data to be learned and fitted
tC = t
dC = d
dC$Choice <- as.factor(dC$Choice)
tC$Choice <- as.factor(c(rep(1, length(t[[1]])/2),rep(0,length(t[[1]])/2)))
tC$Choice <- as.factor(tC$Choice)
t$Choice <- c(rep(1, length(t[[1]])/2),rep(0,length(t[[1]])/2))
#perf=classify("boosting", dC, dC)

# instead of 0 and 1's, when opinions differ, we could set values in between
# but since regressions isnt used, no need to do it

# Try all algorithms
regressionAll(predictorsBoth, d, t)
regressionAll(predictorsRegr, d, t)
classifyBoth(predictorsBoth, dC, tC)
classifyAll(predictorsClass, dC, tC)

for(j in 1:length(predictorsBoth)) {
	print(predictorsBoth[j])
	classify(predictorsBoth[j], dC, tC)
}
for(j in 1:length(predictorsClass)) {
	print(predictorsClass[j])
	classify(predictorsClass[j], dC, tC)
}

knnParams=mining(Choice~.,dC,model="boosting",Runs=5,method=c("kfold",3),
	search="heuristic5",f="s")
RF=fit(Choice~.,dC,model="boosting",search=knnParams$mpar) 
PRFu1=predict(RF,tC)
PRF1=c(rep(0,length(t[[1]])))
for(i in 1:length(t[[1]])) {
    PRF1[i] <- ifelse(PRFu1[i,1] < PRFu1[i,2],
		PRFu1[i,2]-PRFu1[i,1],
		-(PRFu1[i,1]-PRFu1[i,2]))
}

RF <- gbm(Choice ~ ., data=d, dist="bernoulli", n.tree = 400, 
	shrinkage = 1, train.fraction = 1)
PRFu2=predict(RF, t, n.trees = 255)
PRF2=PRFu2/7
#PRF2=c(rep(0,length(t[[1]])))
#for(i in 1:length(t[[1]])) {
    #PRF2[i]=ifelse(PRFu2[i]<-8,-8,PRFu2[i])
    #PRF2[i]=ifelse(PRFu2[i]>8,8,PRFu2[i])
#    PRF2[i]=PRFu2[i]/7
#}

knnParams=mining(Choice~.,d,model="mars",Runs=5,method=c("kfold",3),
	search="heuristic5",f="s")
RF=fit(Choice~.,d,model="mars",search=knnParams$mpar) 
PRFu3=predict(RF,t)
PRF3=PRFu3*2-1

knnParams=mining(Choice~.,dC,model="mlpe",Runs=5,method=c("kfold",3),
	search="heuristic5",f="s")
RF=fit(Choice~.,dC,model="mlpe",search=knnParams$mpar) 
PRFu4=predict(RF,tC)
PRF4=c(rep(0,length(t[[1]])))
for(i in 1:length(t[[1]])) {
    PRF4[i] <- ifelse(PRFu4[i,1] < PRFu4[i,2],
		PRFu4[i,2]-PRFu4[i,1],
		-(PRFu4[i,1]-PRFu4[i,2]))
}


knnParams=mining(Choice~.,d,model="cubist",Runs=5,method=c("kfold",3),
	search="heuristic5",f="s")
RF=fit(Choice~.,d,model="cubist",search=knnParams$mpar) 
PRFu5=predict(RF,t)
PRF5=PRFu5*2-1

knnParams=mining(Choice~.,dC,model="ctree",Runs=5,method=c("kfold",3),
	search="heuristic5")
RF=fit(Choice~.,dC,model="ctree",search=knnParams$mpar) 
PRFu6=predict(RF,tC)
PRF6=c(rep(0,length(t[[1]])))
for(i in 1:length(t[[1]])) {
    PRF6[i] <- ifelse(PRFu6[i,1] < PRFu6[i,2],
		PRFu6[i,2]-PRFu6[i,1],
		-(PRFu6[i,1]-PRFu6[i,2]))
}


results=c(rep(0,length(t[[1]])))
for(i in 1:length(t[[1]])){
	PRF1[i] <- ifelse(PRF1[i] > 0,PRF1[i]^2,-(PRF1[i]^2))
	PRF2[i] <- ifelse(PRF2[i] > 0,PRF2[i]^2,-(PRF2[i]^2))
	PRF3[i] <- ifelse(PRF3[i] > 0,PRF3[i]^2,-(PRF3[i]^2))
	PRF4[i] <- ifelse(PRF4[i] > 0,PRF4[i]^2,-(PRF4[i]^2))
	PRF5[i] <- ifelse(PRF5[i] > 0,PRF5[i]^2,-(PRF5[i]^2))
	PRF6[i] <- ifelse(PRF6[i] > 0,PRF6[i]^2,-(PRF6[i]^2))
}
for(i in 1:length(t[[1]])){
	results[i]=PRF1[i]+PRF2[i]
#+PRF3[i]+PRF4[i]+PRF5[i]+PRF6[i]
}

for(i in 1:length(t[[1]])){
	results[i] <- ifelse(results[i] > 0,1,0)
}

P=data.frame(ID=c(1:length(t[[1]])),Choice=results)
write.csv(P,"results.csv", row.names=FALSE)

pred = prediction(results, t$Choice)
acc.tmp = performance(pred,"auc");
acc = as.numeric(acc.tmp@y.values);
print(acc)







