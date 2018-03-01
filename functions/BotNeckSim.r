BotNeckSim<-function(PopSize=1500,BotSize=10,fA=0.5,fB=0.5,fC=0.5,fD=0.5,fE=0.5){
	#NGen=200
	#Bottleneck reduction on 30 step
	FreqVector<-c(fA,fB,fC,fD,fE)
	ColMat<-c("darkorange","dodgerblue","firebrick1","deeppink","chartreuse")
	AddTransparency<-function(color,alpha=0.4){
	#Add alpha level to color in character notation
		T<-as.numeric(col2rgb(color)/255)
		color<-rgb(T[1],T[2],T[3],alpha)
		return(color)
	}
	ColT<-as.character(sapply(ColMat,AddTransparency))
	#Add new tranparent colors to color matrix
	ColMat<-as.matrix(rbind(ColMat,ColT))
	
	SampleSize<-rep(PopSize,29)
	SampleSize<-c(SampleSize,BotSize)
	#Growth with sigmoidal function
	T<-seq(-6,6,by=0.1)
	S<-PopSize/(1+exp(1)**(-T))
	#Add growth curve to SampleSize vector
	S<-S[S>BotSize]
	SampleSize<-c(SampleSize,S)
	#Expound SampleSize vector to length 180 with PopSize values
	SampleSize<-c(SampleSize,rep(PopSize,160-length(SampleSize)))
	
	GenFun<-function(size,prob){
	#Function to perform computation of allele frequence
	#in next generation
		Temp<-rbinom(1,size,prob)
		return(Temp/size)
	}
	
	#Calculation of all frequences vectors
	ResFreq<-NULL
	for (i in c(1:5)){
		Res<-FreqVector[i]
		for (j in c(2:length(SampleSize))){
			Res<-c(Res,GenFun(round(SampleSize[j]*2),Res[length(Res)]))
		}
		ResFreq<-cbind(ResFreq,Res)
	}
	colnames(ResFreq)<-c("A","B","C","D","E")
	
	#Plotting
	layout(matrix(c(1,2,1,3),2))
	#Plot initial frame for frequences
	par(mar=c(3,3,2,3),mgp=c(1,0.25,0),tck=0.01)
	plot(NULL,xlim=c(1,length(SampleSize)+1),ylim=c(0,PopSize+PopSize*0.05),xaxt="n",yaxt="n",ylab="",xlab="")
	#Add polygon of population size
	Px<-c(1:length(SampleSize))
	Px<-c(Px[1],Px,Px[length(Px)])
	Py<-c(0,SampleSize,0)
	polygon(Px,Py,col="lightgrey",border="lightgrey")
	#Add some axis labels and text
	axis(side=4)
	axis(side=1)
	mtext(side=4,line=1.2,"Размер популяции",font=2,cex=1.1)
	mtext(side=1,line=1.2,"Поколение")
	
	#Add lines for each frequence
	par(new=TRUE)
	plot(NULL,xlim=c(1,length(SampleSize)+1),ylim=c(0,1),xaxt="n",yaxt="n",ylab="",xlab="")
	for (i in c(1:5)){
		points(ResFreq[,i],t="l",lwd=2,col=rev(ColMat[1,])[i])
	}
	axis(side=2)
	mtext(side=2,line=1.2,"Частота аллели",font=2,cex=1.1)
	
	#Add barplots of frequences
		#Calcilate matrixes for barplots
	FSt<-ResFreq[5,]
	FEn<-ResFreq[length(ResFreq[,1])-5,]
	FSt<-rbind(FSt,1-FSt)
	FEn<-rbind(FEn,1-FEn)
	TSt<-matrix(rep(0,50),10)
	TEn<-matrix(rep(0,50),10)
	for (i in c(1:5)){
		M<-c(9,10)
		TSt[M-(i-1)*2,i]<-FSt[,i]
		TEn[M-(i-1)*2,i]<-FEn[,i]
	}
	colnames(TSt)<-colnames(FSt)
	colnames(TEn)<-colnames(FEn)
		#Add barplots to plotting window
	par(mar=c(3,3,2,1))
	barplot(TSt,col=ColMat,main="До редукции")
	mtext(side=2,"Частота аллели",line=1.2,font=2,cex=1.1)
	par(mar=c(3,1,2,3))
	barplot(TEn,col=ColMat,main="После редукции",yaxt="n")
	axis(side=4)
	mtext(side=4,"Частота аллели",line=1.2,font=2,cex=1.1)
}
