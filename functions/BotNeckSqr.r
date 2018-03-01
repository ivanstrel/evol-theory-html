BotNeckSqr<-function(PopSize=1600, BotSize=30,P_a=1,P_b=1,P_c=1,P_d=1){
	#Function to simulate bottle neck effect in population
	#PopSize -- initial size of population
	#BotSize -- population size after bottle neck reduction
	#P_a, P_b, P_c, P_d -- initial probabilities of a,b,c and d genotipes
	
	#It looks best with: dev.new(height=8,width=14,unit="in")
	
	X<-ceiling(sqrt(PopSize))#Calculate plotting square dimensions
	A<-rep(NA,X**2)
	#Generate initial combination of genotipes
	A[c(1:PopSize)]<-sample(c(2,3,4,5),PopSize,replace=TRUE,prob=c(P_a,P_b,P_c,P_d))
	A<-rev(A)
	#Creating matrix with polygones centres location
	XY<-expand.grid(c(1:X),c(1:X))
	foo<-function(x){
		#Internal function to plot polygons in given locations
		A<-x[1]+c(-0.5,0.5,0.5,-0.5)
		B<-x[2]+c(0.5,0.5,-0.5,-0.5)
		polygon(A,B,col=x[3],border="lightgray")
	}
	#Create layout for future plotting
	layout( matrix(c(1,1,1,1,1,1,2,3,4,5),2,5))
	par(mar=c(0,0,2,0))
	#Calculating obtained probabilities of genotipes
	Prob_a<-as.character(round(sum(A==2,na.rm=TRUE)/PopSize,3))
	Prob_b<-as.character(round(sum(A==3,na.rm=TRUE)/PopSize,3))
	Prob_c<-as.character(round(sum(A==4,na.rm=TRUE)/PopSize,3))
	Prob_d<-as.character(round(sum(A==5,na.rm=TRUE)/PopSize,3))
	#Create string to be plotted as main text (it will contain probabilities)
	Prob_text<-paste("A=",Prob_a," ","B=",Prob_b," ","C=",Prob_c," ","D=",Prob_d,sep="")
	plot(x=NULL,xlim=range(1:X)+c(-0.5,0.5),ylim=range(1:X)+c(-0.5,0.5),
	bty="n",xaxt="n",yaxt="n",main=Prob_text)
	#Plot polygones
	apply(cbind(XY,A),1,foo)
	
	#Add 4 new frames with bottle neck population size
	for (i in c(1:4)){
		#Recalculate size of bottle neck square
		X_b<-ceiling(sqrt(BotSize))
		A_b<-rep(NA,X_b**2)
		#Generate bottle neck genotipes
		A_b[c(1:BotSize)]<-sample(A[c(1:PopSize)],BotSize,rep=TRUE)
		A_b<-rev(A_b)
		XY_b<-expand.grid(c(1:X_b),c(1:X_b))
		#Calculating obtained probabilities of genotipes after bottle neck
		Prob_a<-as.character(round(sum(A_b==2,na.rm=TRUE)/BotSize,2))
		Prob_b<-as.character(round(sum(A_b==3,na.rm=TRUE)/BotSize,2))
		Prob_c<-as.character(round(sum(A_b==4,na.rm=TRUE)/BotSize,2))
		Prob_d<-as.character(round(sum(A_b==5,na.rm=TRUE)/BotSize,2))
		par(mar=c(2,0,4,0))
		#Create string to be plotted as main text
		Prob_text<-paste("A=",Prob_a," ","B=",Prob_b," ","C=",Prob_c," ","D=",Prob_d,sep="")
		plot(x=NULL,xlim=range(1:X_b)+c(-0.5,0.5),ylim=range(1:X_b)+c(-0.5,0.5),bty="n",xaxt="n",yaxt="n",main=Prob_text)
		#Add polygones
		apply(cbind(XY_b,A_b),1,foo)
	}
}
