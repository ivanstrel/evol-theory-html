HWCommonCase<-function(PopN=200,PopSize=200){
	#Function for demonstration of hardy weinberg equilibrium
	P<-seq(0,1,length.out=PopN)
	Q<-1-P
	PQ<-cbind(P,Q)
	Freq<-function(x,PopSize){
		#Internal function for new frequences culculation
		#x -- frequence of p or (A) allele
		A<-rmultinom(1,PopSize,prob=c((x[1]**2),(2*x[1]*x[2]),(x[2]**2)))
		A<-as.numeric(A)
		A<-A/PopSize
		return(A)
	}
	#Calculation of Hardy-Weinberg frequences for each
	#starting conditions
	FreqVect<-apply(PQ,1,Freq,PopSize=PopSize)
	FreqVect<-t(FreqVect)
	colnames(FreqVect)<-c("AA","Aa","aa")
	
	#Calculate teoretical (expected) genotypes frequences
	AA_line<-seq(0,1,length.out=100)**2
	Aa_line<-2*seq(0,1,length.out=100)*(1-seq(0,1,length.out=100))
	aa_line<-(1-seq(0,1,length.out=100))**2
	
	#Plot expected frequences as lines
	plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="Частоты гамет f(A) или 1/f(a)",ylab="Частоты генотипов",main="Распределение частот по Харди и Вайнбергу",font=2,font.lab=2)
	points(seq(0,1,length.out=100),AA_line,t="l",lwd=2,col=2)
	points(seq(0,1,length.out=100),Aa_line,t="l",lwd=2,col="forestgreen")
	points(seq(0,1,length.out=100),aa_line,t="l",lwd=2,col="blue")
	
	#Plot simulated values as points
	points(P,FreqVect[,1],col="red")
	points(P,FreqVect[,2],col="forestgreen")
	points(P,FreqVect[,3],col="blue")
	#Add the legend to the plot
	legend("top",legend=c("АА","Аа","аа"),pch=c(1,1,1),lwd=c(2,2,2),col=c("red","forestgreen","blue"),ins=0.1)
}
	