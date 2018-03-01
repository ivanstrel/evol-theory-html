HW_isolation<-function(NGen=150, Isolat=FALSE, fP=0.5){
	#Function for simulation of population differentiation
	#after isolation
	#Initial population size =
	PopSize=1800
	#Palette for plotting
	Palette=c("brown1","olivedrab2","lightblue")
	
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#	Main part
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	if (Isolat){
		fP1<-fP
		fP2<-fP
		Pop1<-sample(c(1,2,3),900,prob=c(fP1**2,2*fP1*(1-fP1),(1-fP1)**2),rep=TRUE)
		Pop2<-sample(c(1,2,3),900,prob=c(fP2**2,2*fP2*(1-fP2),(1-fP2)**2),rep=TRUE)
		Restab1<-matrix(as.numeric(table(Pop1)),1)
		Restab2<-matrix(as.numeric(table(Pop2)),1)
		for (i in c(1:(NGen-1))){
			fP1<-(Restab1[length(Restab1[,1]),1]*2+Restab1[length(Restab1[,1]),2])/(PopSize)
			fP2<-(Restab2[length(Restab2[,1]),1]*2+Restab2[length(Restab2[,1]),2])/(PopSize)
			Pop1<-sample(c(1,2,3),900,prob=c(fP1**2,2*fP1*(1-fP1),(1-fP1)**2),rep=TRUE)
			Pop2<-sample(c(1,2,3),900,prob=c(fP2**2,2*fP2*(1-fP2),(1-fP2)**2),rep=TRUE)
			Restab1<-rbind(Restab1,matrix(as.numeric(table(c(Pop1,c(1,2,3)))),1)-c(1,1,1))
			Restab2<-rbind(Restab2,matrix(as.numeric(table(c(Pop2,c(1,2,3)))),1)-c(1,1,1))
		}
		Pop<-c(Pop1,Pop2)
	}else{
		fP1<-fP
		Pop1<-sample(c(1,2,3),1800,prob=c(fP1**2,2*fP1*(1-fP1),(1-fP1)**2),rep=TRUE)
		Restab1<-matrix(as.numeric(table(Pop1[1:900])),1)
		Restab2<-matrix(as.numeric(table(Pop1[901:1800])),1)
		for (i in c(1:(NGen-1))){
			fP1<-(Restab1[length(Restab1[,1]),1]*2+Restab1[length(Restab1[,1]),2])/(PopSize*2)
			fP1<-fP1+(Restab2[length(Restab2[,1]),1]*2+Restab2[length(Restab2[,1]),2])/(PopSize*2)
			Pop1<-sample(c(1,2,3),1800,prob=c(fP1**2,2*fP1*(1-fP1),(1-fP1)**2),rep=TRUE)
			Restab1<-rbind(Restab1,matrix(as.numeric(table(c(Pop1[1:900],c(1,2,3)))),1)-c(1,1,1))
			Restab2<-rbind(Restab2,matrix(as.numeric(table(c(Pop1[901:1800],c(1,2,3)))),1)-c(1,1,1))
		}
		Pop<-Pop1
	}
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#	Plotting part
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#Set layout
	layout(matrix(c(1,2,1,3),2,2))
	
	#Grid preparation
	XY<-rbind(expand.grid(1:30,1:30),expand.grid(31:60,1:30))
	XY<-cbind(XY,Pop)
	foo<-function(x,col){
		#Internal function to plot polygons in given locations
		A<-x[1]+c(-0.5,0.5,0.5,-0.5)
		B<-x[2]+c(0.5,0.5,-0.5,-0.5)
		polygon(A,B,col=col[x[3]],border="lightgray")
	}
	
	#Cell plotting
	par(mar=c(1,0,0,0))
	Xlim<-c(0.5,60.5)
	Ylim<-c(0.5,30.5)
	plot(NA,xlim=Xlim,ylim=Ylim,bty="n",xaxt="n",yaxt="n",xaxt="n",xlab="",ylab="")
	A<-apply(XY,1,FUN=foo,col=Palette)
	#add isolation line
	if (Isolat){
		abline(v=30.5,lwd=4)
	}else{
		abline(v=30.5,lwd=4,col="white")}
	par(mar=c(0,0,0,0))
	legend("bottom",legend=c("AA","Aa","aa"),fill=rev(c("lightblue","olivedrab2","brown1")),bty="n",horiz=TRUE,cex=1.5,xpd=TRUE,ins=-0.08)
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#Add proportions scatter plot for Pop1
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#Prepare initial proportions
	faa<-(1-fP)**2*(PopSize/2)
	fAa<-(2*fP*(1-fP))*(PopSize/2)
	par(mar=c(4,4,0,0),mgp=c(2,1,0))
	plot(NULL,ylim=c(1,PopSize/2),xlim=c(1,NGen),xlab="Поколений", ylab="Количество генотипов",font.lab=2,font=2,bty="n")
	AA<-Restab1[,1]
	Aa<-Restab1[,2]
	aa<-Restab1[,3]
		#Create polygons for each genotype (by count)
	polygon(c(1, 1:NGen, NGen),c(0, aa, 0),col="lightblue")
	Aa<-Aa+aa
	polygon(c(1:NGen, NGen:1),c(Aa, rev(aa)),col="olivedrab2")
	AA<-AA+Aa
	polygon(c(1:NGen, NGen:1),c(AA, rev(Aa)),col="brown1")
		#Add lines of starting genotypes counts
	abline(h = faa, lty = 2, lwd = 2)
	abline(h = faa+fAa, lty = 2, lwd = 2)
	legend("top",legend=("Субпопуляция 1"),ins=0.05,bty="n",cex=1.3)
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#Add proportions scatter plot for Pop2
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	par(mar=c(4,0,0,4),mgp=c(2,1,0))
	plot(NULL,ylim=c(1,PopSize/2),xlim=c(1,NGen),xlab="Поколений", ylab="",yaxt="n",font.lab=2,font=2,bty="n")
	AA<-Restab2[,1]
	Aa<-Restab2[,2]
	aa<-Restab2[,3]
		#Create polygons for each genotype (by count)
	polygon(c(1, 1:NGen, NGen),c(0, aa, 0),col="lightblue")
	Aa<-Aa+aa
	polygon(c(1:NGen, NGen:1),c(Aa, rev(aa)),col="olivedrab2")
	AA<-AA+Aa
	polygon(c(1:NGen, NGen:1),c(AA, rev(Aa)),col="brown1")
		#Add lines of starting genotypes counts
	abline(h = faa, lty = 2, lwd = 2)
	abline(h = faa+fAa, lty = 2, lwd = 2)
	legend("top",legend=("Субпопуляция 2"),ins=0.05,bty="n",cex=1.3)
	axis(side=4,font=2)
	mtext(side=4,"Количество генотипов",font=2,cex=0.75,line=2)
	
	#Return final proportions
	return(rbind(Restab1[NGen,],Restab2[NGen,]))
}