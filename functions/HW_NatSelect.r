HW_NatSelect<-function(NGen=50,fP=0.5,PopSize=10000,fAA=0.85,fAa=0.9,faa=0.8){
	#Function for visualization of genoripes proportions under an
	#influence of selection and gene drift
	#fP -- frequence of A allele
	#fQ -- frequence of a allele
	FreqInit<-fP
	#calculate fQ
	fQ<-1-fP
	#set initial 
	Res_tab<-rmultinom(1,PopSize,prob=c((fP**2),(2*fQ*fP),(fQ**2)))
	Res_tab<-as.numeric(Res_tab)
	#Generate genotypes sequences
	for (i in c(2:NGen)){
		fQ<-1-fP
		#Generation of new frequences with multinomial distribution of size 3
		Pop<-rmultinom(1,PopSize,prob=c((fP**2)*fAA,(2*fQ*fP)*fAa,(fQ**2)*faa))
		Pop<-as.numeric(Pop)
		fP<-(Pop[1]*2+Pop[2])/(PopSize*2)
		Res_tab<-rbind(Res_tab,Pop)
	}
	#Plot results
	par(mar=c(7,4,1,1))
		#Add empty plot
	plot(NULL,ylim=c(1,PopSize),xlim=c(1,NGen),xlab="Поколений", ylab="Количество генотипов",font.lab=2,font=2)
	AA<-Res_tab[,1]
	Aa<-Res_tab[,2]
	aa<-Res_tab[,3]
		#Create polygons for each genotype (by count)
	polygon(c(1,1:NGen,NGen),c(0,aa,0),col="lightblue")
	Aa<-Aa+aa
	polygon(c(1:NGen,NGen:1),c(Aa,rev(aa)),col="olivedrab2")
	AA<-AA+Aa
	polygon(c(1:NGen,NGen:1),c(AA,rev(Aa)),col="brown1")
		#Add lines of starting genotypes counts
	abline(h=aa[1],lty=2,lwd=2)
	abline(h=Aa[1],lty=2,lwd=2)
	
	#Add legend
	par(mar=c(0,4,1,1),new=TRUE)
	plot(NULL,xlim=c(1,1),ylim=c(1,1),xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
	legend("bottom",legend=c("AA","Aa","aa"),fill=rev(c("lightblue","olivedrab2","brown1")),bty="n",horiz=TRUE,cex=1.5,xpd=TRUE)
	
	#Add computation of Hardy-Weinberg
	Tab<-Res_tab[c(1,NGen),]
	#chisq.test(Tab)
}
			