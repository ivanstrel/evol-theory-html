HW_inbrieeding<-function(NGen = 50, fP = 0.5, PopSize = 100, self_poll_ratio = 1/2){
	#function for visualization of inbreeding
	#NGen -- number of generations
	#fP -- initial frequency of A allele
	#PopSize -- population size
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#	Internal functions
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	Ret_gamete<-function(Genotype=1){
	#Function to produce random gamete from genotype
	#Genotype - one of 1, 2, 3
	#where
	#1=AA, 2=Aa, 3=aa
	if (Genotype == 1){
		return(1)}
	if (Genotype == 2){
		return(sample(c(1,2),1))}
	if (Genotype == 3){
		return(2)}
	}
	
	Prod_genotype<-function(Pop, self_poll_ratio){
	#Function to produce breed genotype
	#from two or one parents genotypes
		Self<-sample(c(TRUE,FALSE),1,rep=TRUE,prob=c(self_poll_ratio,1-self_poll_ratio))
		Gen1<-sample(Pop,1)
		if (Self){
			Gen2<-Gen1
		}else{
			Gen2<-sample(Pop,1)
		}
		Gamete1<-Ret_gamete(Gen1)
		Gamete2<-Ret_gamete(Gen2)
		if ((Gamete1+Gamete2) == 2){
			return(1)}				#AA
		if ((Gamete1+Gamete2) == 3){
			return(2)}				#Aa
		if ((Gamete1+Gamete2) == 4){
			return(3)}				#aa
	}

	New_population<-function(Pop){
	#Function to produce breed population
	#from crossing random parents from
	#initial population
		Len<-length(Pop)
		New_pop<-sapply(Pop,function(x){Prod_genotype(Pop,self_poll_ratio)})
		return(New_pop)
	}
	
	T_fun<-function(x){
		if(all(x==c(1,0,0))){
			return(1)
		}
		if(all(x==c(0,1,0))){
			return(2)
		}
		if(all(x==c(0,0,1))){
			return(3)
		}
	}
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	#	Main part
	#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	fQ<-1-fP
	Pop<-rmultinom(PopSize,1,prob=c((fP**2),(2*fQ*fP),(fQ**2)))
	Pop<-apply(Pop,2,T_fun)
	Res_tab<-as.numeric(table(Pop))
	Len<-length(Pop)
	for (i in c(2:NGen)){
		Pop<-New_population(Pop)
		Pop<-c(Pop,c(1,2,3))
		Res_tab<-rbind(Res_tab,as.numeric(table(Pop))-c(1,1,1))
		Pop<-Pop[1:Len]
	}
	
	#Plot results
	par(mar=c(7,4,1,1))
		#Add empty plot
	plot(NULL,ylim=c(1,PopSize),xlim=c(1,NGen),xlab="Поколений", ylab="Количество генотипов",font.lab=2,font=2)
	AA<-Res_tab[,1]
	Aa<-Res_tab[,2]
	aa<-Res_tab[,3]
		#Create polygons for each genotype (by count)
	polygon(c(1, 1:NGen, NGen),c(0, aa, 0),col="lightblue")
	Aa<-Aa+aa
	polygon(c(1:NGen, NGen:1),c(Aa, rev(aa)),col="olivedrab2")
	AA<-AA+Aa
	polygon(c(1:NGen, NGen:1),c(AA, rev(Aa)),col="brown1")
		#Add lines of starting genotypes counts
	abline(h = aa[1], lty = 2, lwd = 2)
	abline(h = Aa[1], lty = 2, lwd = 2)
	
	#Add legend
	par(mar=c(0, 4, 1, 1), new=TRUE)
	plot(NULL,xlim=c(1,1), ylim=c(1, 1) ,xaxt = "n",yaxt = "n",bty = "n",xlab = "",ylab = "")
	legend("bottom",legend=c("AA","Aa","aa"),fill=rev(c("lightblue","olivedrab2","brown1")),bty="n",horiz=TRUE,cex=1.5,xpd=TRUE)
	return(Res_tab)
}
	