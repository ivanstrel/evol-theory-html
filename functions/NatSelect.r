NatSelect<-function(NGen=50,PA=0.5,PopSize=10000,PopN=5,fAA=0.8,fAa=0.85,faa=0.9){
	#Function for butch simulation of allele frequences under an influence
	#of selection (gene drift cause deviation when PopSize is small)
	FreqInit<-PA
	#Generation of color palette
	Col<-rainbow(PopN)
	#Plot initial frequences
	plot(c(1:NGen),rep(FreqInit,NGen),type="l",ylim=c(0,1),lwd=2,lty=2,xlab="Время (поколения)",ylab="Частота аллели",main="Результаты симуляции естественного отбора")
	Res_tab<-NULL
	ColInd<-1
	for (i in c(1:PopN)){
		#loop over NGen populations to calculate forward frequences
		PA<-FreqInit
		Freq_res<-PA
		for (j in c(2:NGen)){
			Pa<-1-PA
			Pop<-sample(c(1,2,3), PopSize, prob=c((PA**2)*fAA,(2*PA*Pa)*fAa,(Pa**2)*faa),rep=TRUE)
			PA<-(sum(Pop==2)+(sum(Pop==1))*2)/(PopSize*2)
			Freq_res[length(Freq_res)+1]<-PA
		}
		#Add lines of each population frequences to the plot
		lines(c(1:NGen),Freq_res,t="l",lwd=2,col=Col[ColInd])
		ColInd<-ColInd+1
		Res_tab<-rbind(Res_tab,Freq_res)
	}
}
			