SimDrift = function(NGen=100,Freq=0.5,PopSize=250,PopN=10, mode="plot", out=FALSE){
  if ((mode!="plot")&(mode!="hist")){
    print("Выбрано неправильное значение 'mode', должно быть одно из plot или hist")
  }
  FreqInit<-Freq
  Col<-rainbow(PopN)
  if (mode == "plot"){
    plot(c(1:NGen),rep(FreqInit,NGen),type="l",ylim=c(0,1),lwd=2,lty=2,xlab="Время (поколений)",
		 ylab="Частота аллели",main="Результаты симуляции\nгенетического дрейфа",cex.axis=1.1,
		 cex.lab=1.1,font.lab=2,font.axis=2)
  }
  
  Res_tab<-data.frame(Pop=c(1:PopN),Final=rep(NA,PopN))
  ColInd<-1
  for (j in c(1:PopN)){
    Freq<-FreqInit
    Res<-Freq
    for( i in c(2:NGen)){
      A1=rbinom(1,2*PopSize,Freq)
      Freq=A1/(PopSize*2)
      Res[length(Res)+1]<-Freq
    }
    if (mode=="plot"){
      lines(c(1:NGen),Res,t="l",lwd=2,col=Col[ColInd])
    }
    ColInd<-ColInd+1
    if (Freq==0){
      Res_tab[j,2]<-"Утеряна"
    }else if (Freq==1){
      Res_tab[j,2]<-"Фиксирована"
    }else{
      Res_tab[j,2]<-"Равновесие"
    }
  }
  if (mode=="plot"){
    legend("topleft",legend=("Исходная частота"),bty="n",lwd=2,lty=2,col=1 )
  }else{
    Res<-c(Равновесие=sum(Res_tab[,2]=="Равновесие"),Фиксирована=sum(Res_tab[,2]=="Фиксирована"),
		   Утерена=sum(Res_tab[,2]=="Утеряна"))
	names.arg<-c(paste("Равновесие",as.character(Res[1]),sep="\n"),paste("Фиксирована",as.character(Res[2]),sep="\n"),
				paste("Утеряна",as.character(Res[3]),sep="\n"))
    barplot(Res,col=c("lightgrey","darkgreen","tomato"), main="Результаты симуляции генетического дрейфа",
		    ylab="популяций (шт.)",cex.axis=1.1,cex.lab=1.1,font.lab=2,font.axis=2,names.arg=names.arg)
  }
}
