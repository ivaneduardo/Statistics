beta_alfa_analysist1<-function(alfa,mean_null,d_null,mean2,d2=sd_null,sample_s,type){
  t1<-Sys.time()
  sd_null=d_null/sqrt(sample_s)
  sd2=d2/sqrt(sample_s)
  alfa=ifelse(type=="D",alfa/2,alfa)
  maxsd=max(sd_null,sd2)
  xmin=min(mean_null,mean2)-3.5*maxsd
  xmax=max(mean_null,mean2)+3.5*maxsd
  topy=dnorm(mean_null,mean_null,sd_null)
  ymax=1.5*topy
  x1<-round(seq(from=xmin,to=xmax,by=0.01),2)
  y1.2 <- dnorm(x1,mean=mean_null,sd=sd_null)  
  y1.1 <- dnorm(x1,mean=mean2,sd=sd2)
  
  t2<-Sys.time()
  typeII=pnorm(qnorm(ifelse(type=="L",alfa,1-alfa),mean=mean_null,sd=sd_null),mean=mean2,sd=sd2,lower.tail = ifelse(type=="G",TRUE,FALSE))
  RReg = round(ifelse(type=="L",qnorm(alfa,mean=mean_null,sd=sd_null),
                      qnorm(1-alfa,mean=mean_null,sd=sd_null)),2)
  df_teoricas <- data.frame(x=x1,y1=y1.1,y2=y1.2)
  azb<-c(x1[x1<RReg],rep(0,times=length(x1[x1>=RReg])))
  zab<-c(rep(0,times=length(x1[x1<RReg])),x1[x1>=RReg])
  print(length(x1))
  print(length(azb))
  print(length(zab))
  yalfa<-if(type=="L"){
    dnorm(azb,mean_null,sd_null)}
  else{
    dnorm(zab,mean_null,sd_null)
  }
  ybeta<-if(type=="L"){
    dnorm(zab,mean2,sd2)}
  else{
    dnorm(azb,mean2,sd2)
  }
  t3<-Sys.time()
  gf3 <- ggplot(df_teoricas,aes(x=x1,y=y1.2)) + geom_line(size=1) + 
    geom_line(aes(x=x1,y=y1.1),size=1) + xlim(xmin,xmax) +
    geom_area(aes(x=x1,y=yalfa,fill="green"),alpha=0.3) +
    geom_area(aes(x=x1,y=ybeta,fill="red"),alpha=0.3) +
    annotate(geom="text",x=(mean_null+mean2)/2,y=1.25*topy,label=paste("Beta:",round(typeII,2)))+
    ylim(-0.2*ymax,1.5*ymax) + geom_segment(x=mean_null,y=0,xend=mean_null,yend=topy,linetype="dashed") + 
    geom_segment(x=mean2,y=0,xend=mean2,yend=dnorm(mean2,mean2,sd2),linetype="dashed") +
    annotate(geom="text",x=mean_null,y=-.075*ymax,label=paste("H0: Mu = ",mean_null),size=2.5) +
    annotate(geom="text",x=mean2,y=-.15*ymax,label=paste("Xbar = ",mean2),size=2.5) +
    labs(title="Graphing and calculation of P(Error type II = P(Fail to Reject Null/Null is False", x="x",y="Value for PDF normal") +
    geom_segment(x=RReg,y=0,xend=RReg,yend=max(dnorm(RReg,mean_null,sd_null),dnorm(RReg,mean2,sd2)),size=1,color="black") +
    scale_fill_discrete(labels=c("alpha","Beta"))
  t4<-Sys.time()
  
  t21=(t2-t1)
  t32=(t3-t2)
  t43=(t4-t3)
  l<-list(gf3,t21,t32,t43)
  return(l)
}


