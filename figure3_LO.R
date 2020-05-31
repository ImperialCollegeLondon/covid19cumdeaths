merged<-read.csv("Data/summary_sero_vs_deaths.csv",stringsAsFactors = F)
write2file<-T
if(write2file) {
  jpeg(file="sero_vs_deaths.jpg", width=2200,height=1600,res=300)
  par(mar=c(5,4,4,8))
  plot(merged$seroprevalence*100,scale*merged$n_deaths/merged$pop,xlab="seroprevalence (%)",
       ylab="cumulative deaths per million",xlim=c(0,13))
  inds<-which(merged$country=="spain")
  points(merged$seroprevalence[inds]*100,scale*merged$n_deaths[inds]/merged$pop[inds], pch=21, col.main="black", bg="red")
  inds<-which(merged$country=="denmark")
  points(merged$seroprevalence[inds]*100,scale*merged$n_deaths[inds]/merged$pop[inds], pch=21, col.main="black", bg="blue")
  inds<-which(merged$country=="switzerland")
  points(merged$seroprevalence[inds]*100,scale*merged$n_deaths[inds]/merged$pop[inds], pch=21, col.main="black", bg="purple")
  inds<-which(merged$country=="Sweden")
  points(merged$seroprevalence[inds]*100,scale*merged$n_deaths[inds]/merged$pop[inds], pch=21, col.main="black", bg="cyan")
  greycol<-"gray70"
  lines(c(0,15),c(0,3000),lty=2,col=greycol)
  lines(c(0,15),c(0,1500),lty=2,col=greycol)
  lines(c(0,15),c(0,750),lty=2,col=greycol)
  lines(c(0,15),c(0,150),lty=2,col=greycol)
  text(6,1300,"2%",col=greycol)
  text(9,1000,"1%",col=greycol)
  text(11,620,"0.5%",col=greycol)
  text(12,170,"0.1%",col=greycol)
  par(xpd=TRUE)
  legend(14.5,1000,c("Spain","Denmark","Switzerland","Sweden"),pch=rep(21,4),col=rep("black",4), bty='n',
         pt.bg=c("red","blue","purple","cyan"))
  dev.off()
  
}
