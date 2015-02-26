
group=function(niv,mois,names){
  T=mois
  col=which(colnames(T)==names)  
  T=T[order(T[,col],decreasing=TRUE),]
  som=0
  limit=niv*sum(T[,col])
  for (i in 1:length(T[,col])) {
    som=som+T[i,col];
    first=i;
    if(som >= limit) 
      break;
    
  }
  first
}


trans=function(mois,names,names1) {
  col=which(colnames(mois)==names)
  J=mois[which(mois[,col]==names1),]
  J
}


B1<-read.csv2("1401.csv",sep="\t")
B2<-read.csv2("1402.csv",sep="\t")
B3<-read.csv2("1403.csv",sep="\t")
B4<-read.csv2("1404.csv",sep="\t")
B5<-read.csv2("1405.csv",sep="\t")
B6<-read.csv2("1406.csv",sep="\t")





Jour=function(B){

test<-as.character(B[,3])
test1=matrix(0,length(test),3)
for (i in 1:length(test)) (
  for (j in 1:3)  (
    test1[i,j]=unlist(strsplit(test[i],"/"))[j]                       
  )        
)
date1=paste(test1[,2],test1[,3],sep="-")
B[,3]=date1


B=B[which(B[,12]!=0|B[,14]!=0|B[,10]!=0|B[,16]!=0|B[,17]!=0|B[,11]!=0),c(2,3,4,5,6,9,10,11,12,13,14,15,16,17)]

colnames(B)[9]="cost"
colnames(B)[14]="Rev"

Jour1=aggregate(B[,c(7,8,9,10,11,13,14)],by=list(Keyword=B[,4],Date=B[,2],Campaign=B[,3],Appareil=B[,1],Group=B[,5],Devise=B[,12],Type.de.correspondance=B[,6]),FUN=sum,na.rm=TRUE)
Jour1[,11]=aggregate(B[,10],by=list(Keyword=B[,4],Date=B[,2],Campaign=B[,3],Appareil=B[,1],Group=B[,5],Devise=B[,11],Type.de.correspondance=B[,6]),FUN=mean,na.rm=TRUE)[,8]
Jour1[,12]=aggregate(B[,11],by=list(Keyword=B[,4],Date=B[,2],Campaign=B[,3],Appareil=B[,1],Group=B[,5],Devise=B[,11],Type.de.correspondance=B[,6]),FUN=mean,na.rm=TRUE)[,8]

Jour1
}







CPC=function(mois){
  t=mois
  cout=which(colnames(t)=="cost")
  clics=which(colnames(t)=="Clics")
  t=t[which(t[,clics]!=0),]
  cpc=t[,cout]/t[,clics]
  
  t=cbind(t,cpc)
  
  
  t
  
  
}







maxBid=function(tt,posi){
  
  a=which(colnames(tt)=="cpc")
  b=which(colnames(tt)=="Position.moy.")
 
  
  
  tt=aggregate(tt[,a],by=list(Position.moy.=tt[,b]),FUN=mean,na.rm=TRUE)
  
  
  cons=lm(tt[,2] ~ tt[,1], data =tt)$coe[1]
  para=lm(tt[,2] ~ tt[,1], data =tt)$coe[2]
  
  
  max=cons+para*(posi-1)
  
  
  max
  
}




minBid=function(tt,posi){
  
  a=which(colnames(tt)=="cpc")
  b=which(colnames(tt)=="Position.moy.")
  
  
  
  tt=aggregate(tt[,a],by=list(Position.moy.=tt[,b]),FUN=mean,na.rm=TRUE)
  
  
  cons=lm(tt[,2] ~ tt[,1], data =tt)$coe[1]
  para=lm(tt[,2] ~ tt[,1], data =tt)$coe[2]
  

  min=cons+para*posi
  
  
  min
  
}
