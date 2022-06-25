getwd()
setwd('D:/A KULIAH KVN/7. Regnonpar/kelompok 2')

library('lintools')
GCV2=function(para)
{
  data=read.table("soal.txt", sep="", header=T) #file txt dalam . bukan ,
  data=as.matrix(data)
  p=length(data[,1])
  q=length(data[1,])
  m=ncol(data)-1
  F=matrix(0,nrow=p,ncol=p)
  diag(F)=1
  nk= length(seq(min(data[,2]),max(data[,2]),length.out=50))
  knot=matrix(ncol=m,nrow=nk)
  for (i in (1:m))
  {
    for (j in (1:nk))
    {
      a=seq(min(data[,(i+1)]),max(data[,(i+1)]),length.out=50)
      knot[j,i]=a[j]
    }
  }
  z=(nk*(nk-1)/2)
  knot2=cbind(rep(NA,(z+1)))
  for (i in (1:m))
  {
    knot1=rbind(rep(NA,2))
    for ( j in 1:(nk-1))
    {
      for (k in (j+1):nk)
      {
        xx=cbind(knot[j,i],knot[k,i])
        knot1=rbind(knot1,xx)
      }
    }
    knot2=cbind(knot2,knot1)
  }
  knot2=knot2[2:(z+1),2:(2*m+1)]
  aa=rep(1,p)
  data2=matrix(ncol=(2*m),nrow=p)
  data1=data[,2:q]
  a1=length(knot2[,1])
  GCV=rep(NA,a1)
  Rsq=rep(NA,a1)
  
  for (i in 1:a1)
  {
    for (j in 1:(2*m))
    {
      if (j%%2==1) b=floor(j/2)+1 else b=j/2
      for (k in 1:p)
      {
        if (data1[k,b]<knot2[i,j]) data2[k,j]=0 else data2[k,j]=data1[k,b]-knot2[i,j]
      }
    }
    mx=cbind(aa,data1,data2)
    mx=as.matrix(mx)
    library('pracma')
    C=pinv(t(mx)%*%mx)
    B=C%*%(t(mx)%*%data[,1])
    yhat=mx%*%B
    SSE=0
    SSR=0
    for (r in (1:p))
    {
      sum=(data[r,1]-yhat[r,])^2
      sum1=(yhat[r,]-mean(data[,1]))^2
      SSE=SSE+sum
      SSR=SSR+sum1
    }
    Rsq[i]=(SSR/(SSE+SSR))*100
    MSE=SSE/p
    A=mx%*%C%*%t(mx)
    A1=(F-A)
    A2=(sum(diag(A1))/p)^2
    GCV[i]=MSE/A2
  }
  GCV=as.matrix(GCV)
  Rsq=as.matrix(Rsq)
  cat("===========================================================","\n")
  cat("Nilai Knot dengan Spline linear 2 knot","\n")
  cat("===========================================================","\n")
  print (knot2)
  cat("===========================================================","\n")
  cat("Rsq dengan Spline linear 2 knot","\n")
  cat("===========================================================","\n")
  print (Rsq)
  cat("===========================================================","\n")
  cat("HASIL GCV dengan Spline linear 2 knot","\n")
  cat("===========================================================","\n")
  print (GCV)
  s1=min(GCV)
  cat("===========================================================","\n")
  cat("HASIL GCV terkecil dengan Spline linear 2 knot","\n")
  cat("===========================================================","\n")
  cat(" GCV =",s1,"\n")
  write.csv(GCV,file="output GCV knot 2 bener .csv")
  write.csv(Rsq,file="output Rsq knot 2 bener.csv")
  write.csv(knot2,file="output knot 2 bener .csv")
}
GCV2(0)
