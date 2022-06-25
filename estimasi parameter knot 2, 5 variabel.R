getwd()
setwd('D:/A KULIAH KVN/7. Regnonpar/kelompok 2/non par tugas 3')
library(pracma)
uji=function(alpha,para)
{
  data=read.csv("data no 4.csv", sep=",", header=T)
  as.data.frame(data)
  knot=read.csv("output knot 2.csv", sep=",", header=T)
  data=as.matrix(data)
  knot=as.matrix(knot)
  ybar=mean(data[,1])
  m=para+2
  p=nrow(data)
  q=ncol(data)
  dataA=cbind(data[,m],data[,m],data[,m+1],data[,m+1],data[,m+2],data[,m+2],data[,m+3],data[,m+3],data[,m+4],data[,m+4])
  dataA=as.matrix(dataA)
  satu=rep(1,p)
  n1=ncol(knot)
  data.knot=matrix(ncol=n1,nrow=p)
  for (i in 1:n1)
  {
    for(j in 1:p)
    {
      if (dataA[j,i]<knot[1,i]) data.knot[j,i]=0 else
        data.knot[j,i]=dataA[j,i]-knot[1,i]
    }
  }
  mx=cbind(satu,data[,2],data.knot[,1:2],data[,3],data.knot[,3:4],data[,4],data.knot[,5:6],data[,5],data.knot[,7:8],data[,6],data.knot[,9:10])
  mx=as.matrix(mx)
  library(pracma)
  B=(pinv(t(mx)%*%mx))%*%t(mx)%*%data[,1]
  cat("=======================================","\n")
  cat("Estimasi Parameter","\n")
  cat("=======================================","\n")
  print (B)
  n1=nrow(B)
  yhat=mx%*%B
  res=data[,1]-yhat
  SSE=sum((data[,1]-yhat)^2)
  SSR=sum((yhat-ybar)^2)
  SST=SSR+SSE
  MSE=SSE/(p-n1)
  MSR=SSR/(n1-1)
  Rsq=(SSR/(SSR+SSE))*100
  #uji F (uji serentak)
  Fhit=MSR/MSE
  pvalue=pf(Fhit,(n1-1),(p-n1),lower.tail=FALSE)
  if (pvalue<=alpha)
  {
    cat("------------------------------------","\n")
    cat("Kesimpulan hasil uji serentak","\n")
    cat("------------------------------------","\n")
    cat("Tolak Ho yakni minimal terdapat 1 prediktor yang
signifikan","\n")
    cat("","\n")
  }
  else
  {
    cat("------------------------------------","\n")
    cat("Kesimpulan hasil uji serentak","\n")
    cat("------------------------------------","\n")
    cat("Gagal Tolak Ho yakni semua prediktor tidak berpengaruh
signifikan","\n")
    cat("","\n")
  }
  #uji t (uji individu)
  thit=rep(NA,n1)
  pval=rep(NA,n1)
  SE=sqrt(diag(MSE*(pinv(t(mx)%*%mx))))
  cat("------------------------------------","\n")
  cat("Kesimpulan hasil uji individu","\n")
  cat("------------------------------------","\n")
  thit=rep(NA,n1)
  pval=rep(NA,n1)
  for (i in 1:n1)
  {
    thit[i]=B[i,1]/SE[i]
    pval[i]=2*(pt(abs(thit[i]),(p-n1),lower.tail=FALSE))
    if (pval[i]<=alpha) cat("Tolak Ho yakni prediktor signifikan
dengan pvalue",pval[i],"\n") else cat("Gagal tolak Ho yakni
prediktor tidak signifikan dengan pvalue",pval[i],"\n")
  }
  thit=as.matrix(thit)
  cat("=======================================","\n")
  cat("nilai t hitung","\n")
  cat("=======================================","\n")
  print (thit)
  cat("Analysis of Variance","\n")
  cat("======================================","\n")
  cat("Sumber df SS MS Fhit","\n")
  cat("Regresi ",(n1-1)," ",SSR," ",MSR,"",Fhit,"\n")
  cat("Error ",p-n1," ",SSE,"",MSE,"\n")
  cat("Total ",p-1," ",SST,"\n")
  cat("======================================","\n")
  cat("s=",sqrt(MSE)," Rsq=",Rsq,"\n")
  cat("pvalue(F)=",pvalue,"\n")
  write.csv(res,file="output residual.csv")
  write.csv(pval,file="output pvalue.csv")
  write.csv(mx,file="output mx.csv")
  write.csv(yhat,file="output yhat.csv")
}
uji(0.05,0)
