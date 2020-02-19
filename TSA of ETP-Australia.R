rm(list=ls())
require(rio)
require(pracma)
require(tseries) 
df<-import("C://Hourly Flowrate.csv");
df1<-df$`Hourly flowrate`
df2<-head(df1,100)
plot(df1[1:10000], xlim=c(0,24*100), type='l')
points(seq(1:100)*24-7, df1[seq(1:100)*24-7], col='red', )
df1[is.na(df1)]=0
df1[10000:10057]
df1
# Exploratory Data Analysis -----------------------------------------------
adf.test(df1)
#Autocorrelation Function and Partial Autocorrelation Function
autocorr<-acf(df1,lag.max = 100);
pautocorr<-pacf(df1, lag.max = 100)
autoqua<-quantile(autocorr$acf[24:100], prob=.7)
pos1<-which(autocorr$acf>autoqua)
autocorr$acf[24+pos1]
pautoqua<-quantile(pautocorr$acf[24:100], prob=.7)
pos1<-which(pautocorr$acf>pautoqua)
pautocorr$acf[24+pos1]
training<-10000
testing<-20000
# Alt1 AutoRegression -----------------------------------------------------
lagsar<-c(25,26,27,28,29,30,31,32)
laggedar<-matrix(ncol=length(lagsar), nrow =training)
for (i in 1:length(lagsar)){
  laggedar[1:training,i]<-df1[lagsar[i]:(training + lagsar[i]-1)]
}
dim(laggedar)
y1<-df1[57:(training+56)]
x<-cbind(rep(1,training), laggedar)
th<-inv(t(x)%*%x)%*%(t(x)%*%y1)
h1<-x%*%th
plot(y1, type='l', xlim=c(1000,4000))
points(h1, type='l', col='red')
MSE=mean((h1-y1)^2)
MSE
# Moving Average ----------------------------------------------------------
lagsma<-c(25,30,31,32,33)
laggedma<-matrix(ncol=length(lagsma), nrow =training)
for (i in 1:length(lagsma)){
  laggedma[1:training,i]<-df1[lagsma[i]:(training+lagsma[i]-1)]-df1[1:(training)]
}
laggedma
y1<-as.matrix(df1[(57):(training+57-1)])
x<-cbind(rep(1,training),laggedma)
th<-inv(t(x)%*%x)%*%(t(x)%*%y1)
h1<-x%*%th
plot(y1, type='l', xlim=c(1000,4000))
points(h1, type='l', col='red')
MSE=mean((h1-y1)^2)
MSE
# ARMA --------------------------------------------------------------------
x<-cbind(rep(1,training), laggedar[,c(1,2,3)], laggedma[,c(1,2,3)])
th<-rep(1, dim(x)[2])         #Define theta parameters equal to number of expected features (here features are x)
h1=x%*%th            #Cross multiply matrix by theta parameters to determine hypothesis function
v=length(x) #define variable to contain number of readings taken
costfn=(1/2*v)*sum((h1-y1)^2) #calculate cost function
costfna<-vector() #define variable to contain costfunction calculation per iteration
lr=0.001  #define learning rate
ab<-vector()  #define variable to contain boolean to change or maintain learning rate
for (i in 1:2000){
  # Update Rule -------------------------------------------------------------
  dcfs<-function(uv){
    (1/v)*(sum((h1-y1)*uv))
  }
  dcf<-apply(x, MARGIN=2, FUN=dcfs)
  th=th-lr*dcf
  h1=x%*%th
  costfna[i]=(1/2*v)*sum((h1-y1)^2)
  costfna
  ab[i]<-(costfna[i-1]-costfna[i]>0)
  ab<<-ab
  ifelse(ab[i]=='FALSE', lr<-(lr/10), lr<-lr)
  MSE=sum((y1-h1)^2)
  MSE<<-MSE
}
plot(costfna)
plot(y1, type='l', xlim=c(5000,1000))
points(h1, type='l', col='red')
