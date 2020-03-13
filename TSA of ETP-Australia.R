rm(list=ls());
require (rio);
require (pracma);
require (tseries);
require (sqldf);
require (odbc);
require (DBI);
DF<-import("C:\\Users\\kobin\\Documents\\Wastewater_Inlet__ETP_Hourly_Flow (1).csv")
DFsim=DF[1:dim(DF)[1],1:3]

timeconvert<-function(x){
  #paste(substring(x,1,10),' ',substring(x,12,19), sep='', ' CET')
  as.POSIXct(x, format="%Y-%m-%dT%H:%M:%S.000Z")
}

timeconvertstring<-function(x){
  str(x)
}

convertedtime=apply(DFsim[,1:2], FUN=timeconvert, MARGIN=2)
h_time_delta=convertedtime[,2]-convertedtime[,1]

timedeltafind<-function(x){
  v_time_delta=vector()
  for (i in 1:91992){
    v_time_delta[i]<-convertedtime[i+1,1]-convertedtime[1,1]
  }
  v_time_delta<-v_time_delta/3600
  v_time_delta<<-c(0,v_time_delta)
}
timedeltafind()
dfin=cbind(v_time_delta,convertedtime,DFsim[,3])
dfts=dfin[,c(1,4)]
plot(tail(dfts,24*10), type='l')
ts_column=2;precision=0

na.find<-function(f){
  nacolsum<-vector()
  for (i in 1:ncol(f)){
    nacolsum[i]<-sum(is.na(f[,i]))
  }
  na_col<-which(nacolsum>0)
  na_sum<-nacolsum[nacolsum>0]
  na_col<<-na_col
  na_sum<<-na_sum
  na_summary<-cbind(na_col, na_sum)
  na_summary_list<-list(na_col, na_sum)
  print(na_summary)
}
na.find(dfts)

pmat.create<-function(x,ts_column){
  bins<-unique(round(na.remove(x[,ts_column]),precision))
  u_val<-sort(bins)
  u_length<-length(bins)
  u_mat<-matrix(data=0, nrow=u_length+1, ncol=u_length+1)
  u_mat[,1]<-c(100,u_val); u_mat[1,]<-c(100,u_val)
  u_length<<-u_length
  u_val<<-u_val
  u_mat<<-u_mat
}
pmat.create(x=dfts, ts_column = ts_column)
natwos=which(is.na(dfts[,2]))

dfts[natwos,1]=dfts[natwos-1,1]+1

#Set u_mat[1,1] to 100 to prevent that value from being selected as the whole of row 1 and column 1 will only be used for comparison while the rest of distribution matrix will be filled with number of movements from one condition to the next. Row 1 will correspond to initial condition and Row 2 will correspond to final condition.

pmat.fill<-function(f, ts_column, precision){
  for(i in 2:(length(f[,ts_column])-1)){
    #Create row and column comparators
    row_comp<<-u_mat[1,]
    col_comp<<-u_mat[,1]
    #Determine current (MKA) and proceeding(MKB) values for all non-NA values in time series
    MKA<-round(f[i,ts_column],precision)
    MKB<-round(f[i-1,ts_column],precision)
    #Create tally of all movements from condition MKA to MKB
    u_mat[which(row_comp==MKA),which(col_comp==MKB)]<-u_mat[which(row_comp==MKA),which(col_comp==MKB)]+1
  }
  sum(u_mat[2:(nrow(u_mat)-1),2:(ncol(u_mat)-1)])
  pos_na<-which(is.na(f[,ts_column]))
  useable_row_checker<-apply(u_mat[,2:ncol(u_mat)], MARGIN=1, FUN=sum)
  useable_row_pos<-which(useable_row_checker>0)+1
  u_matindex1<-2:nrow(u_mat)
  u_matindex2<-u_matindex1
  useable_fraction<-sum(!u_mat[u_matindex1,u_matindex2]==0)/(length((u_matindex1))^2)
  #L2G
  u_mat<<-u_mat
  pos_na<<-pos_na
  useable_row_pos<<-useable_row_pos
  paste(round(useable_fraction*100,2),'% of the prob matrix is non_zero and will be used for bootstrapping')
}

#Bootstrapping matrix is not used in this implementation of TSA. 
pmat.fill(f=dfts, ts_column=ts_column, precision=precision)
preceding_array<-vector();
proceeding_array<-vector()

moving_average_bootstrap<-function(){
  for (i in 1:length(pos_na)){
    preceding_array[i]<-pos_na[i]-(24*10)
  }
  for (i in 1:length(pos_na)){
    proceeding_array[i]<-pos_na[i]+(24*10)
  }
  for (i in 1:length(dfts[pos_na,2])){
    dfts[pos_na[i],2]=mean(dfts[preceding_array[i]:proceeding_array[i],2][!is.na(dfts[preceding_array[i]:proceeding_array[i],2])])
  }
  for (i in 1:length(dfts[,1])){
    dfts[i,1]<-i-1
  }
  na.find(dfts)
  dfts<<-dfts
}
moving_average_bootstrap()
#Create dataframe containing only useful columns
#Plot complete Time Series for Easy Visualization and check for Addditive or Multiplicative Characteristics
# Exploratory Data Analysis -----------------------------------------------
#1.Determine whether autocorrelation or Partial Autocorrelation can be found in Time Series
#2.Determine autocorrelations and partial autocorrelations above 70th percentile.
#3.Determine poisitions of autocorrelated and partially autocorrelated values above 70th percentile to find their lagtimes
autocorr<-acf(dfts[,2],lag.max = 100);

pautocorr<-pacf(dfts[,2], lag.max = 100)
predgap=24;prob=0.5
corrsummary<-function(predgap,prob){
  autoqua<<-quantile(abs(autocorr$acf[predgap:100]), prob=prob)
  pautoqua<<-quantile(abs(pautocorr$acf[predgap:100]), prob=prob)
  pos1<<-which(autocorr$acf>autoqua)
  pos2<<-which(pautocorr$acf>pautoqua)
  acfmat<<-cbind(pos1,autocorr$acf[autocorr$acf>autoqua])
  acfmat <<- acfmat[order(acfmat[,2], decreasing=TRUE),]
  acfmatpos <<- acfmat[acfmat>predgap]
  ppacfmatpos<<- acfmatpos
  pacfmat<<-cbind(pos2,pautocorr$acf[pautocorr$acf>pautoqua])
  pacfmat <<- pacfmat[order(pacfmat[,2], decreasing=TRUE),]
  pacfmatpos <<- pacfmat[pacfmat>predgap]
}
corrsummary(predgap=1,prob=0.90)
training_number=10000
testing_number=9000
training<-training_number
testing<-training_number+testing_number
# Alt1 AutoRegression -----------------------------------------------------
#1.Use autoregressive method to determine theta theta parameters
#2.Iterate through the lagtimes cumulatively descending from the most autocorrelated lagtime
#3.Find value of i which corresponds to lowest MSE
MSE<-vector()
fit_ar.find<-function(training,testing){
  for(i in 1:length(acfmatpos)){
    lagsar<-head(acfmatpos,i)
    laggedar<-matrix(ncol=length(lagsar), nrow =training)
    for (i in 1:length(lagsar)){
      laggedar[1:training,i]<-dfts[,2][lagsar[i]:(training + lagsar[i]-1)]
    }
    #Ensure that the target value ranges position shifts as maximum lagtime shifts to maintain a time difference of predgap hours between prediction and event
    y1<-dfts[,2][(predgap+max(lagsar[1:i])):(training+(predgap+max(lagsar[1:i]))-1)]
    x1<<-cbind(rep(1,training), laggedar)
    #Perform linear regression analytically
    th<<-inv(t(x1)%*%x1)%*%(t(x1)%*%y1)
    h1<<-x1%*%th
    #Measure MSE per iteration
    MSE[i]<-mean(abs(y1-h1))
  }
  min_MSE<<-which(MSE==min(MSE))
  plot(MSE, type='l', xlab='iteration number', ylab='MSE')
  paste('MSEmin occurs at cumulative iteration number',min_MSE)
  lagsar<<-lagsar
  laggedar<<-laggedar
}
fit_ar.find(training=training, testing=testing)
fit_ar.train<-function(training,testing){
  laggedar<-matrix(ncol=min_MSE, nrow =training)
  for (i in 1:min_MSE){
    laggedar[1:training,i]<-dfts[,2][lagsar[i]:(training + lagsar[i]-1)]
  }
  y1<<-dfts[,2][(predgap+max(lagsar[min_MSE])):(training+(predgap+max(lagsar[min_MSE]))-1)]
  x1<<-cbind(rep(1,training), laggedar)
  th<<-inv(t(x1)%*%x1)%*%(t(x1)%*%y1)
  h1<-x1%*%th
  h1<<-h1
}
fit_ar.train(training=training,testing=testing)
ar.test<-function(){
  laggedar<-matrix(nrow=(testing-training), ncol=min_MSE)
  for (i in 1:min_MSE){
    laggedar[1:(testing-training),i]<-dfts[,2][(training+lagsar[i]):(testing + lagsar[i]-1)]
  }
  x2<<-cbind(rep(1,(testing-training)), laggedar)
  y2<<-dfts[,2][(predgap+max(lagsar[1:i])+training):(testing+(predgap+max(lagsar[1:i]))-1)]
  h2<<-x2%*%th
}
ar.test()
ar.MAPE<-function(){
  MSE<-mean(abs(h2-y2))
  #Find Mean Average Percentage Error
  MAPE<<-(abs(h2-y2)/y2)
  MAPE_inf<-is.infinite(MAPE)
  MAPE_non_zero<-mean(MAPE[!MAPE_inf])
  which(MAPE==max(MAPE[!MAPE_inf]))
  paste('non-zero MAPE is', signif(MAPE_non_zero*100,2), '%')
}
ar.MAPE()

#Variance vs MAPEexp plot describes the relationship between variance of the mean y and y value at time & expected MAPE
MPEplots<-function(y,h){
  varr<-vector()
  threshold<-vector()
  for(i in 1:100){
    threshold[i]<-(sum(MAPE>0.01*(100-i)))*100/length(y)
  }
  #Threshold Represents Probability of Obtaining MAPE less than or equal to a certain value.
  plot(threshold, seq(1,100), type='l', ylab='Probability', xlab='MAPEexp (%)')
  MAPEexp<-vector()
  stdev_array<-seq(0.001,4,0.01)
  length(stdev_array)
  stdev<-vector()
  for(i in 1:length(stdev_array)){
    m<-h[(abs(y-mean(y))<stdev_array[i])]
    n<-y[(abs(y-mean(y))<stdev_array[i])]
    o<-y[(abs(y-mean(y))<stdev_array[i])]
    stdev[i]<-mean(abs(y-mean(y))/(length(o)-1))
    MAPE1<-(100*abs(m-n)/(o))
    MAPE1<-MAPE1[!MAPE1==Inf]
    MAPEexp[i]<-mean(MAPE1)
  }
  #plot(stdev,MAPEexp, type='l', xlim=c(0,0.01), xlab='Standard Deviation', ylab='MAPEexp(%)')
  hist(h-y, breaks=30, xlab='Residual', ylab='Residual')
  meany<-mean(h-y)
  stdev<-sd(h-y)
  print(paste('mean',round(meany,3),'stdev',round(stdev,3)))
}
MPEplots(y2,h2)


plot(y2, type='l',  xlim=c(length(y2)-24*8,length(y2)));points(h2, type='l', col='red')

#con <- dbConnect(odbc(), 
 #                Driver = "MySQL ODBC 8.0 ANSI Driver", 
  #               Server = "127.0.0.1", 
   #              Database = "jaja", 
    #             UID = rstudioapi::askForPassword("Database user"), 
     #            PWD = rstudioapi::askForPassword("Database password"), 
      #           Port = 3306)
#res <- dbGetQuery(con, "SELECT * FROM jaja.hourly_flowrate_in")
#df=res
#df
#c=1

ff=vector();tt=vector();vv=vector()
color=c('yellow','red','green','blue')
convertedtime

graphfun<-function(){
  timeseq<-as.POSIXct(convertedtime[,1]+3600*dfts[,1], origin = "1970-01-01", tz = "CET")
  hour_now=(as.numeric(substring(Sys.time(),first=12,last=13)))
  minute_now=(as.numeric(substring(Sys.time(),first=15,last=16)))
  second_now=(as.numeric(substring(Sys.time(),first=18,last=19)))
  while(as.numeric(substring(Sys.time(),15,16))!=(minute_now+20)%%60){
    tt=Sys.time()
    g=as.numeric(substring(print(as.numeric(Sys.time())*1000,10),10,13))
    if(as.numeric(substring(Sys.time(),19,19))%%5==0){
      #plot(x,y)
      d=as.numeric(substring(print(as.numeric(Sys.time())*1000,10),10,13))
      while(abs(d-g)<10){
        colorcode=round(runif(1, min=1, max=4),0)
        vv=Sys.time()
        plot(timeseq[(50000+c):(50024+c)], dfts[(50000+c):(50024+c),2], type='l', usr=(c(0,24,0,10)), lab=c(24,20,1))
        c=c+1*0.1
        ifelse(c>10, c<-0, c<-c)
        c<<-c
        d=as.numeric(substring(print(as.numeric(Sys.time())*1000,10),10,13))
      }
      substring(print(as.numeric(Sys.time())*1000,10),15)=="07"
    }
    print(as.numeric(substring(Sys.time(),first=18,last=19)))
  }
}

graphfun()
#Next steps are to include exogenous regressors (ARMAX) from 
# i. wastewater characterization
# ii. Connection to Database
# iii. Weather forecasts (Rainfall:-mm & distance from WWTP)
# iv. Calculation of COD flux, TSS flux, etc.