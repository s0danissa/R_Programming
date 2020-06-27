pollutantmean<-function(directory,pollutant,id=1:332){
  #reading all files with id numbers
  temp = list.files(path=directory,pattern="*.csv")
  a<-1
  for(x in id){
    pathh<-paste(directory,"/",temp[x],sep="")
    data_curr<-read.csv(pathh,header=TRUE)
    if(a==1){
      data_total<-data_curr
    } else{
      data_total<-rbind(data_total,data_curr)
    }
    a=a+1
  }
  #calculating the mean
  mean(data_total[[pollutant]],na.rm=TRUE)
}

complete<-function(directory,id=1:332){
  temp = list.files(path=directory,pattern="*.csv")
  data_k<-data.frame(id=id,nobs=length(id))
  a<-1
  for(x in id){
    pathh<-paste(directory,"/",temp[x],sep="")
    data_curr<-read.csv(pathh,header=TRUE)
    good<-complete.cases(data_curr)
    numb<-nrow(data_curr[good,])
    data_k$nobs[a]<-numb
    a=a+1
  }
  data_k
}

corr<-function(directory, threshold=0){
  data_com<-complete("specdata",1:332) #data frame of id's
  #and their numbers of complete cases
  data_com<-subset(data_com,nobs>threshold)
  data_com<-data_com$id #ids of 'good' datasets
  temp = list.files(path=directory,pattern="*.csv")
  a<-1
  corre<-1:length(data_com)
  for(f in data_com){
    pathh<-paste(directory,"/",temp[f],sep="")
    data_curr<-read.csv(pathh,header=TRUE)
    good<-complete.cases(data_curr)
    data_curr<-data_curr[good,]
    corre[a]<-cor(data_curr$sulfate,data_curr$nitrate)
    a=a+1
  }
  corre
}