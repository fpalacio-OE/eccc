imposegrowthrates<-function(DT){
  x<-DT
  investmentnames<-paste("stock","investment","depreciation",sep="|")
  #print(c(as.character(unique(x$variable)),unique(x$geography),unique(x$code)))
  
  #create new growthrate
  start  <-grep("value-growth", colnames(x)) + 1
  end    <-ncol(x)
  if((x$variable[1]=="gva - current prices (x 1,000,000)" | x$variable[1]=="gva - chained prices (x 1,000,000)") & x$geography[1]=="Canada"){
    primary<-names(x)[grep("^value$", colnames(x)) + 2]
  }else{
    primary<-names(x)[grep("^value$", colnames(x)) + 1]
    if(length(grep(investmentnames, x$variable[1]))!=0 & all(is.na(x[,get(primary)]))){
      primary<-names(x)[grep("^value$", colnames(x)) + 2]
    }
  }
  
  
  suppressWarnings(x[, temp:=NA])
  
  for(i in start:end){
    comparison<- colnames(x)[i]
    x[!is.na(eval(comparison)),temp:=ifelse(is.na(temp), get(comparison), temp)]
  }
  
  x[, test:= ifelse(is.na(temp), `value-growth`, temp)]
  
  #use new growthrate to update series
  #if we have data in the primary series, we'll make all the values cohere with that series. Otherwise, we'll just go from the beginning
  if(all(is.na(x[,get(primary)]))){
    x[,try:=value]
    for(y in min(x$year):max(x$year)){
      x[year==y,    `:=`
        (try    =ifelse(is.nan(temp)|is.infinite(temp)|is.na(temp),  value, x[year==as.character(as.numeric(y)-1) ,try]*temp))]
    }
  }else{
    #get some info on the primary series
    primarylastyear <-x$year[x[,  apply(.SD, 2, function(x) max(which(!is.na(x)))),.SD=(primary)]]
    primaryfirstyear<-x$year[x[,  apply(.SD, 2, function(x) min(which(!is.na(x)))),.SD=(primary)]]
    
    x[,try:=value]
    #forwards from the last year in the primary series
    for(y in primarylastyear:max(x$year)){
      x[year==y,    `:=`
        (try    =ifelse(is.nan(temp)|is.infinite(temp)|is.na(temp),value,  
                        ifelse(is.na(x[year==as.character(as.numeric(y)-1) ,try]),value, 
                               ifelse(temp==0,0 ,x[year==as.character(as.numeric(y)-1) ,try]*temp))))]
    }
    #backwards from the first year in the primary series
    for(y in (as.numeric(primaryfirstyear)-1):min(x$year)){
      x[year==y,    `:=`
        (try    =ifelse(is.nan(x[year==as.character(as.numeric(y)+1) ,temp])|is.infinite(x[year==as.character(as.numeric(y)+1) ,temp])|is.na(x[year==as.character(as.numeric(y)+1) ,temp]),  value, 
                        ifelse(is.na(x[year==as.character(as.numeric(y)+1) ,try]),value, 
                               ifelse(x[year==as.character(as.numeric(y)+1) ,temp]==0, 0 ,x[year==as.character(as.numeric(y)+1) ,try]/x[year==as.character(as.numeric(y)+1) ,temp]))))]
    }
    x[is.nan(try),try:=0]
  }
  
  #keep only the new value
  x[, value:= try]
  colstokeep<-c("geography", "code", "year","variable","value")
  x<-x[,..colstokeep]
  
}

