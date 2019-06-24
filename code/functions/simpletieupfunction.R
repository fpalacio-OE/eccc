tieupfunction<-function(x){
  for(i in umbrellasects){
    for(k in 1:5){
    #print(i)
    #set tolerance
    tol=.05
    #choose relevant sectors
    sect     <-shiftshares[umbrella %in% i, code]
    #calculate sums 
    suppressWarnings(x[ code %in% sect, sectorsum:=sum(value), by=list(geography, year)])
    
    #now we move the sums to only the umbrella value using a merge
    namesformerge<-names(x)[!(names(x) %in% c("value", "umbrella", "try","tag"))]
    y<- x[code %in% sect[1], ..namesformerge][,code:=as.character(i)]
    names(y)[names(y)=="sectorsum"]<-"sums"
    setkey(y,         code, variable, geography,year )
    setkey(x, code, variable, geography,year)
    x<-y[x]
    
    #adjustment based on lower levels (only for real data)
    namesformerge<-names(x)[!(names(x) %in% c("try", "sums", "vertdiff", "numfree", "adjustment"))]
    y<-x[  (umbrella %in% sect), ..namesformerge] #tag==1 &
    y[,value:=as.numeric(value)]
    y[tag==1, subsectorsum:=sum(value,na.rm=TRUE), by=list(geography, year,umbrella)]
    y[,code:=umbrella]
    y[subsectorsum==0, subsectorsum:=NA]
    setkey(y, code, variable, geography, year)
    y<-y[!duplicated(y, by=key(y))]
    y[, c("sectorsum","tag", "umbrella", "value"):=NULL]
    x<-y[x]
    
    
    
    #calculate error
    x[, vertdiff:=((value-sums)/sums)][,sectorsum:=NULL]
    x[, vertdiff:=ifelse(abs(vertdiff)<tol, 0, vertdiff)]
    #create and paste adjustment
    slots<-length(sect)
    x[code %in% sect, numfree:= slots-sum(tag,na.rm=T),by=list(geography, year)]
    x[, vertdiff:=vertdiff[match(umbrella,code,nomatch=NA)],by=list(geography,year,variable)]
    x[code %in% sect | code %in% as.character(i), adjustment:=as.numeric(ifelse(numfree!=0,vertdiff*value,as.numeric(NA))), by=list(geography)][tag==1,fix:= as.numeric(adjustment), by=list(geography,umbrella, year)][tag==1 |code %in% as.character(i), adjustment:=as.numeric(NA)][,fix:=sum(fix,na.rm=T),by=list(geography,umbrella,year)]
    x[,prop:=sum(adjustment,na.rm=T), by=list(geography, year)][,prop:=ifelse(prop==0, 0,adjustment/prop)]
    x[,fix:=prop*fix][,prop:=NULL]
    x[,adjustment:=ifelse(is.na(adjustment), fix, fix+adjustment)][,fix:=NULL]
    
    #use the lower level anchor
    x[,adjusterror:= ifelse(is.na(adjustment), value-subsectorsum, value+adjustment-subsectorsum)]
    x[!is.na(subsectorsum),adjustment2:=ifelse(adjusterror<0, ifelse(is.na(adjustment), adjusterror*-1,adjustment-adjusterror), adjustment)]
    x[ code %in% sect, totalfix:=sum(adjusterror,na.rm=T), by=list(geography, year)]
    
    #x[, adjustment3:= ifelse(is.na(adjustment), adjusterror, adjustment - totalfix/numfree)]
    x[ code %in% sect, df:=slots-sum(!is.na(adjusterror)), by=list(geography, year)]
    x[!(code%in% i) & is.na(adjusterror),prop:=sum(value,na.rm=T), by=list(geography, year, umbrella)][,prop:=value/prop]
    
    x[df!=0 & is.na(adjustment2) & tag!=1 & totalfix<0,adjustment2:=ifelse(is.na(adjustment), prop*totalfix,adjustment+(totalfix*prop))]
    x[,adjustment:=ifelse(is.na(adjustment2),adjustment, adjustment2)][,c("adjustment2", "totalfix", "df", "adjusterror","prop"):=NULL]
    
    # prevent negative values
    x[,overshot:=ifelse(adjustment+value<0, adjustment+value,NA)]
    x[,totalfix:=-1*sum(overshot, na.rm = T),by=list(geography,umbrella,year)]
    x[(tag!=1|is.na(tag)) & is.na(overshot),prop:=sum(adjustment,na.rm=T), by=list(geography, year)]
    x[(tag!=1|is.na(tag)) & is.na(overshot),prop:=ifelse(prop==0,0,adjustment/prop)]
    x[(tag!=1|is.na(tag)) & is.na(overshot),adjustment:=ifelse(is.na(adjustment), prop*totalfix,adjustment+(totalfix*prop))]
    x[(tag!=1|is.na(tag)) & !is.na(overshot), value:=0]
    x[(tag!=1|is.na(tag)) & !is.na(overshot), adjustment:=NA]
    #apply scale factor
    x[tag!=1|is.na(tag),value:=ifelse(is.na(adjustment), value, value+adjustment)][,c( "totalfix","prop","overshot"):=NULL]
    
    #clean
    x[,c("sums", "vertdiff", "numfree", "adjustment", "subsectorsum"):=NULL]
    }
  }
  out<<-x
}
