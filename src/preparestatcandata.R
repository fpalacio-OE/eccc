srcDir     = "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/resources/"


###unpack statcan investment data

newcaplocation<- paste(srcDir,"newcap.xlsx", sep="")
newcap<- as.data.table(read_excel(newcaplocation,  na = "NA" ,col_names= TRUE))

metadatafornewcaplocation<-paste(srcDir,"newcapmetadata.xlsx", sep="")
meta<-as.data.table(read_excel(metadatafornewcaplocation,  na = "NA" ,col_names= F))
names(meta)<-c("full","abb")

#fix first column
id<-c("junk", "prices","var", "assettype","code","geography")
newcap[,(id):= tstrsplit(X__1, "[[:punct:]]")][,c("junk","X__1"):=NULL]

#rename
newcap[,prices:=tolower(meta$full[match(newcap$prices,meta$abb,nomatch = NA)])]
newcap[,var:=meta$full[match(newcap$var,meta$abb,nomatch = NA)]]
newcap[,assettype:=meta$full[match(newcap$assettype,meta$abb,nomatch = NA)]]
newcap[,geography:=meta$full[match(newcap$geography,meta$abb,nomatch = NA)]]
#fix code
newcap[,code:=gsub("^B","",code)]
newcap[,code:=gsub("S","",code)]
newcap[,code:=gsub("00","",code)]
newcap[code=="92",code:="91"]

#fix varname

newcap[var %like% "depreciation",var:="depreciation"]
newcap[var %like% "stock",var:="stock"]
newcap[var %like% "Investment",var:="investment"]

newcap[assettype %like% "Intellectual",assettype:=", ip"]
newcap[assettype %like% "Total",assettype:=""]
newcap[assettype %like% "buildings",assettype:=", build"]
newcap[assettype %like% "Engineering",assettype:=", eng"]
newcap[assettype %like% "equipment",assettype:=", mach"]

newcap[,newvar:=paste(var,assettype,sep = "")]

newcap[,newvar:=paste(newvar,prices,"statcan",sep = " - ")]

#clean and reshape
varstokill<-c("prices", "var", "assettype")
newcap[,(varstokill):=NULL]

newcap<-melt(newcap,id.vars = c("geography", "code", "newvar"), variable.name = "year")
newcap<-newcap[!newvar %like% "constant"]

##sum relevant sectors
#make numeric
newcap[,value:=as.numeric(gsub("[^0-9.]", "", value))]

#sum
firstsum<-newcap[code %in% c("32518","32519","3256","3259")]
firstsum<-firstsum[,list(value=sum(value)),by=list(newvar,year,geography)]
firstsum[,code:="3252,3255-3259"]

secondsum<-newcap[code %in% c("32733","32739")]
secondsum<-secondsum[,list(value=sum(value)),by=list(newvar,year,geography)]
secondsum[,code:="32733-32739"]

thirdsum<-newcap[code %in% c("3271","3272","3274","3279")]
thirdsum<-thirdsum[,list(value=sum(value)),by=list(newvar,year,geography)]
thirdsum[,code:="327A"]

fourthsum<-newcap[code %in% c("32412","32419")]
fourthsum<-fourthsum[,list(value=sum(value)),by=list(newvar,year,geography)]
fourthsum[,code:="3241A"]

#correct units
newcap[,value:=value/1000]
newcap[code==331317,code:="331314, 331315, 331318"]
#regroup
newcap<-rbind(newcap,secondsum,firstsum,thirdsum,fourthsum)
newcap<-dcast(newcap, geography+code+year~newvar)
saveRDS(newcap,paste(srcDir,"newcap.rds",sep=""))
