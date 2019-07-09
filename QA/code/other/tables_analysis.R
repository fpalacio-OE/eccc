#just gva
step5a  <- readRDS(paste0(srcDir,"step5a.rds"))
step5b  <- readRDS(paste0(srcDir,"step5b.rds"))
step6   <- readRDS(paste0(srcDir,"step6.rds"))
step7   <- readRDS(paste0(srcDir,"step7.rds"))
step8   <- readRDS(paste0(srcDir,"step8.rds"))
step9   <- readRDS(paste0(srcDir,"step9.rds"))

#all
step10  <- readRDS(paste0(srcDir,"step10.rds"))
step11  <- readRDS(paste0(srcDir,"step11.rds"))
step12  <- readRDS(paste0(srcDir,"step12.rds"))
step13  <- readRDS(paste0(srcDir,"step13.rds"))
step14  <- readRDS(paste0(srcDir,"step14.rds"))
step15  <- readRDS(paste0(srcDir,"step15.rds"))


# GVA tables --------------------------------
step5a <- dcast(step5a[,.(code,year,geography,variable,value)],...~variable)
step5b <- dcast(step5b[,.(code,year,geography,variable,value)],...~variable)
step6  <- dcast(step6[,.(code,year,geography,variable,value)],...~variable)
step7  <- dcast(step7[,.(code,year,geography,variable,value)],...~variable)
step8

setnames(step5a, c("gva - chained prices (x 1,000,000)","gva - current prices (x 1,000,000)"),c("5a.real","5a.nom"))
setnames(step5b, c("gva - chained prices (x 1,000,000)","gva - current prices (x 1,000,000)"),c("5b.real","5b.nom"))
setnames( step6, c("gva - chained prices (x 1,000,000)","gva - current prices (x 1,000,000)"), c("6.real","6.nom"))
setnames( step7, c("gva - chained prices (x 1,000,000)","gva - current prices (x 1,000,000)"), c("7.real","7.nom"))
setnames( step8, c("gva - chained prices (x 1,000,000)","gva - current prices (x 1,000,000)"), c("8.real","8.nom"))


db <- step5a[step5b, on = list(geography,code,year)]
db <- step6[db, on = list(geography,code,year)]
db <- step7[db, on = list(geography,code,year)]
db <- step8[db, on = list(geography,code,year)]

db.real <- db[, c("code","year", "geography", names(db)[names(db) %like% "real"]), with = FALSE]
db.nom  <- db[, c("code","year", "geography", names(db)[names(db) %like% "nom"]), with = FALSE]

# Complete tables --------------------------------

#all
step10  
step11  
step12  <- dcast(step12[,.(code,year,geography,variable,value)],...~variable)
step13  <- dcast(step13[,.(code,year,geography,variable,value)],...~variable)
step14  <- dcast(step14[,.(code,year,geography,variable,value)],...~variable)

names(step10)[!names(step10) %in% c("code","year","geography")] <- paste0(names(step10)[!names(step10) %in% c("code","year","geography")],"-step10")
names(step11)[!names(step11) %in% c("code","year","geography")] <- paste0(names(step11)[!names(step11) %in% c("code","year","geography")],"-step11")
names(step12)[!names(step12) %in% c("code","year","geography")] <- paste0(names(step12)[!names(step12) %in% c("code","year","geography")],"-step12")
names(step13)[!names(step13) %in% c("code","year","geography")] <- paste0(names(step13)[!names(step13) %in% c("code","year","geography")],"-step13")
names(step14)[!names(step14) %in% c("code","year","geography")] <- paste0(names(step14)[!names(step14) %in% c("code","year","geography")],"-step14")

db <- step11[step10, on = list(geography,code,year)]
db <- step12[db, on = list(geography,code,year)]
db <- step13[db, on = list(geography,code,year)]
db <- step14[db, on = list(geography,code,year)]

#  copy paste var name from here:
#
# "gva - current prices (x 1,000,000)"                  "employment"                                         
# "gross output (x 1,000,000)"                          "price index"                                        
# "capital stock - chained prices (x 1,000,000)"        "investment, mach - chained prices (x 1,000,000)"    
#  "investment, constr - current prices (x 1,000,000)"   "depreciation, mach - chained prices (x 1,000,000)"  
#  "investment - current prices (x 1,000,000)"           "investment - chained prices (x 1,000,000)"          
#  "capital depreciation - chained prices (x 1,000,000)" "investment, ip - current prices (x 1,000,000)"      
#  "investment, constr - chained prices (x 1,000,000)"   "depreciation, constr - chained prices (x 1,000,000)"
#  "stock, mach - chained prices (x 1,000,000)"          "stock, constr - chained prices (x 1,000,000)"       
#  "investment, mach - current prices (x 1,000,000)"     "investment, ip - chained prices (x 1,000,000)"      
#  "hours worked"                                        "depreciation, ip - chained prices (x 1,000,000)"    
#  "gross output - constant prices (x 1,000,000)"        "stock, ip - chained prices (x 1,000,000)"           
#  "wages"                                               "gva - chained prices (x 1,000,000)" 

targetvar <- "wages"
cols      <-  names(db)[names(db) %like% targetvar]

View(db[,c("geography","code","year", cols),with = FALSE])


# db.real
# db.nom
# 
# nrow(db.nom)  - nrow(db.nom[`8.nom` == `7.nom`])
# 
# View(db.real[(abs(`8.real`-`7.real`)/`7.real`) > .02])
# View(db.real[(abs(`6.real`-`5b.real`)/`5b.real`) > .02])
