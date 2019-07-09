step5a <- readRDS(paste0(srcDir,"step5a.rds"))
step5b <- readRDS(paste0(srcDir,"step5b.rds"))
step6  <- readRDS(paste0(srcDir,"step6.rds"))
step7  <- readRDS(paste0(srcDir,"step7.rds"))
step8  <- readRDS(paste0(srcDir,"step8.rds"))
step9  <- readRDS(paste0(srcDir,"step8.rds"))
step10  <- readRDS(paste0(srcDir,"step8.rds"))
step11  <- readRDS(paste0(srcDir,"step8.rds"))
step12  <- readRDS(paste0(srcDir,"step8.rds"))

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


# db.real
# db.nom
# 
# nrow(db.nom)  - nrow(db.nom[`8.nom` == `7.nom`])
# 
# View(db.real[(abs(`8.real`-`7.real`)/`7.real`) > .02])
# View(db.real[(abs(`6.real`-`5b.real`)/`5b.real`) > .02])
