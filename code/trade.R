library(data.table)
library(rJava)
library(readxl)
library(xlsx)
library(XML)
library(quantmod)
library(stringr)
library(rlang)
##############################################################################################################################
####      READ ME                                                                                                         ####
#### This file requires:                                                          Currently held in:                      ####
#### 1. A set of URLs                                                             DataWarehouse-2.xlsm                    ####
#### 2. A comprehensive directory of possible industry names to NAICS codes       Directory-namestonaicscodes.xlsx        ####
####    (Formatting and contents have implications on objects d and m as well                                             ####
####     the import range in line 26 below)                                                                               ####
##############################################################################################################################




#############################################################################################
## Set Directory, Export Name, Import Range, and other parameters:
#############################################################################################
# srcDir     = "/Users/Fabio/Documents/oe/eccc/resources/"
# dstDir     = "/Users/Fabio/Documents/oe/eccc/outputs/"
# srcDir     = "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/resources/"
# dstDir     = "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/outputs/"

provinces <- c(
  "Alberta",
  "British Columbia",
  "Manitoba",
  "New Brunswick",
  "Newfoundland and Labrador",
  "Northwest Territories",
  "Nova Scotia",
  "Nunavut",
  "Ontario",
  "Prince Edward Island",
  "Quebec",
  "Saskatchewan",
  "Yukon",
  "Canada"
)

urlfile <- "DataWarehouse-4.xlsm"
codedir <- "Directory-namestonaicscodes-2.xlsx" # <-------------- # This file contains (on the left are sheet names):
mainlist <- "Main" # 1. The official list of delivered sectors
xtendeddir <- "Expanded" # 2. The list of all the pulled sectors, including those that are used to calculate one of the official sectors
mapdir <- "Mapping" # 3. The mapping of the 2 to 1
smerge <- "SeriesMerge" # 4. The mapping of the various series that make each variable

# mapping file (useful at times)
naicsdir <- paste(srcDir, eval(codedir), sep = "")
naics <- as.data.table(read_excel(naicsdir, na = "NA", sheet = eval(mainlist), col_names = TRUE))
nind <- nrow(naics) # of industries

# upload mapping doc & extended sectors list
mapping <- as.data.table(read_excel(naicsdir, na = "NA", sheet = eval(mapdir), col_names = TRUE, col_types = c("guess", "guess", "guess", "guess", "text", "guess")))
fullnaicslist <- as.data.table(read_excel(naicsdir, na = "NA", sheet = eval(xtendeddir), col_names = TRUE))

# upload series merge map
merge <- as.data.table(read_excel(naicsdir, na = "NA", sheet = eval(smerge), col_names = TRUE))
mnems <- as.data.table(read_excel(naicsdir, na = "NA", sheet = "FinalMnem", col_names = TRUE))
sectors <- as.data.table(read_excel(naicsdir, na = "NA", sheet = "sectorcode", col_names = TRUE))


# this doc maps the shift shares
shiftshares <- as.data.table(read_excel(naicsdir, na = "NA", sheet = "Shiftshares", col_names = TRUE))


# concordance
concordance_file <- "Trade-concordance.xlsx"
concordance <- as.data.table(read_excel(paste0(srcDir, concordance_file), na = "NA", col_names = TRUE))

# int. import weights
intimp_file <- "pmx_weights.xlsx"
intimp_weights <- as.data.table(read_excel(paste0(srcDir, intimp_file), na = "NA", col_names = TRUE))


#############################################################################################
## Get trade data
#############################################################################################
options(scipen = 999)
# Load up bases
# RDSpath <-paste(dstDir, "envcandb-new.rds",sep="")
envcandb <- readRDS(paste(srcDir, "trade-new.rds", sep = ""))
# envcandb<-envcan[[1]]
envcandb <- dcast(envcandb, geography + industry + year ~ var)

envcandbshort <- envcandb[year >= 1980]
rm(envcandb)

tradeseries <- c("geography", "industry", "year", as.character(names(envcandbshort)[grepl("2280064|3860003|3800070", names(envcandbshort)) ]))
trade <- envcandbshort[, ..tradeseries]

#############################################################################################
## Get deflators
#############################################################################################
deflator_file <- "final-goutput2.rds"

deflator_file <- readRDS(paste0(dstDir, deflator_file))
deflator_file <- deflator_file[grepl("^P", mnemonic) & !mnemonic %like% "PGDP", !c("header", "V", "L", "start", "end", "forecastend", "pers"), with = F]
deflator_file[, mnemonic := substring(mnemonic, 2)]
deflator_file <- suppressWarnings(melt(deflator_file, id.vars = c("sectorcode", "mnemonic"), variable.name = "year", value.name = "deflator"))
deflator_file[, `:=`(deflator = as.numeric(deflator), year = as.character(year))]
deflator_file[is.na(deflator) | deflator == "" | deflator == .001, deflator := NA]
deflator_file[mnemonic == "PI", mnemonic := NA]
setnames(deflator_file, "mnemonic", "mnem")

#############################################################################################
## Begin building new series
#############################################################################################


# get the necessary files
weightsfilename <- "Trade - Product to Ind mapping.xlsx"
exports <- "Exports"
intproimp <- "Interprovincial Imports"
intnatimp <- "International imports"


# Weights
weightsfilepath <- paste(srcDir, weightsfilename, sep = "")
weights <- as.data.table(read_excel(weightsfilepath, na = "NA", sheet = eval(exports), col_names = TRUE))

# Group maps
groupmap <- read.csv(paste(srcDir, "groupmapping.csv", sep = ""), header = T)
groupmap[groupmap == ""] <- NA
groupmap <- na.locf(groupmap)

# expand weights file and re match products

remap <- as.data.table(read.csv(paste(srcDir, "remap.csv", sep = ""), na = "NA", stringsAsFactors = F))
weights <- remap[weights, on = c("PRODUCT", "sector_cd")][, ProductCode := NULL]
setnames(weights, "i.ProductCode", "ProductCode")
weights[, sector_cd := ifelse(is.na(sector_cd_new), sector_cd, sector_cd_new)][, sector_cd_new := NULL]

#############################################################################
#### begin with exports
#############################################################################
# merge group map with weights & trade data for ref year
fullmap <- as.data.table(merge(groupmap, weights, by.x = "Code", by.y = "ProductCode", all.y = T))
fullmap[, Ref_Date := as.character(Ref_Date)]

# collapse down to a group mapping (groups from table 386-0003)
grouptable <- fullmap[, lapply(.SD, sum), by = list(GEO, Group.Code, Group.Title, sector_cd, model_sector), .SDcols = "SumOfValue" ]

# calculate new proportions (from groups to sector codes)
grouptable[, GroupTotal := sum(SumOfValue), by = list(GEO, Group.Code)][, props := SumOfValue / GroupTotal]

## prepare trade data for merge
exportseries <- c("geography", "industry", "year", as.character(names(trade)[names(trade) %like% "export"]))
tradeformerge <- trade[, ..exportseries]
tradeformerge[, industry := str_trim(gsub(" \\(x 1,000\\)", "", industry))]

# Merge and consolidate
keep <- c("GEO", "sector_cd", "model_sector", "year", "interprovexports", "internatexports")
exports <- merge(grouptable, tradeformerge, by.x = c("Group.Title", "GEO"), by.y = c("industry", "geography"), allow.cartesian = T)
exports[, `:=`
(
  `interprovincial exports-3860003-current prices` = as.numeric(gsub("[^0-9.]", "", `interprovincial exports-3860003-current prices`)),
  `international exports-3860003-current prices` = as.numeric(gsub("[^0-9.]", "", `international exports-3860003-current prices`)),
  `international re-exports-3860003-current prices` = as.numeric(gsub("[^0-9.]", "", `international re-exports-3860003-current prices`))
)][, `:=`
  (
    interprovexports = props * `interprovincial exports-3860003-current prices`,
    internatexports = props * `international exports-3860003-current prices`
  )]
exports[GEO == "Canada", internatreexports := props * `international re-exports-3860003-current prices`]

mysum <- function(x) if (all(is.na(x))) as.numeric(NA) else sum(x, na.rm = T)

exports <- exports[, lapply(.SD, mysum), by = list(GEO, year, sector_cd, model_sector), .SDcols = c("interprovexports", "internatexports", "internatreexports")]
exports <- exports[, ..keep]
#############################################################################
## int prov. imports
#############################################################################
# Weights
weightsfilepath <- paste(srcDir, weightsfilename, sep = "")
weights <- as.data.table(read_excel(weightsfilepath, na = "NA", sheet = eval(intproimp), col_names = TRUE))
weights <- remap[weights, on = c("PRODUCT", "sector_cd")][, ProductCode := NULL]
setnames(weights, "i.ProductCode", "ProductCode")
weights[, sector_cd := ifelse(is.na(sector_cd_new), sector_cd, sector_cd_new)][, sector_cd_new := NULL]
# clean
weights[, c("CanTotSup", "X__1") := NULL]
names(weights)[names(weights) == "CanTotSup__1"] <- "CanTotSup"
weights[, ProvIndSup := Mshare * ProvTotSup]
weights[, AllOthIndSup := CanTotSup - ProvIndSup]


# merge group map with weights & trade data for ref year
fullmap <- as.data.table(merge(groupmap, weights, by.x = "Code", by.y = "ProductCode", all.y = T))
fullmap[, Ref_Date := as.character(Ref_Date)][, c("AllOthTotSup", "ProvTotSup", "CanIndSup", "CanTotSup") := NULL]

# collapse down to a group mapping (groups from table 386-0003)
group <- function(x) {
  dt <- x
  j <- 1
  list <- list()
  for (i in provinces) {
    list[[j]] <- dt[GEO != i]
    names(list)[j] <- i
    j <- j + 1
  }
  list <- list
}

list <- group(fullmap)

weights <- lapply(list, function(x) {
  x <- x[, lapply(.SD, mysum), by = list(Group.Code, Group.Title, sector_cd, model_sector), .SDcols = "ProvIndSup" ]
  x[, GroupTotal := mysum(ProvIndSup), by = list(Group.Code)][, props := ProvIndSup / GroupTotal]
})

grouptable <- rbindlist(weights, idcol = "GEO")

## prepare trade data for merge
importseries <- names(trade)[tolower(names(trade)) %like% "interprovincial imports"]
tokeep <- c("geography", "industry", "year", importseries)
tradeformerge <- trade[, ..tokeep]
tradeformerge[, industry := str_trim(gsub(" \\(x 1,000\\)", "", industry))]

# Merge and consolidate
tokeep <- c("GEO", "sector_cd", "model_sector", "year", "interprovimports")
imports <- merge(grouptable, tradeformerge, by.x = c("Group.Title", "GEO"), by.y = c("industry", "geography"), allow.cartesian = T)
imports[, `:=`
(`interprovincial imports-3860003-current prices` = as.numeric(gsub("[^0-9.]", "", `interprovincial imports-3860003-current prices`)))
][, `:=`
  (interprovimports = props * `interprovincial imports-3860003-current prices`)]
imports <- imports[, lapply(.SD, mysum), by = list(GEO, year, sector_cd, model_sector), .SDcols = c("interprovimports")]
imports <- imports[, ..tokeep]
#############################################################################
#### 3. int nat. import index                                            ####
#############################################################################
# Weights
# weightsfilepath<- paste(srcDir,weightsfilename, sep="")
# weights<- as.data.table(read_excel(weightsfilepath,  na = "NA", sheet= eval(intnatimp) ,col_names= TRUE))
# weights<-remap[weights,on=c("PRODUCT","sector_cd")][,ProductCode:=NULL]
# setnames(weights,"i.ProductCode","ProductCode")
# weights[,sector_cd:=ifelse(is.na(sector_cd_new),sector_cd,sector_cd_new)][,sector_cd_new:=NULL]
# weights[,PRODUCT:=str_trim(gsub(" \\(x 1,000\\)","",PRODUCT))]
#
#
intimp_weights_id <- merge(intimp_weights, concordance, by.x = "NAPCS 3-digit", by.y = "NAPCS")


importseries <- names(trade)[tolower(names(trade)) %like% "price index-import"]
tokeep <- c("geography", "industry", "year", importseries)
tradeformerge <- unique(trade[, ..tokeep])
tradeformerge[, industry := str_trim(gsub(" \\(x 1,000\\)", "", industry))]



# Merge and consolidate
keep <- c("GEO", "sector_cd", "model_sector", "year", "importindex")
# intimportindex<-merge(weights, tradeformerge, by.x = c("PRODUCT"), by.y=c("industry"), allow.cartesian=T)
intimportindex <- unique(merge(intimp_weights_id, tradeformerge, by.x = c("Product"), by.y = c("industry"), allow.cartesian = T))
intimportindex[, GEO := NULL]

# calculate sector indexes from weights (weighted averages)
indeces <- c("paasche", "laspeyres")
# intimportindex[,basis:=sum(SumOfValue),by=list(year,model_sector,geography)]
# intimportindex[,proportions:=ifelse(is.finite(SumOfValue/basis),SumOfValue/basis,NA)]

intimportindex[, (indeces) := list(`paasche current weighted-price index-import-2280064` * PMXwt, `laspeyres fixed weighted-price index-import-2280064` * PMXwt)]
intimportindex[, (indeces) := list(sum(paasche), sum(laspeyres)), by = list(year, model_sector, geography)]
intimportindex[, test := sum(PMXwt), by = list(year, model_sector, geography)]
intimportindex[, (indeces) := list(ifelse(abs(test - 1) > .1, paasche / test, paasche), ifelse(abs(test - 1) > .1, laspeyres / test, laspeyres))]

# calculate final index (paasche * laspeyres)^.5
intimportindex[, importindex := (paasche * laspeyres)^.5]
setnames(intimportindex, "geography", "GEO")

intimportindex <- unique(intimportindex[, ..keep])

#############################################################################
## int nat. imports ####
#############################################################################
# Weights
weightsfilepath <- paste(srcDir, weightsfilename, sep = "")
weights <- as.data.table(read_excel(weightsfilepath, na = "NA", sheet = eval(intnatimp), col_names = TRUE))
weights <- remap[weights, on = c("PRODUCT", "sector_cd")][, ProductCode := NULL]
setnames(weights, "i.ProductCode", "ProductCode")
weights[, sector_cd := ifelse(is.na(sector_cd_new), sector_cd, sector_cd_new)][, sector_cd_new := NULL]

# merge group map with weights & trade data for ref year
fullmap <- as.data.table(merge(groupmap, weights, by.x = "Code", by.y = "ProductCode", all.y = T))
fullmap[, Ref_Date := as.character(Ref_Date)][, c("ShareOfDom", "DomSup") := NULL]

# collapse down to a group mapping (groups from table 386-0003)
grouptable <- fullmap[, lapply(.SD, sum), by = list(GEO, Group.Code, Group.Title, sector_cd, model_sector), .SDcols = "SumOfValue" ]

# calculate new proportions (from groups to sector codes)
grouptable[, GroupTotal := sum(SumOfValue), by = list(GEO, Group.Code)][, props := SumOfValue / GroupTotal]

## prepare trade data for merge
importseries <- names(trade)[tolower(names(trade)) %like% "international imports"]
tokeep <- c("geography", "industry", "year", importseries)
tradeformerge <- trade[, ..tokeep]
tradeformerge[, industry := str_trim(gsub(" \\(x 1,000\\)", "", industry))]

# Merge and consolidate
keep <- c("GEO", "sector_cd", "model_sector", "year", "internatimports")
# note that this merge is different because we use canada weights for all provinces
intimports <- merge(grouptable, tradeformerge, by.x = c("Group.Title"), by.y = c("industry"), allow.cartesian = T)
intimports[, GEO := geography][, geography := NULL]

intimports[, `:=`
(`international imports-3860003-current prices` = as.numeric(gsub("[^0-9.]", "", `international imports-3860003-current prices`)))
][, `:=`
  (internatimports = props * `international imports-3860003-current prices`)]

intimports <- intimports[, lapply(.SD, mysum), by = list(GEO, year, sector_cd, model_sector), .SDcols = c("internatimports")]
intimports <- intimports[, ..keep]

###

tradefinal <- merge(intimports, imports, by = c("GEO", "sector_cd", "model_sector", "year"), all = T)
tradefinal <- merge(tradefinal, exports, by = c("GEO", "sector_cd", "model_sector", "year"), all = T)
tradefinal <- merge(tradefinal, intimportindex, by = c("GEO", "sector_cd", "model_sector", "year"), all = T)

tradefinal <- tradefinal[GEO != "Canadian territorial enclaves abroad"]

# fix names
names(tradefinal) <- c("geography", "mnem", "industry", "year", "internatimports", "interprovimports", "interprovexports", "internatexports", "importindex")

#############################################################################
## int nat. trade services deflators                                     #####
#############################################################################
# serdef <- names(trade)[ grepl("total.*services", tolower(names(trade)))]
# 
# tokeep <- c("geography", "industry", "year", serdef)
# serdef <- trade[industry == "National", ..tokeep]
# 
# deflators <- c("importdeflator", "exportdeflator")
# serdef[, (deflators) := list(
#   `imp-total imports of services-3800070-current prices` / `imp-total imports of services-3800070-chained (2007) dollars` * 100,
#   `exp-total exports of services-3800070-current prices` / `exp-total exports of services-3800070-chained (2007) dollars` * 100
# )]
# 
# 
# # Merge and consolidate
# tokeep <- c("geography", "industry", "year", deflators)
# serdef <- serdef[, ..tokeep]
# serdef <- melt(serdef, id.vars = c("geography", "industry", "year"))
# serdef[, `:=`(sectorcode = "CANADA", mnemonic = ifelse(variable %like% "import","PM","PX"), code = "NAT", mnem = "")]
######################################################
# make agriculture                                    #
######################################################



# ag<-tradefinal[code %in% c(111,112,113,114,115)]
# ag   <-ag[, lapply(.SD,mysum), .SDcols=c("internatimports",  "interprovimports","interprovexports","internatexports"), by=c("year","geography")]
# ag[,code:="11"][,industry:="agriculture"]
# tradefinal<-rbind(tradefinal,ag)
######################################################
# Step 7:                                         #####
# Reshape, add Mnemonics export                  #####
######################################################

# stipulate aggregation functions

sumfunct <- function(x, y) {
  target <- x
  components <- y

  forsum <- tradefinal[code %in% components]
  sum <- forsum[, lapply(.SD, mysum), by = list(geography, year), .SDcol = names(forsum)[names(forsum) %like% "inter"]]

  sum[, `:=`(code = target, mnem = mnems$mnem[match(target, mnems$code, nomatch = NA)], industry = mnems$X__1[match(target, mnems$code, nomatch = NA)]) ]

  sum <- as.data.table(sum)

  # tradefinal<-rbind(sum,tradefinal)
}

avgfunct <- function(x, y) {
  target <- x
  components <- y

  foravg <- tradefinal[code %in% components]
  avg <- foravg[, lapply(.SD, function(x) mean(x, na.rm = TRUE)), by = list(geography, year), .SDcol = names(foravg)[names(foravg) %like% "index"]]

  avg[, `:=`(code = target, mnem = mnems$mnem[match(target, mnems$code, nomatch = NA)], industry = mnems$X__1[match(target, mnems$code, nomatch = NA)]) ]

  avg <- as.data.table(avg)

  # tradefinal<-rbind(sum,tradefinal)
}

# prepare tradefinal
codes <- mnems$code[match(tradefinal$mnem, mnems$mnem, nomatch = NA)]
tradefinal[, code := codes]
tradefinal[industry == "Couriers and Messengers", c("code", "mnem") := list("492", "TC")]
tradefinal[industry == "Postal Sevice", c("code", "mnem") := list("491", "TS")]

## first aggregate the top-down sectors:
matchcolumns <- shiftshares[!is.na(umbrella)]
target_sects <- unique(mnems$code[match(remap$sector_cd, mnems$mnem, nomatch = NA)])
sum_sects <- lapply(target_sects, function(x) matchcolumns$code[matchcolumns$umbrella == x])
work_graph <- as.data.table(cbind(target_sects, sum_sects))

for (i in 1:nrow(work_graph)) {
  print(i)
  sum <- sumfunct(work_graph$target_sects[[i]], work_graph$sum_sects[[i]])
  avg <- avgfunct(work_graph$target_sects[[i]], work_graph$sum_sects[[i]])
  newsects <- merge(avg, sum, by = c("geography", "year", "code", "mnem", "industry"))
  tradefinal <- rbind(tradefinal[!code %in% unique(newsects$code)], newsects)
}

tradefinal[is.nan(importindex), importindex := NA]


## second add sums for higher level sectors, especially the ones that we replaced with subsectors (think mchemb)
# collect target sectors

target_sects <- mnems$code[!mnems$mnem %in% tradefinal$mnem]
target_sects <- target_sects[target_sects != "NAT"]
sum_sects <- lapply(target_sects, function(x) matchcolumns$code[matchcolumns$umbrella == x])
sum_levels <- lapply(target_sects, function(x) unique(matchcolumns$level[matchcolumns$umbrella == x]))

work_graph <- as.data.table(cbind(target_sects, sum_levels, sum_sects))
work_graph <- work_graph[unlist(lapply(sum_levels, function(x) length(x) > 0))]
work_graph[, sum_levels := unlist(sum_levels)]
setorder(work_graph, -sum_levels)

# apply sum function
for (i in 1:nrow(work_graph)) {
  print(i)
  sum <- sumfunct(work_graph$target_sects[[i]], work_graph$sum_sects[[i]])
  avg <- avgfunct(work_graph$target_sects[[i]], work_graph$sum_sects[[i]])
  newsects <- merge(avg, sum, by = c("geography", "year", "code", "mnem", "industry"))

  tradefinal <- rbind(tradefinal, newsects)
}

tradefinal[is.nan(importindex), importindex := NA]

######################################################
# Step 8:                                         #####
# Create real series                             #####
######################################################

tradefinal[, sectorcode := sectors$code[match(geography, sectors$geography, nomatch = NA)]]

# fixing the international import index
importindex <- tradefinal[geography == "Canada", .(importindex, code, year)]
# for interprovincial imports
intprov_importindex <- deflator_file[sectorcode == "CANADA", .(mnem, deflator, year)]
setnames(intprov_importindex, "deflator", "intprovdefl")

# re merging
tradefinal2 <- merge(tradefinal[, !"importindex", with = F], importindex, by = c("code", "year"))
tradefinal2 <- merge(tradefinal2, intprov_importindex, by = c("mnem", "year"))
tradefinal2 <- merge(tradefinal2, deflator_file, by = c("mnem", "year", "sectorcode"))

# calculate
real <- paste0(c("internatimports", "interprovimports", "interprovexports", "internatexports"), "_real")

tradefinal2[, (real) := list(
  internatimports / (importindex / 100),
  interprovimports / (intprovdefl / 100),
  interprovexports / (deflator / 100),
  internatexports / (deflator / 100)
)]

tradefinal2[, sectorcode := NULL]

######################################################
# Step 9:                                         #####
# Prepare for export                             #####
######################################################


fulldb <- tradefinal2[geography != "Canada", importindex := NA]



vars <- names(fulldb)[names(fulldb) %like% "ports"]
squashdelete <- function(new, old1, old2) {
  # this table calculates sums
  test <- forsums[code == old1 | code == old2, lapply(.SD, sum), by = .(geography, year), .SDcols = (vars)]
  test[, code := new]

  # merge into working db
  merged <- merge(forsums, test, by = c("geography", "code", "year"), all = TRUE)

  # replace
  for (z in 6:(length(keep))) {
    origname <- names(merged)[z]
    newname <- names(merged)[z + length(vars)]
    merged[, eval(origname) := ifelse(is.na(get(origname)), get(newname), get(origname))]
  }
  # fix names for final variables
  names(merged) <- gsub("\\.x", "", names(merged))
  # get rid of temporary columns
  x <- merged[, names(merged)[grep("\\.y", names(merged))] := NULL]
  # save to global
  forsums <<- merged
}

forsums <- fulldb
infocols <- c("geography", "code", "year", "mnem", "industry")
setcolorder(forsums, c(infocols, setdiff(names(forsums), infocols)))

squashdelete("31A", "313", "314")
squashdelete("31B", "315", "316")
squashdelete("49A", "491", "492")
squashdelete("48Z", "485", "487")

forsums <- forsums[!(code %in% c("313", "314", "315", "316", "491", "492", "485", "487"))]
fulldbmelted <- melt(forsums, id.vars = c("geography", "code", "industry", "year", "mnem"))
# Mnemonics
matchbase <- merge[!is.na(VarNeumonic)]

fulldbmelted[, `:=`
(
  variable = str_trim(variable, side = "both"),
  geography = str_trim(geography, side = "both")
)             ]
fulldbmelted[, `:=`
(
  varsymbol = matchbase$VarNeumonic[match(fulldbmelted$variable, matchbase$newvar, nomatch = NA)],
  pricesymbol = matchbase$PriceSymbol[match(fulldbmelted$variable, matchbase$newvar, nomatch = NA)],
  sectorcode = sectors$code[match(fulldbmelted$geography, sectors$geography, nomatch = NA)],
  sectormnems = mnems$mnem[match(fulldbmelted$code, mnems$code, nomatch = NA)]
)             ]
fulldbmelted <- fulldbmelted[!is.na(varsymbol) & !is.na(sectormnems)]
# additional fixes to trade mnemonics
fulldbmelted[geography == "Canada" & varsymbol == "MX", varsymbol := "M"]
fulldbmelted[geography == "Canada" & varsymbol == "XX", varsymbol := "X" ]

fulldbmelted[is.na(varsymbol), varsymbol := ""]
fulldbmelted[is.na(pricesymbol), pricesymbol := ""]
fulldbmelted[is.na(sectorcode), sectorcode := ""]
fulldbmelted[is.na(sectormnems), sectormnems := ""]

fulldbmelted[, `:=`
(mnemonic = paste(varsymbol, sectormnems, pricesymbol, sep = ""))             ][, c("pricesymbol", "varsymbol", "sectormnems") := NULL]

fulldbmelted <- unique(fulldbmelted)
# reshape again
widedb <- dcast(fulldbmelted, geography + code + sectorcode + variable + mnemonic + mnem ~ year)
widedb[, mnem := NULL]
# get rid of the uneeded intersection
forexport <- widedb
forexport <- forexport[ !(code == "NAT") ]

# remove NA series
nyear <- as.numeric(max(fulldbmelted$year)) - as.numeric(min(fulldbmelted$year))
yearcols <- names(forexport)[6:(6 + nyear)]

############################# This command suppresses warnigns associated with the conversion to numeric
#        ###ATTENTION###    # The conversion is useful to A) perform further manipulations (if necessary)
#############################                             B) count the NAs and identify empty series
############################# Be sure that you do not want to keep any cells that are all characters

# create numerics (and all cells that are strings will be NAs)
suppressWarnings(forexport[, (yearcols) := lapply(.SD, function(y) as.numeric(gsub("[^0-9.]", "", y))), .SD = yearcols])
# temporary column that adds the NA values
forexport[, test := Reduce(`+`, lapply(.SD, function(x) is.na(x)))]
# exclude any row with the number of NA's equals the number of years
forexport <- forexport[test <= nyear][, test := NULL]

# Now replace early NA's with .001 for model readable format
# first non-NA
forexport[, test := apply(.SD, 1, function(x) min(which(!is.na(x)))), .SD = (yearcols)]
# replace
for (i in seq_along(forexport[, (yearcols), with = F])) {
  set(forexport, which(forexport$test > i), as.integer(i + 5), value = .001)
}
# find gaps
# first non-NA
forexport[, test := apply(.SD, 1, function(x) max(which(!is.na(x)))), .SD = (yearcols)]
# replace
for (i in seq_along(forexport[, (yearcols), with = F])) {
  # print(i)
  # print(as.character(intersect(which(forexport$test>i) , which(is.na(forexport[[i+6]])))))
  set(forexport, intersect(which(forexport$test > i), which(is.na(forexport[[i + 6]]))), as.integer(i + 6), value = -100)
}
# first non-NA
suppressWarnings(forexport[, test := apply(.SD, 1, function(x) min(which(x == -100))), .SD = (yearcols)])
forexport[is.infinite(test), test := 0]
# replace
for (i in seq_along(forexport[, (yearcols), with = F])) {
  set(forexport, which(forexport$test > i), as.integer(i + 6), value = .001)
}
forexport[forexport == -100] <- .001

fyear <- yearcols[[1]]
# make model readable
forexport[, test := Reduce(`+`, lapply(.SD, function(x) is.na(x)))]
forexport <- forexport[, !c("geography", "code", "variable"), with = F]
forexport[, `:=`
(
  V = "V",
  L = "L",
  pers = nyear - test + 1
)][, `:=`
(
  start = paste0(fyear, "01"),
  end = paste(as.numeric(fyear) + pers - 1, "01", sep = "")
)][, `:=`
(
  header = "1.upload",
  pers = paste(pers, "@01", sep = ""),
  forecastend = end
)][, test := NULL]



setcolorder(forexport, c("header", "sectorcode", "mnemonic", "V", "L", "start", "end", "forecastend", "pers", setdiff(names(forexport), c("header", "sectorcode", "mnemonic", "V", "L", "start", "end", "forecastend", "pers"))))
forexport[is.na(forexport)] <- ""
yearcols <- names(forexport)[10:(10 + nyear)]

# export
exportname <- "trade"
rdsexport <- paste(exportname, ".rds", sep = "")
destname <- paste(dstDir, rdsexport, sep = "")
saveRDS(forexport, eval(destname))

csvexport <- paste(exportname, ".csv", sep = "")
destname <- paste(dstDir, csvexport, sep = "")
write.table(forexport, file = eval(destname), row.names = FALSE, col.names = FALSE, sep = ",")
