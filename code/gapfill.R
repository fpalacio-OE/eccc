library(rJava)
library(readxl)
library(xlsx)
library(XML)
library(quantmod)
library(stringr)
# library(rlang)

library(ggplot2)
library(data.table)
library("pbapply")

##

# srcDir     = "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/resources/"
# dstDir     = "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/outputs/"
# exportname = "envcandb-readable.csv"
# sourcebase = "envcandb-new.rds"


urlfile <- "DataWarehouse-4.xlsm"
codedir <- "Directory-namestonaicscodes-2.xlsx" # <-------------- # This file contains (on the left are sheet names):
mainlist <- "Main" # 1. The official list of delivered sectors
xtendeddir <- "Expanded" # 2. The list of all the pulled sectors, including those that are used to calculate one of the official sectors
mapdir <- "Mapping" # 3. The mapping of the 2 to 1
smerge <- "SeriesMerge" # 4. The mapping of the various series that make each variable


# mapping file (useful at times)
naicsdir <- paste(srcDir, eval(codedir), sep = "")
naics    <- as.data.table(read_excel(naicsdir, na = "NA", sheet = eval(mainlist), col_names = TRUE))
nind     <- nrow(naics) # of industries

# upload mapping doc & extended sectors list
mapping <- as.data.table(read_excel(naicsdir, na = "NA", sheet = eval(mapdir), col_names = TRUE, col_types = c("guess", "guess", "guess", "guess", "text", "guess")))
fullnaicslist <- as.data.table(read_excel(naicsdir, na = "NA", sheet = eval(xtendeddir), col_names = TRUE))

# upload series merge map
merge <- as.data.table(read_excel(naicsdir, na = "NA", sheet = eval(smerge), col_names = TRUE))
mnems <- as.data.table(read_excel(naicsdir, na = "NA", sheet = "FinalMnem", col_names = TRUE))
sectors <- as.data.table(read_excel(naicsdir, na = "NA", sheet = "sectorcode", col_names = TRUE))


# this doc maps the shift shares
shiftshares   <- as.data.table(read_excel(naicsdir, na = "NA", sheet = "Shiftshares", col_names = TRUE))
umbrellasects <- unique(shiftshares$umbrella)[!is.na(unique(shiftshares$umbrella)) & unique(shiftshares$umbrella) != "NAT"]


sectorals <- readRDS(paste(srcDir, "sectorals.rds", sep = ""))

# key functions
source(paste0(codeDir,"functions/imposegrowthrates.r"))
source(paste0(codeDir,"functions/verticaltieupfunction.r"))
source(paste0(codeDir,"functions/simpletieupfunction.r"))

options(scipen = 999)
# Load up bases
# RDSpath <-paste(dstDir, "envcandb-new.rds",sep="")
envcandb <- readRDS(paste(srcDir, "envcandb-new.rds", sep = ""))

# fixing an API issue temporarily
# envcandb <- envcandb[!(code == "327A" & var %like% "3810031")]
# mpetao <- as.data.table(read.csv(paste0(srcDir, "mpetao-temp.csv")))
# mpetao <- melt(mpetao, id.vars = c("geography", "var", "code"), variable.name = "year")
# mpetao[, `:=`(geography = as.character(geography), code = as.character(code), year = gsub("X", "", year))]

# envcandb <- rbind(envcandb, mpetao)
# envcandb<-envcan[[1]]
envcandb <- dcast(envcandb, geography + code + year ~ var)


# working db with shorter time
envcandbshort <- envcandb[year >= 1980]
rm(envcandb)

#------------------------------------------------------------------------------------#
# Merge all the series that correspond to the relevant variables into one series
#------------------------------------------------------------------------------------#
# variables of concern: these must match exactly from the merge file
finalvariables <- c(
  "employment", "gross output (x 1,000,000)", "price index", "capital stock - chained prices (x 1,000,000)",
  "investment, mach - chained prices (x 1,000,000)", "investment, constr - current prices (x 1,000,000)",
  "depreciation, mach - chained prices (x 1,000,000)", "gva - current prices (x 1,000,000)", "investment - current prices (x 1,000,000)",
  "investment - chained prices (x 1,000,000)", "capital depreciation - chained prices (x 1,000,000)",
  "investment, ip - current prices (x 1,000,000)", "investment, constr - chained prices (x 1,000,000)",
  "depreciation, constr - chained prices (x 1,000,000)", "stock, mach - chained prices (x 1,000,000)",
  "stock, constr - chained prices (x 1,000,000)", "gva - chained prices (x 1,000,000)", "investment, mach - current prices (x 1,000,000)",
  "investment, ip - chained prices (x 1,000,000)", "hours worked", "depreciation, ip - chained prices (x 1,000,000)",
  "gross output - constant prices (x 1,000,000)", "stock, ip - chained prices (x 1,000,000)", "wages"
)

print(c("Variables to create: ", finalvariables))

for (i in 1:length(finalvariables)) {
  print(finalvariables[i])
  # these is a predefined mapping of series to variables
  submap <- merge[newvar == eval(finalvariables[i])]
  series <- submap$series
  # print(series)
  # now we subset the data that we care about
  toretrieve <- c("geography", "code", "year", eval(series))
  subgroup <- as.data.table(envcandbshort[, mget(toretrieve)])

  broken <- split(subgroup, by = c("code", "geography"))
  broken <- lapply(broken, function(broken) {
    # print(broken[1,])
    for (j in 4:ncol(broken)) {
      # print(j)
      indseries <- broken[[j]]
      if (all(is.na(indseries)) | all(!is.na(indseries))) {
        next
      }
      streaks <- (rle(!is.na(indseries))$lengths)
      # selects the longest consecutive set
      usefulstreak <- max(rle(!is.na(indseries))$lengths[rle(!is.na(indseries))$values])
      # where the longset set starts
      usefulstreakindex <- which(streaks == usefulstreak)[which(streaks == usefulstreak) %in% which(rle(!is.na(indseries))$values)][1]
      firstna <- rep(NA, sum(streaks[1:(usefulstreakindex - 1)]))
      lastna <- rep(NA, sum(streaks[(usefulstreakindex + 1):length(streaks)], na.rm = T))
      indseries <- if (usefulstreakindex == 1) {
        c(indseries[1:(length(indseries) - length(lastna))], lastna)
      } else if (usefulstreakindex == length(streaks)) {
        c(firstna, indseries[(length(firstna) + 1):length(indseries)])
      } else {
        c(firstna, indseries[(length(firstna) + 1):(length(indseries) - length(lastna))], lastna)
      }
      broken[[j]] <- indseries
    }
    broken
  })
  subgroup <- rbindlist(broken)
  # to tag overlaps
  overlaptag <- paste(eval(finalvariables[i]), "overlap", sep = "-")
  subgroup[, eval(overlaptag) := 0]
  subgroup[rowSums(is.na(subgroup)) < ncol(subgroup) - 5, eval(overlaptag) := 1]

  # the first series in the subgroup is treated as the primary
  subgroup[, eval(finalvariables[i]) := get(series[1])]
  subgroup[get(finalvariables[i]) == "x" | get(finalvariables[i]) == "..", eval(finalvariables[i]) := NA] # however, we'll still omit any "x" or ".."

  # basically fills in going down the list
  if (length(series) > 1) {
    for (c in 2:length(series)) {
      tempcol <- subgroup[is.na(get(finalvariables[i])), get(series[c])]
      suppressWarnings(subgroup[is.na(get(finalvariables[i])), eval(finalvariables[i]) := tempcol])
      subgroup[get(finalvariables[i]) == "x" | get(finalvariables[i]) == "..", eval(finalvariables[i]) := NA] # these two lines ensure that we get real numeric values
    }
  }
  subgroup[, eval(finalvariables[i]) := as.numeric(gsub("[^0-9.]", "", (get(finalvariables[i]))))] #

  # adds the new variable to the shortened database
  keep <- c("geography", "code", "year", eval(overlaptag), eval(finalvariables[i]))
  mergecol <- subgroup[, ..keep]
  envcandbshort <- merge(envcandbshort, mergecol, by = c("geography", "year", "code"))

  # in case I want to inspect just this variable's components
  subtablename <- paste(eval(finalvariables[i]), "subgroup", sep = "-")
  assign(subtablename, subgroup)
  print(i)
}
# View(envcandbshort)

setkey(envcandbshort, geography)
listpath <- paste(srcDir, "shortdb.rds",sep = "")
saveRDS(envcandbshort, eval(listpath))


# envcandbshort<-readRDS(eval(listpath))

#------------------------------------------------------------------------------------#                                                                             #####
#                              Begin Gap Filling                                 #####
#------------------------------------------------------------------------------------#

#------------------------------------------------------------------------------------# 
# Step 1: Impose real growth rates (where available)                             #####
#------------------------------------------------------------------------------------#
print("Imposing true growth rates, where possible")
# this list contains everything besides macro variables
omitmacro <- c("geography", "code", "year", finalvariables)

# working db for this chunk
forGVAallocation <- envcandbshort[, ..omitmacro]

# complete variable list
step1vars <- omitmacro[!(omitmacro %in% c("geography", "code", "year"))]

#
# The "real" growthrates used in this chunk are those from the various series that were merged together,
# so here we bring those back from the "subgroups" created earlier
#
# get subgroups
suppressWarnings(rm(list = ls(pattern = "^subgroup")))
subgroups <- sort(ls()[ls() %like% "subgroup"])
subgrouplist <- mget(copy(subgroups))
desiredorder <- gsub("-subgroup", "", subgroups)
# delete the last columns (we'll merge those in)
suppressWarnings(
  subgrouplist2 <- lapply(subgrouplist, function(x) x[, names(x)[names(x) %in% step1vars] := NULL])
)
subgrouplist2 <- lapply(subgrouplist2, function(x) x[order(geography, code)])

# prepare full variable list

suppressWarnings(forGVAallocation[, (step1vars) := lapply(.SD, function(y) as.numeric(gsub("[^0-9.]", "", y))), .SD = step1vars])

# we're aiming for one subtable per variable/geography/code
dbforsplit <- melt(forGVAallocation, id.vars = c("geography", "code", "year"))
dbforsplit <- dbforsplit[, variable := as.character(variable)]
dbforsplit$variable <- factor(as.character(dbforsplit$variable), levels = desiredorder)
dbforsplit <- dbforsplit[order(variable)]
dbformerge <- split(dbforsplit, by = "variable")

dbpartialmerge <- mapply(function(x, y) merge(x, y, by = c("geography", "code", "year")), x = dbformerge, y = subgrouplist2, SIMPLIFY = FALSE)
dbmerge <- rbindlist(dbpartialmerge, fill = TRUE)
rm(list = c("dbformerge", "subgrouplist", "subgrouplist2", "dbforsplit"))

### calculate growth rates for existing series###

# columns to work with
# foroperation<-names(dbmerge)[!(names(dbmerge) %in% c("geography", "code", "year", "variable")) ]
# foroperation<-foroperation[!(foroperation %like% "overlap")]
# #switch to numerics
# suppressWarnings(dbmerge[,(foroperation):=lapply(.SD, function(y) as.numeric(gsub("[^0-9.]", "", y))),.SD=(foroperation)])
# newnames<-paste(foroperation,"growth",sep="-")
# #calculate
# dbmerge[, (newnames):= lapply(foroperation, function(x) {c(NA,exp(diff(log(get(x)))))}), by=list(geography, code)]
# dbmerge<-split(dbmerge, by="variable")
# this takes each subtable and adds the growth columns
finalprep <- function(x) {
  foroperation <- names(x)[!grepl("geography|code|year|variable|overlap", names(x))]
  newnames <- paste(foroperation, "growth", sep = "-")
  x[, (foroperation) := lapply(.SD, function(y) as.numeric(gsub("[^0-9.]", "", y))), .SD = (foroperation)]
  x[, (newnames) := lapply(foroperation, function(x) {
    c(NA, exp(diff(log(get(x)))))
  }), by = list(geography, code)]
}
y <- lapply(dbpartialmerge, finalprep)

# split again into a list of lists #var list elements (one for each varaible) and each has sectors*geographies sublist elements
dbsplit3 <- lapply(y, function(x) split(x, by = c("geography", "code")))

# apply function
fixedseriesformerge <- pblapply(dbsplit3, lapply, imposegrowthrates)

# recompose the list and reshape
fixedseries <- rbindlist(lapply(fixedseriesformerge, rbindlist))
saveRDS(fixedseries, paste(srcDir, "fixedseries.rds", sep = ""))


#------------------------------------------------------------------------------------#
# Step 2: Check addition among umbrella groups across provinces, focusing on     #####
#         gva, nominal 2007-2014                                                 #####
#------------------------------------------------------------------------------------#
# Fill in all sectors/years
# ngeo <- length(unique(fixedseries$geography))
# year <- rep(kronecker(1980:2017, rep(1, nind)), ngeo)
# ny <- length(1980:2017)
# code <- rep(naics$code, ny)
# geography <- rep(unique(fixedseries$geography), each = ny * nind)
# 
# leftcol <- as.data.table(cbind(geography, code, year))
# fixedseries <- dcast(fixedseries, geography + code + year ~ variable)
# fixedseries <- merge(leftcol, fixedseries, by = c("code", "year", "geography"), all.x = T)
# fixedseries <- melt(fixedseries, id.vars = c("code", "year", "geography"))
#------------------------------------------------------------------------------------#
#     2a: Start with nominals                                                    #####
#------------------------------------------------------------------------------------#
# # gvatieup2<- melt(highlevelgva, id.vars = c("variable", "code", "year"))
# gvatieup2 <- fixedseries
# # names(gvatieup2)[names(gvatieup2)=="variable.1"]<-"geography"
# gvatieup2 <- gvatieup2[variable == "gva - current prices (x 1,000,000)"]
# 
# gvatieup2[, umbrella := shiftshares$umbrella[match(gvatieup2$code, shiftshares$code, nomatch = NA)]]
# 
# 
# # mark the cells that are in the 379-0030 (1 means it's  in 379-0030 and therefore fixed)
# `gva - current prices (x 1,000,000)-subgroup`[geography != "Canada", tag := ifelse(is.na(`gross domestic product (gdp) at basic prices-3790030-current prices`), 0, 1)]
# `gva - current prices (x 1,000,000)-subgroup`[geography == "Canada", tag := ifelse(is.na(`gross domestic product (gdp) at basic prices-3790029-current prices`), 0, 1)]
# 
# taglistcurrent <- `gva - current prices (x 1,000,000)-subgroup`[, c("geography", "code", "year", "tag"), with = F]
# setkey(taglistcurrent, code, geography, year)
# setkey(gvatieup2, code, geography, year)
# 
# gvatieup2 <- taglistcurrent[gvatieup2]
# 
# # get list of sectors
# umbrellasects <- unique(shiftshares$umbrella)[!is.na(unique(shiftshares$umbrella)) & unique(shiftshares$umbrella) != "NAT"]
# # begin loop on vertical fixes
# 
# tieupfunction(gvatieup2)
# 
# gvatieup2[, c("umbrella", "tag", "sectorsum") := NULL]
# 
# 
# #------------------------------------------------------------------------------------#
#     2b: quick check on the chained, focusing only on 2007. Anything that gets  #####
#         changed will get regrown (back and forward using the original growth   #####
#         rates                                                                  #####
# #------------------------------------------------------------------------------------#
# gvatieup2chained <- fixedseries[variable == "gva - chained prices (x 1,000,000)"]
# 
# gvatieup2chained[, umbrella := shiftshares$umbrella[match(gvatieup2chained$code, shiftshares$code, nomatch = NA)]]
# 
# # mark the cells that are in the 379-0030 (1 means it's  in 379-0030 and therefore fixed)
# `gva - chained prices (x 1,000,000)-subgroup`[geography != "Canada", tag := ifelse(is.na(`gross domestic product (gdp) at basic prices-3790030-chained (2012) dollars`), 0, 1)]
# `gva - chained prices (x 1,000,000)-subgroup`[geography == "Canada", tag := ifelse(is.na(`gross domestic product (gdp) at basic prices-3790031-chained (2012) dollars`), 0, 1)]
# 
# 
# taglistchained <- `gva - chained prices (x 1,000,000)-subgroup`[, c("geography", "code", "year", "tag"), with = F]
# setkey(taglistchained, code, geography, year)
# setkey(gvatieup2chained, code, geography, year)
# 
# gvatieup2chainedcomplete <- taglistchained[gvatieup2chained]
# 
# gvatieup2chained <- gvatieup2chainedcomplete[year == 2007]
# # begin loop on vertical fixes
# 
# for (i in umbrellasects) {
#   print(i)
#   # choose relevant sectors
#   sect <- shiftshares[umbrella %in% i, code]
#   # calculate sums
#   suppressWarnings(gvatieup2chained[ code %in% sect, sectorsum := sum(value), by = list(geography, year)])
# 
#   # now we move the sums to only the umbrella value using a merge
#   namesformerge <- names(gvatieup2chained)[!(names(gvatieup2chained) %in% c("value", "umbrella", "try", "tag"))]
#   y <- gvatieup2chained[code %in% sect[1], ..namesformerge][, code := as.character(i)]
#   names(y)[names(y) == "sectorsum"] <- "sums"
#   setkey(y, code, variable, geography, year)
#   setkey(gvatieup2chained, code, variable, geography, year)
#   gvatieup2chained <- y[gvatieup2chained]
# 
# 
#   # calculate error
#   gvatieup2chained[, vertdiff := (value - sums)][, sectorsum := NULL]
#   # create and paste adjustment
#   gvatieup2chained[code %in% sect, numfree := length(sect) - sum(tag), by = list(geography, year)]
#   gvatieup2chained[(code %in% sect | code %in% i) & numfree != 0, adjustment := vertdiff[code == i] / numfree, by = list(geography)]
#   gvatieup2chained[(code %in% sect | code %in% i) & numfree == 0, adjustment := as.numeric(NA), by = list(geography)]
#   gvatieup2chained[tag == 1 | code %in% i, adjustment := NA]
# 
#   # apply scale factor
#   gvatieup2chained[, value := ifelse(is.na(adjustment), value, value + adjustment)]
#   # clean
#   gvatieup2chained[, c("sums", "vertdiff", "numfree", "adjustment") := NULL]
# }
# 
# gvatieup2chained[, c("umbrella", "tag", "variable") := NULL]
# names(gvatieup2chained)[names(gvatieup2chained) == "value"] <- "adjustedvals"
# # now grow back adjusted series accordingly
# setkey(gvatieup2chainedcomplete, code, geography, year)
# setkey(gvatieup2chained, code, geography, year)
# # calculate original growth rates
# forgrowback <- gvatieup2chained[gvatieup2chainedcomplete]
# forgrowback[, equality := ifelse(adjustedvals == value, 1, 0)]
# forgrowback[, origgrowth := c(NA, exp(diff(log(value)))), by = list(geography, code)]
# 
# # grow
# forgrowback[, try := adjustedvals]
# for (y in 2008:max(forgrowback$year)) {
#   forgrowback[year == y, `:=`
#   (try = ifelse(is.nan(origgrowth) | is.infinite(origgrowth) | is.na(origgrowth), value,
#       ifelse(is.na(forgrowback[year == as.character(as.numeric(y) - 1), try]), value, forgrowback[year == as.character(as.numeric(y) - 1), try] * origgrowth)
#   ))]
# }
# 
# 
# # backwards from the first year in the primary series
# for (y in 2006:min(forgrowback$year)) {
#   forgrowback[year == y, `:=`
#   (try = ifelse(is.nan(forgrowback[year == as.character(as.numeric(y) + 1), origgrowth]) | is.infinite(forgrowback[year == as.character(as.numeric(y) + 1), origgrowth]) | is.na(forgrowback[year == as.character(as.numeric(y) + 1), origgrowth]), value,
#       ifelse(is.na(forgrowback[year == as.character(as.numeric(y) + 1), try]), value, forgrowback[year == as.character(as.numeric(y) + 1), try] / forgrowback[year == as.character(as.numeric(y) + 1), origgrowth])
#   ))]
# }
# # prepare for merge with current
# tokeep <- c("geography", "code", "year", "try", "variable")
# gvatieup2chained <- forgrowback[, ..tokeep]
# names(gvatieup2chained)[names(gvatieup2chained) == "try"] <- "value"
# 
# # remerge and recast
# gapfilldb <- rbind(gvatieup2chained, gvatieup2)
# gapfilldb <- dcast(gapfilldb, code + year + geography ~ variable)

#not sure what the previous two blocks were for:
gapfilldb <- fixedseries[variable %like% "gva"]

saveRDS(gapfilldb, paste(srcDir, "gapfilldb.rds", sep = ""))
#------------------------------------------------------------------------------------#
# Step 3: Fills GVA gaps using one level up                                      #####
#------------------------------------------------------------------------------------#
print("Working on GVA series")
# working dt for this chunk
forgvaextension <- gapfilldb[code != "NAT"]

# two columns at once
var <- c("gva - chained prices (x 1,000,000)", "gva - current prices (x 1,000,000)")

# input the umbrellas (the rest is in the loop)
forgvaextension[, `:=`
(
  umbrella = shiftshares$umbrella[match(forgvaextension$code, shiftshares$code, nomatch = NA)],
  level = shiftshares$level[match(code, shiftshares$code, nomatch = NA)]
)                ]
umbrellavalues <- c("umbrellavalreal", "umbrellavalnom")
# placeholder columns and names
newcols <- c("tryreal", "trynom")
newcolslag <- c("tryreallag", "trynomlag")
forgvaextension[, (newcols) := as.numeric(NA)]
umbrellagrowth <- c("umbrellagroreal", "umbrellagronom")



#   define the growforward  and growback functions

extendgva <- function(x, i) {
  DT <- x
  for (y in (as.numeric(min(DT$year)) + 1):max(DT$year)) {
    # print(y)
    DT[year == y & level == i, `:=`
    (
      tryreal = ifelse(DT[year == y - 1, tryreal] %in% 0, DT[year == y, get(var[1])],
        ifelse(is.na(DT[year == y, get(var[1])]), DT[year == y - 1, tryreal] * (DT[year == y, get(umbrellagrowth[1])]), DT[year == y, get(var[1])])
      ),
      trynom = ifelse(DT[year == y - 1, trynom] %in% 0, DT[year == y, get(var[2])],
        ifelse(is.na(DT[year == y, get(var[2])]), DT[year == y - 1, trynom] * (DT[year == y, get(umbrellagrowth[2])]), DT[year == y, get(var[2])])
      )
    )       ]
  }

  DT[level == i, (var) := lapply(newcols, function(y) {
    get(y)
  })] # [,c(umbrellagrowth,umbrellavalues):=NULL]
}

backextendgva <- function(x, i) {
  DT <- x
  for (y in (as.numeric(max(DT$year)) - 1):min(DT$year)) {
    # print(y)
    DT[year == y & level == i, `:=`
    (
      tryreal = ifelse(DT[year == y + 1, tryreal] %in% 0, DT[year == y, get(var[1])],
        ifelse(is.na(DT[year == y, get(var[1])]), DT[year == y + 1, tryreal] / (DT[year == y + 1, get(umbrellagrowth[1])]), DT[year == y, get(var[1])])
      ),
      trynom = ifelse(DT[year == y + 1, trynom] %in% 0, DT[year == y, get(var[2])],
        ifelse(is.na(DT[year == y, get(var[2])]), DT[year == y + 1, trynom] / (DT[year == y + 1, get(umbrellagrowth[2])]), DT[year == y, get(var[2])])
      )
    )]
  }

  DT[level == i, (var) := lapply(newcols, function(y) {
    get(y)
  })][, c(umbrellagrowth, umbrellavalues) := NULL]
}
##


# loop this so that higher digit sectors benefit from newly filled values. This is slow.
pb = txtProgressBar(min = 1, max = 5, initial = 0, style = 3 )
for (i in 1:5) {
  # temporary column with the values of the umbrella
  forgvaextension[, (umbrellavalues) := lapply(var, function(var) {
    get(var)[match(umbrella, code, nomatch = NA)]
  }),
  by = list(year, geography)
  ]

  # zero out vals if umbrella is zero

  forgvaextension[umbrellavalreal == 0, `gva - chained prices (x 1,000,000)` := 0]
  forgvaextension[umbrellavalnom == 0, `gva - current prices (x 1,000,000)` := 0]

  # get growth rates nearest level up
  suppressWarnings(forgvaextension[, (umbrellagrowth) := lapply(umbrellavalues, function(umbrellavalues) {
    c(NA, exp(diff(log(get(umbrellavalues)))))
  }), by = code])

  forgvaextension[is.nan(umbrellagroreal), umbrellagroreal := 0]
  forgvaextension[is.nan(umbrellagronom), umbrellagronom := 0]
  # lagged newcols
  # forgvaextension[,

  # split (for parallel work)
  listforextend <- split(forgvaextension, by = c("geography", "code"))
  # grow forward
  forwardextended <- lapply(listforextend, extendgva, i)
  # reach backward
  listformerge <- lapply(forwardextended, backextendgva, i)
  # re-merge
  forgvaextension <- rbindlist(listformerge)
  
  setTxtProgressBar(pb,i)
}

gvaextended <- forgvaextension[, c(newcols, "level") := NULL]

saveRDS(gvaextended, paste(srcDir, "step1.rds", sep = ""))

#------------------------------------------------------------------------------------#
# Step 4: Re-do the scaling on the extended gva series                           #####
#         Rescale vertically (newly added nominal observations only)             #####
#------------------------------------------------------------------------------------#



# gva before extension
beforextension <- gapfilldb
names(gapfilldb)[names(gapfilldb) == "gva - chained prices (x 1,000,000)"] <- "originalchained"
names(gapfilldb)[names(gapfilldb) == "gva - current prices (x 1,000,000)"] <- "originalnom"

setkey(gvaextended, geography, code, year)
setkey(gapfilldb, geography, code, year)


forgrowback <- gapfilldb[gvaextended]
# forgrowback<- melt(forgrowback, id.vars =c("geography", "code", "year","umbrella"))

# 1 is fixed - only playing with newly filled vars
forgrowback[, tagreal := ifelse(is.na(originalchained) & !is.na(`gva - chained prices (x 1,000,000)`), 0, 1)]
forgrowback[, tagnom := ifelse(is.na(originalnom) & !is.na(`gva - current prices (x 1,000,000)`), 0, 1)]
forgrowback[, c("originalchained", "originalnom") := NULL]
# reshape for tie up function
forgrowbackcomplete <- melt(forgrowback, id.vars = c("code", "year", "geography", "umbrella", "tagreal", "tagnom"))
forgrowbackcomplete[, tag := ifelse(variable == "gva - chained prices (x 1,000,000)", tagreal, tagnom)][, c("tagreal", "tagnom") := NULL]
forgrowbacknominal <- forgrowbackcomplete[variable == "gva - current prices (x 1,000,000)"]

# apply function
tieupfunction(forgrowbacknominal)
# clean and merge
forgrowbacknominal[, sectorsum := NULL]
gvatiedvertical <- rbind(forgrowbackcomplete[variable == "gva - chained prices (x 1,000,000)"], forgrowbacknominal)

#------------------------------------------------------------------------------------#
# Step 5: Now Rescale horizontally                                               #####
#------------------------------------------------------------------------------------#


gvatieup <- gvatiedvertical[, c("tag", "umbrella") := NULL]

# gvatieup<-melt(gvatieup, id.vars = c("code", "year", "geography"))
gvatieup <- dcast(gvatieup, code + year + variable ~ geography)
gvatieup <- gvatieup[order(variable, code, year)]


# move Canada to the end
setcolorder(gvatieup, c(setdiff(names(gvatieup), "Canada"), "Canada"))

# calculate row sums
notcanada <- names(gvatieup)[!(names(gvatieup) %in% c("Canada", "year", "code", "variable"))]
gvatieup[, provtotal := rowSums(.SD), .SDcols = (notcanada)]
gvatieup[, scale := (Canada - provtotal) / provtotal]

#
# gvatieup2[,try:=as.numerica(NA)]
# for(i in umbrellasects){
#
#   print(i)
#   #choose relevant sectors
#   sect     <-shiftshares[umbrella %in% i, code]
#   #calculate sums
#   suppressWarnings(gvatieup2[ code %in% sect, sectorsum:=sum(value), by=list(geography,variable, year)])
#
#   #now we move the sums to only the umbrella value using a merge
#   namesformerge<-names(gvatieup2)[!(names(gvatieup2) %in% c("value", "umbrella", "try"))]
#   y<- gvatieup2[code %in% sect[1], ..namesformerge][,code:=as.character(i)]
#   names(y)[names(y)=="sectorsum"]<-"sums"
#   setkey(y,         code, variable, geography,year )
#   setkey(gvatieup2, code, variable, geography,year)
#   gvatieup2<-y[gvatieup2][,sectorsums:=NULL]
#   gvatieup2[,try:=ifelse(is.na(sums), try, sums)]
# }

#------------------------------------------------------------------------------------#
# Step 6: Fill remaining GVA gaps using Natl level                               #####
#------------------------------------------------------------------------------------#
# working db for step2
remaininggvagaps <- dcast(gvatiedvertical, geography + code + year ~ variable)
# prepare the natinoal gva table
nationalgva <- remaininggvagaps[geography == "Canada"][, umbrella := NULL]

# apply proportions from current to real (onlyu where absolutely necessary)

nationalgva[, `:=`
(
  level = shiftshares$level[match(code, shiftshares$code, nomatch = NA)],
  umbrella = shiftshares$umbrella[match(code, shiftshares$code, nomatch = NA)]
)][,
  `:=`
  (
    umbrellavalnom = `gva - current prices (x 1,000,000)`[match(umbrella, code, nomatch = NA)],
    umbrellavalreal = `gva - chained prices (x 1,000,000)`[match(umbrella, code, nomatch = NA)]
  ),
  by = year
][
  ,
  `:=`
  (
    natlsharenom = ifelse(is.na(`gva - current prices (x 1,000,000)` / umbrellavalnom), NA, `gva - current prices (x 1,000,000)` / umbrellavalnom),
    natlsharereal = ifelse(is.na(`gva - chained prices (x 1,000,000)` / umbrellavalreal), NA, `gva - chained prices (x 1,000,000)` / umbrellavalreal)
  )
][,
  marker := all(is.na(natlsharereal)),
  by = code
][,
  natlsharereal := ifelse(marker == TRUE, natlsharenom, natlsharereal),
  by = code
][
  ,
  `gva - chained prices (x 1,000,000)` := ifelse(is.na(`gva - chained prices (x 1,000,000)`), natlsharenom * umbrellavalreal, `gva - chained prices (x 1,000,000)`)
][
  ,
  c("natlsharenom", "natlsharereal", "umbrellavalnom", "umbrellavalreal", "marker", "level", "umbrella") := NULL
]


colnames(nationalgva)[4:5] <- c("natlvalreal", "natlvalnom")
# merge into the extended gva table
setkey(remaininggvagaps, year, geography, code)
remaininggvagaps[, umbrella := shiftshares$umbrella[match(remaininggvagaps$code, shiftshares$code, nomatch = NA)]]

# first merge gives the national value
remaininggvagaps <- merge(remaininggvagaps, nationalgva[, !"geography", with = FALSE], all.x = TRUE, by = c("year", "code"))

# now we rename the columns in the national table and remerge, this gives the umbrella values at national level
colnames(nationalgva)[colnames(nationalgva) %in% c("code", "natlvalnom", "natlvalreal")] <- c("umbrella", "natlumbvalnom", "natlumbvalreal")
remaininggvagaps <- nationalgva[, !"geography", with = FALSE][remaininggvagaps, on = c("year", "umbrella")]

# perform calculation
remaininggvagaps[, `:=`
(
  natlsharenom = natlvalnom / natlumbvalnom,
  natlsharereal = natlvalreal / natlumbvalreal,
  level = shiftshares$level[match(code, shiftshares$code, nomatch = NA)],
  umbrellavalnom = as.numeric(NA),
  umbrellavalreal = as.numeric(NA)
)                ]



# for inspection
setcolorder(remaininggvagaps, c(
  "year", "code", "level", "geography", "gva - current prices (x 1,000,000)",
  "umbrella", "natlvalnom", "natlumbvalnom", "natlsharenom",
  "umbrellavalnom", "gva - chained prices (x 1,000,000)",
  "natlvalreal", "natlumbvalreal", "natlsharereal", "umbrellavalreal"
))
remaininggvagaps <- remaininggvagaps[order(code, geography)]

# do this in loop so that lower levels benefit from newly filled umbrellas (kind of crude but it works)
for (i in 1:5) {
  # first we recalculate the umbrella values
  remaininggvagaps[, `:=`
  (
    umbrellavalnom = `gva - current prices (x 1,000,000)`[match(umbrella, code, nomatch = NA)],
    umbrellavalreal = `gva - chained prices (x 1,000,000)`[match(umbrella, code, nomatch = NA)]
  ),
  by = list(geography, year)
  ]
  # then multiply by the national share
  remaininggvagaps[, `:=`

  # ( tryreal=ifelse(is.na(`gva - chained prices (x 1,000,000)`),umbrellavalreal*natlsharereal, `gva - chained prices (x 1,000,000)`),
  # trynom=ifelse(is.na(`gva - current prices (x 1,000,000)`),umbrellavalnom*natlsharenom  , `gva - current prices (x 1,000,000)`)),

  (
    `gva - chained prices (x 1,000,000)` = ifelse(is.na(`gva - chained prices (x 1,000,000)`), umbrellavalreal * natlsharereal, `gva - chained prices (x 1,000,000)`),
    `gva - current prices (x 1,000,000)` = ifelse(is.na(`gva - current prices (x 1,000,000)`), umbrellavalnom * natlsharenom, `gva - current prices (x 1,000,000)`)
  ), ]
}

remaininggvagaps[, c(
  "umbrella", "natlvalnom", "natlumbvalnom", "natlsharenom",
  "umbrellavalnom", "natlvalreal", "natlumbvalreal", "natlsharereal", "umbrellavalreal", "level"
) := NULL]

# names(nationalgva)[names(nationalgva) %in% c("umbrella" , "natlumbvalnom", "natlumbvalreal")]<-c("code", "gva - current prices (x 1,000,000)","gva - chained prices (x 1,000,000)")
# gvaseries<-merge(nationalgva, remaininggvagaps, by=c("geography", "code", "year"), all=T)
gvaseries <- remaininggvagaps
saveRDS(gvaseries, paste(srcDir, "step2.rds", sep = ""))
# getting 331314 by difference
gvaseries[, `:=`
(
  `gva - chained prices (x 1,000,000)` =
    ifelse(code %like% "331314",
      `gva - chained prices (x 1,000,000)`[code == 3313] - `gva - chained prices (x 1,000,000)`[code == 331313],
      `gva - chained prices (x 1,000,000)`
    ),
  `gva - current prices (x 1,000,000)` =
    ifelse(code %like% "331314",
      `gva - current prices (x 1,000,000)`[code == 3313] - `gva - current prices (x 1,000,000)`[code == 331313],
      `gva - current prices (x 1,000,000)`
    )
), by = list(geography, year)]
#------------------------------------------------------------------------------------#
# umbrella growth for gva
#------------------------------------------------------------------------------------#
gvavariables <- c("gva - current prices (x 1,000,000)", "gva - chained prices (x 1,000,000)")
for (i in 1:5) {
  for (var in gvavariables) {
    print(var)
    # var="investment - chained prices (x 1,000,000)"
    # calculate
    suppressWarnings(
      gvaseries[, `:=`
      (
        umbrella = shiftshares$umbrella[match(gvaseries$code, shiftshares$code, nomatch = NA)],
        try = as.numeric(NA)
      )              ][, `:=`
      (umbrellaval = get(var)[match(umbrella, code, nomatch = NA)]),
      by = list(year, geography)
      ][,
        umbrellagrowth := c(NA, exp(diff(log(umbrellaval)))),
        by = code
      ]
    )
    gvaseries[is.infinite(umbrellagrowth) | is.nan((umbrellagrowth)), umbrellagrowth := 1]
    for (y in (as.numeric(min(gvaseries$year)) + 1):max(gvaseries$year)) {
      gvaseries[year == y, `:=`
      (try = ifelse(is.na(gvaseries[year == y, get(var)]), gvaseries[year == y - 1, try] * (gvaseries[year == y, umbrellagrowth]), gvaseries[year == y, get(var)]))              ]
    }

    # assign and get rid of extrainfo
    gvaseries[, eval(var) := try]
  }
}
gvaseries[, c("umbrellagrowth", "try", "umbrella", "umbrellaval") := NULL]
#------------------------------------------------------------------------------------#
#     6a: Once again re do the vertical tie up                                   #####
#------------------------------------------------------------------------------------#

# gva before extension
# beforenationalscaling<-dcast(gvatiedvertical,geography+code+year~variable)
# names(beforenationalscaling)[names(beforenationalscaling)=="gva - chained prices (x 1,000,000)"]<-"originalchained"
# names(beforenationalscaling)[names(beforenationalscaling)=="gva - current prices (x 1,000,000)"]<-"originalnom"
#
# setkey(beforenationalscaling, geography, code, year)
# setkey(gvaseries, geography, code, year)

# try with more free cells
beforenationalscaling <- gapfilldb
names(beforenationalscaling)[names(beforenationalscaling) == "gva - chained prices (x 1,000,000)"] <- "originalchained"
names(beforenationalscaling)[names(beforenationalscaling) == "gva - current prices (x 1,000,000)"] <- "originalnom"

setkey(gapfilldb, geography, code, year)
setkey(gvaseries, geography, code, year)




forgrowback <- beforenationalscaling[gvaseries]
# forgrowback<- melt(forgrowback, id.vars =c("geography", "code", "year","umbrella"))

# 1 is fixed
forgrowback <- merge(forgrowback, `gva - current prices (x 1,000,000)-subgroup`[, c("geography", "code", "year", "gross domestic product (gdp) at basic prices-3790030-current prices")], by = c("code", "year", "geography"))
forgrowback[geography != "Canada", tagnom := ifelse(all(is.na(`gross domestic product (gdp) at basic prices-3790030-current prices`)), 0, 1), by = list(code, geography)][, `gross domestic product (gdp) at basic prices-3790030-current prices` := NULL]
forgrowback <- merge(forgrowback, `gva - chained prices (x 1,000,000)-subgroup`[, c("geography", "code", "year", "gross domestic product (gdp) at basic prices-3790030-chained (2012) dollars")], by = c("code", "year", "geography"))
forgrowback[geography != "Canada", tagreal := ifelse(all(is.na(`gross domestic product (gdp) at basic prices-3790030-chained (2012) dollars`)), 0, 1), by = list(code, geography)][, `gross domestic product (gdp) at basic prices-3790030-chained (2012) dollars` := NULL]
forgrowback <- merge(forgrowback, `gva - current prices (x 1,000,000)-subgroup`[, c("geography", "code", "year", "gross domestic product (gdp) at basic prices-3790029-current prices")], by = c("code", "year", "geography"))
forgrowback[geography == "Canada", tagnom := ifelse(all(is.na(`gross domestic product (gdp) at basic prices-3790029-current prices`)), 0, 1), by = list(code, geography)][, `gross domestic product (gdp) at basic prices-3790029-current prices` := NULL]
forgrowback <- merge(forgrowback, `gva - chained prices (x 1,000,000)-subgroup`[, c("geography", "code", "year", "gross domestic product (gdp) at basic prices-3790031-chained (2012) dollars")], by = c("code", "year", "geography"))
forgrowback[geography == "Canada", tagreal := ifelse(all(is.na(`gross domestic product (gdp) at basic prices-3790031-chained (2012) dollars`)), 0, 1), by = list(code, geography)][, `gross domestic product (gdp) at basic prices-3790031-chained (2012) dollars` := NULL]

# forgrowback[,tagreal:=ifelse(is.na(originalchained) & !is.na(`gva - chained prices (x 1,000,000)`), 0,1)]
# forgrowback[,tagnom :=ifelse(is.na(originalnom)     & !is.na(`gva - current prices (x 1,000,000)`), 0,1)]

forgrowback[, c("originalchained", "originalnom") := NULL]

forgrowbackcomplete <- melt(forgrowback, id.vars = c("code", "year", "geography", "tagreal", "tagnom"))
forgrowbackcomplete[, tag := ifelse(variable == "gva - chained prices (x 1,000,000)", tagreal, tagnom)][, c("tagreal", "tagnom") := NULL]

# umbrellas
forgrowbackcomplete[, umbrella := shiftshares$umbrella[match(forgrowbackcomplete$code, shiftshares$code, nomatch = NA)]]

forgrowbacknominal <- forgrowbackcomplete[variable == "gva - current prices (x 1,000,000)"]


# This function is used again and works as long as it has this colname structure : "geography" "code"      "year"   "umbrella"   "variable"  "value" "tag"
tieupfunction(forgrowbacknominal)


gvaseries2 <- rbind(forgrowbackcomplete[variable == "gva - chained prices (x 1,000,000)"], forgrowbacknominal[, c("code", "year", "geography", "variable", "value", "tag", "umbrella"), with = F])
#------------------------------------------------------------------------------------#
#         **OPTIONAL** now we have the canada totals, so we can ensure the       #####
#         subsectors add across provinces. Default setting is to ignore          #####
#         problem series outside of the 379-0030 range                           #####
#------------------------------------------------------------------------------------#


# gvaseries2<-gvaseries[, c("tag","umbrella"):=NULL]

# gvaseries2<-melt(gvaseries2, id.vars = c("code", "year", "geography"))
gvaseries2 <- dcast(gvaseries2, code + year + variable ~ geography)
gvaseries2 <- gvaseries2[order(variable, code, year)]


# move Canada to the end
setcolorder(gvaseries2, c(setdiff(names(gvaseries2), "Canada"), "Canada"))

# calculate row sums
notcanada <- names(gvaseries2)[!(names(gvaseries2) %in% c("Canada", "year", "code", "variable"))]
gvaseries2[, provtotal := rowSums(.SD), .SDcols = (notcanada)]
gvaseries2[, scale := (Canada - provtotal) / provtotal]

# split apart and merge for fixes
scales <- gvaseries2[, c("code", "year", "scale", "variable"), with = F]
nationalvals <- melt(gvaseries2[, c("code", "year", "variable", "Canada"), with = F], id.vars = c("code", "year", "variable"))
names(nationalvals)[names(nationalvals) == "variable.1"] <- "geography"

gvaseriesremelt <- melt(gvaseries2[, c("code", "year", "variable", eval(notcanada)), with = F], id.vars = c("code", "year", "variable"))
names(gvaseriesremelt)[names(gvaseriesremelt) == "variable.1"] <- "geography"

# db ready:
gvaseriesforhorfix <- scales[gvaseriesremelt, on = c("code", "year", "variable")]
gvaseriesforhorfix <- gvaseriesforhorfix[variable == "gva - current prices (x 1,000,000)"]

# quick transform
gvaseriesforhorfix[abs(scale) > .05, tag := 1]
gvaseriesforhorfix[tag == 1, value := value + scale * value]

# bring back old fixes and umbrella
tagsformerge <- forgrowbacknominal[geography != "Canada", c("code", "year", "geography", "variable", "tag"), with = F]
gvaseriesforhorfix <- tagsformerge[gvaseriesforhorfix, on = c("geography", "variable", "code", "year")]
gvaseriesforhorfix[, tag := ifelse(is.na(i.tag), tag, i.tag)][, i.tag := NULL][, scale := NULL]
gvaseriesforhorfix[, umbrella := shiftshares$umbrella[match(gvaseriesforhorfix$code, shiftshares$code, nomatch = NA)]]

# This function is used again and works as long as it has this colname structure : "geography" "code"      "year"      "variable"  "value" "tag"
tieupfunction(gvaseriesforhorfix)

# bring back national values and chained values
gvahorfixed <- rbind(nationalvals, gvaseriesforhorfix[, c("code", "year", "geography", "variable", "value"), with = F])
gvachained <- forgrowbackcomplete[, !c("umbrella", "tag"), with = F]

gvahorfixed <- rbind(gvahorfixed, gvachained[geography != "Canada" & variable == "gva - chained prices (x 1,000,000)"])
gvahorfixed[, variable := as.character(variable)]
#------------------------------------------------------------------------------------#
# Step 7: Impose all real data growth rates to the gva series                        #
#------------------------------------------------------------------------------------#
sectorsformerge <- dcast(gvahorfixed, year + code + geography ~ variable)

variables <- c("gva - current prices (x 1,000,000)", "gva - chained prices (x 1,000,000)")

subgroups <- c("gva - current prices (x 1,000,000)-subgroup", "gva - chained prices (x 1,000,000)-subgroup")
subgrouplist <- mget(subgroups)
subgrouplist <- lapply(subgrouplist, function(x) x[, tag := NULL])

suppressWarnings(
  subgrouplist2 <- lapply(subgrouplist, function(x) x[, names(x)[names(x) %in% variables] := NULL])
)

subgrouplist2 <- lapply(subgrouplist, function(x) x[order(geography, code)])

# prepare full variable list
dbforsplit <- melt(sectorsformerge, id.vars = c("geography", "code", "year"))
dbforsplit <- dbforsplit[, variable := as.character(variable)]
dbforsplit$variable <- factor(as.character(dbforsplit$variable), levels = variables)
dbforsplit <- dbforsplit[order(variable, geography, code)]
dbformerge <- split(dbforsplit, by = "variable")

dbpartialmerge <- mapply(function(x, y) merge(x, y, by = c("geography", "code", "year")), x = dbformerge, y = subgrouplist2, SIMPLIFY = FALSE)


dbmerge <- rbindlist(dbpartialmerge, fill = TRUE)


### calculate growth rates for existing series###

# columns to work with
foroperation <- names(dbmerge)[!(names(dbmerge) %in% c("geography", "code", "year", "variable")) ]
foroperation <- foroperation[!(foroperation %like% "overlap")]
# switch to numerics
suppressWarnings(dbmerge[, (foroperation) := lapply(.SD, function(y) as.numeric(gsub("[^0-9.]", "", y))), .SDcols = (foroperation)])
newnames <- paste(foroperation, "growth", sep = "-")
# calculate
dbmerge[, (newnames) := lapply(foroperation, function(x) {
  c(NA, exp(diff(log(get(x)))))
}), by = list(geography, code)]

# split and clean
dbsplit2 <- split(dbmerge, by = "variable")
dbsplit2 <- lapply(dbsplit2, function(x) Filter(function(y) !all(is.na(y)), x))

# split again into a list of lists 17 list elements (one for each varaible) and each has sectors*geographies sublist elements
dbsplit3 <- lapply(dbsplit2, function(x) split(x, by = c("geography", "code")))


fixedseriesformerge <- lapply(dbsplit3, lapply, imposegrowthrates)

# recompose the list and reshape
fixedgvaseries <- rbindlist(lapply(fixedseriesformerge, rbindlist))
fixedgvaseries[, umbrella := shiftshares$umbrella[match(fixedgvaseries$code, shiftshares$code, nomatch = NA)]]
# gapfilldb<-dcast(fixedseries,geography+code+year~variable)
saveRDS(fixedgvaseries, paste(srcDir, "fixedseriesgva", sep = ""))
#------------------------------------------------------------------------------------#
#     7a: Once again re do the vertical tie up                                   #####
#------------------------------------------------------------------------------------#


# this function combines the horizontal and vertical tie up functions. It takes two arguments
# x = the complete table of GVA (including Canada and chained vars)
#     colnames(x)= code, year, variable, geography
# y = the table of tags that I want to use (in addition to the tags that will be created in the function)
#     do not include Canada
#     colnames(y)= "geography", "variable" "code" "year" "tag"

# set x
gvaforfinalfix <- fixedgvaseries[, !"umbrella", with = F]
# set y
# these are the fixes before imposing true growth rates
# tagsforfix<-gvaseriesforhorfix[geography!="Canada", c("geography", "variable", "code", "year", "tag"), with=F]

# mark the cells that are in the 379-0030 (1 means it's  in 379-0030 and therefore fixed)
`gva - current prices (x 1,000,000)-subgroup`[geography != "Canada", tag := ifelse(is.na(`gross domestic product (gdp) at basic prices-3790030-current prices`), 0, 1)]
`gva - current prices (x 1,000,000)-subgroup`[geography == "Canada", tag := ifelse(is.na(`gross domestic product (gdp) at basic prices-3790029-current prices`), 0, 1)]
`gva - chained prices (x 1,000,000)-subgroup`[geography != "Canada", tag := ifelse(is.na(`gross domestic product (gdp) at basic prices-3790030-chained (2012) dollars`), 0, 1)]
`gva - chained prices (x 1,000,000)-subgroup`[geography == "Canada", tag := ifelse(is.na(`gross domestic product (gdp) at basic prices-3790031-chained (2012) dollars`), 0, 1)]


taglistchained <- `gva - chained prices (x 1,000,000)-subgroup`[, c("geography", "code", "year", "tag"), with = F]
taglistchained[, variable := "gva - chained prices (x 1,000,000)"]

tagsforfix <- `gva - current prices (x 1,000,000)-subgroup`[, c("geography", "code", "year", "tag"), with = F]
tagsforfix[, variable := "gva - current prices (x 1,000,000)"]

tagsforfix <- rbind(tagsforfix, taglistchained)

# these fix down data changed using real growth rates
temp <- dcast(fixedgvaseries, geography + code + year ~ variable)
temp <- merge(sectorsformerge, temp, by = c("geography", "code", "year"))
temp[, tag := ifelse(`gva - current prices (x 1,000,000).x` - `gva - current prices (x 1,000,000).y` < .01, 0, 1)]
temp[, names(temp)[names(temp) %like% "gva "] := NULL]

tagsforfix <- tagsforfix[temp, on = c("geography", "code", "year")]
tagsforfix[, tag := ifelse(tag == 1 | i.tag == 1, 1, 0)][, c("i.tag") := NULL]


finalgvaseries <- verthortieup(gvaforfinalfix, tagsforfix, gvatag = 1)

#------------------------------------------------------------------------------------#
#         Rescale new series                                                     #####
#------------------------------------------------------------------------------------#
forgrowback <- beforenationalscaling[gvaseries]
# forgrowback<- melt(forgrowback, id.vars =c("geography", "code", "year","umbrella"))

# 1 is fixed
forgrowback <- merge(forgrowback, `gva - current prices (x 1,000,000)-subgroup`[, c("geography", "code", "year", "gross domestic product (gdp) at basic prices-3790030-current prices")], by = c("code", "year", "geography"))
forgrowback[geography != "Canada", tagnom := ifelse(all(is.na(`gross domestic product (gdp) at basic prices-3790030-current prices`)), 0, 1), by = list(code, geography)][, `gross domestic product (gdp) at basic prices-3790030-current prices` := NULL]
forgrowback <- merge(forgrowback, `gva - chained prices (x 1,000,000)-subgroup`[, c("geography", "code", "year", "gross domestic product (gdp) at basic prices-3790030-chained (2012) dollars")], by = c("code", "year", "geography"))
forgrowback[geography != "Canada", tagreal := ifelse(all(is.na(`gross domestic product (gdp) at basic prices-3790030-chained (2012) dollars`)), 0, 1), by = list(code, geography)][, `gross domestic product (gdp) at basic prices-3790030-chained (2012) dollars` := NULL]
forgrowback <- merge(forgrowback, `gva - current prices (x 1,000,000)-subgroup`[, c("geography", "code", "year", "gross domestic product (gdp) at basic prices-3790029-current prices")], by = c("code", "year", "geography"))
forgrowback[geography == "Canada", tagnom := ifelse(all(is.na(`gross domestic product (gdp) at basic prices-3790029-current prices`)), 0, 1), by = list(code, geography)][, `gross domestic product (gdp) at basic prices-3790029-current prices` := NULL]
forgrowback <- merge(forgrowback, `gva - chained prices (x 1,000,000)-subgroup`[, c("geography", "code", "year", "gross domestic product (gdp) at basic prices-3790031-chained (2012) dollars")], by = c("code", "year", "geography"))
forgrowback[geography == "Canada", tagreal := ifelse(all(is.na(`gross domestic product (gdp) at basic prices-3790031-chained (2012) dollars`)), 0, 1), by = list(code, geography)][, `gross domestic product (gdp) at basic prices-3790031-chained (2012) dollars` := NULL]

# forgrowback[,tagreal:=ifelse(is.na(originalchained) & !is.na(`gva - chained prices (x 1,000,000)`), 0,1)]
# forgrowback[,tagnom :=ifelse(is.na(originalnom)     & !is.na(`gva - current prices (x 1,000,000)`), 0,1)]

forgrowback[, c("originalchained", "originalnom") := NULL]

forgrowbackcomplete <- melt(forgrowback, id.vars = c("code", "year", "geography", "tagreal", "tagnom"))
forgrowbackcomplete[, tag := ifelse(variable == "gva - chained prices (x 1,000,000)", tagreal, tagnom)][, c("tagreal", "tagnom") := NULL]

# umbrellas
forgrowbackcomplete[, umbrella := shiftshares$umbrella[match(forgrowbackcomplete$code, shiftshares$code, nomatch = NA)]]

test <- merge(finalgvaseries, forgrowbackcomplete[, !"value"], by = c("code", "year", "geography", "variable"))
test[geography != "Canada", mark := ifelse(all(tag == 0), 1, 0), by = list(umbrella, variable, year)]

nationalscales <- test
replacement <- nationalscales[geography == "Alberta", .(code, year, geography, mark, variable)]
nationalscales <- nationalscales[geography == "Canada", .(code, year, geography, variable, value, tag, umbrella)]
nationalscales <- replacement[nationalscales, on = c("code", "year", "variable")]
nationalscales[mark == 1, sums := sum(value), by = list(variable, umbrella, year)]

# put the sums next to the umbrella value

chart <- unique(nationalscales[!is.na(sums), sums, by = list(umbrella, year, variable)])
names(chart)[names(chart) == "sums"] <- "sectorsum"
x <- merge(nationalscales, chart, by.y = c("umbrella", "year", "variable"), by.x = c("code", "year", "variable"), all = T)

# calculate scale
x[, scalefactor := 1 - (sectorsum - value) / sectorsum]
chart <- x[!is.na(sectorsum), c("code", "year", "scalefactor", "variable"), with = F]
test2 <- merge(test, chart, by.x = c("umbrella", "year", "variable"), by.y = c("code", "year", "variable"), all.x = T)

# apply
test2[, value := ifelse(is.na(scalefactor), value, value * scalefactor)]

# clean up for growth
test2[, c("tag", "mark", "scalefactor", "umbrella") := NULL]

# growthrates
test2 <- dcast(test2, year + code + geography ~ variable)
test2[, growth := c(NA, exp(diff(log(`gva - chained prices (x 1,000,000)`)))), by = list(geography, code)]
test2[year == base, try := `gva - current prices (x 1,000,000)`]

# stipulate and apply growth functions
test2 <- split(test2, by = c("geography", "code"))
extendgva <- function(x) {
  DT <- x
  for (y in (base + 1):max(DT$year)) {
    print(y)
    DT[year == y, `:=`
    (try = ifelse(DT[year == y - 1, try] %in% 0, DT[year == y, `gva - chained prices (x 1,000,000)`],
        ifelse(is.finite(DT[year == y, growth]), DT[year == y - 1, try] * (DT[year == y, growth]), DT[year == y, `gva - chained prices (x 1,000,000)`])
    ))]
  }
  x <- x
}

y <- lapply(test2, extendgva)
backextendgva <- function(x) {
  DT <- x
  for (y in (base - 1):min(DT$year)) {
    # print(y)
    DT[year == y, `:=`
    (try = ifelse(DT[year == y + 1, try] %in% 0, DT[year == y, `gva - chained prices (x 1,000,000)`],
        ifelse(is.finite(DT[year == y + 1, growth]), DT[year == y + 1, try] / (DT[year == y + 1, growth]), DT[year == y, `gva - chained prices (x 1,000,000)`])
    ))]
  }
  x <- x
}

y <- lapply(y, backextendgva)

gva <- rbindlist(y)
gva[, `gva - chained prices (x 1,000,000)` := try][, c("try", "growth") := NULL]



#------------------------------------------------------------------------------------#
## Step 7b: Correct zeros                                                             #
#------------------------------------------------------------------------------------#
# bring in primary series
primary <- envcandbshort[, c("code", "year", "geography", "gross domestic product (gdp) at basic prices-3790030-current prices", "gross domestic product (gdp) at basic prices-3790029-current prices", "gross domestic product (gdp) at basic prices-3790031-chained (2012) dollars", "gross domestic product (gdp) at basic prices-3790030-chained (2012) dollars"), with = F]
primary[geography == "Canada", `:=`
(
  primaryreal = `gross domestic product (gdp) at basic prices-3790031-chained (2012) dollars`,
  primarynom = `gross domestic product (gdp) at basic prices-3790029-current prices`
)]

primary[geography != "Canada", `:=`
(
  primaryreal = `gross domestic product (gdp) at basic prices-3790030-chained (2012) dollars`,
  primarynom = `gross domestic product (gdp) at basic prices-3790030-current prices`
)]
primary <- primary[, c("code", "year", "geography", "primaryreal", "primarynom"), with = F]
gva <- merge(gva, primary, by = c("code", "year", "geography"), all.x = T)

# zero everything if any of these are 0
gva[primaryreal == 0 | primarynom == 0, `:=`
(`gva - chained prices (x 1,000,000)` = 0, `gva - current prices (x 1,000,000)` = 0)]


# now zero all the subsectors of the newly zeroed sectors
gva[, umbrella := shiftshares$umbrella[match(gva$code, shiftshares$code, nomatch = NA)]]

gva[,
  `:=`
  (
    `gva - chained prices (x 1,000,000)` = ifelse(`gva - chained prices (x 1,000,000)`[match(umbrella, code, nomatch = NA)] %in% 0, 0, `gva - chained prices (x 1,000,000)`),
    `gva - current prices (x 1,000,000)` = ifelse(`gva - current prices (x 1,000,000)`[match(umbrella, code, nomatch = NA)] %in% 0, 0, `gva - current prices (x 1,000,000)`)
  ),
  by = list(geography, year)
]

gva <- gva[, c("code", "year", "geography", "gva - chained prices (x 1,000,000)", "gva - current prices (x 1,000,000)"), with = F ]



saveRDS(gva, paste(srcDir, "finalgvaseries.rds", sep = ""))
print("GVA series ready.")

#------------------------------------------------------------------------------------#
# Step 8: Use GVA series to fill in other variables                              #####
#------------------------------------------------------------------------------------#
omitmacro <- c("geography", "code", "year", finalvariables)

finalgvaseries <- readRDS(paste(srcDir, "finalgvaseries.rds", sep = ""))
fixedseries <- readRDS(paste(srcDir, "fixedseries.rds", sep = ""))
umbrellasects <- unique(shiftshares$umbrella)[!is.na(unique(shiftshares$umbrella)) & unique(shiftshares$umbrella) != "NAT"]
formacros <- names(envcandbshort)[!(names(envcandbshort) %in% c(sectorals, finalvariables, "industry"))] # <------------NOTICE THESE NAMES ARE HARDCODED
macrovars <- formacros[!(formacros %like% "overlap")]



omitgva <- omitmacro[!(omitmacro %in% c("gva - current prices (x 1,000,000)", "gva - chained prices (x 1,000,000)")) ]


# Merge using early database wehre we imposed growth rates
variablesformerge <- dcast(fixedseries, geography + code + year ~ variable)
variablesformerge <- variablesformerge[, ..omitgva]
setkey(variablesformerge, geography, code, year)

# bring in the latest gva
# finalgvaseries<-dcast(finalgvaseries, geography+code+year~variable)
setkey(finalgvaseries, geography, code, year)

# merge
GVAallocation <- finalgvaseries[variablesformerge]


## To calculate GVA shares (real and nominal)##

# bring back the umbrellas
GVAallocation[, umbrella := shiftshares$umbrella[match(GVAallocation$code, shiftshares$code, nomatch = NA)]]

print("Begin interpolating other variables. First step, use GVA to split down from parent sectors.")
# calculate
# first nominal vars

variablescurr <- c(
  "employment", "gva - current prices (x 1,000,000)", "gross output (x 1,000,000)",
  "wages", "price index", "hours worked", "investment - current prices (x 1,000,000)",
  "investment, mach - current prices (x 1,000,000)", "investment, ip - current prices (x 1,000,000)",
  "investment, constr - current prices (x 1,000,000)"
)
for (i in 1:5) {
  for (var in variablescurr) {
    print(var)
    # var="investment - chained prices (x 1,000,000)"
    # calculate
    GVAallocation[, `:=`
    (
      share = ifelse(is.nan(`gva - current prices (x 1,000,000)` / `gva - current prices (x 1,000,000)`[match(umbrella, code, nomatch = NA)]), 0,
        `gva - current prices (x 1,000,000)` / `gva - current prices (x 1,000,000)`[match(umbrella, code, nomatch = NA)]
      ),
      umbrellaval = get(var)[match(umbrella, code, nomatch = NA)]
    ),
    by = list(year, geography)
    ][, try := ifelse(is.na(get(var)), umbrellaval * share, get(var)) ]
    # zero out value if umbrella is zero
    GVAallocation[umbrellaval == 0, try := 0]
    # assign and get rid of extrainfo
    GVAallocation[, eval(var) := try]
  }
}

# now real
variableschained <- c(
  "capital stock - chained prices (x 1,000,000)",
  "stock, mach - chained prices (x 1,000,000)",
  "stock, ip - chained prices (x 1,000,000)", "stock, constr - chained prices (x 1,000,000)",
  "gross output - constant prices (x 1,000,000)", "investment - chained prices (x 1,000,000)", "capital depreciation - chained prices (x 1,000,000)", "investment, mach - chained prices (x 1,000,000)", "investment, ip - chained prices (x 1,000,000)", "investment, constr - chained prices (x 1,000,000)", "depreciation, mach - chained prices (x 1,000,000)", "depreciation, ip - chained prices (x 1,000,000)", "depreciation, constr - chained prices (x 1,000,000)"
)
for (i in 1:5) {
  for (var in variableschained) {
    print(var)
    # var="investment - chained prices (x 1,000,000)"
    # calculate
    GVAallocation[, `:=`
    (
      share = ifelse(is.nan(`gva - chained prices (x 1,000,000)` / `gva - chained prices (x 1,000,000)`[match(umbrella, code, nomatch = NA)]), 0,
        `gva - chained prices (x 1,000,000)` / `gva - chained prices (x 1,000,000)`[match(umbrella, code, nomatch = NA)]
      ),
      umbrellaval = get(var)[match(umbrella, code, nomatch = NA)]
    ),
    by = list(year, geography)
    ][, try := ifelse(is.na(get(var)), umbrellaval * share, get(var)) ]
    # zero out value if umbrella is zero
    GVAallocation[umbrellaval == 0, try := 0]

    # assign and get rid of extrainfo
    GVAallocation[, eval(var) := try]
  }
}

GVAallocation[, c("share", "umbrella", "umbrellaval", "try") := NULL]

saveRDS(GVAallocation, paste(srcDir, "step3.rds", sep = ""))

#------------------------------------------------------------------------------------#
# Step 9: Use umbrella growth rates to fill gaps                                 ######
#------------------------------------------------------------------------------------#
step5db <- GVAallocation

print("Use growth rates of parent sector to fill remaining gaps.")

for (i in 1:5) {
  for (var in finalvariables) {
    print(var)
    # var="investment - chained prices (x 1,000,000)"
    # calculate
    suppressWarnings(
      step5db[, `:=`
      (
        umbrella = shiftshares$umbrella[match(step5db$code, shiftshares$code, nomatch = NA)],
        try = as.numeric(NA)
      )            ][, `:=`
      (umbrellaval = get(var)[match(umbrella, code, nomatch = NA)]),
      by = list(year, geography)
      ][,
        umbrellagrowth := c(NA, exp(diff(log(umbrellaval)))),
        by = code
      ]
    )
    step5db[is.infinite(umbrellagrowth) | is.nan((umbrellagrowth)), umbrellagrowth := 1]
    for (y in (as.numeric(min(step5db$year)) + 1):max(step5db$year)) {
      suppressWarnings(
        step5db[year == y, `:=`
        (try = ifelse(is.na(step5db[year == y, get(var)]), step5db[year == y - 1, try] * (step5db[year == y, umbrellagrowth]), step5db[year == y, get(var)]))              ]
      )
    }

    # assign and get rid of extrainfo
    step5db[, eval(var) := try]
  }
}

dbformerge <- step5db[, c("umbrella", "umbrellaval", "umbrellagrowth", "try") := NULL]
dbformerge <- melt(dbformerge, id = c("geography", "year", "code"))
dbformerge <- dbformerge[, variable := as.character(variable)]
# dbformerge$variable<-factor(dbformerge$variable,levels=desiredorder)
# dbformerge<-dbformerge[order(variable, geography, code)]

chainedvarsforgrowth <- dbformerge[variable %in% variableschained][!(variable %like% "gva")]


saveRDS(step5db, paste(srcDir, "step5.rds", sep = ""))




#------------------------------------------------------------------------------------#
# Step 10: Test tie ups                                                          #####
#------------------------------------------------------------------------------------#
print("Apply tie up function.")


variablescurrnogva <- variablescurr[!(variablescurr %like% "gva")]
variableschainogva <- variableschained[!(variableschained %like% "gva")]

# this function combines the horizontal and vertical tie up functions. It takes two arguments
# x = the complete table of GVA (including Canada and chained vars)
#     colnames(x)= code, year, variable, geography
# y = the table of tags that I want to use (in addition to the tags that will be created in the function)
#     do not include Canada
#     colnames(y)= "geography", "variable" "code" "year" "tag"

# list order(s)
suppressWarnings(rm(list = ls(pattern = "^subgroup")))
subgroups <- sort(ls()[ls() %like% "subgroup"])
desiredorder <- gsub("-subgroup", "", subgroups)

# set x
dbformerge$variable <- factor(dbformerge$variable, levels = desiredorder)
dbformerge <- dbformerge[code != "NAT"][order(variable)]
allseriesfortieup <- split(dbformerge, by = "variable")

allseriesfortieup <- allseriesfortieup[names(allseriesfortieup) %in% variablescurrnogva]
# set y
# these are the fixes before imposing true growth rates
temp <- readRDS(paste(srcDir, "fixedseries.rds", sep = ""))

# these fix down data that comes from real numbers
temp <- merge(dbformerge, temp, by = c("geography", "code", "year", "variable"))
temp[, tag := ifelse(value.x == value.y, 1, 0)]
temp[, names(temp)[names(temp) %like% "value"] := NULL]
# set order

temp$variable <- factor(temp$variable, levels = desiredorder)
tagsforfix <- temp[order(variable)]
tagsforfix <- split(tagsforfix, by = "variable")

tagsforfix <- tagsforfix[names(tagsforfix) %in% variablescurrnogva]
tagsforfix2 <- tagsforfix
# tagsforfix2[c("employment","wages")]<-NULL
tagsforfix2[c("wages")] <- NULL

newemptags <- copy(tagsforfix2["employment"])
newemptags <- `employment-subgroup`[newemptags, on = c("geography", "code", "year")]
newemptags[, tag := ifelse(is.na(`total number of jobs-3830031`), NA, tag)]
tagsforfix2["employment"] <- list(newemptags[, .(geography, code, year, variable, tag)])
# apply function over the two lists

# omit employment.
# employment<-allseriesfortieup[["employment"]]
wages <- allseriesfortieup[["wages"]]
# allseriesfortieup[c("employment","wages")]<-NULL
allseriesfortieup[c("wages")] <- NULL

firsttieall <- mapply(verthortieup, allseriesfortieup, tagsforfix2, gvatag = 0, SIMPLIFY = F)

# allseriesforgrowthrates<-rbind(rbindlist(firsttieall),employment,wages,chainedvarsforgrowth)
allseriesforgrowthrates <- rbind(rbindlist(firsttieall), wages, chainedvarsforgrowth)

saveRDS(allseriesforgrowthrates, paste(srcDir, "step10.rds", sep = ""))
rm(list = c("allseriesfortieup", "firsttieall"))
#------------------------------------------------------------------------------------#
# Step 11: Impose all real data growth rates to all the series                   #####
#------------------------------------------------------------------------------------#
print("Impose growth rates from actual data.")

vars <- omitmacro[!(omitmacro %in% c("geography", "code", "year")) & !(omitmacro %like% "gva")]

# prepare base
forgrowth <- allseriesforgrowthrates
forgrowth[, variable := as.character(variable)]
forgrowth$variable <- factor(as.character(forgrowth$variable), levels = desiredorder)

forgrowth <- forgrowth[order(variable)]
dbformerge <- split(forgrowth, by = "variable")
dbformerge <- dbformerge[!(names(dbformerge) %like% "gva")]
#
# The "real" growthrates used in this chunk are those from the various series that were merged together,
# so here we bring those back from the "subgroups" created earlier
#
suppressWarnings(rm(list = ls(pattern = "^subgroup")))
subgroups <- sort(ls()[ls() %like% "subgroup"])
desiredorder <- gsub("-subgroup", "", subgroups)
subgroups <- subgroups[order(subgroups, by = desiredorder)]
subgrouplist <- mget(subgroups)

# will use the same subgroups list as step 2, minus the gva stuff
subgroupliststep10 <- subgrouplist
subgroupliststep10 <- (subgroupliststep10[!(names(subgroupliststep10) %like% "gva")])


# we're aiming for one subtable per variable/geography/code



dbpartialmerge <- mapply(function(x, y) merge(x, y, by = c("geography", "code", "year")), x = dbformerge, y = subgroupliststep10, SIMPLIFY = FALSE)
# dbmerge<-rbindlist(dbpartialmerge, fill= TRUE)

### calculate growth rates for existing series###
# this takes each subtable and adds the growth columns
finalprep <- function(x) {
  foroperation <- names(x)[!grepl("geography|code|year|variable|overlap", names(x))]
  newnames <- paste(foroperation, "growth", sep = "-")
  x[, (foroperation) := lapply(.SD, function(y) as.numeric(gsub("[^0-9.]", "", y))), .SD = (foroperation)]
  x[, (newnames) := lapply(foroperation, function(x) {
    c(NA, exp(diff(log(get(x)))))
  }), by = list(geography, code)]
}
y <- lapply(dbpartialmerge, finalprep)

# split again into a list of lists 17 list elements (one for each varaible) and each has sectors*geographies sublist elements
dbsplit3 <- lapply(y, function(x) split(x, by = c("geography", "code")))
saveRDS(dbsplit3, paste(srcDir, "dbsplit3.rds", sep = ""))
# apply function
fixedseriesformerge <- pblapply(dbsplit3, lapply, imposegrowthrates)

# recompose the list and reshape
fixedseriesall <- rbindlist(lapply(fixedseriesformerge, rbindlist))
# gapfilldb<-dcast(fixedseries,geography+code+year~variable)
saveRDS(fixedseriesall, paste(srcDir, "fixedseriesall.rds", sep = ""))

#------------------------------------------------------------------------------------#
# Step 12: Test tie ups                                                          #####
#------------------------------------------------------------------------------------#
print("Last tie up function.")
# this function combines the horizontal and vertical tie up functions. It takes two arguments
# x = the complete table of GVA (including Canada and chained vars)
#     colnames(x)= code, year, variable, geography
# y = the table of tags that I want to use (in addition to the tags that will be created in the function)
#     do not include Canada
#     colnames(y)= "geography", "variable" "code" "year" "tag"
# set x
fixedseriesfinaltieup <- fixedseriesall
fixedseriesfinaltieup <- fixedseriesfinaltieup[order(variable, geography, code)]
fixedseriesfinaltieup$variable <- factor(as.character(fixedseriesfinaltieup$variable), levels = desiredorder)
fixedseriesfinaltieup <- fixedseriesfinaltieup[order(variable)]
fixedseriesfinaltieup <- split(fixedseriesfinaltieup, by = "variable")
fixedseriesfinaltieup <- fixedseriesfinaltieup[names(fixedseriesfinaltieup) %in% variablescurrnogva]
# set y - will take previous fixes and additionally fix the things that were just changed with growth rates
prevtags <- rbindlist(tagsforfix) # these are the previous ones
temp <- merge(fixedseriesall[variable %in% variablescurr], allseriesforgrowthrates[variable %in% variablescurr], by = c("geography", "code", "year", "variable"))
temp[!is.na(value.x) & !is.na(value.y), tag := ifelse(value.x == value.y, 0, 1)]
temp[, names(temp)[names(temp) %like% "value"] := NULL]
temp <- merge(temp, prevtags, by = c("geography", "code", "year", "variable"))

tagsforfinaltieup <- temp[, tag := ifelse(tag.x == 1 | tag.y == 1, 1, 0)][, c("tag.x", "tag.y") := NULL]
tagsforfinaltieup <- tagsforfinaltieup[order(variable, geography, code)]
tagsforfinaltieup$variable <- factor(as.character(tagsforfinaltieup$variable), levels = desiredorder)
tagsforfinaltieup <- tagsforfinaltieup[order(variable)]


tagsforfinaltieup <- split(tagsforfinaltieup, by = "variable")
# tagsforfinaltieup[c("employment","wages","hours worked")]<-NULL
tagsforfinaltieup[c("wages", "hours worked")] <- NULL

newemptags <- copy(tagsforfinaltieup["employment"])
newemptags <- `employment-subgroup`[newemptags, on = c("geography", "code", "year")]
newemptags[, tag := ifelse(is.na(`total number of jobs-3830031`), NA, tag)]
tagsforfinaltieup["employment"] <- list(newemptags[, .(geography, code, year, variable, tag)])

tagsforfinaltieup <- tagsforfinaltieup[lapply(tagsforfinaltieup, nrow) != 0]


# apply function over the two lists
# employment<-fixedseriesfinaltieup[["employment"]]
wages <- fixedseriesfinaltieup[["wages"]]
hours <- fixedseriesfinaltieup[["hours worked"]]
# fixedseriesfinaltieup[c("employment","wages", "hours worked")]<-NULL
fixedseriesfinaltieup[c("wages", "hours worked")] <- NULL

finaltiesformerge <- pbmapply(verthortieup, fixedseriesfinaltieup, tagsforfinaltieup, gvatag = 0, SIMPLIFY = F)
# regroup
allseriestied <- rbind(rbindlist(finaltiesformerge), fixedseriesall[(variable %in% variableschainogva)])

# reshape gva if necessary
if (!("variable" %in% names(finalgvaseries))) {
  finalgvaseries <- melt(finalgvaseries, id.vars = c("geography", "code", "year"))
}
# dbtieups<-rbind(allseriestied,employment,wages,hours,finalgvaseries)
dbtieups <- rbind(allseriestied, wages, hours, finalgvaseries)

######################################################
# Step 13:                                        #####
# Remerge with macro vars and prepare for export  #####
######################################################
# seriesformerge<-tieups
print("Bring back macro data.")

formacros <- names(envcandbshort)[!(names(envcandbshort) %in% c(sectorals, finalvariables, "industry"))] # <------------NOTICE THESE NAMES ARE HARDCODED
macrovars <- formacros[!(formacros %like% "overlap")]
macroformerge <- envcandbshort[, ..macrovars]
fulldb <- dcast(dbtieups, code + year + geography ~ variable)

fulldb <- merge(fulldb, macroformerge, by = c("code", "year", "geography"))

# reattach industry names
matchbase <- as.data.table(naics[, cbind(code, alias1)])
indnames <- naics$alias1[match(fulldb$code, matchbase$code, nomatch = NA)] # this does not complete the match
fulldb[, industry := indnames]
suppressWarnings(fulldb[, level := NULL])


exportpath <- paste(srcDir, "envcandb-filled.rds", sep = "")
saveRDS(fulldb, eval(exportpath))

# recover
# exportpath <-paste(srcDir, "envcandb-filled.rds",sep="")
# fulldb<-as.data.table(readRDS(eval(exportpath)))

######################################################
# Step 14:                                        #####
# Calculate delta, avg hrs and wages             #####
######################################################




# delta
depreciation <- finalvariables[finalvariables %like% "dep"]
stock <- finalvariables[finalvariables %like% "stock"]
depreciation <- c(depreciation[order(depreciation)], macrovars[macrovars %like% "depreciation-total residential"])
stock <- c(stock[order(stock)], macrovars[macrovars %like% "stock-total residential"])
names <- cbind((depreciation), stock)

for (k in 1:length(depreciation)) {
  dep <- as.character(names[k, 1])
  st <- as.character(names[k, 2])
  fulldb[, test2 := c(NA, get(st)[-.N]), by = list(geography, code)]
  fulldb[, eval(dep) := get(dep) / test2]
  fulldb[get(st) == 0, eval(dep) := 0]
  fulldb[, test2 := NULL]
}


# wages
fulldb[, totalpayroll := wages]
fulldb[, wages := 1000 * wages / employment]
fulldb[employment == 0, wages := 0]

# avghrs
fulldb[, avghrs := `hours worked` * 1000 / employment][, `hours worked` := NULL]
fulldb[employment == 0, avghrs := 0]

fulldb <- fulldb[code %in% naics$code]

#small fix to .001 vals (if I don't do this, the program will think I tagged them as missing values)
fulldb[fulldb == .001] <- .001000001


######################################################
# Step 14:                                        #####
# Calculations on Macro vars                     #####
######################################################

fulldb[, `durable goods-3840038-current prices` := `durable goods-3840038-current prices` + `semi-durable goods-3840038-current prices`]
fulldb[, `durable goods-3840038-chained (2012) dollars` := `durable goods-3840038-chained (2012) dollars` + `semi-durable goods-3840038-chained (2012) dollars`]



######################################################
# Step 15:                                        #####
# Reshape, add Mnemonics export                  #####
######################################################
# industry mnems
# fulldb[,sectormnems:=mnems$mnem[match(fulldb$code,mnems$code,nomatch=NA)]]
print("Prepare for export.")

# reshape
fulldbmelted <- melt(fulldb, id = c("geography", "year", "industry", "code")) # warning should be okay


fulldbmelted <- fulldbmelted[ !(code == "NAT" & !(variable %in% macrovars))]
fulldbmelted <- fulldbmelted[ !(code != "NAT" & (variable %in% macrovars))]

# Mnemonics
fulldbmelted[, `:=`
(
  variable = str_trim(variable, side = "both"),
  geography = str_trim(geography, side = "both")
)             ]

matchbase <- merge[!is.na(VarNeumonic)]


fulldbmelted[, `:=`
(
  varsymbol = matchbase$VarNeumonic[match(fulldbmelted$variable, matchbase$newvar, nomatch = NA)],
  pricesymbol = matchbase$PriceSymbol[match(fulldbmelted$variable, matchbase$newvar, nomatch = NA)],
  sectorcode = sectors$code[match(fulldbmelted$geography, sectors$geography, nomatch = NA)],
  sectormnems = mnems$mnem[match(fulldbmelted$code, mnems$code, nomatch = NA)],
  leveltag = mnems$Tag[match(fulldbmelted$code, mnems$code, nomatch = NA)]
)            ]
# get rid of any extra sectors
fulldbmelted <- fulldbmelted[!is.na(sectormnems) | code == 1 | code == "NAT"]
# get rid of top downs and aggregates in the delta and avhr series
# fulldbmelted<-fulldbmelted[!(leveltag %in% c("aggregate","topdown") & grepl("^K|DELTA|AVHR",varsymbol))]
# get rid of other assorted vars
fulldbmelted <- fulldbmelted[!(geography == "Canada" & grepl("consolidated provincial-territorial and local governments-3850042", variable))]
# get rid of any variables that we don't actually need (ie dont have mnemonics)
fulldbmelted <- fulldbmelted[!is.na(varsymbol)]

fulldbmelted[is.na(varsymbol), varsymbol := ""]
fulldbmelted[is.na(pricesymbol), pricesymbol := ""]
fulldbmelted[is.na(sectorcode), sectorcode := ""]
fulldbmelted[is.na(sectormnems), sectormnems := ""]


fulldbmelted[, `:=`
(mnemonic = paste(varsymbol, sectormnems, pricesymbol, sep = ""))            ][, c("pricesymbol", "varsymbol", "sectormnems") := NULL]


# reshape again
widedb <- dcast(fulldbmelted, geography + code + sectorcode + industry + variable + mnemonic ~ year)
# get rid of the uneeded intersection
macrovars <- str_trim(macrovars, side = "both")
forexport <- widedb[variable != "gross output - constant prices (x 1,000,000)"]
# forexport<-forexport[ !(code=="NAT" & !(variable %in% macrovars))]
# forexport<-forexport[ !(code!="NAT" &  (variable %in% macrovars))]

# remove NA series
nyear <- (max(as.numeric(fulldb$year))) - min(as.numeric(fulldb$year))
yearcols <- names(forexport)[7:(7 + nyear)]

############################# This command suppresses warnigns associated with the conversion to numeric
#        ###ATTENTION###    # The conversion is useful to A) perform further manipulations (if necessary)
#############################                             B) count the NAs and identify empty series
############################# Be sure that you do not want to keep any cells that are all characters

# create numerics (and all cells that are strings will be NAs)
suppressWarnings(forexport[, (yearcols) := lapply(.SD, function(y) as.numeric(gsub("[^0-9.]", "", y))), .SD = yearcols])
# temporary column that adds the NA values
forexport[, test := Reduce(`+`, lapply(.SD, function(x) is.na(x)))]
# exclude any row with the number of NA's equals the number of years
forexport <- forexport[test < nyear][, test := NULL]

# Now replace early NA's with .001 for model readable format
# first non-NA
forexport[, test := apply(.SD, 1, function(x) min(which(!is.na(x)))), .SD = (yearcols)]
# replace
for (i in seq_along(forexport[, (yearcols), with = F])) {
  set(forexport, which(forexport$test > i), as.integer(i + 6), value = .001)
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
# y<-lapply(transpose(forexport), function(x) (-100 %in% (x)))
# forexport[,k:=y]#apply(forexport, 1, function(x) any(-100 %in% x))]
# now get rid of everything before the gaps
# Now replace early NA's with .001 for model readable format
# first non-NA
suppressWarnings(forexport[, test := apply(.SD, 1, function(x) min(which(x == -100))), .SD = (yearcols)])
forexport[is.infinite(test), test := 0]
# replace
for (i in seq_along(forexport[, (yearcols), with = F])) {
  set(forexport, which(forexport$test > i), as.integer(i + 6), value = .001)
}
forexport[forexport == -100] <- .001

# now make sure there are no .001 in the middle of a series
suppressWarnings(forexport[, test := apply(.SD, 1, function(x) max(which(x == .001))), .SD = (yearcols)])
forexport[is.infinite(test), test := 0]
# replace
for (i in seq_along(forexport[, (yearcols), with = F])) {
  set(forexport, which(forexport$test > i), as.integer(i + 6), value = .001)
}



# make model readable
forexport[, test := Reduce(`+`, lapply(.SD, function(x) is.na(x)))]
forexport <- forexport[, !c("geography", "code", "industry", "variable"), with = F]
forexport[, `:=`
(
  V = "V",
  L = "L",
  pers = nyear - test + 1
)][, `:=`
(
  start = 198001,
  end = paste(1980 + pers - 1, "01", sep = "")
)][, `:=`
(
  header = "1.upload",
  pers = paste(pers, "@01", sep = ""),
  forecastend = end
)][, test := NULL]



setcolorder(forexport, c("header", "sectorcode", "mnemonic", "V", "L", "start", "end", "forecastend", "pers", setdiff(names(forexport), c("header", "sectorcode", "mnemonic", "V", "L", "start", "end", "forecastend", "pers"))))
forexport[is.na(forexport)] <- ""
yearcols <- names(forexport)[10:(10 + nyear)]

# remove gross output
forexport <- forexport[!grepl("^GY", mnemonic)]

# export
exportname <- "envcandb-filled"
rdsexport <- paste(exportname, ".rds", sep = "")
destname <- paste(dstDir, rdsexport, sep = "")
saveRDS(forexport, eval(destname))

csvexport <- paste(exportname, ".csv", sep = "")
destname <- paste(dstDir, csvexport, sep = "")
# write.csv(forexport, eval(destname), col.names = F)
write.table(forexport, file = eval(destname), row.names = FALSE, col.names = FALSE, sep = ",")
