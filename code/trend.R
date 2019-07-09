library(data.table)
library(rJava)
library(readxl)
library(xlsx)

# setwd("C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/hpfilter")
# srcDir     = "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/resources/"
# dstDir     = "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/outputs/"
options(scipen = 999)

hpfilter2 <- function(x, lambda = 6.25) {
  eye <- diag(length(x))
  result <- solve(eye + lambda * crossprod(diff(eye, lag = 1, d = 2)), x)
  return(result)
}

forexport <- readRDS(paste(dstDir, "envcandb-filled.rds", sep = ""))


dt <- forexport[(grepl("^K|^Y|^EMP|^AV", mnemonic) & !(grepl("^KD|^KME|^KIP|^KC", mnemonic)) | grepl("^KCON", mnemonic) | grepl("KMELC", mnemonic) | grepl("KCEM", mnemonic)) & !(grepl("!$", mnemonic))]
# rm(forexport)

# tag variable
dt[grepl("EMP", mnemonic), variable := "EMP"]
dt[grepl("^Y", mnemonic) , variable := "Y"]
dt[grepl("^AV", mnemonic), variable := "AVHR"]
dt[grepl("^K", mnemonic) , variable := "K"]
# tag sector
dt[variable == "EMP", ind := gsub("EMP", "", mnemonic)]
dt[variable == "Y", ind := gsub("^Y", "", mnemonic)]
dt[variable == "AVHR", ind := gsub("AVHR", "", mnemonic)]
dt[variable == "K", ind := gsub("K", "", mnemonic)]
# dt[ind=="", ind:="total"]
# remove extra info
dt[, c("header", "V", "L", "start", "end", "forecastend", "pers", "mnemonic") := NULL]
# reshape & fix name
dt <- suppressWarnings(melt(dt, id.vars = c("sectorcode", "variable", "ind"), variable.name = "year", variable.factor = FALSE))
dtsplit <- split(dt, by = c("ind", "sectorcode"))


dtsplit <- lapply(dtsplit, function(x) {
  # print(as.character(x[1,c(1,3)]))
  dcast(x, sectorcode + ind + year ~ variable)
})

# get rid of incomplete vars
missingcol <- lapply(dtsplit, function(x) ncol(x) == 7)
missingcol <- as.data.table(cbind(name = names(missingcol), "complete" = unlist(missingcol)))
missingcol <- missingcol[complete == FALSE]

foroperation <- dtsplit[!(names(dtsplit) %in% missingcol$name)]
foroperation <- rbindlist(foroperation)

# bring in coefficients
coefpath <- paste(srcDir, "YHATfactorshares.xlsx", sep = "")
coefs <- as.data.table(read_excel(coefpath, col_names = T))

# prepare coefs for merge
coefs <- melt(coefs, id.vars = c("X__1", "X__2"))
coefs[, X__1 := NULL]
names(coefs)[names(coefs) %in% c("X__2", "variable", "value")] <- c("ind", "sectorcode", "lcoef")

# temporary fix to add these aggregate sectors (should go back and fix the YHATfactorshares file)
coefs[ind == "TPO", ind := "TPCO"]
x <- coefs[ind %in% c("MPETA", "MNMO")]
x[[1]] <- c("MPETAO", "MNMAO")
coefs <- rbind(coefs, x)

# Calculate trend
foroperation <- merge(coefs, foroperation, by = c("ind", "sectorcode"), all.y = T)
nums <- c("lcoef", "AVHR", "EMP", "Y", "K")
foroperation[, (nums) := lapply(.SD, function(x) as.numeric(x)), .SDcols = nums]
foroperation[K == .001 | K == "", K := NA][Y == .001 | Y == "", Y := NA][EMP == .001 | EMP == "", EMP := NA][AVHR == .001 | AVHR == "", AVHR := NA]

foroperation[, `:=`
(
  kcoef = 1 - lcoef,
  labor = ifelse(is.finite(log(EMP * AVHR)) | is.na(log(EMP * AVHR)), log(EMP * AVHR), 0),
  capital = ifelse(is.finite(log(K)) | is.na(log(K)), log(K), 0),
  gva = ifelse(is.finite(log(Y)) | is.na(log(Y)), log(Y * 1000000), 0)
)][, TREND := gva - lcoef * labor - kcoef * capital]

head(foroperation)
trend <- foroperation[, c(1, 2, 4, 13)]

#
# read data
years <- as.numeric(max(as.character(trend$year))) - as.numeric(min(as.character(trend$year))) + 1
trend[, name := paste(sectorcode, ",TREND", ind, sep = "")][, c("ind", "sectorcode") := NULL]
trend <- dcast(trend, name ~ year, value.var = "TREND")

trend <- (setNames(data.table(t(trend[, -1])), as.character(trend[[1]])))
start <- min(as.numeric(foroperation$year))
end   <- max(as.numeric(foroperation$year))


DT2 <- data.table(year = start:end)
nyear <- end - start + 1
for (i in names(trend)) {
  #print(i)
  j <- paste(i, "-filtered", sep = "")
  # get series
  series <- trend[, get(i)]
  seriesomitna <- as.double(na.omit(trend[, get(i)]))
  # identify na's
  if (all(is.na(series))) {
    DT2[, (j) := series]
    DT2[, (i) := series]
    next
  }
  m <- nyear - length(seriesomitna)
  lastdata <- max(which(!is.na(series)))
  firstdata <- min(which(!is.na(series)))
  # these fixes will complete the series after stripping na's
  if (is.na(series[nyear])) {
    fix1 <- rep(NA, firstdata - 1)
    fix2 <- rep(NA, nyear - lastdata)
  } else {
    fix <- rep(NA, m)
  }
  # run filter
  if (!all(seriesomitna == 0)) {
    # filter3= hpfilter(seriesomitna,6.25,type="lambda", drift=FALSE)
    filtered <- hpfilter2(seriesomitna)
  }

  # bring back na's
  if (m != 0) {
    if (all(seriesomitna == 0)) {
      y3 <- series
    } else if (is.na(series[nyear])) {
      # y3 = c(fix1, filter3$trend,fix2)
      y3 <- c(fix1, filtered, fix2)
    } else {
      # y3 = c(fix, filter3$trend)
      y3 <- c(fix, filtered)
    }
  } else {
    y3 <- filtered
  }
  # add to new table
  DT2[, (i) := series]
  DT2[, (j) := y3]
}

missing <- list()
for (i in seq_along(DT2)) {
  if (all(is.na(DT2[[i]]))) {
    missing <- c(missing, names(DT2)[i])
  }
}

# prepare export#
# create table
trend <- setNames(data.table(name = names(DT2)[-1], t(DT2[, -"year"])), c("name", as.character(DT2[["year"]])))
yearcols <- names(trend)[2:((end - start) + 2)]

trend <- trend[grepl("-filtered", name)]
trend <- trend[!(name %in% missing)]
trend[, name := gsub("-filtered|total", "", name)]
# make everything numeric
suppressWarnings(trend[, (yearcols) := lapply(.SD, function(y) as.numeric(gsub("[^0-9.]", "", y))), .SD = yearcols])


# remerge with y,k,emp to re-do yhats
foryhat <- trend[, c("sectorcode", "ind") := tstrsplit(name, ",")][, ind := gsub("TREND", "", ind)][, name := NULL]
foryhat <- melt(foryhat, id.vars = c("sectorcode", "ind"), variable.name = "year", value.name = "TREND")

# calculate yhat and cumod
foroperation[, c("TREND") := NULL]
foryhatcalc <- merge(foryhat, foroperation, by = c("sectorcode", "ind", "year"), all.y = T)
foryhatcalc[, `:=`
(YHAT = (lcoef * labor + kcoef * capital + TREND))]
foryhatcalc[, CUMOD := (gva - YHAT) * 100]
foryhatcalc[, YHAT := ifelse(YHAT == 0, 0, exp(YHAT) / 1000000)]

# begin preparing for export
trend <- foryhatcalc[, c("sectorcode", "ind", "YHAT", "CUMOD", "TREND", "year"), with = F]
trend <- melt(trend, id.vars = c("sectorcode", "ind", "year"), variable.name = "varsymbol")
trend <- dcast(trend, sectorcode + ind + varsymbol ~ year)
trend[, mnemonic := paste(varsymbol, ind, sep = "")][, c("varsymbol", "ind") := NULL]


# for plotting:
# trend<-foryhatcalc[,c("sectorcode","ind", "YHAT","CUMOD","TREND","gva","year"),with=F]
# forplot<-trend[ind=="MNML" & sectorcode=="B_COLUMB"]
# plotcolors<-c("blue","red")
# plotelements<- c("YHAT", "gva")
# windows();
# par(mfrow=c(2,1))
# forplot[, matplot(year, .SD, type="l", ylab="value", xlab = "time",col=plotcolors),.SDcols=plotelements]
# forplot[, matplot(year, CUMOD, type="l", ylab="CUMOD", xlab = "time",col="black")][,abline(h = 0, lty = 2,col="red")]
#
# forplot[matplot(year, TREND, type="l", ylab="value", xlab = "time",col=plotcolors)]
# forplot[, matplot(year, .SD, type="l", ylab="value", xlab = "time",col=plotcolors),.SDcols=plotelements][,legend("bottomright",legend=plotelements,col=plotcolors,lty= 1:2,text.font=4,bg="lightblue")]


# kill early NAs
suppressWarnings(trend[, test := apply(.SD, 1, function(x) min(which(!is.na(x)))), .SD = (yearcols)])
# replace
for (i in seq_along(trend[, (yearcols), with = FALSE])) {
  set(trend, which(trend$test > i), as.integer(i + 1), value = .001)
}



# kill late NAs
trend[is.na(trend)] <- ""

# finish formatting
trend[, test := Reduce(`+`, lapply(.SD, function(x) x == ""))]
trend[, `:=`
(
  V = "V",
  L = "L",
  pers = nyear - test
)][, `:=`
(
  start = 198001,
  end = paste(1980 + pers, "01", sep = "")
)][, `:=`
(
  header = "1.upload",
  pers = paste(pers, "@01", sep = ""),
  forecastend = end
)][, "test" := NULL]

# trend[,c("sectorcode","mnemonic"):=tstrsplit(name,",")][,name:=NULL]

setcolorder(trend, c("header", "sectorcode", "mnemonic", "V", "L", "start", "end", "forecastend", "pers", setdiff(names(trend), c("header", "sectorcode", "mnemonic", "V", "L", "start", "end", "forecastend", "pers"))))
head(trend)
View(trend[all(.SD == .001), .SDcols = yearcols])

# trend[,test:=apply(.SD,1,function(x) all(0.001==as.numeric(x))),.SDcols=yearcols]

trend <- (trend[`2017` != "0.001"])

write.table(trend, file = paste(dstDir, "trend.csv", sep = ""), row.names = FALSE, col.names = FALSE, sep = ",")

# Create CUMOD forecasts:
cumods <- foryhatcalc[, .(sectorcode, ind, CUMOD, year)]
cumods <- cumods[!is.na(CUMOD)]
cumods <- cumods[, .SD[.N], by = list(sectorcode, ind)]
cumods[, `:=`(rate = CUMOD / 5, cumod1 = CUMOD)]


for (i in 2:6) {
  cumods[, paste("cumod", i, sep = "") := get(paste("cumod", i - 1, sep = "")) - rate]
}

cumods[, c("CUMOD", "rate") := NULL]

# break the table into the different end years (maybe just one)
cumod_list <- split(cumods, by = "year")

# create year names
years <- lapply(cumod_list, function(x) {
  y <- ifelse(is.na(as.numeric(as.character(x$year[1]))), NA, (as.numeric(as.character(x$year[1]))))
  z <- ifelse(is.na(as.numeric(as.character(x$year[1]))), NA, (as.numeric(as.character(x$year[1])) + 5))
  if (is.na(z)) {
    NA
  } else {
    y:z
  }
})
# apply names
cumod_list <- mapply(function(x, y) {
  if (!is.na(y[1])) {
    names(x)[names(x) %like% "cumod"] <- y
    x
  }
}, cumod_list, years,SIMPLIFY = FALSE)

cumod_list <- cumod_list[unlist(lapply(cumod_list, function(x) length(x) > 1))]
cumod_list <- lapply(cumod_list, function(x) x[, year := as.numeric(as.character(year))])
cumod_list <- lapply(cumod_list, function(x) x[, -4])

# write mnemonics
cumod_list <- lapply(cumod_list, function(x) x[, mnemonic := paste("CUMOD", ind, sep = "")][, ind := NULL])

cumod_list <- lapply(cumod_list, function(x) {
  x[is.na(x)] <- ""

  # finish formatting
  x[, test := Reduce(`+`, lapply(.SD, function(x) x == ""))]
  x[, `:=`
  (
    V = "V",
    L = "L",
    pers = 5
  )][, `:=`
  (
    start = paste(year + 1, "01", sep = ""),
    end = paste(year + 1 + pers, "01", sep = "")
  )][, `:=`
  (
    header = "1.upload",
    pers = paste(pers, "@01", sep = ""),
    forecastend = end
  )][, c("year", "test") := NULL]

  # x[,c("sectorcode","mnemonic"):=tstrsplit(name,",")][,name:=NULL]

  setcolorder(x, c("header", "sectorcode", "mnemonic", "V", "L", "start", "end", "forecastend", "pers", setdiff(names(x), c("header", "sectorcode", "mnemonic", "V", "L", "start", "end", "forecastend", "pers"))))
})

# export
cumod_forecasts <- cumod_list
lapply(cumod_forecasts, function(x) write.table(x, paste(dstDir, "cumod_forecasts.csv", sep = ""), append = T, col.names = F, row.names = F, sep = ","))

