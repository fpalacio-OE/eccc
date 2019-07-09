# This file interpolates the gross output series.
# For best results, run after the gapfill process.
# At the very least GVA needs to be fully interpolated



# srcDir  <- "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/resources/"
# dstDir  <- "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/outputs/"

options(scipen = 999)

#------------------------------------------------------------------------------------#
# 1.  Collect and reshape the relevant data to create "output                          #####
#------------------------------------------------------------------------------------#
fulldb <- readRDS(paste0(srcDir, "envcandb-full.rds"))
goutput <- fulldb[, c("geography", "code", "year", "gross output (x 1,000,000)"), with = F]
goutput <- melt(goutput, id.vars = c("geography", "code", "year"))


listpath <- paste(srcDir, "shortdb.rds", sep = "")
envcandbshort <- readRDS(eval(listpath))
rgoutput <- envcandbshort[, .(code, year, geography, `real gross output-3830032-constant prices`)]
setnames(rgoutput, "real gross output-3830032-constant prices", "gross output - constant prices (x 1,000,000)")
rgoutput <- melt(rgoutput, id.vars = c("geography", "code", "year"))


# merge tables
output <- rbind(rgoutput, goutput)
output <- dcast(output, geography + code + year ~ variable)

output[, `gross output (x 1,000,000)` := as.numeric(`gross output (x 1,000,000)`)]
output[, `gross output - constant prices (x 1,000,000)` := as.numeric(`gross output - constant prices (x 1,000,000)`)]

# sector 112 fix
ac_val <- output[code == 111 & geography == "Canada", `gross output - constant prices (x 1,000,000)`]
output[code == 112 & geography == "Canada", `gross output - constant prices (x 1,000,000)` := ac_val]


output[,  level := shiftshares$level[match(code, shiftshares$code, nomatch = NA)]]
foravg <- output[geography == "Canada" & level == 2]
foravg[,  sumoutput := sum(`gross output (x 1,000,000)`, na.omit = T), by = "year"][, weights := `gross output (x 1,000,000)` / sumoutput][, comp := weights * `gross output - constant prices (x 1,000,000)`][, final := sum(comp, na.rm = T), by = year]
adj    <- as.numeric(unique(foravg[year == base, 100 - final]))
foravg$final[foravg$final == 0] <- NA
foravg <- foravg[code == 11, final := final + adj]


output[geography == "Canada" & code == 1, `gross output - constant prices (x 1,000,000)` := foravg$final[foravg$code == 11]][, level := NULL]

#------------------------------------------------------------------------------------#
# 2.  Use base year nominal vlaues to create a national-level series from the raw index####
#------------------------------------------------------------------------------------#
# get growthrates on the index(real)
output[, rgrowth := c(NA, exp(diff(log(`gross output - constant prices (x 1,000,000)`)))), by = list(geography, code)]

# apply the growth rate to the nominal series, base year forward and base year backwards

output[year == base, try := `gross output (x 1,000,000)`]
output <- output[order(geography, code, year)]
for (y in (base + 1):max(output$year)) {
  # print(y)
  suppressWarnings(
    output[year == y, try := output[year == y - 1, try] * (output[year == y, rgrowth])]
  )
}


for (y in (base - 1):min(output$year)) {
  # print(y)
  suppressWarnings(
    output[year == y, try := output[year == y + 1, try] / (output[year == y + 1, rgrowth])]
  )
}

output[, `gross output - constant prices (x 1,000,000)` := try] # [,c("try", "gross output (x 1,000,000)","rgrowth"):=NULL]

# now, run this through the series smash, or just pasted below:

variables <- c("gross output - constant prices (x 1,000,000)", "gross output (x 1,000,000)")
omitmacro <- c("year", "code", "geography", variables)
omitgva <- omitmacro[!(omitmacro %like% "gva")]
rm(list = ls(pattern = "subgroup"))

#------------------------------------------------------------------------------------#
#  -- Flesh out the Canada level data using GVA and umbrella growth rates --           #####
#------------------------------------------------------------------------------------#
finalgvaseries <- readRDS(paste(srcDir, "finalgvaseries.rds", sep = ""))

#------------------------------------------------------------------------------------#
# 3.  Use GVA to gross output proportions in base year to expand the real gross output #### 
#     series at Canada level                                                         ####
#------------------------------------------------------------------------------------#
# Merge using early database where we imposed growth rates
variablesformerge <- output
variablesformerge <- variablesformerge[, ..omitgva]
setkey(variablesformerge, geography, code, year)

# bring in the latest gva
# finalgvaseries<-dcast(finalgvaseries, geography+code+year~variable)
setkey(finalgvaseries, geography, code, year)

# merge
GVAallocation <- finalgvaseries[variablesformerge]

# zero out nominal output where gva is 0
GVAallocation[`gva - current prices (x 1,000,000)` == 0, `gross output (x 1,000,000)` := 0]

## To calculate GVA shares (real and nominal)##

# bring back the umbrellas
GVAallocation[, umbrella := shiftshares$umbrella[match(GVAallocation$code, shiftshares$code, nomatch = NA)]]

# now real
variableschained <- c("gross output - constant prices (x 1,000,000)")

# calculate gross output/gvachained proportions
GVAallocation[year == base, `gross output - constant prices (x 1,000,000)` := `gross output (x 1,000,000)`][, share := ifelse(`gross output - constant prices (x 1,000,000)` == 0 | `gva - chained prices (x 1,000,000)` == 0, 0,
  `gross output - constant prices (x 1,000,000)` / `gva - chained prices (x 1,000,000)`
)]

# set them constant over the time period
formatch      <- GVAallocation[geography == "Canada" & year == base, .(code, geography, share)]
GVAallocation <- formatch[GVAallocation, on = .(code, geography)]
# use these proportions to calculate real gross output
GVAallocation[, share := ifelse(is.na(share), i.share, share)][is.na(`gross output - constant prices (x 1,000,000)`), `:=`
(
  `gross output - constant prices (x 1,000,000)` = `gva - chained prices (x 1,000,000)` * share
)][,i.share := NULL]

#------------------------------------------------------------------------------------#
# 4.  Use GVA shares to split out the real gross output series                       ####
#------------------------------------------------------------------------------------#
# var="gross output - constant prices (x 1,000,000)"
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

# saveRDS(GVAallocation,paste(srcDir,"step3.rds",sep=""))

#------------------------------------------------------------------------------------#
# 5.  Use parent sector growth rates to extend series and fill remaining gaps        #####
#------------------------------------------------------------------------------------#
step5db <- GVAallocation

variables <- omitgva[!(omitgva %in% c("geography", "code", "year"))]


for (i in 1:5) {
  for (var in variables) {
    print(var)
    # var="investment - chained prices (x 1,000,000)"
    # calculate
    suppressWarnings(
      step5db[, `:=`
      (
        umbrella = shiftshares$umbrella[match(step5db$code, shiftshares$code, nomatch = NA)],
        try = as.numeric(NA)
      )              ][, `:=`
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
        (try = ifelse(is.na(step5db[year == y, get(var)]), step5db[year == y - 1, try] * (step5db[year == y, umbrellagrowth]), step5db[year == y, get(var)]))                ]
      )
    }

    # assign and get rid of extrainfo
    step5db[, eval(var) := try]
  }
}

dbformerge <- step5db[, c("umbrella", "umbrellaval", "umbrellagrowth", "try") := NULL]
dbformerge <- melt(dbformerge, id = c("geography", "year", "code"))
dbformerge <- dbformerge[, variable := as.character(variable)]
dbformerge <- dbformerge[order(variable, geography, code)]

#------------------------------------------------------------------------------------#
#  -- Start filling in other geographies                                  --         ####
#------------------------------------------------------------------------------------# 

#------------------------------------------------------------------------------------#
# 6.  Calculate output to GVA ratio in Canada and apply it to the other geographies  ####
#------------------------------------------------------------------------------------#

#prepare table
forgrowth <- dcast(dbformerge, code + geography + year ~ variable)
forgrowth[, try := `gross output - constant prices (x 1,000,000)`]

#calculate national proportions and give it to the the other geographies
forgrowth[geography == "Canada", natprop := ifelse(is.infinite(`gross output - constant prices (x 1,000,000)` / `gva - chained prices (x 1,000,000)`) | is.nan(`gross output - constant prices (x 1,000,000)` / `gva - chained prices (x 1,000,000)`), 0, `gross output - constant prices (x 1,000,000)` / `gva - chained prices (x 1,000,000)`) ]
forgrowth[, natprop := natprop[geography == "Canada"], by = list(code, year)]

#calculate real gross output using national proportions
forgrowth[geography != "Canada" & is.na(`gross output - constant prices (x 1,000,000)`), try := natprop * `gva - chained prices (x 1,000,000)`]
forgrowth[, natprop := NULL]

rgrossoutputtable <- forgrowth

#------------------------------------------------------------------------------------#
# 7.  Re-scale to get the horizontal tie-up                                          ####
#------------------------------------------------------------------------------------#

# now calculate national level sums
rgrossoutputtable[                     , level := shiftshares$level[match(code, shiftshares$code, nomatch = NA)]]
rgrossoutputtable[geography != "Canada", totals := sum(try), by = list(year, code)]
rgrossoutputtable[                     , nattotals := `gross output - constant prices (x 1,000,000)`[geography == "Canada"], 
                                         by = list(code, year)]

# scale
rgrossoutputtable[, propdiff := nattotals / totals]
rgrossoutputtable[, try2     := propdiff  * try   ]

# apply
rgrossoutputtable[year != base & geography != "Canada", `gross output - constant prices (x 1,000,000)` := try2]

# there's a problem in this gva data for these years so erasing it
rgrossoutputtable[code %in% c("113", "114", "115") & year <= 1985, c("gva - chained prices (x 1,000,000)", "gross output - constant prices (x 1,000,000)", "gross output (x 1,000,000)", "gva - current prices (x 1,000,000)") := NA]

rgrossoutputtable <- rgrossoutputtable[, c("year", "code", "geography", "gva - chained prices (x 1,000,000)", "gross output - constant prices (x 1,000,000)", "gross output (x 1,000,000)", "gva - current prices (x 1,000,000)"), with = F]

#------------------------------------------------------------------------------------#
# 8.  Calculate Price indeces & use these to fix 0 mismatches                        #####
#------------------------------------------------------------------------------------#

fulldb        <- rgrossoutputtable
umbrellasects <- unique(shiftshares$umbrella)[!is.na(unique(shiftshares$umbrella)) & unique(shiftshares$umbrella) != "NAT"]
matchbase     <- as.data.table(naics[, cbind(code, alias1)])

# first id the exceptions
exceptions <- (fulldb[(`gva - chained prices (x 1,000,000)` != 0 & `gva - current prices (x 1,000,000)` == 0) | (`gva - chained prices (x 1,000,000)` == 0 & `gva - current prices (x 1,000,000)` != 0)])
exceptions[, toreplace := ifelse(`gva - chained prices (x 1,000,000)` == 0, "real", "nominal")]
exceptions <- unique(exceptions[, c("geography", "code", "toreplace", "year")])

# keep only lowest levels
# make umbrellas
exceptions[, umbrella := shiftshares$umbrella[match(exceptions$code, shiftshares$code, nomatch = NA)]]

exceptions[, test := match(code, umbrella), by = list(toreplace, geography)]
exceptions       <- exceptions[is.na(test)][, c("test", "umbrella") := NULL]

listforfirststep <- paste(exceptions$geography, exceptions$code)

##  Calculate PGDP ##

fulldb[, pgdp := ifelse(`gva - current prices (x 1,000,000)` == 0, 0, `gva - current prices (x 1,000,000)` / `gva - chained prices (x 1,000,000)` * 100)]

# bring in the replacement deflators (from Alberta)
test <- fulldb[paste(geography, code) %in% listforfirststep]
test <- merge(test, exceptions, by = c("geography", "code", "year"), allow.cartesian = T)
test <- merge(test[, !"pgdp", with = F], fulldb[geography == "Alberta", c("pgdp", "code", "year"), with = F], by = c("year", "code"))
# use new deflators to correct gvas where necessary & calculate differences (the differences are the important bit)
test[, newnominalgva := ifelse(toreplace == "nominal" & `gva - chained prices (x 1,000,000)` != 0, pgdp * `gva - chained prices (x 1,000,000)` / 100, `gva - current prices (x 1,000,000)`)][
  ,
  changenom := newnominalgva - `gva - current prices (x 1,000,000)`
]
test[, newrealgva := ifelse(toreplace == "real" & `gva - current prices (x 1,000,000)` != 0, `gva - current prices (x 1,000,000)` * 100 / pgdp, `gva - chained prices (x 1,000,000)`)][
  ,
  changereal := newrealgva - `gva - chained prices (x 1,000,000)`
]

test[, c("newnominalgva", "newrealgva", "toreplace") := NULL]

# bring the updated values back to the fullset, first delete the exception rows from the full set
correctedbase <- rbind(fulldb[!paste(geography, year, code) %in% paste(test$geography, test$year, test$code)], test, fill = T)

# add the new gva to the umbrellas
correctedbase[, umbrella := shiftshares$umbrella[match(correctedbase$code, shiftshares$code, nomatch = NA)]]

for (i in rev(umbrellasects)) {
  # moving from lowest sectors up

  # calculate
  sect <- shiftshares[umbrella %in% i, code]
  sums <- c("sumnom", "sumreal")
  correctedbase[ code %in% sect, (sums) := list(sum(changenom, na.rm = T), sum(changereal, na.rm = T)), by = list(geography, year)]

  # paste to umbrella sector
  namesformerge <- c("geography", "code", "year", sums)
  y <- correctedbase[code %in% sect[1], ..namesformerge][, code := as.character(i)]
  correctedbase[, (sums) := NULL]
  setkey(correctedbase, code, geography, year)
  setkey(y, code, geography, year)

  correctedbase <- y[correctedbase]

  # add to the chagne column
  changes <- c("changenom", "changereal")
  correctedbase[, (changes) := list(ifelse(is.na(changenom), sumnom, changenom), ifelse(is.na(changereal), sumreal, changereal))]

  # clean
  correctedbase[, (sums) := NULL]
}

correctedbase[, `:=`
(
  `gva - chained prices (x 1,000,000)` = ifelse(is.na(changereal), `gva - chained prices (x 1,000,000)`, `gva - chained prices (x 1,000,000)` + changereal),
  `gva - current prices (x 1,000,000)` = ifelse(is.na(changenom), `gva - current prices (x 1,000,000)`, `gva - current prices (x 1,000,000)` + changenom)
)              ][
  ,
  c(changes, "umbrella", "gross output - constant prices (x 1,000,000)", "gross output (x 1,000,000)") := NULL
]

# finally recalculate the deflator:
correctedbase[, pgdp := ifelse(`gva - current prices (x 1,000,000)` == 0, 0, `gva - current prices (x 1,000,000)` / `gva - chained prices (x 1,000,000)` * 100)]
#correctedbase[, p := NULL]

## Now calculate output price ##

fulldb[, pgdp := NULL]
fix_zeros_output <- function(fulldb) {
  # first id the exceptions
  exceptions <- (fulldb[(`gross output - constant prices (x 1,000,000)` != 0 & `gross output (x 1,000,000)` == 0) | (`gross output - constant prices (x 1,000,000)` == 0 & `gross output (x 1,000,000)` != 0)])
  exceptions[, toreplace := ifelse(`gross output - constant prices (x 1,000,000)` == 0, "real", "nominal")]
  exceptions <- unique(exceptions[, c("geography", "code", "toreplace", "year")])

  # keep only lowest levels
  # make umbrellas
  exceptions[, umbrella := shiftshares$umbrella[match(exceptions$code, shiftshares$code, nomatch = NA)]]

  exceptions[, test := match(code, umbrella), by = list(toreplace, geography, year)]
  exceptions <- exceptions[is.na(test)][, c("test", "umbrella") := NULL]

  # first just
  exceptionsforfirstmerge <- unique(exceptions[, c("geography", "code")])


  # Calculate p
  fulldb[, p := ifelse(`gross output (x 1,000,000)` == 0, 0, `gross output (x 1,000,000)` / `gross output - constant prices (x 1,000,000)` * 100)]

  # bring in the replacement deflators (from Canada) (above we used Alberta, but Alberta has some exceptions here)
  test <- fulldb[paste(geography, code) %in% paste(exceptionsforfirstmerge$geography, exceptionsforfirstmerge$code)]
  test <- merge(test, exceptions, by = c("geography", "code", "year"), allow.cartesian = T)
  test <- merge(test[, !"p", with = F], fulldb[geography == "Canada", c("p", "code", "year"), with = F], by = c("year", "code"))

  # use new deflators to correct gvas where necessary & calculate differences (the differences are the important bit)
  test[, newnominalgva := ifelse(toreplace == "nominal" & `gross output - constant prices (x 1,000,000)` != 0, p * `gross output - constant prices (x 1,000,000)` / 100, `gross output (x 1,000,000)`)][
    ,
    changenom := newnominalgva - `gross output (x 1,000,000)`
  ]
  test[, newrealgva := ifelse(toreplace == "real" & `gross output (x 1,000,000)` != 0, `gross output (x 1,000,000)` * 100 / p, `gross output - constant prices (x 1,000,000)`)][
    ,
    changereal := newrealgva - `gross output - constant prices (x 1,000,000)`
  ]

  test[, c("newnominalgva", "newrealgva", "toreplace") := NULL]

  # bring the updated values back to the fullset, first delete the exception rows from the full set
  correctedbase2 <- rbind(fulldb[!paste(geography, year, code) %in% paste(test$geography, test$year, test$code)], test, fill = T)

  # add the new gva to the umbrellas
  correctedbase2[, umbrella := shiftshares$umbrella[match(correctedbase2$code, shiftshares$code, nomatch = NA)]]

  for (i in rev(umbrellasects)) {
    # moving from lowest sectors up

    # calculate
    sect <- shiftshares[umbrella %in% i, code]
    sums <- c("sumnom", "sumreal")
    correctedbase2[ code %in% sect, (sums) := list(sum(changenom, na.rm = T), sum(changereal, na.rm = T)), by = list(geography, year)]

    # paste to umbrella sector
    namesformerge <- c("geography", "code", "year", sums)
    y <- correctedbase2[code %in% sect[1], ..namesformerge][, code := as.character(i)]
    correctedbase2[, (sums) := NULL]
    setkey(correctedbase2, code, geography, year)
    setkey(y, code, geography, year)

    correctedbase2 <- y[correctedbase2]

    # add to the chagne column
    changes <- c("changenom", "changereal")
    correctedbase2[, (changes) := list(ifelse(is.na(changenom), sumnom, changenom), ifelse(is.na(changereal), sumreal, changereal))]

    # clean
    correctedbase2[, (sums) := NULL]
  }

  correctedbase2[, `:=`
  (
    `gross output - constant prices (x 1,000,000)` = ifelse(is.na(changereal), `gross output - constant prices (x 1,000,000)`, `gross output - constant prices (x 1,000,000)` + changereal),
    `gross output (x 1,000,000)` = ifelse(is.na(changenom), `gross output (x 1,000,000)`, `gross output (x 1,000,000)` + changenom)
  )                ][
    ,
    c(changes, "gva - chained prices (x 1,000,000)", "gva - current prices (x 1,000,000)", "umbrella") := NULL
  ]
  # recalculate  p
  correctedbase2[, p := ifelse(`gross output (x 1,000,000)` == 0, 100, `gross output (x 1,000,000)` / `gross output - constant prices (x 1,000,000)` * 100)]
}
correctedbase2 <- fix_zeros_output(fulldb)
# finally, merge the correctedbases
fulldb <- merge(correctedbase, correctedbase2, by = c("geography", "code", "year"))

# Replicate the above but to make sure that if gva is non-zero then gross output is non-zero


# first id the exceptions
exceptions <- (fulldb[(`gross output - constant prices (x 1,000,000)` == 0 & `gva - chained prices (x 1,000,000)` != 0)])
exceptions[,   toreplace := "real"]
exceptions <- unique(exceptions[, c("geography", "code", "toreplace", "year")])

# keep only lowest levels
# make umbrellas
exceptions[, umbrella := shiftshares$umbrella[match(exceptions$code, shiftshares$code, nomatch = NA)]]

exceptions[, test := match(code, umbrella), by = list(toreplace, geography, year)]
exceptions <- exceptions[is.na(test)][, c("test", "umbrella") := NULL]

# first just
exceptionsforfirstmerge <- unique(exceptions[, c("geography", "code")])

# keep years for later
tags <- exceptions[, .(geography, code, year)]
tags[, tag := 1]
# Calculate p
fulldb[, ratio := `gross output - constant prices (x 1,000,000)` / `gva - chained prices (x 1,000,000)`]

# bring in the replacement ratios (from Canada) (above we used Alberta, but Alberta has some exceptions here)
test <- fulldb[paste(geography, code) %in% paste(exceptionsforfirstmerge$geography, exceptionsforfirstmerge$code)]
test <- merge(test, exceptions, by = c("geography", "code", "year"), allow.cartesian = T)
test <- merge(test[, !"ratio", with = F], fulldb[geography == "Canada", c("ratio", "code", "year"), with = F], by = c("year", "code"))

# use new deflators to correct gvas where necessary & calculate differences (the differences are the important bit)
# test<-test[tags, on=list(geography,code,year)]
test[, newrealoutput := ifelse(toreplace == "real", ratio * `gva - chained prices (x 1,000,000)`, `gross output - constant prices (x 1,000,000)`)][
  ,
  changereal := newrealoutput - `gross output - constant prices (x 1,000,000)`
]

test[, c("newrealoutput", "toreplace") := NULL]

# bring the updated values back to the fullset, first delete the exception rows from the full set
correctedbase3 <- rbind(fulldb[!paste(geography, year, code) %in% paste(test$geography, test$year, test$code)], test, fill = T)

# add the new gva to the umbrellas
correctedbase3[, umbrella := shiftshares$umbrella[match(correctedbase3$code, shiftshares$code, nomatch = NA)]]

for (i in rev(umbrellasects)) {
  # moving from lowest sectors up

  # calculate
  sect <- shiftshares[umbrella %in% i, code]
  sums <- c("sumreal")
  correctedbase3[ code %in% sect, (sums) := list(sum(changereal, na.rm = T)), by = list(geography, year)]

  # paste to umbrella sector
  namesformerge <- c("geography", "code", "year", sums)
  y <- correctedbase3[code %in% sect[1], ..namesformerge][, code := as.character(i)]
  correctedbase3[, (sums) := NULL]
  setkey(correctedbase3, code, geography, year)
  setkey(y, code, geography, year)

  correctedbase3 <- y[correctedbase3]

  # add to the chagne column
  changes <- c("changereal")
  correctedbase3[, (changes) := list(ifelse(is.na(changereal), sumreal, changereal))]

  # clean
  correctedbase3[, (sums) := NULL]
}

correctedbase3[, `:=`
(`gross output - constant prices (x 1,000,000)` = ifelse(is.na(changereal), `gross output - constant prices (x 1,000,000)`, `gross output - constant prices (x 1,000,000)` + changereal))               ][
  ,
  c(changes, "umbrella", "ratio") := NULL
]

# finally, merge the correctedbases
fulldb <- correctedbase3

# now apply the fix to the nominal series
correctedbase4 <- fix_zeros_output(fulldb)

fulldb <- merge(fulldb[, .(geography, code, year, `gva - chained prices (x 1,000,000)`, `gva - current prices (x 1,000,000)`)], correctedbase4, by = c("code", "year", "geography"))

#------------------------------------------------------------------------------------#
# 9.  Prepare for export                                                             #####
#------------------------------------------------------------------------------------#

# reshape
fulldbmelted <- melt(fulldb, id = c("geography", "year", "code")) # warning should be okay

# Mnemonics
matchbase <- merge[!is.na(VarNeumonic)]

fulldbmelted[, `:=`
(
  variable = str_trim(variable, side = "both"),
  geography = str_trim(geography, side = "both")
)             ]
fulldbmelted <- fulldbmelted[code %in% mnems$code]
fulldbmelted[, `:=`
(
  varsymbol = matchbase$VarNeumonic[match(fulldbmelted$variable, matchbase$newvar, nomatch = NA)],
  pricesymbol = matchbase$PriceSymbol[match(fulldbmelted$variable, matchbase$newvar, nomatch = NA)],
  sectorcode = sectors$code[match(fulldbmelted$geography, sectors$geography, nomatch = NA)],
  sectormnems = mnems$mnem[match(fulldbmelted$code, mnems$code, nomatch = NA)]
)             ]
fulldbmelted[variable == "p", varsymbol := "P"]
fulldbmelted[variable == "pgdp", varsymbol := "PGDP"]
fulldbmelted <- fulldbmelted[!is.na(varsymbol)]


fulldbmelted[is.na(varsymbol), varsymbol := ""]
fulldbmelted[is.na(pricesymbol), pricesymbol := ""]
fulldbmelted[is.na(sectorcode), sectorcode := ""]
fulldbmelted[is.na(sectormnems), sectormnems := ""]

fulldbmelted[, `:=`
(mnemonic = paste(varsymbol, sectormnems, pricesymbol, sep = ""))             ][, c("pricesymbol", "varsymbol", "sectormnems") := NULL]


# reshape again
widedb <- dcast(fulldbmelted, geography + code + sectorcode + variable + mnemonic ~ year)
# get rid of the uneeded intersection
macrovars <- str_trim(macrovars, side = "both")
forexport <- widedb
forexport <- forexport[ !(code == "NAT" & !(variable %in% macrovars))]
forexport <- forexport[ !(code != "NAT" & (variable %in% macrovars))]

# remove NA series
nyear    <- as.numeric(max(fulldb$year)) - as.numeric(min(fulldb$year))
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

# fix a couple of mnemonics
forexport <- forexport[mnemonic != "PGDP"]
forexport[mnemonic == "P", mnemonic := "PPI"]

forexport <- forexport[!grepl("^Y", mnemonic)]
#------------------------------------------------------------------------------------#
# 10. Export                                                                         ####
#------------------------------------------------------------------------------------#

# exportname= "final-goutput2"
rdsexport <- paste(exportname, ".rds", sep = "")
destname <- paste(dstDir, rdsexport, sep = "")
saveRDS(forexport, eval(destname))

csvexport <- paste(exportname, ".csv", sep = "")
destname <- paste(dstDir, csvexport, sep = "")
write.table(forexport, file = eval(destname), row.names = FALSE, col.names = FALSE, sep = ",")
