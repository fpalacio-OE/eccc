

AttachMnems <- function(fulldbmelted,srcDir){
  codedir <- "Directory-namestonaicscodes-2.xlsx" 
  smerge  <- "SeriesMerge"
  naicsdir <- paste(srcDir, eval(codedir), sep = "")
  mnems   <- as.data.table(read_excel(naicsdir, na = "NA", sheet = "FinalMnem", col_names = TRUE))
  

  merge    <- as.data.table(read_excel(naicsdir, na = "NA", sheet = eval(smerge), col_names = TRUE))
  
  fulldbmelted <- fulldbmelted[ !(code == "NAT" & !(variable %in% macrovars))]
  fulldbmelted <- fulldbmelted[ !(code != "NAT" & (variable %in% macrovars))]
  
  # Mnemonics
  fulldbmelted[, `:=`
               (
                 variable = str_trim(variable, side = "both"),
                 geography = str_trim(geography, side = "both")
               )]
  
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
               (mnemonic = paste(varsymbol, sectormnems, pricesymbol, sep = ""))            ][, c("pricesymbol", "varsymbol", "sectormnems","leveltag") := NULL]
}

