# import modules
require("httr")
require("jsonlite")
library("tidyr")
library("plyr")
library("data.table")
library("stringr")
library("readxl")
library("pbapply")
# program constants
#


BASE <- "https://www150.statcan.gc.ca/t1/wds/rest"


# this function makes the necessary HTTP calls for downloading
# and combining raw data
# returns: dataframe
#
download_data <- function(vectorIds, startDate, endDate) {
  vectorIds <- vectorIds[!is.na(vectorIds)]
  # select resource endpoint, in this case /download

  endpoint <- "/getBulkVectorDataByRange"

  postBody <- toJSON(list(
    vectorIds = vectorIds,
    startDataPointReleaseDate = startDate,
    endDataPointReleaseDate = endDate
  ), auto_unbox = TRUE, pretty = TRUE)

  #print(postBody)

  http_request <- paste(BASE, endpoint, sep = "")

  http_response <- POST(
    http_request,
    add_headers("Content-Type" = "application/json"),
    body = postBody
  )

  if (status_code(http_response) > 400) {
    print("Error - couldn't download selection")
    print(paste("Status code: ", status_code(http_response)))
    print(paste("Url: ", http_request))
    return(NULL)
  }

  new_data <- (fromJSON(content(http_response, "text", flatten = TRUE)))


  return(new_data)
}
# this cleans data
clean_data_outputs <- function(downloadeddata) {
  data <- downloadeddata
  
  if (is.null(ncol(data))) {
    data <- (data[lapply(data,length) == 5])
    data <- as.data.table((lapply(data, function(x) x[1:5])))
    data <- as.data.table(t(matrix(do.call(c, data), nrow = 5)))
    setnames(data, paste("V", 1:5, sep = ""), c("status", "responseStatusCode", "productId", "coordinate", "vectorId"))
  }

  if (ncol(data) < 5) {
    data <- as.data.table(cbind(data$status, data$object))
    data <- data[V1 == "SUCCESS"]
    cols <- as.data.table(lapply(data$V2, function(x) x[1:4]))
    cols <- t(matrix(do.call(c, unlist(cols, recursive = F)), nrow = 4))
    vectorDataPoint <- (lapply(data$V2, function(x) as.data.table(x[5])))
    data <- as.data.table(cbind(data$V1, cols, vectorDataPoint))
    setnames(data, paste("V", 1:5, sep = ""), c("status", "responseStatusCode", "productId", "coordinate", "vectorId"))
    invisible(lapply(data$vectorDataPoint, function(x) setnames(x, names(x), gsub("vectorDataPoint.", "", names(x)))))
  }
  data <- as.data.table(data)
  print(data[1, ])

  # collect errors
  dataerrors <- data[responseStatusCode != 0]
  data <- data[responseStatusCode == 0]
  # collect adjusted tables
  datatables <- lapply(data$vectorDataPoint, function(x) as.data.table(x))
  # collect product ids (table numbers)
  product_ids <- data$productId
  # collect coordinates
  coordinates <- data$coordinate
  # collect vectors
  vectors <- data$vectorId
  # add vector tags
  invisible(mapply(function(x, y, z, w) x[, `:=`(productId = z, vector = y, coordinate = w)], datatables, vectors, product_ids, coordinates))
  # keep only useful columns
  datatables <- lapply(datatables, function(x) x[, c("refPer", "value", "scalarFactorCode", "frequencyCode", "vector", "coordinate", "productId"), with = F])

  data_and_errors <- list(datatables, dataerrors)
  return(data_and_errors)
}

# this downloads metadata
download_metadata_pID <- function(product_ID) {
  # select resource endpoint, in this case /download
  endpoint <- "/getCubeMetadata"

  new_data_list <- list()

  item <- 1
  for (i in product_ID) {
    postBody <- paste("[", toJSON(
      list(
        productId = i
      ),
      auto_unbox = TRUE, pretty = T
    ), "]", sep = "")

    print(postBody)

    http_request <- paste(BASE, endpoint, sep = "")

    http_response <- POST(
      http_request,
      add_headers("Content-Type" = "application/json"),
      body = postBody
    )

    if (status_code(http_response) > 400) {
      print("Error - couldn't download selection")
      print(paste("Status code: ", status_code(http_response)))
      print(paste("Url: ", http_request))
      print(paste("Vector: ", i))
      return(NULL)
    }

    new_data <- fromJSON(content(http_response, "text", flatten = TRUE))
    new_data <- as.data.table(new_data$object)

    new_data_list[[item]] <- new_data

    Sys.sleep(5)
    item <- item + 1
  }
  return(new_data_list)
}

# this cleans the metadata
clean_metadata_outputs <- function(raw_metadata) {
  if (length(raw_metadata) == 1) {
    print(raw_metadata)
    return()
  } else {
    # hold relevant metadata
    suppressWarnings(metadata <- (melt(raw_metadata, id.vars = c("productId", "cansimId", "cubeTitleEn"))))
    # dimensions
    dimensions <- as.data.table(metadata$value[metadata$variable == "dimension"])

    # bring info down one level
    productid <- unique(metadata$productId)
    print(productid)
    cansimid <- unique(metadata$cansimId)
    cubetitle <- unique(metadata$cubeTitleEn)
    dimensions[, `:=`(productId = productid, cansimId = cansimid, cubeTitleEn = cubetitle)]

    # clean and split
    members <- lapply(dimensions$member, as.data.table)
    members <- lapply(members, function(y) y[, c("memberNameFr", "memberUomCode") := NULL])

    infobank <- split(dimensions[, !c("hasUom", "member", "dimensionNameFr"), with = F], by = "dimensionPositionId")

    # collapse info
    test <- mapply(
      function(x, w) {
        position <- w$dimensionPositionId
        name <- w$dimensionNameEn
        x[, `:=`(productId = productid, cansimId = cansimid, cubeTitleEn = cubetitle, dim_name = name, dim = position, geoLevel = NULL, vintage = NULL, classificationTypeCode = NULL, classificationCode = NULL, parentMemberId = NULL)]
      }, members, infobank
    )




    mergedblock <- members[[1]]
    if (length(members) > 1) {
      for (j in 2:length(members)) {
        mergedblock <- merge(mergedblock, members[[j]], by = c("cansimId", "cubeTitleEn", "productId"), allow.cartesian = T)
        names(mergedblock) <- gsub(".x", "", names(mergedblock))
        names(mergedblock) <- gsub(".y", j, names(mergedblock))
      }
    }

    out <- mergedblock[, coordinate := do.call(paste, c(.SD, sep = ".")), .SDcols = names(mergedblock)[names(mergedblock) %like% "memberId"]]
  }
}

# this merges the data
merge_data_meta <- function(cleaned_data, cleaned_metadata) {
  cleaneddata <- as.data.table(rbindlist(lapply(cleaned_data, rbindlist)))
  cleanedmetadata <- as.data.table(rbindlist(cleaned_metadata, fill = T))
  # house keeping
  data <- cleaneddata[, coordinate := as.character(gsub("\\.0", "", coordinate))]
  data <- data[, productId := as.character(productId)]
  # merge
  final <- merge(data, cleanedmetadata, by = c("coordinate", "productId"), all.x = T, allow.cartesian = T)
  # clean and condense
  final[, terminated := rowSums(.SD, na.rm = T), .SDcols = names(final)[names(final) %like% "terminated"]][, term := ifelse(terminated > 0, 1, 0)]
  final <- final[, !names(final)[names(final) %like% "memberId" | names(final) %like% "terminated"], with = F]
  
  final <- final[memberNameEn3 != "Balance of payments" | is.na(memberNameEn3)] #don't really remember why this is done...
}

final_cleaning <- function(final) {
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

  tradetables <- c("3860003", "2280064", "3800070")
  final[, refPer := as.Date(refPer)]
  final[, year := year(refPer)]

  final[, cubeTitleEn := tolower(cubeTitleEn)]
  # attach aggregation codes
  final <- merge(final, unique(aggregation_codes), by.x = "vector", by.y = "Vector", all.x = T)

  # get EOPs
  foreop <- final[Aggregation == "EOP"]
  foreop[, month := month(refPer)]
  foreop <- foreop[month == 12]
  foreop[, c("refPer", "month", "Aggregation") := NULL]

  # get avgs
  foravg <- final[Aggregation == "avg"]
  foravg[, c("Aggregation", "refPer") := NULL]

  keep_cols <- setdiff(colnames(foravg), "value")
  foravg <- foravg[, .(value = mean(value)), by = keep_cols]

  # get sums
  forsum <- final[tolower(Aggregation) == "sum"]
  forsum[, c("Aggregation", "refPer") := NULL]

  keep_cols <- setdiff(colnames(forsum), "value")
  forsum <- forsum[, .(value = sum(value)), by = keep_cols]

  # recompose
  final <- final[is.na(Aggregation)]
  misaggregated <- unique(final[frequencyCode != 12,cansimId])
  warning(c('High frequency vars in ', misaggregated, ' were not aggregated. Series removed.'))
  final <- final[frequencyCode == 12]
  final <- rbind(forsum, foravg, foreop, final[, c("Aggregation", "refPer") := NULL])

  # extract geography

  #geo_tag <- as.data.table(which(Vectorize(function(x) x %in% "Geography")(final), arr.ind = TRUE))
  final[, geography := memberNameEn]

  # extract industry
  naics_tag <- as.data.table(which(Vectorize(function(x) x %in% c("Commodity", "North American Industry Classification System (NAICS)", "Industry", "North American Product Classification System (NAPCS)", "Product"))(final), arr.ind = TRUE))
  final[, industry := as.character(NA)]
  naics_tag <- naics_tag[col < 30] # & col!=12]
  for (i in unique(naics_tag$col)) {
    rows <- naics_tag[col == i, row]
    col <- i - 1
    final[rows, industry := final[rows, ..col]]
  }

  # extract prices
  price_tag <- as.data.table(which(Vectorize(function(x) x %in% c("Prices", "Value","Sectors"))(final), arr.ind = TRUE))
  final[, prices := as.character(NA)]
  price_tag <- price_tag[col < 27]
  for (i in unique(price_tag$col)) {
    rows <- price_tag[col == i, row]
    col <- i - 1
    suppressWarnings(final[rows, prices := (final[rows, ..col])])
  }
  final[, prices := ifelse(prices == 1, NA, prices)]
  final[is.na(prices) & (cubeTitleEn %like% "basic price" | cubeTitleEn %like% "basic prices"), prices := "Current prices"]
  final[tolower(cubeTitleEn) %like% "index", prices := "Index"]
  final[tolower(memberNameEn2) %like% "real", prices := "Constant prices"]
  final[prices == "Chained 2007 dollars", prices := "Chained (2007) dollars"]
  final[prices == "Current dollars", prices := "Current prices"]


  # extract variable
  var_tag <- as.data.table(which(Vectorize(function(x) x %in% c("Weighting", "Estimates", "Rates", "Components of population growth", "Financial estimates", "Labour statistics", "Multifactor productivity and related variables", "Income components", "Income, consumption and savings", "Trade flow detail", "Flows and stocks", "Statement of operations and balance sheet", "Components of population growth", "Housing estimates", "Principal statistics", "Labour force characteristics"))(final), arr.ind = TRUE))
  final[, variable := as.character(NA)]
  var_tag <- var_tag[col < 27]
  for (i in unique(var_tag$col)) {
    rows <- var_tag[col == i, row]
    col <- i - 1
    final[rows, variable := final[rows, ..col]]
  }
  final[, variable := ifelse(is.na(variable), cubeTitleEn, variable)]

  # extract info
  info_tag <- as.data.table(which(Vectorize(function(x) x %in% c("Commodity groups", "Products and product groups", "Type of expenditure", "Capital and repair expenditures", "North American Product Classification System (NAPCS)", "Assets and liabilities", "Capital and repair expenditures", "Supply and disposition", "Levels of government", "Age group", "New housing price indexes", "Wealth", "Sex", "Type of asset", "Assets", "Type of store", "Sector", "Public sector component", "Public sector components", "Levels of government", "Geography, province of destination", "Index", "Financial estimates","Categories"))(final), arr.ind = TRUE))
  final[, info := as.character(NA)]
  info_tag <- info_tag[col < 27]
  for (i in unique(info_tag$col)) {
    rows <- info_tag[col == i, row]
    col <- i - 1
    final[rows, info := final[rows, ..col]]
  }
  
  # final[info %like% "Electric power selling price over", info:=gsub("Electric power selling price", "",info)]

  # extract adjustment
  sa_tag <- as.data.table(which(Vectorize(function(x) x %in% c("Seasonal adjustment", "Adjustment"))(final), arr.ind = TRUE))
  final[, sa := as.character(NA)]
  sa_tag <- sa_tag[col < 27]
  for (i in unique(var_tag$col)) {
    rows <- sa_tag[col == i, row]
    col <- i - 1
    final[rows, sa := final[rows, ..col]]
  }

  # units
  # left alone for now

  # drop everything else
  # final<-final[,c("year", "geography", "industry", "prices", "variable", "info", "sa", "term", "cubeTitleEn", "value", "productId", "scalarFactorCode","vector","cansimId"),with=F]

  # geography
  final <- final[!geography %in% c("Whitehorse, Yukon", "Yellowknife, Northwest Territories", "Iqaluit, Nunavut", "Canada, provinces and international", "Outside Canada", "Canadian territorial enclaves abroad")]
  final[, geography := gsub(", province of origin", "", geography)]

  # optional
  final[, cansimId := gsub("-", "", cansimId)]

  # name variables
  final[, new_prices := ifelse(prices == "Index", NA, prices)]
  final[cansimId == "2280064", info := paste3(info, memberNameEn2, sep = "-")]
  final[industry %like% "Service-producing" | industry %like% "Industrial production", `:=`(info = industry, industry = NA)]
  final[!variable %like% "Taxes on income" & !info %like% "price indexes" & !memberNameEn3 %in% "Non-residential structures, machinery and equipment", 
        new_var := tstrsplit(variable, ",", fixed = TRUE, keep = 1)]
  final[variable %like% "Taxes on income", new_var := gsub("capital gains payable by", "cap gains-", variable)]
  final[info %like% "price indexes" | memberNameEn3 %in% "Non-residential structures, machinery and equipment" | cansimId %in% c("3850042", "3850033", "3850038","2820087"), new_var := variable]

  # fyi this paste3 function excludes NA vals
  final[, var := tolower(do.call(paste3, c(.SD, sep = "-"))), .SDcols = c("new_var", "info", "cansimId", "new_prices")]

  # this 380 table needs some modifications
  final[cansimId == "3800080", test := tstrsplit(coordinate, ".", fixed = T, type.convert = T, keep = 4)]
  final[test == 3, var := paste("tax", var, sep = "-")]
  final[test %in% c(57:68, 133:144), var := paste("cap transfer", var, sep = "-")]
  final[test %in% c(105:106), var := paste("subsidies on production", var, sep = "-")]

  final[cansimId == "3800080", var := ifelse(test >= 69, paste("exp", var, sep = "-"), paste("rev", var, sep = "-"))]
  final <- final[vector != 62426295]

  # and so does 3800070
  final[cansimId == "3800070", test := tstrsplit(coordinate, ".", fixed = T, type.convert = T, keep = 4)]
  final[cansimId == "3800070", var := ifelse((test >= 56 & test < 105) | (test >= 111 & test < 113), paste("imp", var, sep = "-"), var)]
  final[cansimId == "3800070", var := ifelse((test >= 3 & test < 53) | (test >= 108 & test < 111), paste("exp", var, sep = "-"), var)]

  # and so does 378-0121
  final <- final[cansimId != "3780121"]

  
  final[, c("test", "new_prices", "new_var") := NULL]


  # attach naics codes (this is ugly but salvaged from original code, can be improved)
  final[is.na(industry), industry := "National"]

  final[, industry2 := toupper(gsub("[[:space:]]", "", industry))]
  poo <- rep(0, nrow(final))
  for (b in 1:als) {
    aliaz <- paste("alias", b, sep = "")
    matchbase <- as.data.table(fullnaicslist[, cbind(code, get(aliaz))])
    setnames(matchbase, c("code", "industry"))
    matchbase[, industry := toupper(gsub("[[:space:]]", "", industry))]
    poo <- poo + match(final$industry2, matchbase$industry, nomatch = 0)
    # test
    # testname= paste(URLs[1,j],i,k,"codes",sep = "_")
    # assign(testname,poo)
  }
  poo[poo == 0] <- NA
  codex <- fullnaicslist$code[poo]
  final[, code := codex]
  final[, industry2 := NULL]
  # kill anything that doesn't match
  # exceptions
  final[is.na(code) & cansimId %in% c("3790030", "3790031"), `:=`(code = "NAT", var = paste(tolower(industry), var, sep = "-"))]

  final <- final[cansimId %in% tradetables | !is.na(code)]

  # add the new investment data
  newcap <- readRDS(paste(srcDir, "newcap.rds", sep = ""))
  newcap[, year := as.character(year)]
  newcap <- melt(newcap, id.vars = c("geography", "code", "year"))
  newcap[, var := variable]

  final <- rbind(final, newcap, fill = T)

  # make new construcion series
  dt2 <- final[(var %like% "build" | var %like% "engineering" | var %like% "eng ") & (cansimId %in% c("0310002", "0310005") | var %like% "statcan")]
  dt2[, tag := ifelse(var %like% "depreciation", "dep", ifelse(var %like% "investment", "inv", "ip"))]
  dt2[, var := gsub("non-residential buildings|engineering construction", "", var)]
  dt2[, var := gsub("building|engineering", "", var)]

  dt2[var %like% "statcan", var := gsub("build|eng", "x", var)]
  dt2[var %like% "statcan", variable := gsub("build|eng", "x", var)]

  sum <- dcast(dt2, cansimId + geography + var + code + tag + year + productId + scalarFactorCode + prices + variable + industry + cubeTitleEn + term + sa ~ ., value.var = "value", fun.aggregate = mysum)
  names(sum)[names(sum) == "."] <- "value"
  sum[, info := "Construction"]
  suppressWarnings(sum[!var %like% "statcan", var := tolower(do.call(paste3, c(.SD, sep = "-"))), .SDcols = c("variable", "info", "cansimId", "prices")])
  sum[var %like% "statcan", `:=`(var = gsub("x", "construction", var), variable = gsub("x", "construction", var))]
  final <- unique(rbind(final, unique(sum), fill = T))

  # make new series (see mapping file)
  tables <- unique(mapping$cansimId)
  mapping <- unique(mapping[, .(code, new_code, cansimId)])

  # make the mapping file
  x <- final[cansimId %in% tables ]
  res <- x[mapping, on = c(cansimId = "cansimId", code = "code")]
  refine <- final[, .(code, cansimId, year, geography)]
  res <- unique(res[!is.na(year)]) # these are the non-matches
  # remove redundencies
  # nrow(res)
  # names1<-unique(res$var)
  # refine<-final
  # setnames(refine,"code","new_code")
  # setkey(refine, geography,new_code,var,year)
  # setkey(res, geography,new_code,var,year)
  # res<-res[!refine]
  # nrow(res)
  # names2<-unique(res$var)
  # rm(refine)
  res <- res[!final, on = c(cansimId = "cansimId", new_code = "code", year = "year", var = "var")]

  vectors_to_rm <- unique(res[!code %in% naics$code, vector])

  res <- res[, sum(value), by = list(year, geography, var, cansimId, new_code)] # [,c("industry", "vector","code"):=NULL])

  setnames(res, c("new_code", "V1"), c("code", "value"))
  final <- unique(rbind(final[is.na(vector) | !vector %in% vectors_to_rm], res, fill = T))

  # remove the sectors that have been aggregated
  final <- final[code %in% naics$code | cansimId %in% tradetables]

  # correct scale on the gva series from 301* tables
  final[cansimId %like% "301000" & !variable %like% "employees", value := value / 1000]
  # keep metadata
  final_metadata <- unique(final[, .(vector, cubeTitleEn, sa, scalarFactorCode, variable, var, info, prices, term, productId, industry, cansimId)])

  # extract trade series
  trade <- unique(final[cansimId %in% tradetables, .(year, geography, var, value, industry)])
  # keep just data
  final_data <- final[!cansimId %in% tradetables, .(year, geography, var, value, code) ]
  
  ly <- max(final$year)
  ny <- length(1955:ly)
  nind <- nrow(naics)
  ngeo <- length(provinces)
  year <- rep(kronecker(1955:ly, rep(1, nind)), ngeo)
  code <- rep(naics$code, ny * ngeo)
  geography <- rep(provinces, each = ny * nind)
  # vars<-rep(unique(final_data$var),length(geography))
  leftcol <- as.data.table(cbind(geography, code, year))

  final_data <- final_data[leftcol, on = c("geography", "code", "year")]
  final_data <- unique(final_data[!is.na(var)])

  # extract trade series

  # keep as list
  final <- list(final_data, final_metadata, trade)
}

# modified paaste function
paste3 <- function(..., sep = ", ") {
  L <- list(...)
  L <- lapply(L, function(x) {
    x[is.na(x)] <- ""
    x
  })
  ret <- gsub(
    paste0("(^", sep, "|", sep, "$)"), "",
    gsub(
      paste0(sep, sep), sep,
      do.call(paste, c(L, list(sep = sep)))
    )
  )
  is.na(ret) <- ret == ""
  ret
}

mysum <- function(x) sum(x, na.rm = any(!is.na(x)))

### Parameters------------------------------------------------------####
# srcDir <- "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/resources/"
# dstDir <- "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/outputs/"
exportname <- "envcandb"

code_directory    <- "Directory-namestonaicscodes-2.xlsx" # <-------------- # This file contains (on the left are sheet names):
mainlist   <- "Main" # 1. The official list of delivered sectors
xtendeddir <- "Expanded" # 2. The list of all the pulled sectors, including those that are used to calculate one of the official sectors
mapdir     <- "Mapping" # 3. The mapping of the 2 to 1
smerge     <- "SeriesMerge" # 4. The mapping of the various series that make each variable


# Key Parameters:
tablerange  <- "b68:gg68"
vectorrange <- "b11:gg11"
urlfile     <- "DataWarehouse-4.xlsm" # <------------- #A little more on the url file, the urls are split so that each one corresponds

# code list
naicsdir      <- paste(srcDir, eval(code_directory), sep = "")
naics         <- as.data.table(read_excel(naicsdir, na = "NA", sheet = eval(mainlist), col_names = TRUE))
fullnaicslist <- as.data.table(read_excel(naicsdir, na = "", sheet = eval(xtendeddir), col_names = TRUE))
als           <- ncol(fullnaicslist) - 1 # of aliases in naics directory (full list)

# Upload tables from Data Warehouse- set range to include table numbers in row 27
wrhs_path <- paste(srcDir, urlfile, sep = "")
wrhs <- (read_excel(wrhs_path, na = "NA", col_names = FALSE))
pID  <- unique(as.list(wrhs[68, ]))
pID  <- pID[!is.na(pID)]
pID_toadd <- readRDS(paste0(srcDir, "pID_toadd.rds"))
pID  <- c(pID, pID_toadd, "10100122")

vectors <- as.list(wrhs[11, ])

# reformat vector list
vectors <- (lapply(vectors, function(x) strsplit(x, ",")))
vectors <- unname(as.list(do.call("c", unlist(vectors, recursive = F))))
vectors <- split(vectors, ceiling(seq_along(vectors) / 300))
vectors <- lapply(vectors, function(x) unique(x[!is.na(x)]))

# date range
startDate <- "1980-1-01T08:30"
endDate   <- Sys.Date() #"2018-03-31T19:00"

# aggregation code
aggc <- paste(srcDir, "aggregation_tags.csv", sep = "")
aggregation_codes <- as.data.table(read.csv(aggc, header = TRUE, sep = ","))
names(aggregation_codes) <- c("Vector", "Aggregation")
suppressWarnings(aggregation_codes[, Vector := as.numeric(gsub("V", "", Vector))])

# to map new series
mapping <- as.data.table(read_excel(naicsdir, na = "NA", sheet = eval(mapdir), col_names = TRUE, col_types = c("guess", "guess", "guess", "guess", "text", "guess")))
mapping <- mapping[, .(cansimId, code, new_code)]
fullnaicslist <- as.data.table(read_excel(naicsdir, na = "NA", sheet = eval(xtendeddir), col_names = TRUE))




### Commands----------------------------------------------------####
#
# data2<-mapply(download_data, vectors, startDate,endDate)
# saveRDS(data2, paste(srcDir,"biglist-new.rds",sep=""))
# #data2<-readRDS(paste(srcDir,"biglist-new.rds",sep=""))
#
# #if appending:
# #1)download data2, 2)apply download_data to create data2append 3)append as follows:
# data2append<-mapply(download_data,vectors, startDate,endDate)
# data2<-c(data2,data2append)
# saveRDS(data2, paste(srcDir,"biglist-new.rds",sep=""))
#
# #reshape
# datas<-(data2[c(FALSE,TRUE)])
# datas<-datas[!unlist(lapply(datas, is.null))]
#
# #clean raw data
# datatables<-lapply(datas,clean_data_outputs)
#
# #collect errors
# errors<-lapply(datatables,function(x) x[[2]])
# errors<-errors[sapply(errors, function(x) nrow(x)>=1)]
#
# #collect just data
# datatables<-lapply(datatables,function(x) x[[1]])
#
# #download metadata
# cubemetadata<-download_metadata_pID(pID)
# cubemetadata<-cubemetadata[unlist(lapply(cubemetadata,function(x) ncol(x)>1))] #removes errors
# saveRDS(cubemetadata, paste(srcDir,"metadata.rds",sep=""))
#
# #cubemetadata<-readRDS(paste(srcDir,"metadata.rds",sep=""))
#
# #clean metadata
#
# metadata<-lapply(cubemetadata,clean_metadata_outputs)
#
# #merge
# final<-merge_data_meta(datatables,metadata)
# final<-final[memberNameEn3!="Balance of payments"|is.na(memberNameEn3)]
# saveRDS(final,paste(srcDir,"final.rds",sep=""))
#
# #final<-readRDS(paste(srcDir,"final.rds",sep=""))
#
#
# #final clean
# envcan<-final_cleaning(final)
# saveRDS(envcan[[1]],paste(srcDir,"envcandb-new.rds",sep=""))
# saveRDS(envcan[[2]],paste(srcDir,"metadata-new.rds",sep=""))
# saveRDS(envcan[[3]],paste(srcDir,"trade-new.rds",sep=""))
