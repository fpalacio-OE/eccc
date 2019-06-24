
# Meta File
# By: Fabio Palacio
# Use this to run the entire update for the ECCC database
# To debug, open the relevant code files

codeDir <- "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/code/"
srcDir  <- "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/resources/"
dstDir  <- "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/outputs/"


## API ----------
## Functions for this block are in statcan-pulls.R

# load functions for pulls and for cleaning plus relevant packages and parameters
source(paste0(codeDir, "statcan-pulls.R"))

# set date range
startDate <- "1980-1-01T08:30"
endDate   <- strftime(Sys.time(),"%Y-%m-%dT%H:%M")

# set names for cleaned data outputs
export_names <- list(paste0(srcDir, "envcandb-new.rds"),
                     paste0(srcDir, "metadata-new.rds"),
                     paste0(srcDir, "trade-new.rds"))

# pull data from API and save the raw data as an RDS
raw  <- pbmapply(download_data, vectors[1:100], startDate, endDate)
raw2 <- pbmapply(download_data, vectors[100:200], startDate, endDate)
raw3 <- pbmapply(download_data, vectors[200:358], startDate, endDate)
raw  <- c(raw,raw2,raw3)
saveRDS(raw, paste(srcDir, "raw_from_api.rds", sep = ""))

# reshape
datas <- (raw[c(FALSE, TRUE)])
datas <- datas[!unlist(lapply(datas, is.null))]

# clean raw data
datatables <- lapply(datas, clean_data_outputs)

# collect errors
errors <- lapply(datatables, function(x) x[[2]])
errors <- errors[sapply(errors, function(x) nrow(x) >= 1)]

# collect just data
datatables <- lapply(datatables, function(x) x[[1]])

# download metadata and save as RDS
cubemetadata <- download_metadata_pID(pID)
cubemetadata <- cubemetadata[unlist(lapply(cubemetadata, function(x) ncol(x) > 1))] # removes errors
saveRDS(cubemetadata, paste(srcDir, "metadata.rds", sep = ""))

# clean metadata
metadata <- lapply(cubemetadata, clean_metadata_outputs)

# merge and save as RDS
final <- merge_data_meta(datatables, metadata)
saveRDS(final, paste(srcDir, "semicleaned_w_metadata.rds", sep = ""))

# final clean and save complete table, metadata, and trade data
envcan <- final_cleaning(final)
mapply(saveRDS,envcan,export_names)

#clear workspace
suppressWarnings(rm(list = c("envcan","final","metadata","cubemetadata","datatables","errors","raw","datas","pID","mapping","fullnaicslist","aggregation_codes","wrhs","vectors","naics")))

## Gapfilling (interpolation) ------

# set export name and source base
exportname = "envcandb-readable.csv"
sourcebase = export_names[[1]]    

# run the gapfill
source(paste0(codeDir,"gapfill.R"))

## Gross output gapfill (interpolation) ------

# set export name and source base
exportname = "final-goutput2"
sourcebase = export_names[[1]]    

# run the gapfill
source(paste0(codeDir,"gapfill.R"))


