
# QA Program
# Project : ECCC
# By: Fabio Palacio
# Use this to A) check differences between two data sets B) to QA a single database
# To debug, open the relevant code files
# Last updates : 6/24/19

# packages
library(ggplot2)
library(gridExtra)
library(shiny)
library(shinydashboard)
library(DT)
library(data.table)
library(readxl)
library(stringr)
#library(Biobase)


# Parameters ---------------------------

# Directories
srcDir.old  <- "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/2007 base/"
srcDir.new  <- "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/"
srcDir.qa   <- "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/QA/code/"
dstDir      <- "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/QA/outputs/"
rsrc        <- "resources/"
out         <- "outputs/"
codedir     <- "Directory-namestonaicscodes-2.xlsx"
# resource files
naicsdir    <- paste(srcDir.new, rsrc, eval(codedir), sep = "")
mnems       <- as.data.table(read_excel(naicsdir, na = "NA", sheet = "FinalMnem", col_names = TRUE))

# function files
source(paste0(srcDir.qa, "qa_skeleton.R"))
source(paste0(srcDir.new,"code/functions/AttachMnemonics.R"))

# data output file names (must be CSVs):
datafiles.old <- c("envcandb-filled.csv",
                   "final-goutput2.csv",
                   "trade.csv",
                   "trend.csv")

datafiles.new <- c("envcandb-filled.csv",
                   "final-goutput.csv",
                   "trade.csv",
                   "trend.csv")

# the pre-interpolation files (must be RDS)
rawdata.filename.old <- "shortdb.rds"
rawdata.filename.new <- "shortdb.rds"

raw.new <- readRDS(paste0(srcDir.new,rsrc, rawdata.filename.new))

# macro and variable names
industryvar.mnems <- c('YHAT','TREND','DELTAC','DELTAIP','DELTAME','EMP','GY','Y','AVHR','IFC','IFIP','IFME','KC','KIP','KME',
                       'PMX','ER','XN','XX','MN','MX','PGDP','K','P','IF','X','M')

industry.mnems    <- c("",'A','AC','AA','AF','AH','AS','E','EO','EM','EMC','EMM','EMMI','EMMG','EMMC','EMMO',
                       'EMN','ES','U','UE','UG','UW','CON','MAN','MF','MB','MT','MC','MW','MP','MPP','MPC',
                       'MPRIN','MPET','MPETR','MPETA','MPETO','MPETAO','MCHEM','MCHEMB','MCHEMP','MCHEMG','MCHEMD',
                       'MCHEMI','MCHEMF','MCHEMM','MCHEMO','MRUBP','MNM','MNMY','MNMG','MNMC','MNMCC','MNMCR',
                       'MNMCO','MNML','MNMO','MNMAO','MMET','MMETI','MMETS','MMETA','MMETAP','MMETAO','MMETN','MMETNR',
                       'MMETNC','MMETNA','MMETF','MMETFF','MMETFN','MFMP','MMAC','MCOM','MELC','MTEQ','MFUR',
                       'MMIS','WHT','RET','T','TA','TR','TW','TT','TGSS','TP','TPCO','TWS','TPG','TS','TCMP','INFO','FIN',
                       'RE','PROF','ENT','AWMS','EDU','HEAL','ART','ACC','OTHS','PUB')


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

sectorals <- readRDS(paste(srcDir.new,rsrc, "sectorals.rds", sep = ""))
formacros <- names(raw.new)[!(names(raw.new) %in% c(sectorals, finalvariables, "industry"))] # <------------NOTICE THESE NAMES ARE HARDCODED
macrovars <- formacros[!(formacros %like% "overlap")]

# load up all the data and consolidate

## first the pre-interpolation data file
raw     <- prep(raw.new)
## then the primary bases
old    <- rbindlist(lapply(paste0(srcDir.old,out, datafiles.old),function(x) read.csv(x, header = FALSE)), fill = TRUE)
new    <- rbindlist(lapply(paste0(srcDir.new,out, datafiles.new),function(x) read.csv(x, header = FALSE)), fill = TRUE)
tables <- list(old = old, new = new)

tables <- invisible(lapply(tables, cleanup))

old    <- tables[['old']]
new    <- tables[['new']]

# duplicates
new.dups <- test_dups(new)
old.dups <- test_dups(old)
raw.dups <- test_dups(raw)

cat("These vars will be removed from analysis. Please return to core code to evaluate source of duplicates.
Some possibilites include : more than one series from API for one variable, 
    or overlapping mnemonics (think P-ART (price of art) and PART (participation rate))")

# remove duplicates

rmv <- as.data.table(matrix(unlist(tstrsplit(new.dups, ",")),ncol = 2))
new <- new[!rmv]

rmv <- as.data.table(matrix(unlist(tstrsplit(old.dups, ",")),ncol = 2))
old <- old[!rmv]

# Comparison (new v old dbs) -----------------

db_dimensions(new,old)

var <- "KMCHEMM,NFOUNLND"
compare_datasets(var, old,new, raw)

# QA ---------------------

# attach codes and industry mnems to new db

new <- clean_new(new)



#function that just determines if a series was interpolated and compares the values
#the key argument is variable

variable <- "KMCHEMM,ALBERTA"
view_interpolation(variable,raw,new)


#For the following function, argument 1 is the geography:
#"ALBERTA"  "B_COLUMB" "CANADA"   "MANITOBA" "NBRUNSWK" 
#"NFOUNLND" "NORTHWT"  "NOVA_SCO" "NUNAVUT"  "ONTARIO"  
#"PRINCE_E" "QUEBEC"   "SASKCHWN" "YUKON" 

#Argument 2 is the code you're interested in if you type "3",
#you'll get charts and plots for all sectors starting with 3.
#Shortcut for services : type "s" for this field

#the function will produce a chart with dots for all
#extreme value growth rates (for data points that were interpolated)
#it will then give you options for additional detail including a small table for specific sectors

produce_summary(     "B_COLUMB","s")

