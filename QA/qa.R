
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

# Parameters ---------------------------

# Directories
srcDir.old  <- "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/2007 base/"
srcDir.new  <- "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/"

rsrc        <- "resources/"
out         <- "outputs/"

# pre-interpolation data file (must be an RDS file)
rawdata.filename.old <- "shortdb.rds"
rawdata.filename.new <- "shortdb.rds"

raw <- readRDS(paste0(srcDir.new,rsrc, rawdata.filename.new))
#raw <- readRDS(paste0(srcDir.old,rsrc, rawdata.filename.new))

# data output file names (must be CSVs):
datafiles.old <- c("envcandb-filled.csv",
                   "final-goutput2.csv",
                   "trade.csv",
                   "trend.csv")

datafiles.new <- c("envcandb-filled.csv",
                   "final-goutput.csv",
                   "trade.csv",
                   "trend.csv")

# macro and variable names

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
formacros <- names(raw)[!(names(raw) %in% c(sectorals, finalvariables, "industry"))] # <------------NOTICE THESE NAMES ARE HARDCODED
macrovars <- formacros[!(formacros %like% "overlap")]

# Useful functions
source(paste0(srcDir.new,"code/functions/AttachMnemonics.R"))

# load up all the data and consolidate

old    <- rbindlist(lapply(paste0(srcDir.old,out, datafiles.old),function(x) read.csv(x, header = FALSE)), fill = TRUE)
new    <- rbindlist(lapply(paste0(srcDir.new,out, datafiles.new),function(x) read.csv(x, header = FALSE)), fill = TRUE)

tables <- list(old = old, new = new)

# clean up the dbs
tables <- invisible(lapply(tables, 
  function(x) {
    setnames(x, c("V2","V3"), c("geography","variable"))
    x[,`:=`(geography = as.character(geography), variable = as.character(variable))]
    x <- x[, (colSums(is.na(x)) < nrow(x)),with = F]
    
    x <- melt(x[,!c("V1",paste0("V",4:9)),with = F], id.vars = c("variable","geography"), variable.name = "year")
    x[,year := as.numeric(gsub("V","",year)) + 1970]
    
    setkey(x, variable, geography)
  }
 ))

old <- tables[['old']]
new <- tables[['new']]
## Comparison -----------------

# var list
newvars <- new[,.(geography,variable)]
oldvars <- old[,.(geography,variable)]


# new variables
gainedvars        <- newvars[!oldvars, on = list(geography,variable)]
gainedvars.unique <- unique(gainedvars$variable)

print(paste(length(gainedvars.unique), " variables gained (some may occur in more than one province)"))

#lost variables
lostvars        <- oldvars[!newvars, on = list(geography,variable)]
lostvars.unique <- unique(lostvars$variable)

print(paste(length(lostvars.unique), "unique variables lost (some may occur in more than one province)"))

#prepare for comparison

var <- "YAA,ALBERTA"

compare_datasets <- function(variable,old,new) {
  var <- tstrsplit(variable,",")[[1]]
  geo <- tstrsplit(variable,",")[[2]]
  
  new.series <- new[.(var,geo), nomatch = 0][,db := "new"]
  old.series <- old[.(var,geo), nomatch = 0][,db := "old"]
  
  tab <- rbind(new.series,old.series)
  
  plt <- ggplot(tab,aes(x = year, y = value, color = db )) + geom_line(na.rm = T) +
         theme(legend.position = c(.9,.2), plot.title = element_text( hjust = .5, vjust = 0.5, face = 'bold')) +
         ggtitle(variable)

  rates <- tab[, growth := c(NA, diff(log(value))), by = db][,.(variable, geography,year,growth,db)]
  rates <- dcast(rates, ...~db, value.var = "growth")
  rates[,  diff := abs(new - old)]
  
  clrs <- round(seq(255, 40, length.out = 2 + 1), 0) %>%
  {paste0("rgb(255,", ., ",", ., ")")}
  
  rates2 <- datatable(rates[, .(year, "new rate" = new, "old rate" = old, diff)], options = list(pageLength = 40)) %>%
    formatStyle('year','diff', backgroundColor = styleInterval(c(.05,.1),clrs)) %>%
    formatRound(c(2:5),2)


  ui <- dashboardPage(
    dashboardHeader(title = "Series Comparison"),
    dashboardSidebar(
      width = 0
    ),
    dashboardBody(
      box(title = "Data Path", status = "primary",height = "500" ,solidHeader = T,
          plotOutput("trace_plot")),
      box( title = "Growth Rate Comparison", status = "primary", height = 
             "595",width = "6",solidHeader = T, 
           column(width = 12,
                  dataTableOutput("trace_table"),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
           )
      )))
  
  server <- function(input, output) { 
    #Plot for Trace Explorer
    output$trace_plot  <- renderPlot(plt)
    output$trace_table <- renderDataTable({rates2})
  }
  
  shinyApp(ui, server)

}


compare_datasets(var, old,new)

# QA ---------------------

# clean the pre-interpolation file and attach mnemonics

raw <- melt(raw, id.vars = c("geography","code","year"))
raw <- raw[ !(code == "NAT" & !(variable %in% macrovars))]
raw <- raw[ !(code != "NAT" & (variable %in% macrovars))]

raw <- AttachMnems(raw, paste0(srcDir.new,rsrc))
setkey(raw,"geography","mnemonic")

raw <- melt(raw, id.vars = c("geography","code","year"))

x[,year := as.numeric(gsub("V","",year)) + 1970]

variable <- "YAGR,ALBERTA"
view_interpolation <- function(variable,raw,new) {
  
  
}

