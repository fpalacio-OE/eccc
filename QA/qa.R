
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
library(Biobase)


# Parameters ---------------------------

# Directories
srcDir.old  <- "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/2007 base/"
srcDir.new  <- "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/"

rsrc        <- "resources/"
out         <- "outputs/"

# pre-interpolation data file (must be an RDS file)
rawdata.filename.old <- "shortdb.rds"
rawdata.filename.new <- "shortdb.rds"

raw.new <- readRDS(paste0(srcDir.new,rsrc, rawdata.filename.new))

raw <- melt(raw.new, id.vars = c("geography","code","year"))

raw <- AttachMnems(raw, paste0(srcDir.new,rsrc))
raw <- raw[!is.na(value)]
setkey(raw,"geography","mnemonic")
raw[, year := as.numeric(year)]
raw <- raw[,.("variable" = mnemonic, "geography" = sectorcode, year, "varname" = variable, value)]
setkey(raw, variable, geography, year)

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
industryvar.mnems <- c('YHAT','TREND','CUMOD','DELTAC','DELTAIP','DELTAME','EMP','GY','Y','AVHR','IFC','IFIP','IFME','KC','KIP','KME',
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
    
    setkey(x, variable, geography,year)
  }
 ))

old <- tables[['old']]
new <- tables[['new']]

# Duplicates -----------------------

test_dups <- function(table, view = FALSE) {
  myDT  <- copy(table)
  #setkey(myDT, geography, year, vintage, variable)
  if (is.null(key(myDT))) {setkey(myDT, geography, year, code, variable)}
  dups = duplicated(myDT, by = key(myDT));
  myDT[, fD := dups | c(tail(dups, -1), FALSE)]
  if (view == TRUE) {View(myDT[fD == TRUE])}
  
  unique(myDT[fD == TRUE,paste(variable,geography,sep = ",")])
}

new.dups <- test_dups(new)
old.dups <- test_dups(old)
raw.dups <- test_dups(raw)

print(c("The following variables are duplicated in the new db : ",new.dups))

print(c("The following variables are duplicated in the old db : ",old.dups))

print(c("The following variables are duplicated in the raw db : ",raw.dups))

cat("These vars will be removed from analysis. Please return to core code to evaluate source of duplicates.
Some possibilites include : more than one series from API for one variable, 
or overlapping mnemonics (think P-ART (price of art) and PART (participation rate))")


# remove duplicates

rmv <- as.data.table(matrix(unlist(tstrsplit(new.dups, ",")),ncol = 2))
new <- new[!rmv]

rmv <- as.data.table(matrix(unlist(tstrsplit(old.dups, ",")),ncol = 2))
old <- old[!rmv]

# Comparison -----------------

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


# Series comparison


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

var <- "YAA,ALBERTA"

compare_datasets(var, old,new)

# QA ---------------------

# clean the pre-interpolation file and attach mnemonics



variable <- "YMMET,ALBERTA"

#function that just determines if a series was interpolated
view_interpolation <- function(variable,raw, new) {
  #parse parameters
  var <- tstrsplit(variable,",")[[1]]
  geo <- tstrsplit(variable,",")[[2]]
  
  #reshape source data
  setnames(     raw, c("geography" ,"value","variable","mnemonic","sectorcode"), c("geoname", "origval","varname","variable","geography"))
  comparison <- raw[new, on = list(variable, geography, year)]
  #setnames(     comparison, c('i.variable','i.value'),c('varname','origval'))
  idvars     <- names(comparison)[!grepl("val",names(comparison))]
  
  comparison <- melt(comparison, id.vars = idvars, variable.name = "vintage")
  
  setkey(       comparison, variable, geography)
  
  #create plot
  tab        <- comparison[.(var,geo), nomatch = 0]
  print(dcast(tab,...~vintage))
  plt <- ggplot(tab,aes(x = year, y = value, color = vintage )) +
    geom_line(na.rm = T, position = position_jitter(w = .2, h = 0)) +
    geom_point(na.rm = T) +
    theme(legend.position = c(.9,.2), plot.title = element_text( hjust = .5, vjust = 0.5, face = 'bold')) +
    ggtitle(variable) +
    geom_text(data = tab[is.na(value)], aes(y = min(tab$value,na.rm = T), label = "x"),
              position = position_dodge(w = .2), show.legend = F)
  plt
  
  print(setorder(dcast(tab,...~vintage),year))
}

produce_summary <- function(geo, plotname, orig = copy(raw), curr = copy(new)) {
  print(head(orig))
  print(head(curr))
  #reshape source data
  setnames(     orig, "value","origval")
  comparison <- orig[curr, on = list(variable, geography, year)]
  #setnames(     comparison, c('i.variable','i.value'),c('varname','origval'))
  idvars     <- names(comparison)[!grepl("val",names(comparison))]
  
  comparison <- melt(comparison, id.vars = idvars, variable.name = "vintage")
  
  setkey(       comparison, variable, geography)
  
  #calculate growthrates
  rates <- unique( comparison[,.(geography, variable,varname, vintage, year,value)])
  rates <- dcast(rates,...~vintage)
  
  rates[, `:=`(growth    = ifelse(shift(value) == .001, 0, value / shift(value) - 1),
               origgrowth = origval / shift(origval) - 1), by = list(variable, geography)]
  
  rates[!value %in% c(.001, 0), quant := ifelse(shift(value) %in% 0, NA,cut(growth,unique(c(-Inf,unique(quantile(growth[!growth %in% c(0,1,-1) ], probs = 0:100/100, na.rm = T)),Inf)), include.lowest = TRUE, labels = FALSE)), by = year]
  rates[, check := quant > 99 | quant < 2][,filter := ifelse(shift(check,type = "lead") %in% TRUE, TRUE, check)]

  # prepare for plotting
  data <- copy(rates)
  data <- data[ grepl(paste0("^",industryvar.mnems,collapse = "|"),variable)]
  data[,`:=`
       (prices = ifelse(grepl("!",variable), "nominal", "real"),
        ind    = gsub(paste0("^",industryvar.mnems,collapse = "|"),"",variable))][, ind := gsub("!","",ind)]
  data[ ind == "ON", ind := "CON"]
  data[,vargroup := gsub("!","",variable)][, vargroup := mapply(function(x,y) gsub(paste0(x,"$"),"", y), ind,vargroup)]
  
  setkey(data, variable, geography)
  dataT     <- data[ind %in% industry.mnems][check == TRUE][,.(geography,variable, year, prices,ind,check,vargroup)]
  dataT$ind <- factor(as.character(dataT$ind), levels = rev(industry.mnems))
  # plot extreme values for industry vars
  
  p <- ggplot(dataT[geography == geo], aes(year, ind)) +
    geom_point() +
    facet_wrap(c("vargroup","prices")) +
    theme(legend.position = c(.9,.2), plot.title = element_text( hjust = .5, vjust = 0.5, face = 'bold')) +
    ggtitle(paste("Summary Chart",geo, sep = "-"))
  
  print(p)
  
  uservar <- readline(prompt = "Which variable group for detail: ")   

  pdf(paste0(plotname, ".pdf"))
  
  k <- ggplot(dataT[geography == geo & vargroup == uservar], aes(year, ind)) +
    geom_point(color = "red") +
    facet_wrap(c("vargroup","prices")) +
    theme(axis.text.y = element_text(size = 4)) 
  dev.off()
  
  print(k)
  

}

produce_summary("ALBERTA","detail")




