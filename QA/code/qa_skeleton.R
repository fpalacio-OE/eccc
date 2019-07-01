
# QA Program
# Project : ECCC
# By: Fabio Palacio
# Use this to A) check differences between two data sets B) to QA a single database
# To debug, open the relevant code files
# Last updates : 6/24/19

# packages
# library(ggplot2)
# library(gridExtra)
# library(shiny)
# library(shinydashboard)
# library(DT)
# library(data.table)
# library(readxl)
# library(stringr)
# library(Biobase)


# Parameters ---------------------------

# Directories
# srcDir.old  <- "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/2007 base/"
# srcDir.new  <- "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/"
# dstDir      <- "C:/Users/Fabio Palacio.OEF/OneDrive - Oxford Economics/envcan/eccc/QA/outputs/"
# rsrc        <- "resources/"
# out         <- "outputs/"
# codedir     <- "Directory-namestonaicscodes-2.xlsx"
# # resource files
# naicsdir    <- paste(srcDir.new, rsrc, eval(codedir), sep = "")
# mnems       <- as.data.table(read_excel(naicsdir, na = "NA", sheet = "FinalMnem", col_names = TRUE))
# 
# 
# # pre-interpolation data file (must be an RDS file)
# rawdata.filename.old <- "shortdb.rds"
# rawdata.filename.new <- "shortdb.rds"
# 
# raw.new <- readRDS(paste0(srcDir.new,rsrc, rawdata.filename.new))
# 
prep <- function(raw){
  raw <- melt(raw.new, id.vars = c("geography","code","year"))
  raw <- AttachMnems(raw, paste0(srcDir.new,rsrc))
  raw <- raw[!is.na(value)]
  setkey(raw,"geography","mnemonic")
  raw[, year := as.numeric(year)]
  raw <- raw[,.("variable" = mnemonic, "geography" = sectorcode, year, "varname" = variable, value)]
  setkey(raw, variable, geography, year)
}
# 
# #raw <- readRDS(paste0(srcDir.old,rsrc, rawdata.filename.new))
# 
# # data output file names (must be CSVs):
# datafiles.old <- c("envcandb-filled.csv",
#                    "final-goutput2.csv",
#                    "trade.csv",
#                    "trend.csv")
# 
# datafiles.new <- c("envcandb-filled.csv",
#                    "final-goutput.csv",
#                    "trade.csv",
#                    "trend.csv")
# 
# # macro and variable names
# industryvar.mnems <- c('YHAT','TREND','DELTAC','DELTAIP','DELTAME','EMP','GY','Y','AVHR','IFC','IFIP','IFME','KC','KIP','KME',
#                     'PMX','ER','XN','XX','MN','MX','PGDP','K','P','IF','X','M')
# 
# industry.mnems    <- c("",'A','AC','AA','AF','AH','AS','E','EO','EM','EMC','EMM','EMMI','EMMG','EMMC','EMMO',
#                        'EMN','ES','U','UE','UG','UW','CON','MAN','MF','MB','MT','MC','MW','MP','MPP','MPC',
#                        'MPRIN','MPET','MPETR','MPETA','MPETO','MPETAO','MCHEM','MCHEMB','MCHEMP','MCHEMG','MCHEMD',
#                        'MCHEMI','MCHEMF','MCHEMM','MCHEMO','MRUBP','MNM','MNMY','MNMG','MNMC','MNMCC','MNMCR',
#                        'MNMCO','MNML','MNMO','MNMAO','MMET','MMETI','MMETS','MMETA','MMETAP','MMETAO','MMETN','MMETNR',
#                        'MMETNC','MMETNA','MMETF','MMETFF','MMETFN','MFMP','MMAC','MCOM','MELC','MTEQ','MFUR',
#                        'MMIS','WHT','RET','T','TA','TR','TW','TT','TGSS','TP','TPCO','TWS','TPG','TS','TCMP','INFO','FIN',
#                        'RE','PROF','ENT','AWMS','EDU','HEAL','ART','ACC','OTHS','PUB')
# 
# 
# finalvariables <- c(
#   "employment", "gross output (x 1,000,000)", "price index", "capital stock - chained prices (x 1,000,000)",
#   "investment, mach - chained prices (x 1,000,000)", "investment, constr - current prices (x 1,000,000)",
#   "depreciation, mach - chained prices (x 1,000,000)", "gva - current prices (x 1,000,000)", "investment - current prices (x 1,000,000)",
#   "investment - chained prices (x 1,000,000)", "capital depreciation - chained prices (x 1,000,000)",
#   "investment, ip - current prices (x 1,000,000)", "investment, constr - chained prices (x 1,000,000)",
#   "depreciation, constr - chained prices (x 1,000,000)", "stock, mach - chained prices (x 1,000,000)",
#   "stock, constr - chained prices (x 1,000,000)", "gva - chained prices (x 1,000,000)", "investment, mach - current prices (x 1,000,000)",
#   "investment, ip - chained prices (x 1,000,000)", "hours worked", "depreciation, ip - chained prices (x 1,000,000)",
#   "gross output - constant prices (x 1,000,000)", "stock, ip - chained prices (x 1,000,000)", "wages"
# )
# 
# sectorals <- readRDS(paste(srcDir.new,rsrc, "sectorals.rds", sep = ""))
# formacros <- names(raw.new)[!(names(raw.new) %in% c(sectorals, finalvariables, "industry"))] # <------------NOTICE THESE NAMES ARE HARDCODED
# macrovars <- formacros[!(formacros %like% "overlap")]
# 
# # Useful functions
# source(paste0(srcDir.new,"code/functions/AttachMnemonics.R"))
# 
# # load up all the data and consolidate
# 
# old    <- rbindlist(lapply(paste0(srcDir.old,out, datafiles.old),function(x) read.csv(x, header = FALSE)), fill = TRUE)
# new    <- rbindlist(lapply(paste0(srcDir.new,out, datafiles.new),function(x) read.csv(x, header = FALSE)), fill = TRUE)
# 
# tables <- list(old = old, new = new)

# clean up the dbs

cleanup <-  function(x) {
    setnames(x, c("V2","V3"), c("geography","variable"))
    x[,`:=`(geography = as.character(geography), variable = as.character(variable))]
    x <- x[, (colSums(is.na(x)) < nrow(x)),with = F]
    
    x <- melt(x[,!c("V1",paste0("V",4:9)),with = F], id.vars = c("variable","geography"), variable.name = "year")
    x[,year := as.numeric(gsub("V","",year)) + 1970]
    
    setkey(x, variable, geography,year)
}


# Duplicates -----------------------

test_dups <- function(table, view = FALSE) {
  myDT  <- copy(table)
  #setkey(myDT, geography, year, vintage, variable)
  if (is.null(key(myDT))) {setkey(myDT, geography, year, code, variable)}
  dups = duplicated(myDT, by = key(myDT));
  myDT[, fD := dups | c(tail(dups, -1), FALSE)]
  if (view == TRUE) {View(myDT[fD == TRUE])}
  
  table.dups <- unique(myDT[fD == TRUE,paste(variable,geography,sep = ",")])
  
  nm <- deparse(substitute(table))
  
  print(c(paste0("The following variables are duplicated in the ", nm," db:"),table.dups))
}

# new.dups <- test_dups(new)
# old.dups <- test_dups(old)
# raw.dups <- test_dups(raw)
# 
# cat("These vars will be removed from analysis. Please return to core code to evaluate source of duplicates.
# Some possibilites include : more than one series from API for one variable, 
# or overlapping mnemonics (think P-ART (price of art) and PART (participation rate))")


# remove duplicates
# 
# rmv <- as.data.table(matrix(unlist(tstrsplit(new.dups, ",")),ncol = 2))
# new <- new[!rmv]
# 
# rmv <- as.data.table(matrix(unlist(tstrsplit(old.dups, ",")),ncol = 2))
# old <- old[!rmv]

# Comparison -----------------

db_dimensions <- function(new, old){
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
}

# Series comparison


compare_datasets <- function(variable,old,new,raw) {
  var <- tstrsplit(variable,",")[[1]]
  geo <- tstrsplit(variable,",")[[2]]
  
  new.series <- new[.(var,geo), nomatch = 0][,db := "new"]
  old.series <- old[.(var,geo), nomatch = 0][,db := "old"]
  raw.series <- raw[.(var,geo), nomatch = 0][,`:=`(db = "raw", varname = NULL)]
  
  tab <- rbind(new.series,old.series,raw.series)
  
  
  plt <- ggplot(tab[db != "raw"],aes(x = year, y = value, color = db )) + geom_line(na.rm = T) +
         theme(legend.position = c(.9,.2), plot.title = element_text( hjust = .5, vjust = 0.5, face = 'bold')) +
         ggtitle(variable)

  rates <- tab[, growth := c(NA, diff(log(value))), by = db][
    db == "raw", growth := ifelse(!is.na(value), 1, 0)][,.(variable, geography,year,growth,db)]
  

  
  #rates[!is.na(value) & db == "raw", ]
  rates <- dcast(rates, ...~db, value.var = "growth")
  setnames(rates, "raw", "actual data?")
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

# var <- "YAA,ALBERTA"
# 
# compare_datasets(var, old,new)

# QA ---------------------

# clean the pre-interpolation file and attach mnemonics
# new[,`:=`
#      (prices = ifelse(grepl("!",variable), "nominal", "real"),
#       ind    = gsub(paste0("^",industryvar.mnems,collapse = "|"),"",variable))][, ind := gsub("!","",ind)]
# 
# new[ind == "ON" ,ind  := "CON"]
# new[            ,vargroup := gsub("!","",variable)][, vargroup := mapply(function(x,y) gsub(paste0(x,"$"),"", y), ind,vargroup)]
# new[            ,code  := mnems$code[match(ind, mnems$mnem)]]
# new[ind == ""   ,code := "1"]

# attach codes and industry mnems to new db
clean_new <- function(new){
  new[,`:=`
      (prices = ifelse(grepl("!",variable), "nominal", "real"),
        ind   = gsub(paste0("^",industryvar.mnems,collapse = "|"),"",variable))][, ind := gsub("!","",ind)]
  
  new[ind == "ON" ,ind  := "CON"]
  new[            ,vargroup := gsub("!","",variable)][, vargroup := mapply(function(x,y) gsub(paste0(x,"$"),"", y), ind,vargroup)]
  new[            ,code  := mnems$code[match(ind, mnems$mnem)]]
  new[ind == ""   ,code := "1"]
}


#function that just determines if a series was interpolated
view_interpolation <- function(variable,raw, new) {
  old  <- copy(raw)
  curr <- copy(new)
  #parse parameters
  var <- tstrsplit(variable,",")[[1]]
  geo <- tstrsplit(variable,",")[[2]]
  
  #reshape source data
  setnames(     old, c("value"), c("origval"))
  comparison <- old[curr, on = list(variable, geography, year)]
  #setnames(     comparison, c('i.variable','i.value'),c('varname','origval'))
  idvars     <- names(comparison)[!grepl("val",names(comparison))]
  
  comparison <- melt(comparison, id.vars = idvars, variable.name = "vintage")
  
  setkey(       comparison, variable, geography)
  
  #create plot
  tab        <- comparison[.(var,geo), nomatch = 0]
  #print(dcast(tab,...~vintage))
  plt <- ggplot(tab,aes(x = year, y = value, color = vintage )) +
    geom_line(na.rm = T, position = position_jitter(w = .2, h = 0)) +
    geom_point(na.rm = T) +
    theme(legend.position = c(.9,.2), plot.title = element_text( hjust = .5, vjust = 0.5, face = 'bold')) +
    ggtitle(variable)
  na <- geom_text(data = tab[is.na(value)], aes(y = min(tab$value,na.rm = T), label = "x"),
             position = position_dodge(w = .2), show.legend = F)
  
  #outputs
  if (nrow(tab[is.na(value)]) == 0 ) {
    print(plt)
  }else{
    print(plt + na )
  }

  print(setorder(dcast(tab,...~vintage),year))
}

produce_summary <- function(geo, sectorgroup = "", orig = copy(raw), curr = copy(new), skip_detail = FALSE) {
  
  
  #ensure file can be ovewritten
  path        =  paste0( dstDir, "summary","-",sectorgroup,"-",geo, ".pdf")
  
  open.check <-  suppressWarnings("try-error" %in% class(try(file(path, open = "w"), silent = TRUE)))
  if (open.check == TRUE) {stop(paste0("Close ",path," first"))}
  newconn    <-  names(path == showConnections(all = TRUE)[,1])[path == showConnections(all = TRUE)[,1]]
  close(         getConnection(newconn))
  
  if (tolower(sectorgroup) == "s") {sectorgroup <- 4:9}
  
  #reshape source data
  setnames(     orig, "value","origval")
  comparison <- orig[curr, on = list(variable, geography, year)]
  #setnames(     comparison, c('i.variable','i.value'),c('varname','origval'))
  idvars     <- names(comparison)[!grepl("val",names(comparison))]
  comparison <- melt(comparison, id.vars = idvars, variable.name = "vintage")
  setkey(       comparison, variable, geography)
  
  #calculate growthrates & mark quantiles
  rates <- unique( comparison[,.(geography, variable,varname, vintage, year,value,ind,vargroup,prices,code)])
  rates <- dcast(rates,...~vintage)
  
  rates[, `:=`(growth     = ifelse(shift(value) %in% c(0,.001), 0, value / shift(value) - 1),
               origgrowth = origval / shift(origval) - 1)                                   , by = list(variable, geography)]
  
  rates[!value %in% c(.001, 0) & origval != value, quant := ifelse(shift(value) %in% 0, NA,cut(growth,unique(c(-Inf,unique(quantile(growth[!growth %in% c(0,1,-1) ], probs = 0:100/100, na.rm = T)),Inf)), include.lowest = TRUE, labels = FALSE)), by = year]
  rates[, check := quant > 99 | quant < 2]#[,filter := ifelse(shift(check,type = "lead") %in% TRUE, TRUE, check)]

  # prepare for plotting
  data <- copy(rates)
  
  data <- data[ grepl(paste0("^",industryvar.mnems,collapse = "|"),variable)]
  if (all(sectorgroup != "")) {data <- data[grepl(paste0("^",sectorgroup,collapse = "|"),code)]}
  
  setkey(data, variable, geography)
  
  dataT     <- data[ind %in% industry.mnems][check == TRUE][,.(geography,variable, year, prices,ind,check,vargroup)]
  dataT$ind <- factor(as.character(dataT$ind), levels = rev(industry.mnems))
  
  # plot extreme values for industry vars
  p <- ggplot(dataT[geography == geo], aes(year, ind)) +
    geom_point() +
    facet_wrap(c("vargroup","prices")) +
    theme(legend.position = c(.9,.2), plot.title = element_text( hjust = .5, vjust = 0.5, face = 'bold'),
          axis.text.y = element_text(size = 4)) +
    ggtitle(paste("Summary Chart",geo, sep = "-"))
  
  
  pdf(path, width = 11, height = 8)
  print(p)
  dev.off()
  
  
  file <- system(paste0('cmd /c "', path, '"'),wait = FALSE, ignore.stderr = TRUE)
  file

  #print(p)

  if (skip_detail == TRUE) {break}
  
  # add detail plot
  for (i in 1:100) {
    
    uservar <- toupper(readline(prompt = "Which variable group for detail (no quotes): "))
    
    if (!uservar %in% unique(dataT$vargroup)) {next}
    
    k <- ggplot(dataT[geography == geo & vargroup == uservar], aes(year, ind)) +
      geom_point(color = "red") +
      facet_wrap(c("vargroup","prices")) +
      theme(axis.text.y = element_text(size = 8)) 
    
    dev.new()
    print(k)
    
    # give additional 
    for (i in 1:100) {
      uservar2 <- toupper(readline(prompt = "If you want additional sector detail write sector mnemonic (add '!' for nominal values). 
                                   To continue press enter:"))
      sector   <- paste0(uservar,uservar2)
      if (uservar2 == "") {
        break
      }else{
        print(setorder(data[.(sector, geo), .(geography,variable, year, value, growth,origval,prices,"extreme_val" = check), nomatch = 0],prices,year))
      }
    }
    
    uservar3 <- readline(prompt = "return to summary? (y/n): ")   
     
    if (uservar3 == "y") {
       #dev.off()
     }else{
         break
       }
  }

}

#"ALBERTA"  "B_COLUMB" "CANADA"   "MANITOBA" "NBRUNSWK" "NFOUNLND" "NORTHWT"  "NOVA_SCO" "NUNAVUT"  "ONTARIO"  "PRINCE_E" "QUEBEC"  
#"SASKCHWN" "YUKON" 
#produce_summary("B_COLUMB","3")




