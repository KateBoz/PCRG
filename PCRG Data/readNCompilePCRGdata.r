rm(list=ls(all=TRUE))
options(stringsAsFactors=F)

if(!require("gtools")) install.packages("gtools"); library(gtools);
if(!require("stringr")) install.packages("stringr"); library(stringr);
if(!require("reshape2")) install.packages("reshape2"); library(reshape2);
if(!require("readxl")) install.packages("readxl"); library(readxl);
if(!require("openxlsx")) install.packages("openxlsx"); library(openxlsx);
if(!require("ggplot2")) install.packages("ggplot2"); library(ggplot2);
if(!require("RColorBrewer")) install.packages("RColorBrewer"); library(RColorBrewer);
if(!require("qdapRegex")) install.packages("qdapRegex"); library(qdapRegex);
if(!require("gridExtra")) install.packages("gridExtra"); library(gridExtra);

#Source scripts in source folder
for (nm in list.files("Source", pattern = "\\.[RrSsQq]$")) {
  #if(trace) cat(nm,":")
  source(file.path("Source", nm))
  # if(trace) cat("\n")
}

# read in all unprocessed Light trap data in directory as 2 lists
# one list for each tab in PCRG datasheets
# Datasheets must be current PCRG format with 2 tabs, first tab for counts, second tab for measurements, .xlsx only
files <- list.files(path = "PCRG raw datasheets/",pattern = ".xlsx")
files <- paste("PCRG raw datasheets/", files, sep="")

###CRAB COUNTS
# getting larval counts from first sheet of excel files
# check individual date cell formatting in excel sheet, may generate NAs if not correct
countsList <- lapply(files, readNcleanXLSXcrabCounts)
counts_allDone <- data.frame(do.call(rbind, countsList))
colnames(counts_allDone)[1] <- "date"
counts_split <- split(counts_allDone, counts_allDone$Organization)


###CRAB MEASUREMENTS
# getting crab measurements from second sheet of excel files
# check individual date cell formatting in excel sheet, may generate NAs if not correct
# format should be 'general', also check for trailing spaces
# you may have to go to individual cells in the raw data sheet to figure out the issue
measureList <- lapply(files, readNcleanXLSXcrabMeasurements)
measure_allDone <- data.frame(do.call(rbind, measureList))

# getting rid of lines where all 3 measurements are NA
# blank lines erroneously read in
measure_allDone <- measure_allDone[!(is.na(measure_allDone$CW) & is.na(measure_allDone$CH & is.na(measure_allDone$TH))),]

#making sure measurment columns are numerics
###### GRAPHING ###########



####Time series without tidal range #####


lightTrapGraph <- function(df, counts_allDone){ 
  ggplot(df, aes(x=date, y=TotalDungenessCPUE, col=Site)) + 
    geom_line(size = 1.05 )+
    theme_bw(base_size = 10)+
    theme(axis.title.x = element_text(vjust = -1),
          axis.text.x = element_text(colour = "black"),
          axis.title.y = element_text(vjust = 3),
          axis.text.y = element_text(colour = "black"),
          legend.key.width = unit(2, "line"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position = c(0.9, 0.6),
          plot.margin= unit(c(0.3,1.5,0.2,1.5), "cm"))+
    scale_x_date(date_breaks = "months", date_labels = "%b")+
    xlab("Date")+
    ggtitle(paste("Dungeness Crab Captured by ",unique(df$Organization)," Light Traps - 2019", sep=""))+
    ylab("Total Dungeness Crab CPUE")+
    scale_color_brewer(palette = "Set1")+ 
    coord_cartesian(xlim = c(min(counts_allDone$date), max(counts_allDone$date)))#,
                    #ylim = c(0, max(counts_allDone$TotalDungenessCPUE, na.rm=T)))
}




plots <-lapply(counts_split, lightTrapGraph, counts_allDone)
ggsave(paste("Light_Traps_2019_scaled_", Sys.Date(), ".pdf", sep=""), 
       gridExtra::marrangeGrob(grobs = plots, nrow=4, ncol=1, top=NULL), width=8.5, height=11)




###### Time series with tidal Range #####

# reading in tide data
tidalRange <- read.csv("other input/dailyTidalRange2019_Seattle.csv")
#converting date column
tidalRange$date <- as.Date(tidalRange$date, "%m/%d/%Y")

# function to merge dataframe with dide data
tideMerge <- function(df, tidalRange){
   df2 <- merge(df, tidalRange, all=T)
   df2$Organization <- unique(df$Organization[!is.na(df$Organization)])
   return(df2)
}

# adding tide data to each dataframe in list
countTidesSplit <- lapply(counts_split, tideMerge, tidalRange)
# combining dataframes in list, need full dataframe for function to run correctly (range calculations)
countsWithTides <- merge(counts_allDone, tidalRange, all=T)


#### need to get NA's out of Site
#testing
#df<- counts_split[[8]]
lightTrapTideGraph <- function(df, countsWithTides){ 
  ggplot(df, aes(x=date)) + 
    geom_line(aes(y=TotalDungenessCPUE, col = Site),size = 1)+
    geom_line(data = countsWithTides, aes(x=date ,y=maxDailyTidal*80), colour = "black", size = 0.5, linetype = "dotted")+
    scale_y_continuous(sec.axis = sec_axis(~./80, name = "Max Daily Tidal Exchange"))+
    theme_bw(base_size = 10)+
    theme(axis.title.x = element_text(vjust = -1),
          axis.text.x = element_text(colour = "black"),
          axis.title.y = element_text(vjust = 3),
          axis.text.y = element_text(colour = "black"),
          axis.title.y.right = element_text(vjust = 3),
          axis.text.y.right = element_text(colour = "black"),
          legend.key.width = unit(2, "line"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position = c(0.9, 0.6),
          plot.margin= unit(c(0.3,1.5,0.2,1.5), "cm"))+
    scale_x_date(date_breaks = "months", date_labels = "%b")+
    xlab("Date")+
    ggtitle(paste("Dungeness Crab Captured by ",unique(df$Organization)," Light Traps - 2019", sep=""))+
    ylab("Total Dungeness Crab CPUE")+
    scale_color_brewer(palette = "Set1")+ 
    coord_cartesian(xlim = c(min(countsWithTides$date), max(countsWithTides$date)),
    ylim = c(0, max(countsWithTides$TotalDungenessCPUE, na.rm=T)))
}


plots <-lapply(counts_split, lightTrapTideGraph, countsWithTides)
ggsave(paste("output/Light_Traps_2019_scaled_w_Tides", Sys.Date(), ".pdf", sep=""), 
       gridExtra::marrangeGrob(grobs = plots, nrow=4, ncol=1, top=NULL), width=8.5, height=11)


#testing
#df<- counts_split[[8]]
lightTrapTideGraph_scaler <- function(df, countsWithTides){ 
  # scalar to keep second y axis consistent 
  scaler<- max(df$TotalDungenessCPUE, na.rm =T)/max(countsWithTides$maxDailyTidal, na.rm =T)
  #graph
  ggplot(df, aes(x=date)) + 
    geom_line(aes(y=TotalDungenessCPUE, col = Site),size = 1)+
    geom_line(data = countsWithTides, aes(x=date ,y=maxDailyTidal*scaler), colour = "black", size = 0.5, linetype = "dotted")+
    scale_y_continuous(sec.axis = sec_axis(~./scaler, name = "Max Daily Tidal Exchange"))+
    theme_bw(base_size = 10)+
    theme(axis.title.x = element_text(vjust = -1),
          axis.text.x = element_text(colour = "black"),
          axis.title.y = element_text(vjust = 3),
          axis.text.y = element_text(colour = "black"),
          axis.title.y.right = element_text(vjust = 3),
          axis.text.y.right = element_text(colour = "black"),
          legend.key.width = unit(2, "line"),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position = c(0.9, 0.6),
          plot.margin= unit(c(0.3,1.5,0.2,1.5), "cm"))+
    scale_x_date(date_breaks = "months", date_labels = "%b")+
    xlab("Date")+
    ggtitle(paste("Dungeness Crab Captured by ",unique(df$Organization)," Light Traps - 2019", sep=""))+
    ylab("Total Dungeness Crab CPUE")+
    scale_color_brewer(palette = "Set1")+ 
    coord_cartesian(xlim = c(min(countsWithTides$date), max(countsWithTides$date)))
}


plots <-lapply(counts_split, lightTrapTideGraph_scaler, countsWithTides)
ggsave(paste("output/Light_Traps_2019_w_Tides", Sys.Date(), ".pdf", sep=""), 
       gridExtra::marrangeGrob(grobs = plots, nrow=4, ncol=1, top=NULL), width=8.5, height=11)
