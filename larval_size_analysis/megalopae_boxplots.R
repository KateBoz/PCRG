###################################################
# Build distribution plots with Light trap data
# Created: Katelyn Bosley
# Date: 9/26/2020
###################################################


#This code uses the compiled data that Matt N. put together

rm(list=ls(all=TRUE))

# load libraries 
# I stole what Matt used because it is pretty slick for installing if the libarary if not already installed
load.libraries<-function(){
  if(!require("gtools")) install.packages("gtools"); library(gtools);
  if(!require("stringr")) install.packages("stringr"); library(stringr);
  if(!require("readxl")) install.packages("readxl"); library(readxl);
  if(!require("openxlsx")) install.packages("openxlsx"); library(openxlsx);
  if(!require("RColorBrewer")) install.packages("RColorBrewer"); library(RColorBrewer);
  if(!require("qdapRegex")) install.packages("qdapRegex"); library(qdapRegex);
  if(!require("tidyverse")) install.packages("tidyverse"); library(tidyverse);
  if(!require("mltools")) install.packages("mltools"); library(mltools);
}
  
load.libraries()


#setting the working directory
wd<-"C:\\Users\\boskm477\\Desktop\\Light_Trap\\PCRG Data\\larval_size_analysis"
setwd(wd)

#Read in the data for making the plots. 
#I am just pulling data from the location where it was put from the data compilation code.

#read in count data for making a location/organization lookup table
c.data<-read.csv("C:\\Users\\boskm477\\Desktop\\Light_Trap\\PCRG Data\\PCRG Data\\output\\compiled_2019_count_data.csv")

#create table
site.table<-c.data%>%distinct(Site,Organization)

m.data<-read.csv("C:\\Users\\boskm477\\Desktop\\Light_Trap\\PCRG Data\\PCRG Data\\output\\compiled_2019_measurement_data.csv")
m.data<-m.data[,-1]

#add the Organization name to the measurement data
names(m.data)[2]<-"Site"
m.data<-left_join(m.data,site.table)



#need to standardize the stage catagory names and cleaning the data
unique(m.data$Stage)

m.data<-na.omit(m.data)
m.data[which(m.data$Stage=="megalopae"),]="Megalopae"
m.data[which(m.data$Stage=="instar"),]="Instar"
#m.data<-m.data[!which(is.na(m.data$Stage)),] #remove na rows for Stage
#m.data<-m.data[-which(is.na(m.data$Date)),] #remove na rows for Date



#adjust the class of df cols
sapply(m.data,class)

m.data$Date<-as.Date(m.data$Date)
m.data$Site<-as.factor(m.data$Site)
m.data$CW<-as.numeric(m.data$CW)
m.data$CH<-as.numeric(m.data$CH)
m.data$TH<-as.numeric(m.data$TH)
m.data$Stage<-as.factor(m.data$Stage)
m.data$Organization<-as.factor(m.data$Organization)
m.data$Month<-as.numeric(format(m.data$Date, "%m"))


#ready to plot

#create theme for plotting

my_theme<- theme_bw(base_size = 10)+
  theme(axis.title.x = element_text(vjust = -1),
        axis.text.x = element_text(colour = "black"),
        axis.title.y = element_text(vjust = 3),
        axis.text.y = element_text(colour = "black"),
        legend.key.width = unit(2, "line"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        #legend.position = c(0.9, 0.6),
        plot.margin= unit(c(0.3,1.5,0.2,1.5), "cm"))



all.data<-ggplot(m.data, aes(x=CW))+
  geom_histogram(aes(fill=Stage))+
  my_theme+
  ylab("Count")+
  xlab("Carapace Width (mm)")+
  facet_grid(Month~.)




all.data



