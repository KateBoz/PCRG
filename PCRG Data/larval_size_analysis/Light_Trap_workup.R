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

m.data$Stage[which(m.data$Stage=="megalopae")]="Megalopae"
m.data$Stage[which(m.data$Stage=="instar")]="Instar"
m.data<-m.data[-which(is.na(m.data$Stage)),] #remove na rows for Stage
m.data<-m.data[-which(is.na(m.data$Date)),] #remove na rows for Date



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

my_theme<- theme_bw(base_size = 11)+
  theme(axis.title.x = element_text(vjust = -1),
        axis.text.x = element_text(colour = "black"),
        axis.title.y = element_text(vjust = 3),
        axis.text.y = element_text(colour = "black"),
        legend.key.width = unit(2, "line"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(hjust = 0.5),
        #legend.position = c(0.9, 0.6),
        plot.margin= unit(c(0.3,1.5,0.2,1.5), "cm"))



#table of means
means.table<-m.data%>%group_by(Stage,Month)%>%summarise(mean=mean(CW,na.rm=T),N=length(CW))
write.xlsx(means.table,"Means_Table.xlsx")



#plot of all the data
all.data.hist<-ggplot(m.data, aes(x=CW))+
  geom_histogram(aes(fill=Stage),col="grey22")+
  my_theme+
  ylab("Count")+
  xlab("Carapace Width (mm)")+
  ggtitle("Size Distribution of Megalopae and J1 Instars from 2019")+
  geom_vline(dat=means.table,aes(xintercept=mean, col=Stage),lwd=1, alpha = 0.6)+
  scale_fill_manual(values=c("limegreen","grey90"))+
  scale_color_manual(values=c("grey10","red"))+
  facet_grid(Month~.)

all.data.hist

ggsave(paste("all_data_hist_", Sys.Date(), ".pdf", sep=""))


#Statistical models
mega<-subset(m.data,Stage=="Megalopae")

lm1<-aov(mega$CW~factor(mega$Month))
summary(lm1)
TukeyHSD(lm1)


instar<-subset(m.data,Stage=="Instar")

lm2<-aov(instar$CW~factor(instar$Month))
summary(lm2)
TukeyHSD(lm2)


#Scatterplot by Date

#line
all.data.line<-ggplot(m.data, aes(x=Date,y=CW,group=Stage))+
  stat_smooth(method="lm",se=F)+
  geom_point(aes(col=Stage),alpha = 0.5,size=2)+
  my_theme+
  xlab("Date")+
  ylab("Carapace Width (mm)")+
  scale_color_manual(values=c("limegreen","grey30"))+
  facet_grid(Organization~.)

all.data.line
ggsave(paste("all_data_line_", Sys.Date(), ".pdf", sep=""))


#statistical model

lm3<-lm(mega$CW~mega$Date*mega$Organization)
summary(lm3)

#looking at the lenght/height

all.data.line2<-ggplot(m.data, aes(x=CW,y=CH,group=Organization))+
  geom_point(aes(col=Organization),alpha = 0.3,size=2.5)+
  stat_smooth(aes(col=Organization),method="lm",se=F, alpha = 0.3)+
  my_theme+
  ylab("Carapace Height (mm)")+
  xlab("Carapace Width (mm)")+
  facet_grid(Stage~.)

all.data.line2

ggsave(paste("carapace_width_height_", Sys.Date(), ".pdf", sep=""))





