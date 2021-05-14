library(stringr)
library(base)
#Setting the PWD
setwd("C:/Users/saima/OneDrive/Desktop/Project/Data")
getwd()
# Data for 2010 files 
setwd("C:/Users/saima/OneDrive/Desktop/Project/Data")

#Recursively reading the files belonging to quarters into a dataframe for merging


# Merging Multiple files from the data : Since the data has been collected over a period of time on a quarterly basis starting from 2010 - 2020 and available as multiple files and we want to analyze it all in one go,   I combined all the files into a single data frame. 
# Data Cleaning : Prior to the data merging step, each file needed some amount of cleaning. Each file had a few columns for footnotes giving information about the characteristics of the Home Health Agencies. Since these columns were empty, they had to be dropped.  In addition to that, the number of columns changed over time in each file as some of the measures were added to or dropped from the Home Health Compare datasets. Therefore, only the columns in common across all the years have been retained for consistency and to prevent missing data.  After combining the data I had to perform a few data type conversions. Since there were many missing values in my data frame and some of the missing values were represented using codes such as 199 and 201, I had to make a plot to understand if there was any pattern in how the data was missing and since there was no significant pattern observed, I have removed all the missing values. 

Lst1 <- list("0101"=1, "0401"=2, "0701"=3, "1001"=4)
for (file in list.files(pattern = "^Provider", recursive = TRUE)) {
  quarter <- str_sub(strsplit(file, "/")[[1]][2],-4,-1)
  assign(paste("df",strsplit(file, "/")[[1]][1],"_",Lst1[quarter], sep = ""),read.csv(file, sep=',', na.strings = "NotAvailable"))
}
Lst2 <- list("01"=1, "04"=2, "07"=3, "10"=4, "06"=3, "03"=2, "08"=2)
for (file in list.files(pattern = "^HHC_SOCRATA_PRVDR", recursive = TRUE)) {
  quarter <- strsplit(strsplit(file, "/")[[1]][2], "_")[[1]][5]
  assign(paste("df",strsplit(file, "/")[[1]][1],"_",Lst2[quarter], sep = ""),read.csv(file, sep=',', na.strings = "NotAvailable"))
}
df2020_3 <- read.csv("C:/Users/saima/OneDrive/Desktop/Project/Data/2020/home_health_services_archive_10_2020/HH_Provider_Oct2020.csv", sep=',', na.strings = "NotAvailable")
df2020_4 <- read.csv("C:/Users/saima/OneDrive/Desktop/Project/Data/2020/home_health_services_archive_12_2020/HH_Provider_Oct2020.csv", sep=',', na.strings = "NotAvailable")

### Defining the data types and feature names to be used later during the merging and cleaning process 
mytypes27 <- c(rep('c',3),rep('f',3),'c','f','c',rep('f',6),rep('n',12))
names27 <- c('CCN','name','address','city','state','Zip','phone','type','date','off.nursing','off.physical','off.occupational','off.speech','off.medical','off.hha','betterwalking','betterbed','bladdercontrol','lesspain','betterbathing','betterdrug','lessshortbreath','admitted','ER','stayathome','medicalwound','woundimproved')

mytypes38 <- c(rep('c',3),rep('f',3),'c','f','c',rep('f',6),rep('n',23))
names38 <- c('CCN','name','address','city','state','Zip','phone','type','date','off.nursing','off.physical','off.occupational','off.speech','off.medical','off.hha','timely','taughtdrugs','checkfall','checkdepression','flushot','pnumococcal','taughtfootcare','checkpain','treatpain','treatheart','actionsores','treatsores','checksores','betterwalking','betterbed','betterbathing','lesspain','betterbreathing','betterheal','moresore','betterdrug','ER','admitted')

mytypes37 <- c(rep('c',3),rep('f',3),'c','f','c',rep('f',6),rep('n',22))
names37 <- c('CCN','name','address','city','state','Zip','phone','type','date','off.nursing','off.physical','off.occupational','off.speech','off.medical','off.hha','timely','taughtdrugs','checkfall','checkdepression','flushot','pnumococcal','taughtfootcare','checkpain','treatpain','treatheart','actionsores','treatsores','checksores','betterwalking','betterbed','betterbathing','lesspain','betterbreathing','betterheal','betterdrug','ER','admitted')


mytypes60 <- c('f',rep('c',3),'f',rep('c',2),rep('f',7),'c',rep(c('n','c'),22),'c')
names60 <- c('state','CCN','name','address','city','Zip','phone','type','off.nursing','off.physical','off.occupational','off.speech','off.medical','off.hha','date','timely','note-timely','taughtdrugs','note-taughtdrugs','checkfall','note-checkfall','checkdepression','note-checkdepression','flushot','note-flushot','pnumococcal','note-pnumococcal','taughtfootcare','note-tautfootcare','checkpain','note-checkpain','treatpain','note-treatpain','treatheart','note-treatheart','actionsores','note-actionsores','treatsores','note-treatsores','checksores','note-checksores','betterwalking','note-betterwalking','betterbed','note-betterbed','betterbathing','note-betterbathing','lesspain','note-lesspain','betterbreathing','note-betterbreathing','betterheal','note-betterheal','betterdrug','note-betterdrug','ER','note-ER','admitted','note-admitted','note')

mytypes66_1 <- c('f',rep('c',3),'f',rep('c',2),rep('f',7),'c',rep(c('n','c'),23),rep(c('f','c'),2),'c')
names66_1 <- c('state','CCN','name','address','city','Zip','phone','type','off.nursing','off.physical','off.occupational','off.speech','off.medical','off.hha','date','rating','note-rating','timely','note-timely','taughtdrugs','note-taughtdrugs','checkfall','note-checkfall','checkdepression','note-checkdepression','flushot','note-flushot','pnumococcal','note-pnumococcal','taughtfootcare','note-tautfootcare','checkpain','note-checkpain','treatpain','note-treatpain','treatheart','note-treatheart','betterwalking','note-betterwalking','betterbed','note-betterbed','betterbathing','note-betterbathing','lesspain','note-lesspain','betterbreathing','note-betterbreathing','betterheal','note-betterheal','actionsores','note-actionsores','treatsores','note-treatsores','checksores','note-checksores','betterdrug','note-betterdrug','ER','note-ER','admitted','note-admitted','readmit','note-readmit','emergency','note-emergency','note')

mytypes66_2 <- c('f',rep('c',3),'f',rep('c',2),rep('f',7),'c',rep(c('n','c'),23),rep(c('f','c'),2),'c')
names66_2 <- c('state','CCN','name','address','city','Zip','phone','type','off.nursing','off.physical','off.occupational','off.speech','off.medical','off.hha','date','rating','note-rating','timely','note-timely','taughtdrugs','note-taughtdrugs','checkfall','note-checkfall','checkdepression','note-checkdepression','flushot','note-flushot','pnumococcal','note-pnumococcal','taughtfootcare','note-tautfootcare','checkpain','note-checkpain','treatpain','note-treatpain','treatheart','note-treatheart','betterwalking','note-betterwalking','betterbed','note-betterbed','betterbathing','note-betterbathing','lesspain','note-lesspain','betterbreathing','note-betterbreathing','betterheal','note-betterheal','actionsores','note-actionsores','treatsores','note-treatsores','checksores','note-checksores','betterdrug','note-betterdrug','admitted','note-admitted','ER','note-ER','readmit','note-readmit','emergency','note-emergency','note')

mytypes54 <- c('f',rep('c',3),'f',rep('c',2),rep('f',7),'c',rep(c('n','c'),17),rep(c('f','c'),2),'c')
names54 <- c('state','CCN','name','address','city','Zip','phone','type','off.nursing','off.physical','off.occupational','off.speech','off.medical','off.hha','date','rating','note-rating','timely','note-timely','taughtdrugs','note-taughtdrugs','checkfall','note-checkfall','checkdepression','note-checkdepression','flushot','note-flushot','pnumococcal','note-pnumococcal','taughtfootcare','note-tautfootcare','betterwalking','note-betterwalking','betterbed','note-betterbed','betterbathing','note-betterbathing','lesspain','note-lesspain','betterbreathing','note-betterbreathing','betterheal','note-betterheal','betterdrug','note-betterdrug','admitted','note-admitted','ER','note-ER','readmit','note-readmit','emergency','note-emergency','note')

mytypes63 <- c('f',rep('c',3),'f',rep('c',2),rep('f',7),'c',rep(c('n','c'),17),rep(c('f','c'),2),'f','n','c','f','c',rep(c('n','c'),2),'n')
names63 <- c('state','CCN','name','address','city','Zip','phone','type','off.nursing','off.physical','off.occupational','off.speech','off.medical','off.hha','date','rating','note-rating','timely','note-timely','taughtdrugs','note-taughtdrugs','checkfall','note-checkfall','checkdepression','note-checkdepression','flushot','note-flushot','pnumococcal','note-pnumococcal','taughtfootcare','note-tautfootcare','betterwalking','note-betterwalking','betterbed','note-betterbed','betterbathing','note-betterbathing','lesspain','note-lesspain','betterbreathing','note-betterbreathing','betterheal','note-betterheal','betterdrug','note-betterdrug','admitted','note-admitted','ER','note-ER','readmit','note-readmit','emergency','note-emergency','note','newulcers','note-newulcers','remained','note-remained','medication','note-medication','spend','note-spend','episodes')

mytypes79 <- c('f',rep('c',3),'f',rep('c',2),rep('f',7),'c',rep(c('n','c'),17),rep(c('f','c'),2),'f','n','c','f','c','n','c',rep('n',6),'f','c',rep('n',6),'f','c','n','c','n')
names79 <- c('state','CCN','name','address','city','Zip','phone','type','off.nursing','off.physical','off.occupational','off.speech','off.medical','off.hha','date','rating','note-rating','timely','note-timely','taughtdrugs','note-taughtdrugs','checkfall','note-checkfall','checkdepression','note-checkdepression','flushot','note-flushot','pnumococcal','note-pnumococcal','taughtfootcare','note-tautfootcare','betterwalking','note-betterwalking','betterbed','note-betterbed','betterbathing','note-betterbathing','lesspain','note-lesspain','betterbreathing','note-betterbreathing','betterheal','note-betterheal','betterdrug','note-betterdrug','admitted','note-admitted','ER','note-ER','readmit','note-readmit','emergency','note-emergency','note','newulcers','note-newulcers','remained','note-remained','medication','note-medication','DTC1','DTC2','DTC3','DTC4','DTC5','DTC6','DTC7','note-DTC','PPR1','PPR2','PPR3','PPR4','PPR5','PPR6','PPR7','note-PPR','spend','note-spend','episodes')

mytypes73 <- c('f',rep('c',3),'f',rep('c',2),rep('f',7),'c',rep(c('n','c'),17),'f','n','c','n','c',rep('n',6),'f','c',rep('n',6),'f','c','n','c','n')
names73 <- c('state','CCN','name','address','city','Zip','phone','type','off.nursing','off.physical','off.occupational','off.speech','off.medical','off.hha','date','rating','note-rating','timely','note-timely','taughtdrugs','note-taughtdrugs','checkfall','note-checkfall','checkdepression','note-checkdepression','flushot','note-flushot','pnumococcal','note-pnumococcal','taughtfootcare','note-tautfootcare','betterwalking','note-betterwalking','betterbed','note-betterbed','betterbathing','note-betterbathing','lesspain','note-lesspain','betterbreathing','note-betterbreathing','betterheal','note-betterheal','betterdrug','note-betterdrug','admitted','note-admitted','ER','note-ER','note','newulcers','note-newulcers','medication','note-medication','DTC1','DTC2','DTC3','DTC4','DTC5','DTC6','DTC7','note-DTC','PPR1','PPR2','PPR3','PPR4','PPR5','PPR6','PPR7','note-PPR','spend','note-spend','episodes')


mytypes71 <- c('f',rep('c',3),'f',rep('c',2),rep('f',7),'c',rep(c('n','c'),16),'f','n','c','n','c',rep('n',6),'f','c',rep('n',6),'f','c','n','c','n')
names71 <- c('state','CCN','name','address','city','Zip','phone','type','off.nursing','off.physical','off.occupational','off.speech','off.medical','off.hha','date','rating','note-rating','timely','note-timely','taughtdrugs','note-taughtdrugs','checkfall','note-checkfall','checkdepression','note-checkdepression','flushot','note-flushot','pnumococcal','note-pnumococcal','taughtfootcare','note-tautfootcare','betterwalking','note-betterwalking','betterbed','note-betterbed','betterbathing','note-betterbathing','betterbreathing','note-betterbreathing','betterheal','note-betterheal','betterdrug','note-betterdrug','admitted','note-admitted','ER','note-ER','note','newulcers','note-newulcers','medication','note-medication','DTC1','DTC2','DTC3','DTC4','DTC5','DTC6','DTC7','note-DTC','PPR1','PPR2','PPR3','PPR4','PPR5','PPR6','PPR7','note-PPR','spend','note-spend','episodes')

mytypes70 <- c('f',rep('c',3),'f',rep('c',2),rep('f',7),'c',rep(c('n','c'),16),'n','c','n','c',rep('n',6),'f','c',rep('n',6),'f','c','n','c','n')
names70 <- c('state','CCN','name','address','city','Zip','phone','type','off.nursing','off.physical','off.occupational','off.speech','off.medical','off.hha','date','rating','note-rating','timely','note-timely','taughtdrugs','note-taughtdrugs','checkfall','note-checkfall','checkdepression','note-checkdepression','flushot','note-flushot','pnumococcal','note-pnumococcal','taughtfootcare','note-tautfootcare','betterwalking','note-betterwalking','betterbed','note-betterbed','betterbathing','note-betterbathing','betterbreathing','note-betterbreathing','betterheal','note-betterheal','betterdrug','note-betterdrug','admitted','note-admitted','ER','note-ER','newulcers','note-newulcers','medication','note-medication','DTC1','DTC2','DTC3','DTC4','DTC5','DTC6','DTC7','note-DTC','PPR1','PPR2','PPR3','PPR4','PPR5','PPR6','PPR7','note-PPR','spend','note-spend','episodes')



INTG_types <- c('character','factor',rep('character',2),'factor',rep('character',2),rep('factor',7),'character',rep('numeric',17))
INTG_names <- c('CCN','state','name','address','city','Zip','phone','type','off.nursing','off.physical','off.occupational','off.speech','off.medical','off.hha','date','rating','timely','taughtdrugs','checkfall','checkdepression','flushot','pnumococcal','taughtfootcare','betterwalking','betterbed','betterbathing','lesspain','betterbreathing','betterheal','betterdrug','admitted','ER')
INTG_names ##32


type_RPT <- c(rep('character',3),'numeric',rep('character',14))
name_RPT <- c('RPT_REC_NUM','PRVDR_CTRL_TYPE_CD','PRVDR_NUM','NPI','RPT_STUS_CD','FY_BGN_DT','FY_END_DT','PROC_DT','INITL_RPT_SW','LAST_RPT_SW','TRNSMTL_NUM','FI_NUM','ADR_VNDR_CD','FI_CREAT_DT','UTIL_CD','NPR_DT','SPEC_IND','FI_RCPT_DT')
type_NMRC <- c(rep('character',4),'numeric')
name_NMRC <- c('RPT_REC_NUM','WKSHT_CD','LINE_NUM','CLMN_NUM','ITM_VAL_NUM')

#Function for Recursively reading the episode data
Episode <- function(df_rpt, df_nmrc) {
  names(df_rpt) <- name_RPT
  dim(df_rpt)
  names(df_nmrc) <- name_NMRC
  dim(df_nmrc)
  df_nmrc_ext <- df_nmrc[which( df_nmrc$WKSHT_CD=='S300000' 
                                & df_nmrc$LINE_NUM=='04500'
                                & (df_nmrc$CLMN_NUM == '0100' | df_nmrc$CLMN_NUM == '0200' | df_nmrc$CLMN_NUM == '0300' | df_nmrc$CLMN_NUM == '0400' | df_nmrc$CLMN_NUM == '0500' | df_nmrc$CLMN_NUM == '0600')
  ),]
  dim(df_nmrc_ext)
  table(df_nmrc_ext$WKSHT_CD)
  table(df_nmrc_ext$LINE_NUM)
  table(df_nmrc_ext$CLMN_NUM)
  episode_ext <- aggregate(df_nmrc_ext$ITM_VAL_NUM,list(df_nmrc_ext$RPT_REC_NUM),sum)
  names(episode_ext) <- c('RPT_REC_NUM','ITM_VAL_NUM')
  dim(episode_ext)
  episode <- merge(df_rpt,episode_ext,by='RPT_REC_NUM')[,c(3,19)]
  names(episode) <- c('CCN','episode')
  ### some CCN has more than one RPT_REC_NUM, so I have to average the results, otherwise there would be multiple records for one CCN
  episode <- aggregate(episode$episode,list(episode$CCN),mean)
  names(episode) <- c('CCN','episode')
  dim(episode)
  return(episode)
}

episode2010 = Episode(read.csv("2010/HHAFY2010/HHA_2010_RPT.CSV",sep=',',colClasses=type_RPT,check.names = F), read.csv("2010/HHAFY2010/HHA_2010_NMRC.CSV",sep=',',colClasses=type_NMRC,check.names = F))
episode2011 = Episode(read.csv("2011/HHAFY2011/HHA_2011_RPT.CSV",sep=',',colClasses=type_RPT,check.names = F), read.csv("2011/HHAFY2011/HHA_2011_NMRC.CSV",sep=',',colClasses=type_NMRC,check.names = F))
episode2012 = Episode(read.csv("2012/HHAFY2012/HHA_2012_RPT.CSV",sep=',',colClasses=type_RPT,check.names = F), read.csv("2012/HHAFY2012/HHA_2012_NMRC.CSV",sep=',',colClasses=type_NMRC,check.names = F))
episode2013 = Episode(read.csv("2013/HHAFY2013/HHA_2013_RPT.CSV",sep=',',colClasses=type_RPT,check.names = F), read.csv("2013/HHAFY2013/HHA_2013_NMRC.CSV",sep=',',colClasses=type_NMRC,check.names = F))
episode2014 = Episode(read.csv("2014/HHAFY2014/HHA_2014_RPT.CSV",sep=',',colClasses=type_RPT,check.names = F), read.csv("2014/HHAFY2014/HHA_2014_NMRC.CSV",sep=',',colClasses=type_NMRC,check.names = F))
episode2015 = Episode(read.csv("2015/HHAFY2015/HHA_2015_RPT.CSV",sep=',',colClasses=type_RPT,check.names = F), read.csv("2015/HHAFY2015/HHA_2015_NMRC.CSV",sep=',',colClasses=type_NMRC,check.names = F))
episode2016 = Episode(read.csv("2016/HHAFY2016/HHA_2016_RPT.CSV",sep=',',colClasses=type_RPT,check.names = F), read.csv("2016/HHAFY2016/HHA_2016_NMRC.CSV",sep=',',colClasses=type_NMRC,check.names = F))
episode2017 = Episode(read.csv("2017/HHAFY2017/HHA_2017_RPT.CSV",sep=',',colClasses=type_RPT,check.names = F), read.csv("2017/HHAFY2017/HHA_2017_NMRC.CSV",sep=',',colClasses=type_NMRC,check.names = F))
episode2018 = Episode(read.csv("2018/HHAFY2018/HHA_2018_RPT.CSV",sep=',',colClasses=type_RPT,check.names = F), read.csv("2018/HHAFY2018/HHA_2018_NMRC.CSV",sep=',',colClasses=type_NMRC,check.names = F))
episode2019 = Episode(read.csv("2019/HHAFY2019/HHA_2019_RPT.CSV",sep=',',colClasses=type_RPT,check.names = F), read.csv("2019/HHAFY2019/HHA_2019_NMRC.CSV",sep=',',colClasses=type_NMRC,check.names = F))
episode2020 = Episode(read.csv("2020/HHAFY2020/HHA_2020_RPT.CSV",sep=',',colClasses=type_RPT,check.names = F), read.csv("2020/HHAFY2020/HHA_2020_NMRC.CSV",sep=',',colClasses=type_NMRC,check.names = F))

#Recursively reading the Star Rating Data
Lst2 <- list("01"=1, "04"=2, "07"=3, "10"=4, "06"=3, "03"=2, "08"=2)
for (file in list.files(pattern = "^HHC_SOCRATA_HHCAHPS_PRVDR", recursive = TRUE)) {
  if (strsplit(file, "/")[[1]][1] != "2014" & strsplit(file, "/")[[1]][1] != "2015" & strsplit(file, "/")[[1]][1] != "2020") {
    quarter <- strsplit(strsplit(file, "/")[[1]][2], "_")[[1]][5]
    assign(paste("dfstar",strsplit(file, "/")[[1]][1],"_",Lst2[quarter], sep = ""),read.csv(file, sep=',',check.names = F)[,c(2,16)]) 
  }
}

names(dfstar2016_1) <- c('CCN','star')
names(dfstar2016_2) <- c('CCN','star')
names(dfstar2016_3) <- c('CCN','star')
names(dfstar2016_4) <- c('CCN','star')
names(dfstar2017_1) <- c('CCN','star')
names(dfstar2017_2) <- c('CCN','star')
names(dfstar2017_3) <- c('CCN','star')
names(dfstar2017_4) <- c('CCN','star')
names(dfstar2018_1) <- c('CCN','star')
names(dfstar2018_2) <- c('CCN','star')
names(dfstar2018_3) <- c('CCN','star')
names(dfstar2019_1) <- c('CCN','star')
names(dfstar2019_2) <- c('CCN','star')
names(dfstar2019_3) <- c('CCN','star')
names(dfstar2019_4) <- c('CCN','star')



setwd("C:/Users/saima/OneDrive/Desktop/Project/Data")
dfstar2020_1 <- read.csv("2020/hhs_revised_flatfiles_archive_01_2020/HHC_SOCRATA_HHCAHPS_PRVDR.csv",sep=',',check.names = F)[,c(2,16)]
dfstar2020_2 <- read.csv("2020/home_health_services_archive_08_2020/HHC_SOCRATA_HHCAHPS_PRVDR.csv",sep=',',check.names = F)[,c(1,2)]
dfstar2020_3 <- read.csv("2020/home_health_services_archive_10_2020/HHCAHPS_Provider_Oct2020.csv",sep=',',check.names = F)[,c(1,2)]
dfstar2020_4 <- read.csv("2020/home_health_services_archive_12_2020/HHCAHPS_Provider_Oct2020.csv",sep=',',check.names = F)[,c(1,2)]


names(dfstar2020_1) <- c('CCN','star')
names(dfstar2020_2) <- c('CCN','star')
names(dfstar2020_3) <- c('CCN','star')
names(dfstar2020_4) <- c('CCN','star')

df2012_2 <- subset(df2012_2, ProviderNum != 'NA')
df2013_1 <- subset(df2013_1, ProviderNum != 'NA')
df2013_3 <- subset(df2013_3, ProviderNum != 'NA')
df2013_4 <- subset(df2013_4, ProviderNum != 'NA')

colClasses <- function(df) {
  class <- unlist(lapply(unclass(df), class))
  return(as.data.frame(class))
}


#Reference - https://www.rdocumentation.org/packages/R.utils/versions/2.10.1/topics/colClasses
#Reference - 


`colClasses<-` <- function(df, value) {
  if (nchar(value) != ncol(df)) {
    stop("The number of columns in the dataframe does not match the number of characters in the vector of classes.")
  }
  if (!is.character(value)) {
    stop("The classes must be provided as a character vector containing elements of: \"c\" (character), \"n\" (numeric), \"f\" (factor), \"l\" (logical)")
  }
  value <- unlist(strsplit(value, ""))
  allowedClasses <- c("c", "n", "f", "l")
  for (i in 1:ncol(df)) {
    if (!any(value[[i]] %in% allowedClasses)) {
      stop(paste0("Unknown class \"", value[[i]], "\". Allowed classes are: \"c\", \"n\", \"f\", or \"l\""))
    }
    if (value[[i]] == "c") {
      df[[i]] <- as.character(df[[i]])
    } else if (value[[i]] == "n") {
      df[[i]] <- as.numeric(df[[i]])
    } else if (value[[i]] == "f") {
      df[[i]] <- as.factor(df[[i]])
    } else if (value[[i]] == "l") {
      df[[i]] <- as.logical(df[[i]])
    }
  }
  return(df)
}





#The functions below with names starting with clean are used to read data with specific number of features as explained by the function name for example, clean37 for CSV with 37 columns. Read them into dataframes, Clean them to remove unused text columns and ensure that the final dataframe will only the columns which appear accross all the years to ensure consistency. The functions even merge the episode data with the provider data.   
# We are concerned especially with the provider data.

Clean27 <- function(df, ep, y, s, t) {
  colClasses(df) <- paste(mytypes27, collapse="") #Set the column types
  names(df) <- names27 #set the column names
  setdiff(INTG_names,names27)  
  df_bf <- data.frame(df[,1],df[,5],df[,c(2:4)],df[,c(6:8)],df[,c(10:15)],df[,9]) #removing the unnecessary columns
  df_bf$rating <- NA #ensuring consistency to include the features which are required for analysis across all the years
  df_bf$timely <- NA
  df_bf$taughtdrugs <- NA
  df_bf$checkfall <- NA
  df_bf$checkdepression <- NA
  df_bf$flushot <- NA
  df_bf$pnumococcal <- NA
  df_bf$taughtfootcare <- NA
  df_bf1 <- data.frame(df_bf,df[,c(16:17)],df[,20],df[,19])
  df_bf1$betterbreathing <- NA
  df_bf1$betterheal <- NA
  df_bf2 <- data.frame(df_bf1,df[,21],df[,c(23:24)])
  names(df_bf2) <- INTG_names
  df_bf3 <- merge(x = df_bf2, y = ep, by = "CCN", all.x = TRUE)
  year <- rep(y,nrow(df))
  season <- rep(s,nrow(df))
  timeindex <- rep(t,nrow(df))
  star_rating <- rep(NA,nrow(df))
  df_bf3$star <- star_rating
  df_bf3$year <- year
  df_bf3$season <- season
  df_bf3$timeindex <- timeindex
  data <- df_bf3
  return(data)
}

data20101 <- Clean27(df2010_1, episode2010, 2010, 1, 1)
data20102 <- Clean27(df2010_2, episode2010, 2010, 2, 1)


Clean38 <- function(df, ep, y, t, s) {
  colClasses(df) <- paste(mytypes38, collapse="") 
  names(df) <- names38
  setdiff(INTG_names,names38)
  df_bf <- data.frame(df[,1],df[,5],df[,c(2:4)],df[,c(6:8)],df[,c(10:15)],df[,9])
  df_bf$rating <- NA
  df_bf1 <- data.frame(df_bf,df[,c(16:22)],df[,29:34],df[,36],df[,38:37])
  names(df_bf1) <- INTG_names
  df_bf2 <- merge(x = df_bf1, y = ep, by = "CCN", all.x = TRUE)
  year <- rep(y,nrow(df))
  season <- rep(s,nrow(df))
  timeindex <- rep(t,nrow(df))
  star_rating <- rep(NA,nrow(df))
  df_bf2$star <- star_rating
  df_bf2$year <- year
  df_bf2$season <- season
  df_bf2$timeindex <- timeindex
  data <- df_bf2  
  return(data)
  }

data20104 <- Clean38(df2010_4, episode2010, 2010, 4, 4)
data20111 <- Clean38(df2011_1, episode2011, 2011, 5, 1)
data20112 <- Clean38(df2011_2, episode2011, 2011, 6, 2)
data20113 <- Clean38(df2011_3, episode2011, 2011, 7, 3)
data20114 <- Clean38(df2011_4, episode2011, 2011, 8, 4)

Clean37 <- function(df, ep, y, t, s) {
  colClasses(df) <- paste(mytypes37, collapse="")
  names(df) <- names37
  setdiff(INTG_names,names37)
  df_bf <- data.frame(df[,1],df[,5],df[,c(2:4)],df[,c(6:8)],df[,c(10:15)],df[,9])
  df_bf$rating <- NA
  df_bf1 <- data.frame(df_bf,df[,c(16:22)],df[,29:35],df[,37:36])
  names(df_bf1) <- INTG_names
  df_bf2 <- merge(x = df_bf1, y = ep, by = "CCN", all.x = TRUE)
  year <- rep(y,nrow(df))
  season <- rep(s,nrow(df))
  timeindex <- rep(t,nrow(df))
  star_rating <- rep(NA,nrow(df))
  df_bf2$star <- star_rating
  df_bf2$year <- year
  df_bf2$season <- season
  df_bf2$timeindex <- timeindex
  data <- df_bf2
  return(data)
  }

data20121 <- Clean37(df2012_1, episode2012, 2012, 9, 1)
data20122 <- Clean37(df2012_2, episode2012, 2012, 10, 2)
data20123 <- Clean37(df2012_3, episode2012, 2012, 11, 3)
data20124 <- Clean37(df2012_4, episode2012, 2012, 12, 4)
data20131 <- Clean37(df2013_1, episode2013, 2013, 13, 1)
data20132 <- Clean37(df2013_2, episode2013, 2013, 14, 2)
data20133 <- Clean37(df2013_3, episode2013, 2013, 15, 3)
data20134 <- Clean37(df2013_4, episode2013, 2013, 16, 4)


Clean60 <- function(df, ep, y, t, s) {
  colClasses(df) <- paste(mytypes60, collapse="")
  names(df) <- names60
  setdiff(INTG_names,names60)
  df_bf <- data.frame(df[,-c(seq(17,by=2,len=22),60)])
  setdiff(INTG_names,names(df_bf))
  df_bf1 <- data.frame(df_bf[,c(2:1)],df_bf[,c(3:15)])
  df_bf1$rating <- NA
  df_bf2 <- data.frame(df_bf1,df_bf[,c(16:22)],df_bf[,c(29:35)],df_bf[,37:36])
  names(df_bf2) <- INTG_names
  df_bf3 <- merge(x = df_bf2, y = ep, by = "CCN", all.x = TRUE)
  year <- rep(y,nrow(df))
  season <- rep(s,nrow(df))
  timeindex <- rep(t,nrow(df))
  star_rating <- rep(NA,nrow(df))
  df_bf3$star <- star_rating
  df_bf3$year <- year
  df_bf3$season <- season
  df_bf3$timeindex <- timeindex
  data <- df_bf3
  return(data)
  }

data20141 <- Clean60(df2014_1, episode2014, 2014, 17, 1)
data20142 <- Clean60(df2014_2, episode2014, 2014, 18, 2)
data20143 <- Clean60(df2014_3, episode2014, 2014, 19, 3)
data20144 <- Clean60(df2014_4, episode2014, 2014, 20, 4)
data20151 <- Clean60(df2015_1, episode2015, 2015, 21, 1)



Clean66_1 <- function(df, ep, y, t, s) {
  colClasses(df) <- paste(mytypes66_1, collapse="")
  names(df) <- names66_1
  setdiff(INTG_names,names66_1)
  df_bf <- data.frame(df[,-c(seq(17,by=2,len=25),66)])
  setdiff(INTG_names,names(df_bf))
  df_bf1 <- data.frame(df_bf[,c(2:1)],df_bf[,c(3:23)],df_bf[,c(27:32)],df_bf[,c(36)],df_bf[,c(38:37)])
  names(df_bf1) <- INTG_names
  df_bf2 <- merge(x = df_bf1, y = ep, by = "CCN", all.x = TRUE)
  year <- rep(y,nrow(df))
  season <- rep(s,nrow(df))
  timeindex <- rep(t,nrow(df))
  star_rating <- rep(NA,nrow(df))
  df_bf2$star <- star_rating
  df_bf2$year <- year
  df_bf2$season <- season
  df_bf2$timeindex <- timeindex
  data <- df_bf2
  return(data)
}
data20153 <- Clean66_1(df2015_3, episode2015, 2015, 23, 3)
data20154 <- Clean66_1(df2015_4, episode2015, 2015, 24, 4)

Clean66_2 <- function(df, ep, y, t, s, star) {
  colClasses(df) <- paste(mytypes66_2, collapse="")
  names(df) <- names66_2
  setdiff(INTG_names,names66_2)
  df_bf <- data.frame(df[,-c(seq(17,by=2,len=25),66)])
  df_bf1 <- data.frame(df_bf[,c(2:1)],df_bf[,c(3:23)],df_bf[,c(27:32)],df_bf[,c(36:38)])
  names(df_bf1) <- INTG_names
  df_bf2 <- merge(x = df_bf1, y = ep, by = "CCN", all.x = TRUE)
  df_bf3 <- merge(x = df_bf2, y = star, by = "CCN", all.x = TRUE)
  year <- rep(y,nrow(df))
  season <- rep(s,nrow(df))
  timeindex <- rep(t,nrow(df))
  df_bf3$year <- year
  df_bf3$season <- season
  df_bf3$timeindex <- timeindex
  data <- df_bf3  
  return(data)
}

data20161 <- Clean66_2(df2016_1, episode2016, 2016, 25, 1, dfstar2016_1)
data20162 <- Clean66_2(df2016_2, episode2016, 2016, 26, 2, dfstar2016_2)
data20163 <- Clean66_2(df2016_3, episode2016, 2016, 27, 3, dfstar2016_3)
data20164 <- Clean66_2(df2016_4, episode2016, 2016, 28, 4, dfstar2016_4)


Clean54 <- function(df, ep, y, t, s, star) {
  colClasses(df) <- paste(mytypes54, collapse="")
  names(df) <- names54
  setdiff(INTG_names,names54)
  df_bf <- data.frame(df[,-c(seq(17,by=2,len=19),54)])
  df_bf1 <- data.frame(df_bf[,c(2:1)],df_bf[,c(3:32)])
  names(df_bf1) <- INTG_names
  df_bf2 <- merge(x = df_bf1, y = ep, by = "CCN", all.x = TRUE)
  df_bf3 <- merge(x = df_bf2, y = star, by = "CCN", all.x = TRUE)
  year <- rep(y,nrow(df))
  season <- rep(s,nrow(df))
  timeindex <- rep(t,nrow(df))
  df_bf3$year <- year
  df_bf3$season <- season
  df_bf3$timeindex <- timeindex
  data <- df_bf3
  return(data)
}
data20171 <- Clean54(df2017_1, episode2017, 2017, 29, 1, dfstar2017_1)
data20172 <- Clean54(df2017_2, episode2017, 2017, 30, 2, dfstar2017_2)
data20173 <- Clean54(df2017_3, episode2017, 2017, 31, 3, dfstar2017_3)
data20174 <- Clean54(df2017_4, episode2017, 2017, 32, 4, dfstar2017_4)
data20181 <- Clean54(df2018_1, episode2018, 2018, 33, 1, dfstar2018_1)
data20182 <- Clean54(df2018_2, episode2018, 2018, 34, 2, dfstar2018_2)
data20183 <- Clean54(df2018_3, episode2018, 2018, 35, 3, dfstar2018_3)


Clean63 <- function(df, ep, y, t, s, star) {
  colClasses(df) <- paste(mytypes63, collapse="")
  names(df) <- names63
  setdiff(INTG_names,names63)
  df_bf <- data.frame(df[,-c(seq(17,by=2,len=19),54,seq(56,by=2,len=4))])
  df_bf1 <- data.frame(df_bf[,c(2:1)],df_bf[,c(3:32)],df_bf[,c(39)])
  names(df_bf1) <- c(INTG_names,'episode')
  df_bf2 <- merge(x = df_bf1, y = star, by = "CCN", all.x = TRUE)
  year <- rep(y,nrow(df))
  season <- rep(s,nrow(df))
  timeindex <- rep(t,nrow(df))
  df_bf2$year <- year
  df_bf2$season <- season
  df_bf2$timeindex <- timeindex
  data <- df_bf2
  print(sum(is.na(data$episode)))
  return(data)
}
data20191 <- Clean63(df2019_1, episode2019, 2019, 37, 1, dfstar2019_1)
data20192 <- Clean63(df2019_2, episode2019, 2019, 38, 2, dfstar2019_2)
data20193 <- Clean63(df2019_3, episode2019, 2019, 39, 3, dfstar2019_3)


Clean79 <- function(df, ep, y, t, s, star) {
  colClasses(df) <- paste(mytypes79, collapse="")
  names(df) <- names79
  setdiff(INTG_names,names79)
  df_bf <- data.frame(df[,-c(seq(17,by=2,len=19),54,seq(56,by=2,len=3))])
  df_bf1 <- data.frame(df_bf[,c(2:1)],df_bf[,c(3:32)],df_bf[,56])
  names(df_bf1) <- c(INTG_names,'episode')
  df_bf2 <- merge(x = df_bf1, y = star, by = "CCN", all.x = TRUE)
  year <- rep(y,nrow(df))
  season <- rep(s,nrow(df))
  timeindex <- rep(t,nrow(df))
  df_bf2$year <- year
  df_bf2$season <- season
  df_bf2$timeindex <- timeindex
  data <- df_bf2
  sum(is.na(data$episode))
  return(data)
}
data20194 <- Clean79(df2019_4, episode2019, 2019, 40, 4, dfstar2019_4)

dim(data20194)
head(data20194)

Clean73 <- function(df, ep, y, t, s, star) {
  colClasses(df) <- paste(mytypes73, collapse="")
  names(df) <- names73
  setdiff(INTG_names,names73)
  df_bf <- data.frame(df[,-c(seq(17,by=2,len=17),50,seq(52,by=2,len=2))])
  df_bf1 <- data.frame(df_bf[,c(2:1)],df_bf[,c(3:32)],df_bf[,53])
  names(df_bf1) <- c(INTG_names,'episode')
  df_bf2 <- merge(x = df_bf1, y = star, by = "CCN", all.x = TRUE)
  year <- rep(y,nrow(df))
  season <- rep(s,nrow(df))
  timeindex <- rep(t,nrow(df))
  df_bf2$year <- year
  df_bf2$season <- season
  df_bf2$timeindex <- timeindex
  data <- df_bf2
  sum(is.na(data$episode))
  return(data)
}

data20201 <- Clean73(df2020_1, episode2020, 2020, 40, 1, dfstar2020_1)


Clean71 <- function(df, ep, y, t, s, star) {
  colClasses(df) <- paste(mytypes71, collapse="")
  names(df) <- names71
  setdiff(INTG_names,names71)
  df_bf <- data.frame(df[,-c(seq(17,by=2,len=16),48,seq(50,by=2,len=2))])
  df_bf_min <- data.frame(df_bf[,c(2:1)],df_bf[,c(3:26)])
  df_bf_min$lesspain <- NA
  df_bf1 <- cbind(df_bf_min, betterbreathing = df_bf$betterbreathing, betterheal = df_bf$betterheal, betterdrug = df_bf$betterdrug, admitted = df_bf$admitted, ER = df_bf$ER, episodes = df_bf$episodes)
  names(df_bf1) <- c(INTG_names,'episode')
  df_bf2 <- merge(x = df_bf1, y = star, by = "CCN", all.x = TRUE)
  year <- rep(y,nrow(df))
  season <- rep(s,nrow(df))
  timeindex <- rep(t,nrow(df))
  df_bf2$year <- year
  df_bf2$season <- season
  df_bf2$timeindex <- timeindex
  data <- df_bf2
  sum(is.na(data$episode))
  return(data)
  }

data20202 <- Clean71(df2020_2, episode2020, 2020, 41, 2, dfstar2020_2)


df2020_3 <- data.frame(df2020_3[, -c(3,9,11,19,20,21,22,23,80)])
data20203 <- Clean71(df2020_3, episode2020, 2020, 42, 3, dfstar2020_3)

Clean70 <- function(df, ep, y, t, s, star) {
  colClasses(df) <- paste(mytypes70, collapse="")
  names(df) <- names70
  setdiff(INTG_names,names70)
  df_bf <- data.frame(df[,-c(seq(17,by=2,len=16),seq(49,by=2,len=2))])
  df_bf_min <- data.frame(df_bf[,c(2:1)],df_bf[,c(3:26)])
  df_bf_min$lesspain <- NA
  df_bf1 <- cbind(df_bf_min, betterbreathing = df_bf$betterbreathing, betterheal = df_bf$betterheal, betterdrug = df_bf$betterdrug, admitted = df_bf$admitted, ER = df_bf$ER, episodes = df_bf$episodes)
  names(df_bf1) <- c(INTG_names,'episode')
  df_bf2 <- merge(x = df_bf1, y = star, by = "CCN", all.x = TRUE)
  year <- rep(y,nrow(df))
  season <- rep(s,nrow(df))
  timeindex <- rep(t,nrow(df))
  df_bf2$year <- year
  df_bf2$season <- season
  df_bf2$timeindex <- timeindex
  data <- df_bf2
  sum(is.na(data$episode))
  return(data)
}

data20204 <- Clean70(df2020_4, episode2020, 2020, 43, 4, dfstar2020_4)


head(data20204)

#After all the data has been cleaned, we them merge them into a single dataframe using the rbind() function.

alldata <- rbind(data20101,data20102,data20104,
                 data20111,data20112,data20113,data20114,
                 data20121,data20122,data20123,data20124,
                 data20131,data20132,data20133,data20134,
                 data20141,data20142,data20143,data20144,
                 data20151,data20153,data20154,
                 data20161,data20162,data20163,data20164,
                 data20171,data20172,data20173,data20174,
                 data20181,data20182,data20183,
                 data20191,data20192,data20193,data20194,data20201,data20202,data20203, data20204)


with(alldata, timeindex[CCN == "747329" & year == 2019])


require("readxl")

#Merging the HHA data with the mean and median household income data based on the ZIP code of the Home Health Agency 

income <- read_excel("MedianZIP-3.xlsx")
colClasses(income) <- paste(c('c','n','n','n'), collapse="")
names(income) <- c('Zip','median','mean','pop')


head(income)


#Including the Ruca information which gives information about the Rural or Urban Home Health Agency
ruca <- read_excel("RUCA2006.xls")
colClasses(ruca) <- paste(c('c','f','n','n','f'), collapse="")
ruca <- data.frame(ruca[, c(1,3)])
names(ruca) <- c('Zip','ruca')

head(ruca)

dim(income)
length(unique(income$Zip))
dim(ruca)
length(unique(ruca$Zip))

allZip <- unique(alldata$Zip)
incomeZip <- unique(income$Zip)
rucaZip <- unique(ruca$Zip)

length(allZip) 
sum(allZip %in% incomeZip)
sum(allZip %in% rucaZip)

## 558 HHA's' zip code connot find a match in RUCA

alldata_bf <- merge(x = alldata, y = income, by = "Zip", all.x = TRUE)
alldata_bf1 <- merge(x = alldata_bf, y = ruca, by = "Zip", all.x = TRUE)
names(alldata_bf1)


dim(alldata_bf1)

finaldata <- alldata_bf1

#Exporting the Final Dataframe to a csv file.

write.csv(finaldata, file = "completedata.csv",row.names=FALSE, na="")
