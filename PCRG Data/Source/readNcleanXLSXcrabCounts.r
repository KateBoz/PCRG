#testing
# filepath <- files[8]
readNcleanXLSXcrabCounts <- function(filepath){
  # reading in sheet 1, skipping unused lines
  df <- read_xlsx(filepath, sheet = 1, skip = 3)
  
  #getting rid of row 1 as it contains instructions
  df <-df[-1,]
  
  #remove example rows id they exist
  if(length(grep("example",df$Date)) > 0){
    df <- df[-(grep("example",df$Date)),]
  }
  
  # Generate 'Organization Column from filepath
  df$Organization <- unlist(rm_between(filepath, "_", ".xlsx", extract=TRUE))
  
  # add 1 to end time since light shuts off the next morning
  df$`Timer end`  <- as.character(as.numeric(df$`Timer end`) + 1)
  
  #Converting to date-time with dummy date (dummy date +1 day for timer end)
  df$`Time trap pulled`<- convertToDateTime(df$`Time trap pulled`)
  df$`Timer start`  <- convertToDateTime(df$`Timer start`)
  df$`Timer end`  <- convertToDateTime(df$`Timer end`)
  
  #turning nights fished from 0 to NA
  df$`# Nights Fished`[as.numeric(df$`# Nights Fished`) == 0] <- NA
  
  # recalculating hours fished
  df$`Hours Fished (hours not minutes)` <- (df$`Timer end`-df$`Timer start`)*as.numeric(df$`# Nights Fished`)
  #changing long column name
  colnames(df)[colnames(df) == "Hours Fished (hours not minutes)"] <- "Hours Fished"
  
  #dropping dummy date to just get time
  df$`Time trap pulled`<- strftime(df$`Time trap pulled`, format="%H:%M:%S")
  df$`Timer start`  <- strftime(df$`Timer start`, format="%H:%M:%S")
  df$`Timer end`  <- strftime(df$`Timer end`, format="%H:%M:%S")
  
  # changing a couple columns from character to numeric
  df$`Metacarcinus magistermegalopae` <- as.numeric(df$`Metacarcinus magistermegalopae`)
  df$`Metacarcinus magisterinstar` <- as.numeric(df$`Metacarcinus magisterinstar`)
  df$`Cancer productus` <- as.numeric(df$`Cancer productus`)
  df$`Metacarcinus gracilis` <- as.numeric(df$`Metacarcinus gracilis`)
  df$`Glebocarcinus oregonensis` <- as.numeric(df$`Glebocarcinus oregonensis`)
  df$`Lophopanopeus bellus` <- as.numeric(df$`Lophopanopeus bellus`)
  df$`Oregonia gracilis` <- as.numeric(df$`Oregonia gracilis`)
  df$`Hemigrapsus sp.` <- as.numeric(df$`Hemigrapsus sp.`)
  df$`Pugettia sp.` <- as.numeric(df$`Pugettia sp.`)
  df$`Pinnixa sp.` <- as.numeric(df$`Pinnixa sp.`)
  df$`Fabia sp.` <- as.numeric(df$`Fabia sp.`)
  df$`Temperature (degrees C)` <- as.numeric(df$`Temperature (degrees C)`)
  df$`Salinity (ppt)` <- as.numeric(df$`Salinity (ppt)`)
  
  # formatting date as date
  df$Date <- as.Date(df$Date, "%Y%m%d" )
  
  # total dungeness crab (megalope + instars)
  df$totalCMagister <- df$`Metacarcinus magistermegalopae` + df$`Metacarcinus magisterinstar`
  
  # dungeness crab CPUE
  df$`Hours Fished` <- as.numeric(df$`Hours Fished`)
  df$TotalDungenessCPUE <- df$totalCMagister/df$`Hours Fished`
  df$DungMegalopeCPUE <- df$`Metacarcinus magistermegalopae`/df$`Hours Fished`
  df$DungInstarCPUE <- df$`Metacarcinus magisterinstar`/df$`Hours Fished`
  
  colnames(df)[colnames(df) == "Location (Site abbrevation)"] <- "Site"
  
  return(df)
}