#testing
# filepath <- files
readNcleanXLSXcrabMeasurements <- function(filepath){
  # reading in sheet 2, skipping unused lines
  df <- read_xlsx(filepath, sheet = 2, skip = 2)
  #removing line 1 as it contains instructions
  df <- df[-1,]
  
  #changing to numeric
  #rounding to 1 decimal place because a few numeric values came in weird (e.g. 1.8999999999999 in stead of 1.9)
  df$CW <- round(as.numeric(df$CW), digits = 1)
  df$CH <- round(as.numeric(df$CH), digits = 1)
  df$TH <- round(as.numeric(df$TH), digits = 1)  
  
  # removing any extra sspaces in date values before conversion to date format
  df$Date <- trimws(df$Date)
  
  # formatting date as date
  df$Date <- as.Date(df$Date, "%Y%m%d" )
  
  return(df)
}
