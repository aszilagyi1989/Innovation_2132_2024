source("T1.R")
source("T4.R") #Sokáig fut
source("T7.R")
source("T8.R")
source("T10.R")
source("T11.R")
source("T14.R")
source("T16.R")
source("T17.R")
source("T18.R") #Sokáig fut
source("T21.R")
source("T22.R")
source("T23.R")
source("T24.R")
source("T25.R")
source("T26.R") #Sokáig fut
source("T28.R") #Sokáig fut
source("T29.R")
source("T30.R") #Sokáig fut
source("T31.R") #Sokáig fut
source("T32.R")
source("T33.R")
source("T34.R")

library("data.table")
T_ALL_SDMX_FINAL_RESULT <- "T1-34_SDMX_FINAL_RESULT.txt"

T1_SDMX_FINAL_RESULT <- "T1_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T1_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()
result_ALL <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";")
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
      
    result_table <- rbind(result_table, df)
    
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)


T4_SDMX_FINAL_RESULT <- "T4_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T4_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
  
  result_table <- rbind(result_table, df)
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)


T7_SDMX_FINAL_RESULT <- "T7_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T7_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
  
  result_table <- rbind(result_table, df)
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)


T8_SDMX_FINAL_RESULT <- "T8_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T8_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
  
  result_table <- rbind(result_table, df)
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)


T10_SDMX_FINAL_RESULT <- "T10_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T10_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
  
  result_table <- rbind(result_table, df)
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)


T11_SDMX_FINAL_RESULT <- "T11_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T11_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
  
  result_table <- rbind(result_table, df)
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)


T14_SDMX_FINAL_RESULT <- "T14_SDMX_ALL_FINAL_RESULT_ENT_EXP.txt"
TXTData <- file(description = T14_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
  
  result_table <- rbind(result_table, df)
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)


T16_SDMX_FINAL_RESULT <- "T16_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T16_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
  
  result_table <- rbind(result_table, df)
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)


T17_SDMX_FINAL_RESULT <- "T17_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T17_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
  
  result_table <- rbind(result_table, df)
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)


T18_SDMX_FINAL_RESULT <- "T18_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T18_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
  
  result_table <- rbind(result_table, df)
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)


T21_SDMX_FINAL_RESULT <- "T21_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T21_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
  
  result_table <- rbind(result_table, df)
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)


T22_SDMX_FINAL_RESULT <- "T22_SDMX_ALL_FINAL_RESULT_ENT_TUR.txt"
TXTData <- file(description = T22_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
  
  result_table <- rbind(result_table, df)
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)


T23_SDMX_FINAL_RESULT <- "T23_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T23_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
  
  result_table <- rbind(result_table, df)
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)


T24_SDMX_FINAL_RESULT <- "T24_SDMX_ALL_FINAL_RESULT_ENT_EXP.txt"
TXTData <- file(description = T24_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
  
  result_table <- rbind(result_table, df)
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)


T25_SDMX_FINAL_RESULT <- "T25_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T25_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
  
  result_table <- rbind(result_table, df)
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)


T26_SDMX_FINAL_RESULT <- "T26_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T26_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
  
  result_table <- rbind(result_table, df)
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)


T28_SDMX_FINAL_RESULT <- "T28_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T28_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
  
  result_table <- rbind(result_table, df)
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)


T29_SDMX_FINAL_RESULT <- "T29_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T29_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
  
  result_table <- rbind(result_table, df)
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)


T30_SDMX_FINAL_RESULT <- "T30_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T30_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
  
  result_table <- rbind(result_table, df)
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)


T31_SDMX_FINAL_RESULT <- "T31_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T31_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
  
  result_table <- rbind(result_table, df)
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)


T32_SDMX_FINAL_RESULT <- "T32_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T32_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
  
  result_table <- rbind(result_table, df)
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)


T33_SDMX_FINAL_RESULT <- "T33_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T33_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
  
  result_table <- rbind(result_table, df)
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)


T34_SDMX_FINAL_RESULT <- "T34_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T34_SDMX_FINAL_RESULT, open = "r")
line <- readLines(con = TXTData)
result_table <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  
  if(ncol(df) != 19){
    
    df$X16 <- ""
    df$X17 <- ""
    df$X18 <- ""
    df$X19 <- ""
    
  }
  
  result_table <- rbind(result_table, df)
}

names(result_table) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
nrow(result_table)

result_ALL <- rbind(result_ALL, result_table)

result_ALL[result_ALL$TABLENAME == "T10" & result_ALL$CIS_INDICATOR == "TUR_PRD_NEW_ENT" & result_ALL$INN_PF == "INN_PF1", "TYPE_ENT"] <- "INN"

result_ALL$DATAFLOW <- gsub("\"", "", result_ALL$DATAFLOW)
result_ALL$TABLENAME <- gsub("\"", "", result_ALL$TABLENAME)
result_ALL$FREQ <- gsub("\"", "", result_ALL$FREQ)
result_ALL$REF_AREA <- gsub("\"", "", result_ALL$REF_AREA)
result_ALL$TIME_PERIOD <- gsub("\"", "", result_ALL$TIME_PERIOD)
result_ALL$UNIT_MEASURE <- gsub("\"", "", result_ALL$UNIT_MEASURE)
result_ALL$INDICATOR <- gsub("\"", "", result_ALL$INDICATOR)
result_ALL$TYPE_ENT<- gsub("\"", "", result_ALL$TYPE_ENT)
result_ALL$INN_PF<- gsub("\"", "", result_ALL$INN_PF)
result_ALL$ACTIVITY <- gsub("\"", "", result_ALL$ACTIVITY)
result_ALL$NUMBER_EMPL <- gsub("\"", "", result_ALL$NUMBER_EMPL)
result_ALL$CIS_INDICATOR <- gsub("\"", "", result_ALL$CIS_INDICATOR)
result_ALL$DECIMALS <- gsub("\"", "", result_ALL$DECIMALS)
result_ALL$UNIT_MULT <- gsub("\"", "", result_ALL$UNIT_MULT)
result_ALL$OBS_VALUE <- gsub("\"", "", result_ALL$OBS_VALUE)
result_ALL$OBS_STATUS <- gsub("\"", "", result_ALL$OBS_STATUS)
result_ALL$OBS_STATUS_1 <- gsub("\"", "", result_ALL$OBS_STATUS_1)
result_ALL$CONF_STATUS <- gsub("\"", "", result_ALL$CONF_STATUS)
result_ALL$COMMENT_OBS <- gsub("\"", "", result_ALL$COMMENT_OBS)

write.table(result_ALL, T_ALL_SDMX_FINAL_RESULT, sep = ";", row.names = FALSE, append = FALSE)