source("Functions_Standard.R")
source("Profile_ENT.R")
library("data.table")
library("dplyr")

T31_SDMX <- "T31_SDMX_INN.txt"
T31_SDMX_RESULT <- "T31_SDMX_INN_RESULT.txt"

TABLENAME <- "T31"

TYPE_ENT <- "INN"
INN_PF <- "_Z"
INDICATOR <- "ENT"

UNIT_MEASURE <- "PN"
UNIT_MULT <- "0"
DECIMALS <- "0"

expression <- c("is.na(INN$ENV_ENEREG) == FALSE & INN$ENV_ENEREG == 3", 
                "is.na(INN$ENV_ENETX) == FALSE & INN$ENV_ENETX == 3", 
                "is.na(INN$ENV_ENREGF) == FALSE & INN$ENV_ENREGF == 3", 
                "is.na(INN$ENV_ENGRA) == FALSE & INN$ENV_ENGRA == 3", 
                "is.na(INN$ENV_ENDEM) == FALSE & INN$ENV_ENDEM == 3", 
                "is.na(INN$ENV_ENREP) == FALSE & INN$ENV_ENREP == 3", 
                "is.na(INN$ENV_ENAGR) == FALSE & INN$ENV_ENAGR == 3", 
                "is.na(INN$ENV_ENCOST) == FALSE & INN$ENV_ENCOST == 3", 
                "is.na(INN$ENV_ENREQU) == FALSE & INN$ENV_ENREQU == 3", 
                "is.na(INN$ENV_ENEREG) == FALSE & INN$ENV_ENEREG == 2", 
                "is.na(INN$ENV_ENETX) == FALSE & INN$ENV_ENETX == 2", 
                "is.na(INN$ENV_ENREGF) == FALSE & INN$ENV_ENREGF == 2", 
                "is.na(INN$ENV_ENGRA) == FALSE & INN$ENV_ENGRA == 2", 
                "is.na(INN$ENV_ENDEM) == FALSE & INN$ENV_ENDEM == 2", 
                "is.na(INN$ENV_ENREP) == FALSE & INN$ENV_ENREP == 2", 
                "is.na(INN$ENV_ENAGR) == FALSE & INN$ENV_ENAGR == 2", 
                "is.na(INN$ENV_ENCOST) == FALSE & INN$ENV_ENCOST == 2", 
                "is.na(INN$ENV_ENREQU) == FALSE & INN$ENV_ENREQU == 2", 
                "is.na(INN$ENV_ENEREG) == FALSE & INN$ENV_ENEREG == 1", 
                "is.na(INN$ENV_ENETX) == FALSE & INN$ENV_ENETX == 1", 
                "is.na(INN$ENV_ENREGF) == FALSE & INN$ENV_ENREGF == 1", 
                "is.na(INN$ENV_ENGRA) == FALSE & INN$ENV_ENGRA == 1", 
                "is.na(INN$ENV_ENDEM) == FALSE & INN$ENV_ENDEM == 1", 
                "is.na(INN$ENV_ENREP) == FALSE & INN$ENV_ENREP == 1", 
                "is.na(INN$ENV_ENAGR) == FALSE & INN$ENV_ENAGR == 1", 
                "is.na(INN$ENV_ENCOST) == FALSE & INN$ENV_ENCOST == 1", 
                "is.na(INN$ENV_ENREQU) == FALSE & INN$ENV_ENREQU == 1", 
                "is.na(INN$ENV_ENEREG) == FALSE & INN$ENV_ENEREG == 0", 
                "is.na(INN$ENV_ENETX) == FALSE & INN$ENV_ENETX == 0", 
                "is.na(INN$ENV_ENREGF) == FALSE & INN$ENV_ENREGF == 0", 
                "is.na(INN$ENV_ENGRA) == FALSE & INN$ENV_ENGRA == 0", 
                "is.na(INN$ENV_ENDEM) == FALSE & INN$ENV_ENDEM == 0", 
                "is.na(INN$ENV_ENREP) == FALSE & INN$ENV_ENREP == 0", 
                "is.na(INN$ENV_ENAGR) == FALSE & INN$ENV_ENAGR == 0", 
                "is.na(INN$ENV_ENCOST) == FALSE & INN$ENV_ENCOST == 0", 
                "is.na(INN$ENV_ENREQU) == FALSE & INN$ENV_ENREQU == 0")

expression2 <- c("ENV_ENEREG_3", 
                "ENV_ENETX_3", 
                "ENV_ENREGF_3", 
                "ENV_ENGRA_3", 
                "ENV_ENDEM_3", 
                "ENV_ENREP_3", 
                "ENV_ENAGR_3", 
                "ENV_ENCOST_3", 
                "ENV_ENREQU_3", 
                "ENV_ENEREG_2", 
                "ENV_ENETX_2", 
                "ENV_ENREGF_2", 
                "ENV_ENGRA_2", 
                "ENV_ENDEM_2", 
                "ENV_ENREP_2", 
                "ENV_ENAGR_2", 
                "ENV_ENCOST_2", 
                "ENV_ENREQU_2", 
                "ENV_ENEREG_1", 
                "ENV_ENETX_1", 
                "ENV_ENREGF_1", 
                "ENV_ENGRA_1", 
                "ENV_ENDEM_1", 
                "ENV_ENREP_1", 
                "ENV_ENAGR_1", 
                "ENV_ENCOST_1", 
                "ENV_ENREQU_1", 
                "ENV_ENEREG_0", 
                "ENV_ENETX_0", 
                "ENV_ENREGF_0", 
                "ENV_ENGRA_0", 
                "ENV_ENDEM_0", 
                "ENV_ENREP_0", 
                "ENV_ENAGR_0", 
                "ENV_ENCOST_0", 
                "ENV_ENREQU_0")

cat(paste("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS", sep = ";"), sep = "\n", file = T31_SDMX, append = FALSE)
for(num in 1:length(expression)){
  
  INN_DT <- data.table(INN[eval(parse(text = expression[num])), ])
  INN_DT <- INN_DT[, .(ENT22_SULYOZOTT=sum(VGMA001_SULY)), by = "M065_RETEG1,M0581_2J"]
  
  if(nrow(INN_DT) != 0){
    
    INN_Ordered <- cbind(INN_DT, stringsAsFactors = FALSE)
    INN_Ordered <- INN_Ordered[order(INN_Ordered$M065_RETEG1, INN_Ordered$M0581_2J), ]
    
    CIS_INDICATOR <- expression2[num]
    
    for(i in 1:nrow(INN_Ordered)){
      
      if(INN_Ordered[i, 1] == "KI"){
        
        NUMBER_EMPL <- "E10T49"
        
      } else if(INN_Ordered[i, 1] == "KO"){
        
        NUMBER_EMPL <- "E50T249"
        
      } else{
        
        NUMBER_EMPL <- "E_GE250"   
        
      }
      
      ACTIVITY <- get_NACE(INN_Ordered[i, 2])
      OBS_VALUE <- INN_Ordered[i, 3]
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = T31_SDMX, append = TRUE)
      
    }
    
  }
}

TXTData <- file(description = T31_SDMX, open = "r")
line <- readLines(con = TXTData)
Aggregate <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  Aggregate <- rbind(Aggregate, df)
}

names(Aggregate) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS")
Aggregate$COMMENT_OBS <- "" #"COMMENT_OBS" oszlop nincs
NUMBER_EMPL <- "_T"
for(num in 1:length(expression)){
  
  CIS_INDICATOR <- expression2[num]
  
  for(j in 1:length(ACTIVITY_LIST)){
    
    ACTIVITY <- ACTIVITY_LIST[j]
    insert_Total(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  TXTData <- file(description = T31_SDMX, open = "r")
  line <- readLines(con = TXTData)
  result_Aggregate <- data.frame()
  
  for (i in 2:length(line)){
    values <- strsplit(x = line[i], split = ";");
    df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
    result_Aggregate <- rbind(result_Aggregate, df)
  }
  
  names(result_Aggregate) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS")
  result_Aggregate$COMMENT_OBS <- ""
  result_Aggregate <- result_Aggregate[result_Aggregate$NUMBER_EMPL == "_T", ]
  nrow(result_Aggregate)
  result_Aggregate[, "OBS_VALUE"] <- gsub("\\.", ",", result_Aggregate[, "OBS_VALUE"])
  write.table(result_Aggregate, T31_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)
  
  #Összetett értékek számítása több ACTIVITY kód alapján az összes vállalatcsoportra
  ACTIVITY <- "A"
  NUMBER_EMPL <- "_T"
  insert_A(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "B"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_B(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "C"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_C(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "C10T12"
  insert_C10T12(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "C13T15"
  insert_C13T15(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "C16T18"
  insert_C16T18(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  #ACTIVITY <- "C19_20"
  #insert_C19T20(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "C19T21"
  insert_C19T21(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  #ACTIVITY <- "C19T22"
  #insert_C19T22(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "C22_23"
  insert_C22T23(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "C24_25"
  insert_C24T25(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "C26T28"
  insert_C26T28(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "C29_30"
  insert_C29T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  #ACTIVITY <- "C25T30"
  #insert_C25T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "C31T33"
  insert_C31T33(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "D"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_D(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "E"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_E(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "E36_37"
  insert_E36T37(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "E38_39"
  insert_E38T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  #ACTIVITY <- "E37T39"
  #insert_E37T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "G"
  insert_G(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "G46"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "H"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_H(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "F"
  insert_F(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "H49T51"
  insert_H49T51(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "H52_53"
  insert_H52T53(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "I"
  insert_I(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "J"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_J(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "J58T60"
  insert_J58T60(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "J61T63"
  insert_J61T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  #ACTIVITY <- "J62_63"
  #insert_J62T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "K"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_K(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "M"
  insert_M(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "M71T73"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_M71T73(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "N"
  insert_N(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "BTE"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_BTE(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "GTN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_GTN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "G46TM73_INN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46TM73_INN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "_T"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert__T(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
}

TXTData <- file(description = T31_SDMX, open = "r")
line <- readLines(con = TXTData)
result_Aggregate <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  result_Aggregate <- rbind(result_Aggregate, df)
}

names(result_Aggregate) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS")
result_Aggregate$COMMENT_OBS <- ""
result_Aggregate <- result_Aggregate[(result_Aggregate$ACTIVITY != "F41" & result_Aggregate$ACTIVITY != "F42" & result_Aggregate$ACTIVITY != "F43" & result_Aggregate$ACTIVITY != "I55" & result_Aggregate$ACTIVITY != "I56") & (result_Aggregate$NUMBER_EMPL == "_T" | result_Aggregate$ACTIVITY == "M71T73" | result_Aggregate$ACTIVITY == "K" | result_Aggregate$ACTIVITY == "J" | result_Aggregate$ACTIVITY == "H" | result_Aggregate$ACTIVITY == "G46" | result_Aggregate$ACTIVITY == "A" | result_Aggregate$ACTIVITY == "B" | result_Aggregate$ACTIVITY == "C" | result_Aggregate$ACTIVITY == "D" | result_Aggregate$ACTIVITY == "E" | result_Aggregate$ACTIVITY == "I" | result_Aggregate$ACTIVITY == "BTE" | result_Aggregate$ACTIVITY == "GTN" | result_Aggregate$ACTIVITY == "G46TM73_INN" | result_Aggregate$ACTIVITY == "_T"), ]
result_Aggregate <- unique(result_Aggregate) #G46 miatt
nrow(result_Aggregate)
result_Aggregate[, "OBS_VALUE"] <- gsub("\\.", ",", result_Aggregate[, "OBS_VALUE"])
write.table(result_Aggregate, T31_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)


T31_SDMX <- "T31_SDMX_INN.txt"
T31_SDMX_RESULT <- "T31_SDMX_ALL_RESULT.txt"
T31_SDMX_FINAL_RESULT <- "T31_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T31_SDMX, open = "r")
line <- readLines(con = TXTData)
result_Aggregate <- data.frame()
result_T31 <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  result_Aggregate <- rbind(result_Aggregate, df)
}

names(result_Aggregate) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS")
result_Aggregate$COMMENT_OBS <- ""
result_Aggregate <- result_Aggregate[(result_Aggregate$ACTIVITY != "F41" & result_Aggregate$ACTIVITY != "F42" & result_Aggregate$ACTIVITY != "F43" & result_Aggregate$ACTIVITY != "I55" & result_Aggregate$ACTIVITY != "I56") & (result_Aggregate$NUMBER_EMPL == "_T" | result_Aggregate$ACTIVITY == "M71T73" | result_Aggregate$ACTIVITY == "K" | result_Aggregate$ACTIVITY == "J" | result_Aggregate$ACTIVITY == "H" | result_Aggregate$ACTIVITY == "G46" | result_Aggregate$ACTIVITY == "A" | result_Aggregate$ACTIVITY == "B" | result_Aggregate$ACTIVITY == "C" | result_Aggregate$ACTIVITY == "D" | result_Aggregate$ACTIVITY == "E" | result_Aggregate$ACTIVITY == "I" | result_Aggregate$ACTIVITY == "BTE" | result_Aggregate$ACTIVITY == "GTN" | result_Aggregate$ACTIVITY == "G46TM73_INN" | result_Aggregate$ACTIVITY == "_T"), ]
result_Aggregate <- unique(result_Aggregate) #G46 miatt
nrow(result_Aggregate)

result_T31 <- rbind(result_T31, result_Aggregate)

#NINN számítása kezdődik
expression <- c("is.na(NINN$ENV_ENEREG) == FALSE & NINN$ENV_ENEREG == 3", 
                "is.na(NINN$ENV_ENETX) == FALSE & NINN$ENV_ENETX == 3", 
                "is.na(NINN$ENV_ENREGF) == FALSE & NINN$ENV_ENREGF == 3", 
                "is.na(NINN$ENV_ENGRA) == FALSE & NINN$ENV_ENGRA == 3", 
                "is.na(NINN$ENV_ENDEM) == FALSE & NINN$ENV_ENDEM == 3", 
                "is.na(NINN$ENV_ENREP) == FALSE & NINN$ENV_ENREP == 3", 
                "is.na(NINN$ENV_ENAGR) == FALSE & NINN$ENV_ENAGR == 3", 
                "is.na(NINN$ENV_ENCOST) == FALSE & NINN$ENV_ENCOST == 3", 
                "is.na(NINN$ENV_ENREQU) == FALSE & NINN$ENV_ENREQU == 3", 
                "is.na(NINN$ENV_ENEREG) == FALSE & NINN$ENV_ENEREG == 2", 
                "is.na(NINN$ENV_ENETX) == FALSE & NINN$ENV_ENETX == 2", 
                "is.na(NINN$ENV_ENREGF) == FALSE & NINN$ENV_ENREGF == 2", 
                "is.na(NINN$ENV_ENGRA) == FALSE & NINN$ENV_ENGRA == 2", 
                "is.na(NINN$ENV_ENDEM) == FALSE & NINN$ENV_ENDEM == 2", 
                "is.na(NINN$ENV_ENREP) == FALSE & NINN$ENV_ENREP == 2", 
                "is.na(NINN$ENV_ENAGR) == FALSE & NINN$ENV_ENAGR == 2", 
                "is.na(NINN$ENV_ENCOST) == FALSE & NINN$ENV_ENCOST == 2", 
                "is.na(NINN$ENV_ENREQU) == FALSE & NINN$ENV_ENREQU == 2", 
                "is.na(NINN$ENV_ENEREG) == FALSE & NINN$ENV_ENEREG == 1", 
                "is.na(NINN$ENV_ENETX) == FALSE & NINN$ENV_ENETX == 1", 
                "is.na(NINN$ENV_ENREGF) == FALSE & NINN$ENV_ENREGF == 1", 
                "is.na(NINN$ENV_ENGRA) == FALSE & NINN$ENV_ENGRA == 1", 
                "is.na(NINN$ENV_ENDEM) == FALSE & NINN$ENV_ENDEM == 1", 
                "is.na(NINN$ENV_ENREP) == FALSE & NINN$ENV_ENREP == 1", 
                "is.na(NINN$ENV_ENAGR) == FALSE & NINN$ENV_ENAGR == 1", 
                "is.na(NINN$ENV_ENCOST) == FALSE & NINN$ENV_ENCOST == 1", 
                "is.na(NINN$ENV_ENREQU) == FALSE & NINN$ENV_ENREQU == 1", 
                "is.na(NINN$ENV_ENEREG) == FALSE & NINN$ENV_ENEREG == 0", 
                "is.na(NINN$ENV_ENETX) == FALSE & NINN$ENV_ENETX == 0", 
                "is.na(NINN$ENV_ENREGF) == FALSE & NINN$ENV_ENREGF == 0", 
                "is.na(NINN$ENV_ENGRA) == FALSE & NINN$ENV_ENGRA == 0", 
                "is.na(NINN$ENV_ENDEM) == FALSE & NINN$ENV_ENDEM == 0", 
                "is.na(NINN$ENV_ENREP) == FALSE & NINN$ENV_ENREP == 0", 
                "is.na(NINN$ENV_ENAGR) == FALSE & NINN$ENV_ENAGR == 0", 
                "is.na(NINN$ENV_ENCOST) == FALSE & NINN$ENV_ENCOST == 0", 
                "is.na(NINN$ENV_ENREQU) == FALSE & NINN$ENV_ENREQU == 0")

T31_SDMX <- "T31_SDMX_NINN.txt"
T31_SDMX_RESULT <- "T31_SDMX_NINN_RESULT.txt"

TYPE_ENT <- "NINN"

cat(paste("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS", sep = ";"), sep = "\n", file = T31_SDMX, append = FALSE)
for(num in 1:length(expression)){

  NINN_DT <- data.table(NINN[eval(parse(text = expression[num])), ])
  NINN_DT <- NINN_DT[, .(ENT22_SULYOZOTT=sum(VGMA001_SULY)), by = "M065_RETEG1,M0581_2J"]
  
  if(nrow(NINN_DT) != 0){
    
    NINN_Ordered <- cbind(NINN_DT, stringsAsFactors = FALSE)
    NINN_Ordered <- NINN_Ordered[order(NINN_Ordered$M065_RETEG1, NINN_Ordered$M0581_2J), ]
    
    CIS_INDICATOR <- expression2[num]
    
    for(i in 1:nrow(NINN_Ordered)){
      
      if(NINN_Ordered[i, 1] == "KI"){
        
        NUMBER_EMPL <- "E10T49"
        
      } else if(NINN_Ordered[i, 1] == "KO"){
        
        NUMBER_EMPL <- "E50T249"
        
      } else{
        
        NUMBER_EMPL <- "E_GE250"   
        
      }
      
      ACTIVITY <- get_NACE(NINN_Ordered[i, 2])
      OBS_VALUE <- NINN_Ordered[i, 3]
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = T31_SDMX, append = TRUE)
      
    }
    
  }
}

TXTData <- file(description = T31_SDMX, open = "r")
line <- readLines(con = TXTData)
Aggregate <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  Aggregate <- rbind(Aggregate, df)
}

names(Aggregate) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS")
Aggregate$COMMENT_OBS <- "" #"COMMENT_OBS" oszlop nincs
NUMBER_EMPL <- "_T"
for(num in 1:length(expression)){
  
  CIS_INDICATOR <- expression2[num]
  
  for(j in 1:length(ACTIVITY_LIST)){
    
    ACTIVITY <- ACTIVITY_LIST[j]
    insert_Total(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  TXTData <- file(description = T31_SDMX, open = "r")
  line <- readLines(con = TXTData)
  result_Aggregate <- data.frame()
  
  for (i in 2:length(line)){
    values <- strsplit(x = line[i], split = ";");
    df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
    result_Aggregate <- rbind(result_Aggregate, df)
  }
  
  names(result_Aggregate) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS")
  result_Aggregate$COMMENT_OBS <- ""
  result_Aggregate <- result_Aggregate[result_Aggregate$NUMBER_EMPL == "_T", ]
  nrow(result_Aggregate)
  result_Aggregate[, "OBS_VALUE"] <- gsub("\\.", ",", result_Aggregate[, "OBS_VALUE"])
  write.table(result_Aggregate, T31_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)
  
  #Összetett értékek számítása több ACTIVITY kód alapján az összes vállalatcsoportra
  ACTIVITY <- "A"
  NUMBER_EMPL <- "_T"
  insert_A(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "B"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_B(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "C"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_C(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "C10T12"
  insert_C10T12(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "C13T15"
  insert_C13T15(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "C16T18"
  insert_C16T18(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  #ACTIVITY <- "C19_20"
  #insert_C19T20(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "C19T21"
  insert_C19T21(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  #ACTIVITY <- "C19T22"
  #insert_C19T22(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "C22_23"
  insert_C22T23(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "C24_25"
  insert_C24T25(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "C26T28"
  insert_C26T28(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "C29_30"
  insert_C29T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  #ACTIVITY <- "C25T30"
  #insert_C25T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "C31T33"
  insert_C31T33(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "D"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_D(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "E"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_E(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "E36_37"
  insert_E36T37(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "E38_39"
  insert_E38T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  #ACTIVITY <- "E37T39"
  #insert_E37T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "G"
  insert_G(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "G46"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "H"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_H(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "F"
  insert_F(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "H49T51"
  insert_H49T51(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "H52_53"
  insert_H52T53(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "I"
  insert_I(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "J"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_J(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "J58T60"
  insert_J58T60(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "J61T63"
  insert_J61T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  #ACTIVITY <- "J62_63"
  #insert_J62T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "K"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_K(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "M"
  insert_M(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "M71T73"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_M71T73(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "N"
  insert_N(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
  
  ACTIVITY <- "BTE"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_BTE(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "GTN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_GTN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "G46TM73_INN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46TM73_INN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
  ACTIVITY <- "_T"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert__T(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T31_SDMX)
    
  }
  
}

TXTData <- file(description = T31_SDMX, open = "r")
line <- readLines(con = TXTData)
result_Aggregate <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  result_Aggregate <- rbind(result_Aggregate, df)
}

names(result_Aggregate) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS")
result_Aggregate$COMMENT_OBS <- ""
result_Aggregate <- result_Aggregate[(result_Aggregate$ACTIVITY != "F41" & result_Aggregate$ACTIVITY != "F42" & result_Aggregate$ACTIVITY != "F43" & result_Aggregate$ACTIVITY != "I55" & result_Aggregate$ACTIVITY != "I56") & (result_Aggregate$NUMBER_EMPL == "_T" | result_Aggregate$ACTIVITY == "M71T73" | result_Aggregate$ACTIVITY == "K" | result_Aggregate$ACTIVITY == "J" | result_Aggregate$ACTIVITY == "H" | result_Aggregate$ACTIVITY == "G46" | result_Aggregate$ACTIVITY == "A" | result_Aggregate$ACTIVITY == "B" | result_Aggregate$ACTIVITY == "C" | result_Aggregate$ACTIVITY == "D" | result_Aggregate$ACTIVITY == "E" | result_Aggregate$ACTIVITY == "I" | result_Aggregate$ACTIVITY == "BTE" | result_Aggregate$ACTIVITY == "GTN" | result_Aggregate$ACTIVITY == "G46TM73_INN" | result_Aggregate$ACTIVITY == "_T"), ]
result_Aggregate <- unique(result_Aggregate) #G46 miatt
nrow(result_Aggregate)
result_Aggregate[, "OBS_VALUE"] <- gsub("\\.", ",", result_Aggregate[, "OBS_VALUE"])
write.table(result_Aggregate, T31_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)



T31_SDMX <- "T31_SDMX_INN.txt"
T31_SDMX_RESULT <- "T31_SDMX_ALL_RESULT.txt"
T31_SDMX_FINAL_RESULT <- "T31_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T31_SDMX, open = "r")
line <- readLines(con = TXTData)
result_Aggregate <- data.frame()
result_T31 <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  result_Aggregate <- rbind(result_Aggregate, df)
}

names(result_Aggregate) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS")
result_Aggregate$COMMENT_OBS <- ""
result_Aggregate <- result_Aggregate[(result_Aggregate$ACTIVITY != "F41" & result_Aggregate$ACTIVITY != "F42" & result_Aggregate$ACTIVITY != "F43" & result_Aggregate$ACTIVITY != "I55" & result_Aggregate$ACTIVITY != "I56") & (result_Aggregate$NUMBER_EMPL == "_T" | result_Aggregate$ACTIVITY == "M71T73" | result_Aggregate$ACTIVITY == "K" | result_Aggregate$ACTIVITY == "J" | result_Aggregate$ACTIVITY == "H" | result_Aggregate$ACTIVITY == "G46" | result_Aggregate$ACTIVITY == "A" | result_Aggregate$ACTIVITY == "B" | result_Aggregate$ACTIVITY == "C" | result_Aggregate$ACTIVITY == "D" | result_Aggregate$ACTIVITY == "E" | result_Aggregate$ACTIVITY == "I" | result_Aggregate$ACTIVITY == "BTE" | result_Aggregate$ACTIVITY == "GTN" | result_Aggregate$ACTIVITY == "G46TM73_INN" | result_Aggregate$ACTIVITY == "_T"), ]
result_Aggregate <- unique(result_Aggregate) #G46 miatt
nrow(result_Aggregate)

result_T31 <- rbind(result_T31, result_Aggregate)

T31_SDMX <- "T31_SDMX_NINN.txt"
TXTData <- file(description = T31_SDMX, open = "r")
line <- readLines(con = TXTData)
result_Aggregate <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  result_Aggregate <- rbind(result_Aggregate, df)
}

names(result_Aggregate) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS")
result_Aggregate$COMMENT_OBS <- ""
result_Aggregate <- result_Aggregate[(result_Aggregate$ACTIVITY != "F41" & result_Aggregate$ACTIVITY != "F42" & result_Aggregate$ACTIVITY != "F43" & result_Aggregate$ACTIVITY != "I55" & result_Aggregate$ACTIVITY != "I56") & (result_Aggregate$NUMBER_EMPL == "_T" | result_Aggregate$ACTIVITY == "M71T73" | result_Aggregate$ACTIVITY == "K" | result_Aggregate$ACTIVITY == "J" | result_Aggregate$ACTIVITY == "H" | result_Aggregate$ACTIVITY == "G46" | result_Aggregate$ACTIVITY == "A" | result_Aggregate$ACTIVITY == "B" | result_Aggregate$ACTIVITY == "C" | result_Aggregate$ACTIVITY == "D" | result_Aggregate$ACTIVITY == "E" | result_Aggregate$ACTIVITY == "I" | result_Aggregate$ACTIVITY == "BTE" | result_Aggregate$ACTIVITY == "GTN" | result_Aggregate$ACTIVITY == "G46TM73_INN" | result_Aggregate$ACTIVITY == "_T"), ]
result_Aggregate <- unique(result_Aggregate) #G46 miatt
nrow(result_Aggregate)

result_T31 <- rbind(result_T31, result_Aggregate)

result_T31$OBS_VALUE <- as.character(result_T31$OBS_VALUE)
result_T31[, "OBS_VALUE"] <- gsub("\\.", ",", result_T31[, "OBS_VALUE"])
write.table(result_T31, T31_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)

result_T31[, "OBS_VALUE"] <- gsub(",", "\\.", result_T31[, "OBS_VALUE"])
result_T31$OBS_VALUE <- as.numeric(result_T31$OBS_VALUE)
result_T31_DT <- data.table(result_T31)
result_T31_DT <- result_T31_DT[, .(SUM_OBS_VALUE=sum(OBS_VALUE)), by = "DATAFLOW,FREQ,TIME_PERIOD,REF_AREA,TABLENAME,ACTIVITY,NUMBER_EMPL,INN_PF,INDICATOR,CIS_INDICATOR,UNIT_MEASURE,UNIT_MULT,DECIMALS"]
setnames(result_T31_DT, "SUM_OBS_VALUE", "OBS_VALUE")

result_T31_DT_FINAL <- cbind(result_T31_DT[, 1:7], stringsAsFactors = FALSE)
result_T31_DT_FINAL$TYPE_ENT <- "_T"
result_T31_DT_FINAL <- cbind(result_T31_DT_FINAL, result_T31_DT[, c(8:10, 14, 11:13)], stringsAsFactors = FALSE)
result_T31_DT_FINAL$OBS_STATUS <- ""
result_T31_DT_FINAL$OBS_STATUS_1 <- ""
result_T31_DT_FINAL$CONF_STATUS <- ""
result_T31_DT_FINAL$COMMENT_OBS <- ""
result_T31_DT_FINAL <- as.data.frame(result_T31_DT_FINAL)
result_T31_DT_FINAL$OBS_VALUE <- as.character(result_T31_DT_FINAL$OBS_VALUE)
result_T31_DT_FINAL[, "OBS_VALUE"] <- gsub("\\.", ",", result_T31_DT_FINAL[, "OBS_VALUE"])
result_T31$OBS_VALUE <- as.character(result_T31$OBS_VALUE)
result_T31[, "OBS_VALUE"] <- gsub("\\.", ",", result_T31[, "OBS_VALUE"])
result_T31$COMMENT_OBS <- ""

result_T31_DT_FINAL[result_T31_DT_FINAL$ACTIVITY == "A" | result_T31_DT_FINAL$ACTIVITY == "A01" | result_T31_DT_FINAL$ACTIVITY == "A02" | result_T31_DT_FINAL$ACTIVITY == "A03" | result_T31_DT_FINAL$ACTIVITY == "F" | result_T31_DT_FINAL$ACTIVITY == "G45" | result_T31_DT_FINAL$ACTIVITY == "G47" | result_T31_DT_FINAL$ACTIVITY == "I" | result_T31_DT_FINAL$ACTIVITY == "L" | result_T31_DT_FINAL$ACTIVITY == "M69" | result_T31_DT_FINAL$ACTIVITY == "M70" | result_T31_DT_FINAL$ACTIVITY == "M74" | result_T31_DT_FINAL$ACTIVITY == "M75" | result_T31_DT_FINAL$ACTIVITY == "N" | result_T31_DT_FINAL$ACTIVITY == "N77" | result_T31_DT_FINAL$ACTIVITY == "N78" | result_T31_DT_FINAL$ACTIVITY == "N79" | result_T31_DT_FINAL$ACTIVITY == "N80" | result_T31_DT_FINAL$ACTIVITY == "N81" | result_T31_DT_FINAL$ACTIVITY == "N82", "OBS_VALUE"] <- ""
result_T31[result_T31$ACTIVITY == "A" | result_T31$ACTIVITY == "A01" | result_T31$ACTIVITY == "A02" | result_T31$ACTIVITY == "A03" | result_T31$ACTIVITY == "F" | result_T31$ACTIVITY == "G45" | result_T31$ACTIVITY == "G47" | result_T31$ACTIVITY == "I" | result_T31$ACTIVITY == "L" | result_T31$ACTIVITY == "M69" | result_T31$ACTIVITY == "M70" | result_T31$ACTIVITY == "M74" | result_T31$ACTIVITY == "M75" | result_T31$ACTIVITY == "N" | result_T31$ACTIVITY == "N77" | result_T31$ACTIVITY == "N78" | result_T31$ACTIVITY == "N79" | result_T31$ACTIVITY == "N80" | result_T31$ACTIVITY == "N81" | result_T31$ACTIVITY == "N82", "OBS_VALUE"] <- ""
result_T31_DT_FINAL <- subset(result_T31_DT_FINAL, ACTIVITY != "GTN" | (ACTIVITY == "GTN" & (NUMBER_EMPL == "_T")))
result_T31 <- subset(result_T31, ACTIVITY != "GTN" | (ACTIVITY == "GTN" & (NUMBER_EMPL == "_T")))
result_T31_DT_FINAL <- subset(result_T31_DT_FINAL, ACTIVITY != "G" & ACTIVITY != "M")
result_T31 <- subset(result_T31, ACTIVITY != "G" & ACTIVITY != "M")
result_T31_DT_FINAL[result_T31_DT_FINAL$TYPE_ENT == "_T" | result_T31_DT_FINAL$TYPE_ENT == "NINN", "CONF_STATUS"] <- "N"
result_T31[result_T31$TYPE_ENT == "_T" | result_T31$TYPE_ENT == "NINN", "CONF_STATUS"] <- "N"

write.table(rbind(result_T31_DT_FINAL, result_T31), T31_SDMX_FINAL_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)

set_ENT_Profile("ENV_ENAGR_0", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENAGR_1", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENAGR_2", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENAGR_3", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENCOST_0", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENCOST_1", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENCOST_2", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENCOST_3", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENDEM_0", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENDEM_1", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENDEM_2", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENDEM_3", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENEREG_0", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENEREG_1", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENEREG_2", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENEREG_3", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENETX_0", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENETX_1", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENETX_2", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENETX_3", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENGRA_0", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENGRA_1", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENGRA_2", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENGRA_3", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENREGF_0", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENREGF_1", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENREGF_2", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENREGF_3", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENREP_0", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENREP_1", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENREP_2", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENREP_3", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENREQU_0", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENREQU_1", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENREQU_2", "T31", T31_SDMX_FINAL_RESULT)
set_ENT_Profile("ENV_ENREQU_3", "T31", T31_SDMX_FINAL_RESULT)