source("Functions_Standard.R")
source("Profile_ENT.R")
library("data.table")
library("dplyr")

T30_SDMX <- "T30_SDMX_INN.txt"
T30_SDMX_RESULT <- "T30_SDMX_INN_RESULT.txt"

TABLENAME <- "T30"

TYPE_ENT <- "INN"
INN_PF <- "_Z"
INDICATOR <- "ENT"

UNIT_MEASURE <- "PN"
UNIT_MULT <- "0"
DECIMALS <- "0"

expression <- c("is.na(INN$ECO_MAT_SG) == FALSE & INN$ECO_MAT_SG == 1", 
                "is.na(INN$ECO_MAT_NSG) == FALSE & INN$ECO_MAT_NSG == 1", 
                "is.na(INN$ECO_MAT_NO) == FALSE & INN$ECO_MAT_NO == 1", 
                "is.na(INN$ECO_ENO_SG) == FALSE & INN$ECO_ENO_SG == 1", 
                "is.na(INN$ECO_ENO_NSG) == FALSE & INN$ECO_ENO_NSG == 1", 
                "is.na(INN$ECO_ENO_NO) == FALSE & INN$ECO_ENO_NO == 1", 
                "is.na(INN$ECO_POL_SG) == FALSE & INN$ECO_POL_SG == 1", 
                "is.na(INN$ECO_POL_NSG) == FALSE & INN$ECO_POL_NSG == 1", 
                "is.na(INN$ECO_POL_NO) == FALSE & INN$ECO_POL_NO == 1", 
                "is.na(INN$ECO_SUB_SG) == FALSE & INN$ECO_SUB_SG == 1", 
                "is.na(INN$ECO_SUB_NSG) == FALSE & INN$ECO_SUB_NSG == 1", 
                "is.na(INN$ECO_SUB_NO) == FALSE & INN$ECO_SUB_NO == 1", 
                "is.na(INN$ECO_REP_SG) == FALSE & INN$ECO_REP_SG == 1", 
                "is.na(INN$ECO_REP_NSG) == FALSE & INN$ECO_REP_NSG == 1", 
                "is.na(INN$ECO_REP_NO) == FALSE & INN$ECO_REP_NO == 1", 
                "is.na(INN$ECO_REC_SG) == FALSE & INN$ECO_REC_SG == 1", 
                "is.na(INN$ECO_REC_NSG) == FALSE & INN$ECO_REC_NSG == 1", 
                "is.na(INN$ECO_REC_NO) == FALSE & INN$ECO_REC_NO == 1", 
                "is.na(INN$ECO_BIO_SG) == FALSE & INN$ECO_BIO_SG == 1", 
                "is.na(INN$ECO_BIO_NSG) == FALSE & INN$ECO_BIO_NSG == 1", 
                "is.na(INN$ECO_BIO_NO) == FALSE & INN$ECO_BIO_NO == 1", 
                "is.na(INN$ECO_ENU_SG) == FALSE & INN$ECO_ENU_SG == 1", 
                "is.na(INN$ECO_ENU_NSG) == FALSE & INN$ECO_ENU_NSG == 1", 
                "is.na(INN$ECO_ENU_NO) == FALSE & INN$ECO_ENU_NO == 1", 
                "is.na(INN$ECO_POS_SG) == FALSE & INN$ECO_POS_SG == 1", 
                "is.na(INN$ECO_POS_NSG) == FALSE & INN$ECO_POS_NSG == 1", 
                "is.na(INN$ECO_POS_NO) == FALSE & INN$ECO_POS_NO == 1", 
                "is.na(INN$ECO_REA_SG) == FALSE & INN$ECO_REA_SG == 1", 
                "is.na(INN$ECO_REA_NSG) == FALSE & INN$ECO_REA_NSG == 1", 
                "is.na(INN$ECO_REA_NO) == FALSE & INN$ECO_REA_NO == 1", 
                "is.na(INN$ECO_EXT_SG) == FALSE & INN$ECO_EXT_SG == 1", 
                "is.na(INN$ECO_EXT_NSG) == FALSE & INN$ECO_EXT_NSG == 1", 
                "is.na(INN$ECO_EXT_NO) == FALSE & INN$ECO_EXT_NO == 1",
                "is.na(INN$ECO_BIU_SG) == FALSE & INN$ECO_BIU_SG == 1",
                "is.na(INN$ECO_BIU_NSG) == FALSE & INN$ECO_BIU_NSG == 1",
                "is.na(INN$ECO_BIU_NO) == FALSE & INN$ECO_BIU_NO == 1")

expression2 <- c("ECO_MAT_SG", 
                "ECO_MAT_NSG", 
                "ECO_MAT_NO", 
                "ECO_ENO_SG", 
                "ECO_ENO_NSG", 
                "ECO_ENO_NO", 
                "ECO_POL_SG", 
                "ECO_POL_NSG", 
                "ECO_POL_NO", 
                "ECO_SUB_SG", 
                "ECO_SUB_NSG", 
                "ECO_SUB_NO", 
                "ECO_REP_SG", 
                "ECO_REP_NSG", 
                "ECO_REP_NO", 
                "ECO_REC_SG", 
                "ECO_REC_NSG", 
                "ECO_REC_NO", 
                "ECO_BIO_SG", 
                "ECO_BIO_NSG", 
                "ECO_BIO_NO", 
                "ECO_ENU_SG", 
                "ECO_ENU_NSG", 
                "ECO_ENU_NO", 
                "ECO_POS_SG", 
                "ECO_POS_NSG", 
                "ECO_POS_NO", 
                "ECO_REA_SG", 
                "ECO_REA_NSG", 
                "ECO_REA_NO", 
                "ECO_EXT_SG", 
                "ECO_EXT_NSG", 
                "ECO_EXT_NO",
                "ECO_BIU_SG", 
                "ECO_BIU_NSG", 
                "ECO_BIU_NO")

cat(paste("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS", sep = ";"), sep = "\n", file = T30_SDMX, append = FALSE)
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
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = T30_SDMX, append = TRUE)
      
    }
    
  }
}

TXTData <- file(description = T30_SDMX, open = "r")
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
    insert_Total(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  TXTData <- file(description = T30_SDMX, open = "r")
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
  write.table(result_Aggregate, T30_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)
  
  #Összetett értékek számítása több ACTIVITY kód alapján az összes vállalatcsoportra
  ACTIVITY <- "A"
  NUMBER_EMPL <- "_T"
  insert_A(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "B"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_B(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "C"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_C(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "C10T12"
  insert_C10T12(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "C13T15"
  insert_C13T15(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "C16T18"
  insert_C16T18(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  #ACTIVITY <- "C19_20"
  #insert_C19T20(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "C19T21"
  insert_C19T21(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  #ACTIVITY <- "C19T22"
  #insert_C19T22(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "C22_23"
  insert_C22T23(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "C24_25"
  insert_C24T25(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "C26T28"
  insert_C26T28(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "C29_30"
  insert_C29T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  #ACTIVITY <- "C25T30"
  #insert_C25T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "C31T33"
  insert_C31T33(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "D"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_D(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "E"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_E(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "E36_37"
  insert_E36T37(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "E38_39"
  insert_E38T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  #ACTIVITY <- "E37T39"
  #insert_E37T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "G"
  insert_G(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "G46"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "H"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_H(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "F"
  insert_F(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "H49T51"
  insert_H49T51(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "H52_53"
  insert_H52T53(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "I"
  insert_I(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "J"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_J(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "J58T60"
  insert_J58T60(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "J61T63"
  insert_J61T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  #ACTIVITY <- "J62_63"
  #insert_J62T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "K"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_K(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "M"
  insert_M(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "M71T73"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_M71T73(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "N"
  insert_N(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "BTE"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_BTE(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "GTN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_GTN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "G46TM73_INN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46TM73_INN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "_T"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert__T(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
}

TXTData <- file(description = T30_SDMX, open = "r")
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
write.table(result_Aggregate, T30_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)

#NINN következik
expression <- c("is.na(NINN$ECO_MAT_SG) == FALSE & NINN$ECO_MAT_SG == 1", 
                "is.na(NINN$ECO_MAT_NSG) == FALSE & NINN$ECO_MAT_NSG == 1", 
                "is.na(NINN$ECO_MAT_NO) == FALSE & NINN$ECO_MAT_NO == 1", 
                "is.na(NINN$ECO_ENO_SG) == FALSE & NINN$ECO_ENO_SG == 1", 
                "is.na(NINN$ECO_ENO_NSG) == FALSE & NINN$ECO_ENO_NSG == 1", 
                "is.na(NINN$ECO_ENO_NO) == FALSE & NINN$ECO_ENO_NO == 1", 
                "is.na(NINN$ECO_POL_SG) == FALSE & NINN$ECO_POL_SG == 1", 
                "is.na(NINN$ECO_POL_NSG) == FALSE & NINN$ECO_POL_NSG == 1", 
                "is.na(NINN$ECO_POL_NO) == FALSE & NINN$ECO_POL_NO == 1", 
                "is.na(NINN$ECO_SUB_SG) == FALSE & NINN$ECO_SUB_SG == 1", 
                "is.na(NINN$ECO_SUB_NSG) == FALSE & NINN$ECO_SUB_NSG == 1", 
                "is.na(NINN$ECO_SUB_NO) == FALSE & NINN$ECO_SUB_NO == 1", 
                "is.na(NINN$ECO_REP_SG) == FALSE & NINN$ECO_REP_SG == 1", 
                "is.na(NINN$ECO_REP_NSG) == FALSE & NINN$ECO_REP_NSG == 1", 
                "is.na(NINN$ECO_REP_NO) == FALSE & NINN$ECO_REP_NO == 1", 
                "is.na(NINN$ECO_REC_SG) == FALSE & NINN$ECO_REC_SG == 1", 
                "is.na(NINN$ECO_REC_NSG) == FALSE & NINN$ECO_REC_NSG == 1", 
                "is.na(NINN$ECO_REC_NO) == FALSE & NINN$ECO_REC_NO == 1", 
                "is.na(NINN$ECO_BIO_SG) == FALSE & NINN$ECO_BIO_SG == 1", 
                "is.na(NINN$ECO_BIO_NSG) == FALSE & NINN$ECO_BIO_NSG == 1", 
                "is.na(NINN$ECO_BIO_NO) == FALSE & NINN$ECO_BIO_NO == 1", 
                "is.na(NINN$ECO_ENU_SG) == FALSE & NINN$ECO_ENU_SG == 1", 
                "is.na(NINN$ECO_ENU_NSG) == FALSE & NINN$ECO_ENU_NSG == 1", 
                "is.na(NINN$ECO_ENU_NO) == FALSE & NINN$ECO_ENU_NO == 1", 
                "is.na(NINN$ECO_POS_SG) == FALSE & NINN$ECO_POS_SG == 1", 
                "is.na(NINN$ECO_POS_NSG) == FALSE & NINN$ECO_POS_NSG == 1", 
                "is.na(NINN$ECO_POS_NO) == FALSE & NINN$ECO_POS_NO == 1", 
                "is.na(NINN$ECO_REA_SG) == FALSE & NINN$ECO_REA_SG == 1", 
                "is.na(NINN$ECO_REA_NSG) == FALSE & NINN$ECO_REA_NSG == 1", 
                "is.na(NINN$ECO_REA_NO) == FALSE & NINN$ECO_REA_NO == 1", 
                "is.na(NINN$ECO_EXT_SG) == FALSE & NINN$ECO_EXT_SG == 1", 
                "is.na(NINN$ECO_EXT_NSG) == FALSE & NINN$ECO_EXT_NSG == 1", 
                "is.na(NINN$ECO_EXT_NO) == FALSE & NINN$ECO_EXT_NO == 1",
                "is.na(NINN$ECO_BIU_SG) == FALSE & NINN$ECO_BIU_SG == 1",
                "is.na(NINN$ECO_BIU_NSG) == FALSE & NINN$ECO_BIU_NSG == 1",
                "is.na(NINN$ECO_BIU_NO) == FALSE & NINN$ECO_BIU_NO == 1")

T30_SDMX <- "T30_SDMX_NINN.txt"
T30_SDMX_RESULT <- "T30_SDMX_NINN_RESULT.txt"

TYPE_ENT <- "NINN"

cat(paste("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS", sep = ";"), sep = "\n", file = T30_SDMX, append = FALSE)
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
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = T30_SDMX, append = TRUE)
      
    }
    
  }
}

TXTData <- file(description = T30_SDMX, open = "r")
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
    insert_Total(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  TXTData <- file(description = T30_SDMX, open = "r")
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
  write.table(result_Aggregate, T30_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)
  
  #Összetett értékek számítása több ACTIVITY kód alapján az összes vállalatcsoportra
  ACTIVITY <- "A"
  NUMBER_EMPL <- "_T"
  insert_A(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "B"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_B(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "C"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_C(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "C10T12"
  insert_C10T12(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "C13T15"
  insert_C13T15(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "C16T18"
  insert_C16T18(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  #ACTIVITY <- "C19_20"
  #insert_C19T20(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "C19T21"
  insert_C19T21(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  #ACTIVITY <- "C19T22"
  #insert_C19T22(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "C22_23"
  insert_C22T23(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "C24_25"
  insert_C24T25(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "C26T28"
  insert_C26T28(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "C29_30"
  insert_C29T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  #ACTIVITY <- "C25T30"
  #insert_C25T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "C31T33"
  insert_C31T33(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "D"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_D(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "E"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_E(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "E36_37"
  insert_E36T37(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "E38_39"
  insert_E38T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  #ACTIVITY <- "E37T39"
  #insert_E37T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "G"
  insert_G(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "G46"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "H"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_H(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "F"
  insert_F(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "H49T51"
  insert_H49T51(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "H52_53"
  insert_H52T53(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "I"
  insert_I(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "J"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_J(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "J58T60"
  insert_J58T60(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "J61T63"
  insert_J61T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  #ACTIVITY <- "J62_63"
  #insert_J62T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "K"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_K(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "M"
  insert_M(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "M71T73"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_M71T73(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "N"
  insert_N(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
  
  ACTIVITY <- "BTE"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_BTE(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "GTN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_GTN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "G46TM73_INN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46TM73_INN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
  ACTIVITY <- "_T"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert__T(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T30_SDMX)
    
  }
  
}


TXTData <- file(description = T30_SDMX, open = "r")
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
write.table(result_Aggregate, T30_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)


T30_SDMX <- "T30_SDMX_INN.txt"
T30_SDMX_RESULT <- "T30_SDMX_ALL_RESULT.txt"
T30_SDMX_FINAL_RESULT <- "T30_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T30_SDMX, open = "r")
line <- readLines(con = TXTData)
result_Aggregate <- data.frame()
result_T30 <- data.frame()

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

result_T30 <- rbind(result_T30, result_Aggregate)


T30_SDMX <- "T30_SDMX_NINN.txt"
TXTData <- file(description = T30_SDMX, open = "r")
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

result_T30 <- rbind(result_T30, result_Aggregate)

result_T30$OBS_VALUE <- as.character(result_T30$OBS_VALUE)
result_T30[, "OBS_VALUE"] <- gsub("\\.", ",", result_T30[, "OBS_VALUE"])
write.table(result_T30, T30_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)

result_T30[, "OBS_VALUE"] <- gsub(",", "\\.", result_T30[, "OBS_VALUE"])
result_T30$OBS_VALUE <- as.numeric(result_T30$OBS_VALUE)
result_T30_DT <- data.table(result_T30)
result_T30_DT <- result_T30_DT[, .(SUM_OBS_VALUE=sum(OBS_VALUE)), by = "DATAFLOW,FREQ,TIME_PERIOD,REF_AREA,TABLENAME,ACTIVITY,NUMBER_EMPL,INN_PF,INDICATOR,CIS_INDICATOR,UNIT_MEASURE,UNIT_MULT,DECIMALS"]
setnames(result_T30_DT, "SUM_OBS_VALUE", "OBS_VALUE")

result_T30_DT_FINAL <- cbind(result_T30_DT[, 1:7], stringsAsFactors = FALSE)
result_T30_DT_FINAL$TYPE_ENT <- "_T"
result_T30_DT_FINAL <- cbind(result_T30_DT_FINAL, result_T30_DT[, c(8:10, 14, 11:13)], stringsAsFactors = FALSE)
result_T30_DT_FINAL$OBS_STATUS <- ""
result_T30_DT_FINAL$OBS_STATUS_1 <- ""
result_T30_DT_FINAL$CONF_STATUS <- ""
result_T30_DT_FINAL$COMMENT_OBS <- ""
result_T30_DT_FINAL <- as.data.frame(result_T30_DT_FINAL)
result_T30_DT_FINAL$OBS_VALUE <- as.character(result_T30_DT_FINAL$OBS_VALUE)
result_T30_DT_FINAL[, "OBS_VALUE"] <- gsub("\\.", ",", result_T30_DT_FINAL[, "OBS_VALUE"])
result_T30$OBS_VALUE <- as.character(result_T30$OBS_VALUE)
result_T30[, "OBS_VALUE"] <- gsub("\\.", ",", result_T30[, "OBS_VALUE"])
result_T30$COMMENT_OBS <- ""

result_T30_DT_FINAL[result_T30_DT_FINAL$ACTIVITY == "A" | result_T30_DT_FINAL$ACTIVITY == "A01" | result_T30_DT_FINAL$ACTIVITY == "A02" | result_T30_DT_FINAL$ACTIVITY == "A03" | result_T30_DT_FINAL$ACTIVITY == "F" | result_T30_DT_FINAL$ACTIVITY == "G45" | result_T30_DT_FINAL$ACTIVITY == "G47" | result_T30_DT_FINAL$ACTIVITY == "I" | result_T30_DT_FINAL$ACTIVITY == "L" | result_T30_DT_FINAL$ACTIVITY == "M69" | result_T30_DT_FINAL$ACTIVITY == "M70" | result_T30_DT_FINAL$ACTIVITY == "M74" | result_T30_DT_FINAL$ACTIVITY == "M75" | result_T30_DT_FINAL$ACTIVITY == "N" | result_T30_DT_FINAL$ACTIVITY == "N77" | result_T30_DT_FINAL$ACTIVITY == "N78" | result_T30_DT_FINAL$ACTIVITY == "N79" | result_T30_DT_FINAL$ACTIVITY == "N80" | result_T30_DT_FINAL$ACTIVITY == "N81" | result_T30_DT_FINAL$ACTIVITY == "N82", "OBS_VALUE"] <- ""
result_T30[result_T30$ACTIVITY == "A" | result_T30$ACTIVITY == "A01" | result_T30$ACTIVITY == "A02" | result_T30$ACTIVITY == "A03" | result_T30$ACTIVITY == "F" | result_T30$ACTIVITY == "G45" | result_T30$ACTIVITY == "G47" | result_T30$ACTIVITY == "I" | result_T30$ACTIVITY == "L" | result_T30$ACTIVITY == "M69" | result_T30$ACTIVITY == "M70" | result_T30$ACTIVITY == "M74" | result_T30$ACTIVITY == "M75" | result_T30$ACTIVITY == "N" | result_T30$ACTIVITY == "N77" | result_T30$ACTIVITY == "N78" | result_T30$ACTIVITY == "N79" | result_T30$ACTIVITY == "N80" | result_T30$ACTIVITY == "N81" | result_T30$ACTIVITY == "N82", "OBS_VALUE"] <- ""
result_T30_DT_FINAL <- subset(result_T30_DT_FINAL, ACTIVITY != "GTN" | (ACTIVITY == "GTN" & (NUMBER_EMPL == "_T")))
result_T30 <- subset(result_T30, ACTIVITY != "GTN" | (ACTIVITY == "GTN" & (NUMBER_EMPL == "_T")))
result_T30_DT_FINAL <- subset(result_T30_DT_FINAL, ACTIVITY != "G" & ACTIVITY != "M")
result_T30 <- subset(result_T30, ACTIVITY != "G" & ACTIVITY != "M")
result_T30_DT_FINAL[result_T30_DT_FINAL$TYPE_ENT == "_T" | result_T30_DT_FINAL$TYPE_ENT == "NINN", "CONF_STATUS"] <- "N"
result_T30[result_T30$TYPE_ENT == "_T" | result_T30$TYPE_ENT == "NINN", "CONF_STATUS"] <- "N"

write.table(rbind(result_T30_DT_FINAL, result_T30), T30_SDMX_FINAL_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)

set_ENT_Profile("ECO_BIO_NO", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_BIO_NSG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_BIO_SG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_BIU_NO", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_BIU_NSG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_BIU_SG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_ENO_NO", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_ENO_NSG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_ENO_SG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_ENU_NO", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_ENU_NSG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_ENU_SG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_EXT_NO", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_EXT_NSG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_EXT_SG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_MAT_NO", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_MAT_NSG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_MAT_SG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_POL_NO", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_POL_NSG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_POL_SG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_POS_NO", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_POS_NSG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_POS_SG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_REA_NO", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_REA_NSG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_REA_SG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_REC_NO", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_REC_NSG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_REC_SG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_REP_NO", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_REP_NSG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_REP_SG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_SUB_NO", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_SUB_NSG", "T30", T30_SDMX_FINAL_RESULT)
set_ENT_Profile("ECO_SUB_SG", "T30", T30_SDMX_FINAL_RESULT)