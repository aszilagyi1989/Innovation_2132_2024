source("Functions_Standard.R")
source("Profile_ENT.R")
library("data.table")
library("dplyr")

T28_SDMX <- "T28_SDMX_INN.txt"
T28_SDMX_RESULT <- "T28_SDMX_INN_RESULT.txt"

TABLENAME <- "T28"

TYPE_ENT <- "INN"
INN_PF <- "_Z"
INDICATOR <- "ENT"

UNIT_MEASURE <- "PN"
UNIT_MULT <- "0"
DECIMALS <- "0"

expression <- c("is.na(INN$COND_GOUT_AF) == FALSE & INN$COND_GOUT_AF == 1", 
              "is.na(INN$COND_GOUT_AS) == FALSE & INN$COND_GOUT_AS == 1", 
              "is.na(INN$COND_GOUT_AL) == FALSE & INN$COND_GOUT_AL == 1", 
              "is.na(INN$COND_GOUT_NALL) == FALSE & INN$COND_GOUT_NALL == 1", 
              "is.na(INN$COND_TPRED_AF) == FALSE & INN$COND_TPRED_AF == 1", 
              "is.na(INN$COND_TPRED_AS) == FALSE & INN$COND_TPRED_AS == 1", 
              "is.na(INN$COND_TPRED_AL) == FALSE & INN$COND_TPRED_AL == 1", 
              "is.na(INN$COND_TPRED_NALL) == FALSE & INN$COND_TPRED_NALL == 1", 
              "is.na(INN$COND_ESUB_AF) == FALSE & INN$COND_ESUB_AF == 1", 
              "is.na(INN$COND_ESUB_AS) == FALSE & INN$COND_ESUB_AS == 1", 
              "is.na(INN$COND_ESUB_AL) == FALSE & INN$COND_ESUB_AL == 1", 
              "is.na(INN$COND_ESUB_NALL) == FALSE & INN$COND_ESUB_NALL == 1", 
              "is.na(INN$COND_CTHR_AF) == FALSE & INN$COND_CTHR_AF == 1", 
              "is.na(INN$COND_CTHR_AS) == FALSE & INN$COND_CTHR_AS == 1", 
              "is.na(INN$COND_CTHR_AL) == FALSE & INN$COND_CTHR_AL == 1", 
              "is.na(INN$COND_CTHR_NALL) == FALSE & INN$COND_CTHR_NALL == 1", 
              "is.na(INN$COND_CPRED_AF) == FALSE & INN$COND_CPRED_AF == 1", 
              "is.na(INN$COND_CPRED_AS) == FALSE & INN$COND_CPRED_AS == 1", 
              "is.na(INN$COND_CPRED_AL) == FALSE & INN$COND_CPRED_AL == 1", 
              "is.na(INN$COND_CPRED_NALL) == FALSE & INN$COND_CPRED_NALL == 1", 
              "is.na(INN$COND_CHDEM_AF) == FALSE & INN$COND_CHDEM_AF == 1", 
              "is.na(INN$COND_CHDEM_AS) == FALSE & INN$COND_CHDEM_AS == 1", 
              "is.na(INN$COND_CHDEM_AL) == FALSE & INN$COND_CHDEM_AL == 1", 
              "is.na(INN$COND_CHDEM_NALL) == FALSE & INN$COND_CHDEM_NALL == 1", 
              "is.na(INN$COND_CABR_AF) == FALSE & INN$COND_CABR_AF == 1", 
              "is.na(INN$COND_CABR_AS) == FALSE & INN$COND_CABR_AS == 1", 
              "is.na(INN$COND_CABR_AL) == FALSE & INN$COND_CABR_AL == 1", 
              "is.na(INN$COND_CABR_NALL) == FALSE & INN$COND_CABR_NALL == 1", 
              "is.na(INN$COND_CLLOS_AF) == FALSE & INN$COND_CLLOS_AF == 1", 
              "is.na(INN$COND_CLLOS_AS) == FALSE & INN$COND_CLLOS_AS == 1", 
              "is.na(INN$COND_CLLOS_AL) == FALSE & INN$COND_CLLOS_AL == 1", 
              "is.na(INN$COND_CLLOS_NALL) == FALSE & INN$COND_CLLOS_NALL == 1")

expression2 <- c("COND_GOUT_AF", 
                "COND_GOUT_AS", 
                "COND_GOUT_AL", 
                "COND_GOUT_NALL", 
                "COND_TPRED_AF", 
                "COND_TPRED_AS", 
                "COND_TPRED_AL", 
                "COND_TPRED_NALL", 
                "COND_ESUB_AF", 
                "COND_ESUB_AS", 
                "COND_ESUB_AL", 
                "COND_ESUB_NALL", 
                "COND_CTHR_AF", 
                "COND_CTHR_AS", 
                "COND_CTHR_AL", 
                "COND_CTHR_NALL", 
                "COND_CPRED_AF", 
                "COND_CPRED_AS", 
                "COND_CPRED_AL", 
                "COND_CPRED_NALL", 
                "COND_CHDEM_AF", 
                "COND_CHDEM_AS", 
                "COND_CHDEM_AL", 
                "COND_CHDEM_NALL", 
                "COND_CABR_AF", 
                "COND_CABR_AS", 
                "COND_CABR_AL", 
                "COND_CABR_NALL", 
                "COND_CLLOS_AF", 
                "COND_CLLOS_AS", 
                "COND_CLLOS_AL", 
                "COND_CLLOS_NALL")

cat(paste("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS", sep = ";"), sep = "\n", file = T28_SDMX, append = FALSE)
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
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = T28_SDMX, append = TRUE)
    
    }
  }
}

TXTData <- file(description = T28_SDMX, open = "r")
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
    insert_Total(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  TXTData <- file(description = T28_SDMX, open = "r")
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
  write.table(result_Aggregate, T28_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)
  
  #Összetett értékek számítása több ACTIVITY kód alapján az összes vállalatcsoportra
  ACTIVITY <- "A"
  NUMBER_EMPL <- "_T"
  insert_A(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "B"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_B(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "C"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_C(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "C10T12"
  insert_C10T12(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "C13T15"
  insert_C13T15(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "C16T18"
  insert_C16T18(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  #ACTIVITY <- "C19_20"
  #insert_C19T20(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "C19T21"
  insert_C19T21(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  #ACTIVITY <- "C19T22"
  #insert_C19T22(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "C22_23"
  insert_C22T23(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "C24_25"
  insert_C24T25(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "C26T28"
  insert_C26T28(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "C29_30"
  insert_C29T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  #ACTIVITY <- "C25T30"
  #insert_C25T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "C31T33"
  insert_C31T33(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "D"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_D(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "E"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_E(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "E36_37"
  insert_E36T37(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "E38_39"
  insert_E38T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  #ACTIVITY <- "E37T39"
  #insert_E37T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "G"
  insert_G(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "G46"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "H"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_H(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "F"
  insert_F(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "H49T51"
  insert_H49T51(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "H52_53"
  insert_H52T53(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "I"
  insert_I(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "J"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_J(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "J58T60"
  insert_J58T60(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "J61T63"
  insert_J61T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  #ACTIVITY <- "J62_63"
  #insert_J62T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "K"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_K(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "M"
  insert_M(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "M71T73"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_M71T73(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "N"
  insert_N(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "BTE"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_BTE(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "GTN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_GTN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "G46TM73_INN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46TM73_INN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "_T"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert__T(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
}

TXTData <- file(description = T28_SDMX, open = "r")
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
write.table(result_Aggregate, T28_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)


expression <- c("is.na(NINN$COND_GOUT_AF) == FALSE & NINN$COND_GOUT_AF == 1", 
              "is.na(NINN$COND_GOUT_AS) == FALSE & NINN$COND_GOUT_AS == 1", 
              "is.na(NINN$COND_GOUT_AL) == FALSE & NINN$COND_GOUT_AL == 1", 
              "is.na(NINN$COND_GOUT_NALL) == FALSE & NINN$COND_GOUT_NALL == 1", 
              "is.na(NINN$COND_TPRED_AF) == FALSE & NINN$COND_TPRED_AF == 1", 
              "is.na(NINN$COND_TPRED_AS) == FALSE & NINN$COND_TPRED_AS == 1", 
              "is.na(NINN$COND_TPRED_AL) == FALSE & NINN$COND_TPRED_AL == 1", 
              "is.na(NINN$COND_TPRED_NALL) == FALSE & NINN$COND_TPRED_NALL == 1", 
              "is.na(NINN$COND_ESUB_AF) == FALSE & NINN$COND_ESUB_AF == 1", 
              "is.na(NINN$COND_ESUB_AS) == FALSE & NINN$COND_ESUB_AS == 1", 
              "is.na(NINN$COND_ESUB_AL) == FALSE & NINN$COND_ESUB_AL == 1", 
              "is.na(NINN$COND_ESUB_NALL) == FALSE & NINN$COND_ESUB_NALL == 1", 
              "is.na(NINN$COND_CTHR_AF) == FALSE & NINN$COND_CTHR_AF == 1", 
              "is.na(NINN$COND_CTHR_AS) == FALSE & NINN$COND_CTHR_AS == 1", 
              "is.na(NINN$COND_CTHR_AL) == FALSE & NINN$COND_CTHR_AL == 1", 
              "is.na(NINN$COND_CTHR_NALL) == FALSE & NINN$COND_CTHR_NALL == 1", 
              "is.na(NINN$COND_CPRED_AF) == FALSE & NINN$COND_CPRED_AF == 1", 
              "is.na(NINN$COND_CPRED_AS) == FALSE & NINN$COND_CPRED_AS == 1", 
              "is.na(NINN$COND_CPRED_AL) == FALSE & NINN$COND_CPRED_AL == 1", 
              "is.na(NINN$COND_CPRED_NALL) == FALSE & NINN$COND_CPRED_NALL == 1", 
              "is.na(NINN$COND_CHDEM_AF) == FALSE & NINN$COND_CHDEM_AF == 1", 
              "is.na(NINN$COND_CHDEM_AS) == FALSE & NINN$COND_CHDEM_AS == 1", 
              "is.na(NINN$COND_CHDEM_AL) == FALSE & NINN$COND_CHDEM_AL == 1", 
              "is.na(NINN$COND_CHDEM_NALL) == FALSE & NINN$COND_CHDEM_NALL == 1", 
              "is.na(NINN$COND_CABR_AF) == FALSE & NINN$COND_CABR_AF == 1", 
              "is.na(NINN$COND_CABR_AS) == FALSE & NINN$COND_CABR_AS == 1", 
              "is.na(NINN$COND_CABR_AL) == FALSE & NINN$COND_CABR_AL == 1", 
              "is.na(NINN$COND_CABR_NALL) == FALSE & NINN$COND_CABR_NALL == 1", 
              "is.na(NINN$COND_CLLOS_AF) == FALSE & NINN$COND_CLLOS_AF == 1", 
              "is.na(NINN$COND_CLLOS_AS) == FALSE & NINN$COND_CLLOS_AS == 1", 
              "is.na(NINN$COND_CLLOS_AL) == FALSE & NINN$COND_CLLOS_AL == 1", 
              "is.na(NINN$COND_CLLOS_NALL) == FALSE & NINN$COND_CLLOS_NALL == 1")

T28_SDMX <- "T28_SDMX_NINN.txt"
T28_SDMX_RESULT <- "T28_SDMX_NINN_RESULT.txt"

TYPE_ENT <- "NINN"

cat(paste("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS", sep = ";"), sep = "\n", file = T28_SDMX, append = FALSE)
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
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = T28_SDMX, append = TRUE)
    
    }
  }
}

TXTData <- file(description = T28_SDMX, open = "r")
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
    insert_Total(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  TXTData <- file(description = T28_SDMX, open = "r")
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
  write.table(result_Aggregate, T28_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)
  
  #Összetett értékek számítása több ACTIVITY kód alapján az összes vállalatcsoportra
  ACTIVITY <- "A"
  NUMBER_EMPL <- "_T"
  insert_A(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "B"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_B(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "C"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_C(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "C10T12"
  insert_C10T12(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "C13T15"
  insert_C13T15(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "C16T18"
  insert_C16T18(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  #ACTIVITY <- "C19_20"
  #insert_C19T20(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "C19T21"
  insert_C19T21(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  #ACTIVITY <- "C19T22"
  #insert_C19T22(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "C22_23"
  insert_C22T23(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "C24_25"
  insert_C24T25(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "C26T28"
  insert_C26T28(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "C29_30"
  insert_C29T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  #ACTIVITY <- "C25T30"
  #insert_C25T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "C31T33"
  insert_C31T33(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "D"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_D(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "E"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_E(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "E36_37"
  insert_E36T37(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "E38_39"
  insert_E38T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  #ACTIVITY <- "E37T39"
  #insert_E37T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "G"
  insert_G(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "G46"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "H"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_H(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "F"
  insert_F(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "H49T51"
  insert_H49T51(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "H52_53"
  insert_H52T53(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "I"
  insert_I(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "J"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_J(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "J58T60"
  insert_J58T60(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "J61T63"
  insert_J61T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  #ACTIVITY <- "J62_63"
  #insert_J62T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "K"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_K(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "M"
  insert_M(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "M71T73"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_M71T73(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "N"
  insert_N(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
  
  ACTIVITY <- "BTE"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_BTE(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "GTN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_GTN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "G46TM73_INN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46TM73_INN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
  ACTIVITY <- "_T"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert__T(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T28_SDMX)
    
  }
  
}

TXTData <- file(description = T28_SDMX, open = "r")
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
write.table(result_Aggregate, T28_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)


T28_SDMX <- "T28_SDMX_INN.txt"
T28_SDMX_RESULT <- "T28_SDMX_ALL_RESULT.txt"
T28_SDMX_FINAL_RESULT <- "T28_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T28_SDMX, open = "r")
line <- readLines(con = TXTData)
result_Aggregate <- data.frame()
result_T28 <- data.frame()

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

result_T28 <- rbind(result_T28, result_Aggregate)

T28_SDMX <- "T28_SDMX_NINN.txt"
TXTData <- file(description = T28_SDMX, open = "r")
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

result_T28 <- rbind(result_T28, result_Aggregate)


result_T28$OBS_VALUE <- as.character(result_T28$OBS_VALUE)
result_T28[, "OBS_VALUE"] <- gsub("\\.", ",", result_T28[, "OBS_VALUE"])
write.table(result_T28, T28_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)

result_T28[, "OBS_VALUE"] <- gsub(",", "\\.", result_T28[, "OBS_VALUE"])
result_T28$OBS_VALUE <- as.numeric(result_T28$OBS_VALUE)
result_T28_DT <- data.table(result_T28)
result_T28_DT <- result_T28_DT[, .(SUM_OBS_VALUE=sum(OBS_VALUE)), by = "DATAFLOW,FREQ,TIME_PERIOD,REF_AREA,TABLENAME,ACTIVITY,NUMBER_EMPL,INN_PF,INDICATOR,CIS_INDICATOR,UNIT_MEASURE,UNIT_MULT,DECIMALS"]
setnames(result_T28_DT, "SUM_OBS_VALUE", "OBS_VALUE")

result_T28_DT_FINAL <- cbind(result_T28_DT[, 1:7], stringsAsFactors = FALSE)
result_T28_DT_FINAL$TYPE_ENT <- "_T"
result_T28_DT_FINAL <- cbind(result_T28_DT_FINAL, result_T28_DT[, c(8:10, 14, 11:13)], stringsAsFactors = FALSE)
result_T28_DT_FINAL$OBS_STATUS <- ""
result_T28_DT_FINAL$OBS_STATUS_1 <- ""
result_T28_DT_FINAL$CONF_STATUS <- ""
result_T28_DT_FINAL$COMMENT_OBS <- ""
result_T28_DT_FINAL <- as.data.frame(result_T28_DT_FINAL)
result_T28_DT_FINAL$OBS_VALUE <- as.character(result_T28_DT_FINAL$OBS_VALUE)
result_T28_DT_FINAL[, "OBS_VALUE"] <- gsub("\\.", ",", result_T28_DT_FINAL[, "OBS_VALUE"])
result_T28$OBS_VALUE <- as.character(result_T28$OBS_VALUE)
result_T28[, "OBS_VALUE"] <- gsub("\\.", ",", result_T28[, "OBS_VALUE"])
result_T28$COMMENT_OBS <- ""

result_T28_DT_FINAL[result_T28_DT_FINAL$ACTIVITY == "A" | result_T28_DT_FINAL$ACTIVITY == "A01" | result_T28_DT_FINAL$ACTIVITY == "A02" | result_T28_DT_FINAL$ACTIVITY == "A03" | result_T28_DT_FINAL$ACTIVITY == "F" | result_T28_DT_FINAL$ACTIVITY == "G45" | result_T28_DT_FINAL$ACTIVITY == "G47" | result_T28_DT_FINAL$ACTIVITY == "I" | result_T28_DT_FINAL$ACTIVITY == "L" | result_T28_DT_FINAL$ACTIVITY == "M69" | result_T28_DT_FINAL$ACTIVITY == "M70" | result_T28_DT_FINAL$ACTIVITY == "M74" | result_T28_DT_FINAL$ACTIVITY == "M75" | result_T28_DT_FINAL$ACTIVITY == "N" | result_T28_DT_FINAL$ACTIVITY == "N77" | result_T28_DT_FINAL$ACTIVITY == "N78" | result_T28_DT_FINAL$ACTIVITY == "N79" | result_T28_DT_FINAL$ACTIVITY == "N80" | result_T28_DT_FINAL$ACTIVITY == "N81" | result_T28_DT_FINAL$ACTIVITY == "N82", "OBS_VALUE"] <- ""
result_T28[result_T28$ACTIVITY == "A" | result_T28$ACTIVITY == "A01" | result_T28$ACTIVITY == "A02" | result_T28$ACTIVITY == "A03" | result_T28$ACTIVITY == "F" | result_T28$ACTIVITY == "G45" | result_T28$ACTIVITY == "G47" | result_T28$ACTIVITY == "I" | result_T28$ACTIVITY == "L" | result_T28$ACTIVITY == "M69" | result_T28$ACTIVITY == "M70" | result_T28$ACTIVITY == "M74" | result_T28$ACTIVITY == "M75" | result_T28$ACTIVITY == "N" | result_T28$ACTIVITY == "N77" | result_T28$ACTIVITY == "N78" | result_T28$ACTIVITY == "N79" | result_T28$ACTIVITY == "N80" | result_T28$ACTIVITY == "N81" | result_T28$ACTIVITY == "N82", "OBS_VALUE"] <- ""
result_T28_DT_FINAL <- subset(result_T28_DT_FINAL, ACTIVITY != "GTN" | (ACTIVITY == "GTN" & (NUMBER_EMPL == "_T")))
result_T28 <- subset(result_T28, ACTIVITY != "GTN" | (ACTIVITY == "GTN" & (NUMBER_EMPL == "_T")))
result_T28_DT_FINAL <- subset(result_T28_DT_FINAL, ACTIVITY != "G" & ACTIVITY != "M")
result_T28 <- subset(result_T28, ACTIVITY != "G" & ACTIVITY != "M")

write.table(rbind(result_T28_DT_FINAL, result_T28), T28_SDMX_FINAL_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)

set_ENT_Profile("COND_CABR_AF", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_CABR_AL", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_CABR_AS", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_CABR_NALL", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_CHDEM_AF", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_CHDEM_AL", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_CHDEM_AS", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_CHDEM_NALL", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_CLLOS_AF", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_CLLOS_AL", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_CLLOS_AS", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_CLLOS_NALL", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_CPRED_AF", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_CPRED_AL", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_CPRED_AS", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_CPRED_NALL", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_CTHR_AF", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_CTHR_AL", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_CTHR_AS", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_CTHR_NALL", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_ESUB_AF", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_ESUB_AL", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_ESUB_AS", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_ESUB_NALL", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_GOUT_AF", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_GOUT_AL", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_GOUT_AS", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_GOUT_NALL", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_TPRED_AF", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_TPRED_AL", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_TPRED_AS", "T28", T28_SDMX_FINAL_RESULT)
set_ENT_Profile("COND_TPRED_NALL", "T28", T28_SDMX_FINAL_RESULT)