source("Functions_Standard.R")
source("Profile_ENT.R")
library("rlang")
library("data.table")
library("tidytable")

T16_SDMX <- "T16_SDMX_INN.txt"
T16_SDMX_RESULT <- "T16_SDMX_INN_RESULT.txt"

TABLENAME <- "T16"

TYPE_ENT <- "INN"
INN_PF <- "_Z"
INDICATOR <- "ENT"

UNIT_MEASURE <- "PN"
UNIT_MULT <- "0"
DECIMALS <- "0"

expression <- c("is.na(INN$FUND_EQUIT_SUCC) == FALSE & INN$FUND_EQUIT_SUCC == 1", 
                "is.na(INN$FUND_EQUIT_NSUCC) == FALSE & INN$FUND_EQUIT_NSUCC == 1", 
                "is.na(INN$FUND_EQUIT_SUCC_RNDINN) == FALSE & INN$FUND_EQUIT_SUCC_RNDINN == 1", 
                "is.na(INN$FUND_DEBT_SUCC) == FALSE & INN$FUND_DEBT_SUCC == 1", 
                "is.na(INN$FUND_DEBT_NSUCC) == FALSE & INN$FUND_DEBT_NSUCC == 1", 
                "is.na(INN$FUND_DEBT_SUCC_RNDINN) == FALSE & INN$FUND_DEBT_SUCC_RNDINN == 1", 
                "is.na(INN$FUND_AUT_LOC_REG) == FALSE & INN$FUND_AUT_LOC_REG == 1", 
                "is.na(INN$FUND_AUT_LOC_REG_RNDINN) == FALSE & INN$FUND_AUT_LOC_REG_RNDINN == 1", 
                "is.na(INN$FUND_GOV_CTL) == FALSE & INN$FUND_GOV_CTL == 1", 
                "is.na(INN$FUND_GOV_CTL_RNDINN) == FALSE & INN$FUND_GOV_CTL_RNDINN == 1", 
                "is.na(INN$FUND_EU_HP2020) == FALSE & INN$FUND_EU_HP2020 == 1", 
                "is.na(INN$FUND_EU_HP2020_RNDINN) == FALSE & INN$FUND_EU_HP2020_RNDINN == 1", 
                "is.na(INN$FUND_EU_OTH) == FALSE & INN$FUND_EU_OTH == 1", 
                "is.na(INN$FUND_EU_OTH_RNDINN) == FALSE & INN$FUND_EU_OTH_RNDINN == 1", 
                "INN$M092 %in% FUND_SOURCE$M092", 
                "INN$M092 %in% FUND_FOR_RNDINN$M092")

expression2 <- c("FUND_EQUIT_SUCC", 
                 "FUND_EQUIT_NSUCC", 
                 "FUND_EQUIT_SUCC_RNDINN", 
                 "FUND_DEBT_SUCC", 
                 "FUND_DEBT_NSUCC", 
                 "FUND_DEBT_SUCC_RNDINN", 
                 "FUND_AUT_LOC_REG", 
                 "FUND_AUT_LOC_REG_RNDINN", 
                 "FUND_GOV_CTL", 
                 "FUND_GOV_CTL_RNDINN", 
                 "FUND_EU_HP2020", 
                 "FUND_EU_HP2020_RNDINN", 
                 "FUND_EU_OTH", 
                 "FUND_EU_OTH_RNDINN", 
                 "FUND_SOURCE", 
                 "FUND_FOR_RNDINN")

cat(paste("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS", sep = ";"), sep = "\n", file = T16_SDMX, append = FALSE)
for(num in 1:length(expression)){
  
  INN_DT <- data.table(INN[eval_tidy(parse_expr(expression[num])), ])
  
  if(nrow(INN_DT) != 0){
    
    INN_DT %>% group_by(M065_RETEG1, M0581_2J) %>% summarise(ENT22_SULYOZOTT = sum(VGMA001_SULY)) %>% arrange(M065_RETEG1, M0581_2J) -> INN_Ordered
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
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = T16_SDMX, append = TRUE)
      
    }
    
  }
}

TXTData <- file(description = T16_SDMX, open = "r")
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
    insert_Total(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  TXTData <- file(description = T16_SDMX, open = "r")
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
  write.table(result_Aggregate, T16_SDMX_RESULT, sep = ";", quote = FALSE,  row.names = FALSE, append = FALSE)
  
  #Összetett értékek számítása több ACTIVITY kód alapján az összes vállalatcsoportra
  ACTIVITY <- "A"
  NUMBER_EMPL <- "_T"
  insert_A(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "B"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_B(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "C"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_C(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "C10T12"
  insert_C10T12(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "C13T15"
  insert_C13T15(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "C16T18"
  insert_C16T18(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  #ACTIVITY <- "C19_20"
  #insert_C19T20(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "C19T21"
  insert_C19T21(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  #ACTIVITY <- "C19T22"
  #insert_C19T22(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "C22_23"
  insert_C22T23(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "C24_25"
  insert_C24T25(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "C26T28"
  insert_C26T28(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "C29_30"
  insert_C29T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  #ACTIVITY <- "C25T30"
  #insert_C25T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "C31T33"
  insert_C31T33(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "D"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_D(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "E"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_E(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "E36_37"
  insert_E36T37(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "E38_39"
  insert_E38T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  #ACTIVITY <- "E37T39"
  #insert_E37T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "G"
  insert_G(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "G46"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "H"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_H(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "F"
  insert_F(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "H49T51"
  insert_H49T51(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "H52_53"
  insert_H52T53(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "I"
  insert_I(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "J"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_J(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "J58T60"
  insert_J58T60(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "J61T63"
  insert_J61T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  #ACTIVITY <- "J62_63"
  #insert_J62T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "K"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_K(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "M"
  insert_M(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "M71T73"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_M71T73(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "N"
  insert_N(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "BTE"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_BTE(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "GTN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_GTN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "G46TM73_INN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46TM73_INN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "_T"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert__T(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
}

TXTData <- file(description = T16_SDMX, open = "r")
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
write.table(result_Aggregate, T16_SDMX_RESULT, sep = ";", quote = FALSE,  row.names = FALSE, append = FALSE)

expression <- c("is.na(NINN$FUND_EQUIT_SUCC) == FALSE & NINN$FUND_EQUIT_SUCC == 1", 
                "is.na(NINN$FUND_EQUIT_NSUCC) == FALSE & NINN$FUND_EQUIT_NSUCC == 1", 
                "is.na(NINN$FUND_EQUIT_SUCC_RNDINN) == FALSE & NINN$FUND_EQUIT_SUCC_RNDINN == 1", 
                "is.na(NINN$FUND_DEBT_SUCC) == FALSE & NINN$FUND_DEBT_SUCC == 1", 
                "is.na(NINN$FUND_DEBT_NSUCC) == FALSE & NINN$FUND_DEBT_NSUCC == 1", 
                "is.na(NINN$FUND_DEBT_SUCC_RNDINN) == FALSE & NINN$FUND_DEBT_SUCC_RNDINN == 1", 
                "is.na(NINN$FUND_AUT_LOC_REG) == FALSE & NINN$FUND_AUT_LOC_REG == 1", 
                "is.na(NINN$FUND_AUT_LOC_REG_RNDINN) == FALSE & NINN$FUND_AUT_LOC_REG_RNDINN == 1", 
                "is.na(NINN$FUND_GOV_CTL) == FALSE & NINN$FUND_GOV_CTL == 1", 
                "is.na(NINN$FUND_GOV_CTL_RNDINN) == FALSE & NINN$FUND_GOV_CTL_RNDINN == 1", 
                "is.na(NINN$FUND_EU_HP2020) == FALSE & NINN$FUND_EU_HP2020 == 1", 
                "is.na(NINN$FUND_EU_HP2020_RNDINN) == FALSE & NINN$FUND_EU_HP2020_RNDINN == 1", 
                "is.na(NINN$FUND_EU_OTH) == FALSE & NINN$FUND_EU_OTH == 1", 
                "is.na(NINN$FUND_EU_OTH_RNDINN) == FALSE & NINN$FUND_EU_OTH_RNDINN == 1", 
                "NINN$M092 %in% FUND_SOURCE$M092", 
                "NINN$M092 %in% FUND_FOR_RNDINN$M092")

T16_SDMX <- "T16_SDMX_NINN.txt"
T16_SDMX_RESULT <- "T16_SDMX_NINN_RESULT.txt"

TYPE_ENT <- "NINN"

cat(paste("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS", sep = ";"), sep = "\n", file = T16_SDMX, append = FALSE)
for(num in 1:length(expression)){
  
  NINN_DT <- data.table(NINN[eval_tidy(parse_expr(expression[num])), ])
  
  if(nrow(NINN_DT) != 0){
    
    NINN_DT %>% group_by(M065_RETEG1, M0581_2J) %>% summarise(ENT22_SULYOZOTT = sum(VGMA001_SULY)) %>% arrange(M065_RETEG1, M0581_2J) -> NINN_Ordered
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
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = T16_SDMX, append = TRUE)
    
    }
  }
  
}

TXTData <- file(description = T16_SDMX, open = "r")
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
    insert_Total(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  TXTData <- file(description = T16_SDMX, open = "r")
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
  write.table(result_Aggregate, T16_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)
  
  #Összetett értékek számítása több ACTIVITY kód alapján az összes vállalatcsoportra
  ACTIVITY <- "A"
  NUMBER_EMPL <- "_T"
  insert_A(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "B"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_B(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "C"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_C(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "C10T12"
  insert_C10T12(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "C13T15"
  insert_C13T15(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "C16T18"
  insert_C16T18(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  #ACTIVITY <- "C19_20"
  #insert_C19T20(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "C19T21"
  insert_C19T21(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  #ACTIVITY <- "C19T22"
  #insert_C19T22(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "C22_23"
  insert_C22T23(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "C24_25"
  insert_C24T25(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "C26T28"
  insert_C26T28(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "C29_30"
  insert_C29T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  #ACTIVITY <- "C25T30"
  #insert_C25T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "C31T33"
  insert_C31T33(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "D"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_D(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "E"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_E(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "E36_37"
  insert_E36T37(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "E38_39"
  insert_E38T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  #ACTIVITY <- "E37T39"
  #insert_E37T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "G"
  insert_G(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "G46"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "H"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_H(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "F"
  insert_F(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "H49T51"
  insert_H49T51(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "H52_53"
  insert_H52T53(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "I"
  insert_I(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "J"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_J(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "J58T60"
  insert_J58T60(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "J61T63"
  insert_J61T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  #ACTIVITY <- "J62_63"
  #insert_J62T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "K"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_K(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "M"
  insert_M(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "M71T73"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_M71T73(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "N"
  insert_N(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
  
  ACTIVITY <- "BTE"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_BTE(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "GTN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_GTN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "G46TM73_INN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46TM73_INN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
  ACTIVITY <- "_T"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert__T(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T16_SDMX)
    
  }
  
}

TXTData <- file(description = T16_SDMX, open = "r")
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
write.table(result_Aggregate, T16_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)


T16_SDMX <- "T16_SDMX_INN.txt"
T16_SDMX_RESULT <- "T16_SDMX_ALL_RESULT.txt"
T16_SDMX_FINAL_RESULT <- "T16_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T16_SDMX, open = "r")
line <- readLines(con = TXTData)
result_Aggregate <- data.frame()
result_T16 <- data.frame()

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

result_T16 <- rbind(result_T16, result_Aggregate)

T16_SDMX <- "T16_SDMX_NINN.txt"
TXTData <- file(description = T16_SDMX, open = "r")
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

result_T16 <- rbind(result_T16, result_Aggregate)

result_T16$OBS_VALUE <- as.character(result_T16$OBS_VALUE)
result_T16[, "OBS_VALUE"] <- gsub("\\.", ",", result_T16[, "OBS_VALUE"])
write.table(result_T16, T16_SDMX_RESULT, sep = ";", quote = FALSE,  row.names = FALSE, append = FALSE)

result_T16[, "OBS_VALUE"] <- gsub(",", "\\.", result_T16[, "OBS_VALUE"])
result_T16$OBS_VALUE <- as.numeric(result_T16$OBS_VALUE)
result_T16_DT <- data.table(result_T16)
result_T16_DT <- result_T16_DT[, .(SUM_OBS_VALUE=sum(OBS_VALUE)), by = "DATAFLOW,FREQ,TIME_PERIOD,REF_AREA,TABLENAME,ACTIVITY,NUMBER_EMPL,INN_PF,INDICATOR,CIS_INDICATOR,UNIT_MEASURE,UNIT_MULT,DECIMALS"]
setnames(result_T16_DT, "SUM_OBS_VALUE", "OBS_VALUE")

result_T16_DT_FINAL <- cbind(result_T16_DT[, 1:7], stringsAsFactors = FALSE)
result_T16_DT_FINAL$TYPE_ENT <- "_T"
result_T16_DT_FINAL <- cbind(result_T16_DT_FINAL, result_T16_DT[, c(8:10, 14, 11:13)], stringsAsFactors = FALSE)
result_T16_DT_FINAL$OBS_STATUS <- ""
result_T16_DT_FINAL$OBS_STATUS_1 <- ""
result_T16_DT_FINAL$CONF_STATUS <- ""
result_T16_DT_FINAL$COMMENT_OBS <- ""
result_T16_DT_FINAL <- as.data.frame(result_T16_DT_FINAL)
result_T16_DT_FINAL$OBS_VALUE <- as.character(result_T16_DT_FINAL$OBS_VALUE)
result_T16_DT_FINAL[, "OBS_VALUE"] <- gsub("\\.", ",", result_T16_DT_FINAL[, "OBS_VALUE"])
result_T16$OBS_VALUE <- as.character(result_T16$OBS_VALUE)
result_T16[, "OBS_VALUE"] <- gsub("\\.", ",", result_T16[, "OBS_VALUE"])
result_T16$COMMENT_OBS <- ""

result_T16_DT_FINAL[result_T16_DT_FINAL$ACTIVITY == "A" | result_T16_DT_FINAL$ACTIVITY == "A01" | result_T16_DT_FINAL$ACTIVITY == "A02" | result_T16_DT_FINAL$ACTIVITY == "A03" | result_T16_DT_FINAL$ACTIVITY == "F" | result_T16_DT_FINAL$ACTIVITY == "G45" | result_T16_DT_FINAL$ACTIVITY == "G47" | result_T16_DT_FINAL$ACTIVITY == "I" | result_T16_DT_FINAL$ACTIVITY == "L" | result_T16_DT_FINAL$ACTIVITY == "M69" | result_T16_DT_FINAL$ACTIVITY == "M70" | result_T16_DT_FINAL$ACTIVITY == "M74" | result_T16_DT_FINAL$ACTIVITY == "M75" | result_T16_DT_FINAL$ACTIVITY == "N" | result_T16_DT_FINAL$ACTIVITY == "N77" | result_T16_DT_FINAL$ACTIVITY == "N78" | result_T16_DT_FINAL$ACTIVITY == "N79" | result_T16_DT_FINAL$ACTIVITY == "N80" | result_T16_DT_FINAL$ACTIVITY == "N81" | result_T16_DT_FINAL$ACTIVITY == "N82", "OBS_VALUE"] <- ""
result_T16[result_T16$ACTIVITY == "A" | result_T16$ACTIVITY == "A01" | result_T16$ACTIVITY == "A02" | result_T16$ACTIVITY == "A03" | result_T16$ACTIVITY == "F" | result_T16$ACTIVITY == "G45" | result_T16$ACTIVITY == "G47" | result_T16$ACTIVITY == "I" | result_T16$ACTIVITY == "L" | result_T16$ACTIVITY == "M69" | result_T16$ACTIVITY == "M70" | result_T16$ACTIVITY == "M74" | result_T16$ACTIVITY == "M75" | result_T16$ACTIVITY == "N" | result_T16$ACTIVITY == "N77" | result_T16$ACTIVITY == "N78" | result_T16$ACTIVITY == "N79" | result_T16$ACTIVITY == "N80" | result_T16$ACTIVITY == "N81" | result_T16$ACTIVITY == "N82", "OBS_VALUE"] <- ""
result_T16_DT_FINAL <- subset(result_T16_DT_FINAL, ACTIVITY != "GTN" | (ACTIVITY == "GTN" & (NUMBER_EMPL == "_T")))
result_T16 <- subset(result_T16, ACTIVITY != "GTN" | (ACTIVITY == "GTN" & (NUMBER_EMPL == "_T")))
result_T16_DT_FINAL <- subset(result_T16_DT_FINAL, ACTIVITY != "G" & ACTIVITY != "M")
result_T16 <- subset(result_T16, ACTIVITY != "G" & ACTIVITY != "M")
result_T16_DT_FINAL <- subset(result_T16_DT_FINAL, (TYPE_ENT == "INN" & (CIS_INDICATOR == "FUND_EQUIT_SUCC_RNDINN" | CIS_INDICATOR == "FUND_DEBT_SUCC_RNDINN" | CIS_INDICATOR == "FUND_AUT_LOC_REG_RNDINN" | CIS_INDICATOR == "FUND_GOV_CTL_RNDINN" | CIS_INDICATOR == "FUND_EU_HP2020_RNDINN" | CIS_INDICATOR == "FUND_EU_OTH_RNDINN" | CIS_INDICATOR == "FUND_FOR_RNDINN")) | (CIS_INDICATOR == "FUND_EQUIT_SUCC" | CIS_INDICATOR == "FUND_EQUIT_NSUCC" | CIS_INDICATOR == "FUND_DEBT_SUCC" | CIS_INDICATOR == "FUND_DEBT_NSUCC" | CIS_INDICATOR == "FUND_AUT_LOC_REG" | CIS_INDICATOR == "FUND_GOV_CTL" | CIS_INDICATOR == "FUND_EU_HP2020" | CIS_INDICATOR == "FUND_EU_OTH" | CIS_INDICATOR == "FUND_SOURCE"))
result_T16 <- subset(result_T16, (TYPE_ENT == "INN" & (CIS_INDICATOR == "FUND_EQUIT_SUCC_RNDINN" | CIS_INDICATOR == "FUND_DEBT_SUCC_RNDINN" | CIS_INDICATOR == "FUND_AUT_LOC_REG_RNDINN" | CIS_INDICATOR == "FUND_GOV_CTL_RNDINN" | CIS_INDICATOR == "FUND_EU_HP2020_RNDINN" | CIS_INDICATOR == "FUND_EU_OTH_RNDINN" | CIS_INDICATOR == "FUND_FOR_RNDINN")) | (CIS_INDICATOR == "FUND_EQUIT_SUCC" | CIS_INDICATOR == "FUND_EQUIT_NSUCC" | CIS_INDICATOR == "FUND_DEBT_SUCC" | CIS_INDICATOR == "FUND_DEBT_NSUCC" | CIS_INDICATOR == "FUND_AUT_LOC_REG" | CIS_INDICATOR == "FUND_GOV_CTL" | CIS_INDICATOR == "FUND_EU_HP2020" | CIS_INDICATOR == "FUND_EU_OTH" | CIS_INDICATOR == "FUND_SOURCE"))

write.table(rbind(result_T16_DT_FINAL, result_T16), T16_SDMX_FINAL_RESULT, sep = ";", quote = FALSE,  row.names = FALSE, append = FALSE)

set_ENT_Profile("as.numeric(FUND_AUT_LOC_REG)", "T16", T16_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(FUND_AUT_LOC_REG_RNDINN)", "T16", T16_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(FUND_DEBT_NSUCC)", "T16", T16_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(FUND_DEBT_SUCC)", "T16", T16_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(FUND_DEBT_SUCC_RNDINN)", "T16", T16_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(FUND_EQUIT_NSUCC)", "T16", T16_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(FUND_EQUIT_SUCC)", "T16", T16_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(FUND_EQUIT_SUCC_RNDINN)", "T16", T16_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(FUND_EU_HP2020)", "T16", T16_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(FUND_EU_HP2020_RNDINN)", "T16", T16_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(FUND_EU_OTH)", "T16", T16_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(FUND_EU_OTH_RNDINN)", "T16", T16_SDMX_FINAL_RESULT)
set_ENT_Profile("FUND_FOR_RNDINN", "T16", T16_SDMX_FINAL_RESULT) #Összetett mutató
set_ENT_Profile("as.numeric(FUND_GOV_CTL)", "T16", T16_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(FUND_GOV_CTL_RNDINN)", "T16", T16_SDMX_FINAL_RESULT)
set_ENT_Profile("FUND_SOURCE", "T16", T16_SDMX_FINAL_RESULT) #Összetett mutató