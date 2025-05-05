source("Functions.R")
source("PROFILE_EMPL_T1.R")
source("PROFILE_ENT_T1.R")
source("PROFILE_TOVT_T1.R")
library("data.table")
library("dplyr")

T1_SDMX <- "T1_SDMX_PARTS.txt"
T1_SDMX_RESULT <- "T1_SDMX_ALL_FINAL_RESULT.txt"

TABLENAME <- "T01"

UNIT_MEASURE <- "PN"
UNIT_MULT <- "0"
DECIMALS <- "0"

INN_PF <- "_Z"
CIS_INDICATOR <- "_Z"

expression <- c("BPCS_ONL", "PRD_BPCS_ONL", "PRD_ONL", "INNO_PRD", "INN_X_RND", 
                "PRDBPCS_COMPL", "INNA_COMPL_NOT_ZERO", "INNA_ONGO_NOT_ZERO", "INNA_ABDN_NOT_ZERO", "INNA_IH_RND_NOT_ZERO", 
                "INNA_IH_RND_CONT_NOT_ZERO", "INNA_IH_RND_OCC_NOT_ZERO", "INNA_RND_CONTR_OUT_NOT_ZERO", 
                "INNO_PRD_GD_NOT_ZERO", "INNO_PRD_SERV_NOT_ZERO", "INNA_RND_OUT_N_IH", "PRD_NEW_MKT", 
                "PRD_NEW_ENT", "PRD_NEW_ENT_ONL", "INNO_PCS_PRD_NOT_ZERO", "INNO_PCS_LOG_NOT_ZERO", 
                "INNO_PCS_COMM_NOT_ZERO", "INNO_PCS_ACCT_NOT_ZERO", "INNO_PCS_OPROC_EXTREL_NOT_ZERO", "INNO_PCS_WR_DEC_HRM_NOT_ZERO", 
                "INNO_PCS_SLS_SERV_NOT_ZERO", "INNO_BPCS")

cat(paste("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS", sep = ";"), sep = "\n", file = T1_SDMX, append = FALSE)
for(num in 1:length(expression)){
  
  T01_DT <- data.table(eval(parse(text = expression[num])))
  T01_DT <- T01_DT[, .(ENT22_SULYOZOTT=sum(VGMA001_SULY), TUR22_SULYOZOTT=sum(TUR22 * VGMA001_SULY), EMP22_SULYOZOTT=sum(EMP22 * VGMA001_SULY)), by = "M065_RETEG1,M0581_2J"]
  
  TYPE_ENT <- gsub("_NOT_ZERO", "", expression[num])
  
  if(nrow(T01_DT) != 0){
    
    for(i in 1:nrow(T01_DT)){
      
      if(T01_DT[i, 1] == "KI"){
        
        NUMBER_EMPL <- "E10T49"
        
      } else if(T01_DT[i, 1] == "KO"){
        
        NUMBER_EMPL <- "E50T249"
        
      } else{
        
        NUMBER_EMPL <- "E_GE250"   
        
      }
      
      ACTIVITY <- get_NACE(T01_DT[i, 2])
      
      OBS_VALUE <- T01_DT[i, 3]
      INDICATOR <- "ENT"
      UNIT_MEASURE <- "PN"
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = T1_SDMX, append = TRUE)
      
      OBS_VALUE <- T01_DT[i, 4]
      INDICATOR <- "TOVT" 
      UNIT_MEASURE <- "EUR"
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = T1_SDMX, append = TRUE)
      
      OBS_VALUE <- T01_DT[i, 5]
      INDICATOR <- "EMPL" 
      UNIT_MEASURE <- "PN"
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = T1_SDMX, append = TRUE)
      
    }
    
  }
  
  TXTData <- file(description = T1_SDMX, open = "r")
  line <- readLines(con = TXTData)
  Aggregate <- data.frame()
  
  for (i in 2:length(line)){
    values <- strsplit(x = line[i], split = ";");
    df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
    Aggregate <- rbind(Aggregate, df)
  }
  
  names(Aggregate) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS")
  Aggregate$COMMENT_OBS <- "" #"COMMENT_OBS" oszlop nincs
  
  ACTIVITY <- "BTE"
  for(i in 1:length(INDICATOR_LIST)){
    
    INDICATOR <- INDICATOR_LIST[i]
    
    if (INDICATOR == "ENT" | INDICATOR == "EMPL"){
      
      UNIT_MEASURE <- "PN"
      
    }else{
      
      UNIT_MEASURE <- "EUR"
      
    }
    
    for(j in 1:length(NUMBER_EMPL_LIST)){
      
      NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
      insert_BTE(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
      
    }
    
  }
  
  ACTIVITY <- "G46TM73_INN"
  for(i in 1:length(INDICATOR_LIST)){
    
    INDICATOR <- INDICATOR_LIST[i]
    
    if (INDICATOR == "ENT" | INDICATOR == "EMPL"){
      
      UNIT_MEASURE <- "PN"
      
    }else{
      
      UNIT_MEASURE <- "EUR"
      
    }
    
    for(j in 1:length(NUMBER_EMPL_LIST)){
      
      NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
      insert_G46TM73_INN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
      
    }
    
  }
  
  ACTIVITY <- "_T"
  for(i in 1:length(INDICATOR_LIST)){
    
    INDICATOR <- INDICATOR_LIST[i]
    
    if (INDICATOR == "ENT" | INDICATOR == "EMPL"){
      
      UNIT_MEASURE <- "PN"
      
    }else{
      
      UNIT_MEASURE <- "EUR"
      
    }
    
    for(j in 1:length(NUMBER_EMPL_LIST)){
      
      NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
      insert__T(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
      
    }
    
  }
    
}

TXTData <- file(description = T1_SDMX, open = "r")
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
result_Aggregate <- subset(result_Aggregate, ACTIVITY == "_T" | ACTIVITY == "BTE" | ACTIVITY == "G46TM73_INN")
result_Aggregate[result_Aggregate$UNIT_MEASURE == "EUR", "UNIT_MULT"] <- "3"
write.table(result_Aggregate, T1_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)


#INN
T1_SDMX <- "T1_SDMX_INN.txt"
T1_SDMX_RESULT <- "T1_SDMX_ALL_FINAL_RESULT.txt"

TYPE_ENT <- "INN"

INN_DT <- data.table(INN)
INN_DT <- INN_DT[, .(ENT22_SULYOZOTT=sum(VGMA001_SULY), TUR22_SULYOZOTT=sum(TUR22 * VGMA001_SULY), EMP22_SULYOZOTT=sum(EMP22 * VGMA001_SULY)), by = "M065_RETEG1,M0581_2J"]

if(nrow(INN_DT) != 0){
  
  INN_Ordered <- cbind(INN_DT, stringsAsFactors = FALSE)
  INN_Ordered <- INN_Ordered[order(INN_Ordered$M065_RETEG1, INN_Ordered$M0581_2J), ]
  
  cat(paste("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS", sep = ";"), sep = "\n", file = T1_SDMX, append = FALSE)
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
    INDICATOR <- "ENT"
    UNIT_MEASURE <- "PN"
    cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = T1_SDMX, append = TRUE)
    
    OBS_VALUE <- INN_Ordered[i, 4]
    INDICATOR <- "TOVT" 
    UNIT_MEASURE <- "EUR"
    cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = T1_SDMX, append = TRUE)
    
    OBS_VALUE <- INN_Ordered[i, 5]
    INDICATOR <- "EMPL" 
    UNIT_MEASURE <- "PN"
    cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = T1_SDMX, append = TRUE)
    
  }
  
}

TXTData <- file(description = T1_SDMX, open = "r")
line <- readLines(con = TXTData)
Aggregate <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  Aggregate <- rbind(Aggregate, df)
}

names(Aggregate) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS")
Aggregate$COMMENT_OBS <- "" #"COMMENT_OBS" oszlop nincs

for(j in 1:length(ACTIVITY_LIST)){
  
  ACTIVITY <- ACTIVITY_LIST[j]
  
  for(i in 1:length(INDICATOR_LIST)){
    
    INDICATOR <- INDICATOR_LIST[i]
    
    if (INDICATOR == "ENT" | INDICATOR == "EMPL"){
      
      UNIT_MEASURE <- "PN"
      
    }else{
      
      UNIT_MEASURE <- "EUR"
      
    }
    
    for(k in 1:length(NUMBER_EMPL_LIST)){
      
      NUMBER_EMPL <- NUMBER_EMPL_LIST[k]
      if (ACTIVITY == "G46" & NUMBER_EMPL != "_T")
        next
      
      insert_Total(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
      
    }
    
  }
  
}

TXTData <- file(description = T1_SDMX, open = "r")
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

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "A"
for(i in 1:length(INDICATOR_LIST)){

  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_A(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)

}

UNIT_MEASURE <- "PN"
ACTIVITY <- "B"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_B(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "C"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_C(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "C10T12"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_C10T12(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "C13T15"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_C13T15(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "C16T18"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_C16T18(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

# NUMBER_EMPL <- "_T"
# UNIT_MEASURE <- "PN"
# ACTIVITY <- "C19_20"
# for(i in 1:length(INDICATOR_LIST)){
#   
#   INDICATOR <- INDICATOR_LIST[i]
#   if (INDICATOR == "TOVT")
#     UNIT_MEASURE <- "EUR"
#   
#   insert_C19T20(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
#   
# }

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "C19T21"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_C19T21(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

# NUMBER_EMPL <- "_T"
# UNIT_MEASURE <- "PN"
# ACTIVITY <- "C19T22"
# for(i in 1:length(INDICATOR_LIST)){
#   
#   INDICATOR <- INDICATOR_LIST[i]
#   if (INDICATOR == "TOVT")
#     UNIT_MEASURE <- "EUR"
#   
#   insert_C19T22(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
#   
# }

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "C22_23"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_C22T23(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "C24_25"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_C24T25(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "C26T28"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_C26T28(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "C29_30"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_C29T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

# NUMBER_EMPL <- "_T"
# UNIT_MEASURE <- "PN"
# ACTIVITY <- "C25T30"
# for(i in 1:length(INDICATOR_LIST)){
#   
#   INDICATOR <- INDICATOR_LIST[i]
#   if (INDICATOR == "TOVT")
#     UNIT_MEASURE <- "EUR"
#   
#   insert_C25T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
#   
# }

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "C31T33"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_C31T33(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "D"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_D(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "E"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_E(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "E36_37"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_E36T37(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "E38_39"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_E38T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

# NUMBER_EMPL <- "_T"
# UNIT_MEASURE <- "PN"
# ACTIVITY <- "E37T39"
# for(i in 1:length(INDICATOR_LIST)){
#   
#   INDICATOR <- INDICATOR_LIST[i]
#   if (INDICATOR == "TOVT")
#     UNIT_MEASURE <- "EUR"
#   
#   insert_E37T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
#   
# }

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "G"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_G(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "G46"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "H"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_H(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "F"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_F(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "H49T51"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_H49T51(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "H52_53"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_H52T53(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "I"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_I(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "J"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_J(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "J58T60"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_J58T60(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "J61T63"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_J61T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

# NUMBER_EMPL <- "_T"
# UNIT_MEASURE <- "PN"
# ACTIVITY <- "J62_63"
# for(i in 1:length(INDICATOR_LIST)){
#   
#   INDICATOR <- INDICATOR_LIST[i]
#   if (INDICATOR == "TOVT")
#     UNIT_MEASURE <- "EUR"
#   
#   insert_J62T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
#   
# }

UNIT_MEASURE <- "PN"
ACTIVITY <- "K"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_K(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "M"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_M(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "M71T73"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_M71T73(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "N"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_N(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "BTE"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_BTE(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "GTN"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_GTN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "G46TM73_INN"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46TM73_INN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "_T"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert__T(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

TXTData <- file(description = T1_SDMX, open = "r")
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
result_Aggregate <- subset(result_Aggregate, ACTIVITY != "GTN" | (ACTIVITY == "GTN" & (NUMBER_EMPL == "_T")))
result_Aggregate <- subset(result_Aggregate, ACTIVITY != "G" & ACTIVITY != "M")
result_Aggregate[result_Aggregate$UNIT_MEASURE == "EUR", "UNIT_MULT"] <- "3"
result_Aggregate$OBS_VALUE <- as.character(result_Aggregate$OBS_VALUE)
result_Aggregate[, "OBS_VALUE"] <- gsub("\\.", ",", result_Aggregate[, "OBS_VALUE"])
result_Aggregate[result_Aggregate$ACTIVITY == "A" | result_Aggregate$ACTIVITY == "A01" | result_Aggregate$ACTIVITY == "A02" | result_Aggregate$ACTIVITY == "A03" | result_Aggregate$ACTIVITY == "F" | result_Aggregate$ACTIVITY == "G45" | result_Aggregate$ACTIVITY == "G47" | result_Aggregate$ACTIVITY == "I" | result_Aggregate$ACTIVITY == "L" | result_Aggregate$ACTIVITY == "M69" | result_Aggregate$ACTIVITY == "M70" | result_Aggregate$ACTIVITY == "M74" | result_Aggregate$ACTIVITY == "M75" | result_Aggregate$ACTIVITY == "N" | result_Aggregate$ACTIVITY == "N77" | result_Aggregate$ACTIVITY == "N78" | result_Aggregate$ACTIVITY == "N79" | result_Aggregate$ACTIVITY == "N80" | result_Aggregate$ACTIVITY == "N81" | result_Aggregate$ACTIVITY == "N82", "OBS_VALUE"] <- ""
write.table(result_Aggregate, T1_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
INN_T01 <- result_Aggregate

#NINN
T1_SDMX <- "T1_SDMX_NINN.txt"
T1_SDMX_RESULT <- "T1_SDMX_ALL_FINAL_RESULT.txt"

TYPE_ENT <- "NINN"

NINN_DT <- data.table(NINN)
NINN_DT <- NINN_DT[, .(ENT22_SULYOZOTT=sum(VGMA001_SULY), TUR22_SULYOZOTT=sum(TUR22 * VGMA001_SULY), EMP22_SULYOZOTT=sum(EMP22 * VGMA001_SULY)), by = "M065_RETEG1,M0581_2J"]

if(nrow(NINN_DT) != 0){
  
  NINN_Ordered <- cbind(NINN_DT, stringsAsFactors = FALSE)
  NINN_Ordered <- NINN_Ordered[order(NINN_Ordered$M065_RETEG1, NINN_Ordered$M0581_2J), ]
  
  cat(paste("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS", sep = ";"), sep = "\n", file = T1_SDMX, append = FALSE)
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
    INDICATOR <- "ENT"
    UNIT_MEASURE <- "PN"
    cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = T1_SDMX, append = TRUE)
    
    OBS_VALUE <- NINN_Ordered[i, 4]
    INDICATOR <- "TOVT" 
    UNIT_MEASURE <- "EUR"
    cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = T1_SDMX, append = TRUE)
    
    OBS_VALUE <- NINN_Ordered[i, 5]
    INDICATOR <- "EMPL" 
    UNIT_MEASURE <- "PN"
    cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = T1_SDMX, append = TRUE)
    
  }
  
}

TXTData <- file(description = T1_SDMX, open = "r")
line <- readLines(con = TXTData)
Aggregate <- data.frame()

for (i in 2:length(line)){
  values <- strsplit(x = line[i], split = ";");
  df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
  Aggregate <- rbind(Aggregate, df)
}

names(Aggregate) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS")
Aggregate$COMMENT_OBS <- "" #"COMMENT_OBS" oszlop nincs


for(j in 1:length(ACTIVITY_LIST)){
  
  ACTIVITY <- ACTIVITY_LIST[j]
  
  for(i in 1:length(INDICATOR_LIST)){
    
    INDICATOR <- INDICATOR_LIST[i]
    
    if (INDICATOR == "ENT" | INDICATOR == "EMPL"){
      
      UNIT_MEASURE <- "PN"
      
    }else{
      
      UNIT_MEASURE <- "EUR"
      
    }
    
    for(k in 1:length(NUMBER_EMPL_LIST)){
      
      NUMBER_EMPL <- NUMBER_EMPL_LIST[k]
      if (ACTIVITY == "G46" & NUMBER_EMPL != "_T")
        next
      
      insert_Total(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
      
    }
    
  }
  
}

TXTData <- file(description = T1_SDMX, open = "r")
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


NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "A"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_A(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "B"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_B(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "C"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_C(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "C10T12"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_C10T12(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "C13T15"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_C13T15(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "C16T18"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_C16T18(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

# NUMBER_EMPL <- "_T"
# UNIT_MEASURE <- "PN"
# ACTIVITY <- "C19_20"
# for(i in 1:length(INDICATOR_LIST)){
#   
#   INDICATOR <- INDICATOR_LIST[i]
#   if (INDICATOR == "TOVT")
#     UNIT_MEASURE <- "EUR"
#   
#   insert_C19T20(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
#   
# }

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "C19T21"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_C19T21(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

# NUMBER_EMPL <- "_T"
# UNIT_MEASURE <- "PN"
# ACTIVITY <- "C19T22"
# for(i in 1:length(INDICATOR_LIST)){
#   
#   INDICATOR <- INDICATOR_LIST[i]
#   if (INDICATOR == "TOVT")
#     UNIT_MEASURE <- "EUR"
#   
#   insert_C19T22(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
#   
# }

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "C22_23"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_C22T23(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "C24_25"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_C24T25(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "C26T28"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_C26T28(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "C29_30"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_C29T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

# NUMBER_EMPL <- "_T"
# UNIT_MEASURE <- "PN"
# ACTIVITY <- "C25T30"
# for(i in 1:length(INDICATOR_LIST)){
#   
#   INDICATOR <- INDICATOR_LIST[i]
#   if (INDICATOR == "TOVT")
#     UNIT_MEASURE <- "EUR"
#   
#   insert_C25T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
#   
# }

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "C31T33"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_C31T33(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "D"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_D(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "E"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_E(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "E36_37"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_E36T37(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "E38_39"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_E38T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

# NUMBER_EMPL <- "_T"
# UNIT_MEASURE <- "PN"
# ACTIVITY <- "E37T39"
# for(i in 1:length(INDICATOR_LIST)){
#   
#   INDICATOR <- INDICATOR_LIST[i]
#   if (INDICATOR == "TOVT")
#     UNIT_MEASURE <- "EUR"
#   
#   insert_E37T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
#   
# }

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "G"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_G(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "G46"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "H"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_H(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "F"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_F(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "H49T51"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_H49T51(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "H52_53"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_H52T53(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "I"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_I(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "J"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_J(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "J58T60"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_J58T60(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "J61T63"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_J61T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

# NUMBER_EMPL <- "_T"
# UNIT_MEASURE <- "PN"
# ACTIVITY <- "J62_63"
# for(i in 1:length(INDICATOR_LIST)){
#   
#   INDICATOR <- INDICATOR_LIST[i]
#   if (INDICATOR == "TOVT")
#     UNIT_MEASURE <- "EUR"
#   
#   insert_J62T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
#   
# }

UNIT_MEASURE <- "PN"
ACTIVITY <- "K"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_K(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "M"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_M(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "M71T73"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_M71T73(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

NUMBER_EMPL <- "_T"
UNIT_MEASURE <- "PN"
ACTIVITY <- "N"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  insert_N(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "BTE"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_BTE(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "GTN"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_GTN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "G46TM73_INN"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46TM73_INN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

UNIT_MEASURE <- "PN"
ACTIVITY <- "_T"
for(i in 1:length(INDICATOR_LIST)){
  
  INDICATOR <- INDICATOR_LIST[i]
  if (INDICATOR == "TOVT")
    UNIT_MEASURE <- "EUR"
  
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert__T(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, T1_SDMX)
    
  }
  
}

TXTData <- file(description = T1_SDMX, open = "r")
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
result_Aggregate <- subset(result_Aggregate, ACTIVITY != "GTN" | (ACTIVITY == "GTN" & (NUMBER_EMPL == "_T")))
result_Aggregate <- subset(result_Aggregate, ACTIVITY != "G" & ACTIVITY != "M")
result_Aggregate[result_Aggregate$UNIT_MEASURE == "EUR", "UNIT_MULT"] <- "3"
result_Aggregate$OBS_VALUE <- as.character(result_Aggregate$OBS_VALUE)
result_Aggregate[, "OBS_VALUE"] <- gsub("\\.", ",", result_Aggregate[, "OBS_VALUE"])
result_Aggregate[result_Aggregate$ACTIVITY == "A" | result_Aggregate$ACTIVITY == "A01" | result_Aggregate$ACTIVITY == "A02" | result_Aggregate$ACTIVITY == "A03" | result_Aggregate$ACTIVITY == "F" | result_Aggregate$ACTIVITY == "G45" | result_Aggregate$ACTIVITY == "G47" | result_Aggregate$ACTIVITY == "I" | result_Aggregate$ACTIVITY == "L" | result_Aggregate$ACTIVITY == "M69" | result_Aggregate$ACTIVITY == "M70" | result_Aggregate$ACTIVITY == "M74" | result_Aggregate$ACTIVITY == "M75" | result_Aggregate$ACTIVITY == "N" | result_Aggregate$ACTIVITY == "N77" | result_Aggregate$ACTIVITY == "N78" | result_Aggregate$ACTIVITY == "N79" | result_Aggregate$ACTIVITY == "N80" | result_Aggregate$ACTIVITY == "N81" | result_Aggregate$ACTIVITY == "N82", "OBS_VALUE"] <- ""
write.table(result_Aggregate, T1_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)
NINN_T01 <- result_Aggregate

#INN és NINN összeadása TOTAL-ra
INN_NINN_T01_DT <- data.table(rbind(INN_T01, NINN_T01))
INN_NINN_T01_DT$OBS_VALUE <- gsub(",", "\\.", INN_NINN_T01_DT$OBS_VALUE)
INN_NINN_T01_DT$OBS_VALUE <- as.numeric(INN_NINN_T01_DT$OBS_VALUE)
INN_NINN_T01_DT <- INN_NINN_T01_DT[, .(SUM_OBS_VALUE=sum(OBS_VALUE)), by = "DATAFLOW,FREQ,TIME_PERIOD,REF_AREA,TABLENAME,ACTIVITY,NUMBER_EMPL,INN_PF,INDICATOR,CIS_INDICATOR,UNIT_MEASURE,UNIT_MULT,DECIMALS"]
setnames(INN_NINN_T01_DT, "SUM_OBS_VALUE", "OBS_VALUE")
INN_NINN_T01_DT_FINAL <- cbind(INN_NINN_T01_DT[, 1:7], stringsAsFactors = FALSE)
INN_NINN_T01_DT_FINAL$TYPE_ENT <- "_T"
INN_NINN_T01_DT_FINAL <- cbind(INN_NINN_T01_DT_FINAL, INN_NINN_T01_DT[, c(8:10, 14, 11:13)], stringsAsFactors = FALSE)
INN_NINN_T01_DT_FINAL$OBS_STATUS <- ""
INN_NINN_T01_DT_FINAL$OBS_STATUS_1 <- ""
INN_NINN_T01_DT_FINAL$CONF_STATUS <- ""
INN_NINN_T01_DT_FINAL$COMMENT_OBS <- ""
INN_NINN_T01_DT_FINAL <- as.data.frame(INN_NINN_T01_DT_FINAL)
INN_NINN_T01_DT_FINAL$OBS_VALUE <- as.character(INN_NINN_T01_DT_FINAL$OBS_VALUE)
INN_NINN_T01_DT_FINAL[, "OBS_VALUE"] <- gsub("\\.", ",", INN_NINN_T01_DT_FINAL[, "OBS_VALUE"])
INN_NINN_T01_DT_FINAL[INN_NINN_T01_DT_FINAL$ACTIVITY == "A" | INN_NINN_T01_DT_FINAL$ACTIVITY == "A01" | INN_NINN_T01_DT_FINAL$ACTIVITY == "A02" | INN_NINN_T01_DT_FINAL$ACTIVITY == "A03" | INN_NINN_T01_DT_FINAL$ACTIVITY == "F" | INN_NINN_T01_DT_FINAL$ACTIVITY == "G45" | INN_NINN_T01_DT_FINAL$ACTIVITY == "G47" | INN_NINN_T01_DT_FINAL$ACTIVITY == "I" | INN_NINN_T01_DT_FINAL$ACTIVITY == "L" | INN_NINN_T01_DT_FINAL$ACTIVITY == "M69" | INN_NINN_T01_DT_FINAL$ACTIVITY == "M70" | INN_NINN_T01_DT_FINAL$ACTIVITY == "M74" | INN_NINN_T01_DT_FINAL$ACTIVITY == "M75" | INN_NINN_T01_DT_FINAL$ACTIVITY == "N" | INN_NINN_T01_DT_FINAL$ACTIVITY == "N77" | INN_NINN_T01_DT_FINAL$ACTIVITY == "N78" | INN_NINN_T01_DT_FINAL$ACTIVITY == "N79" | INN_NINN_T01_DT_FINAL$ACTIVITY == "N80" | INN_NINN_T01_DT_FINAL$ACTIVITY == "N81" | INN_NINN_T01_DT_FINAL$ACTIVITY == "N82", "OBS_VALUE"] <- ""

write.table(INN_NINN_T01_DT_FINAL, T1_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, col.names = FALSE, append = TRUE)

set_EMPL_Profile("EMP22", "T01", T1_SDMX_RESULT)
set_ENT_T1_Profile("", "T01", T1_SDMX_RESULT)
set_TOVT_Profile("TUR22", "T01", T1_SDMX_RESULT)