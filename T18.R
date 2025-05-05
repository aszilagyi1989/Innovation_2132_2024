source("Functions_Standard.R")
source("Profile_ENT.R")
library("data.table")
library("dplyr")

T18_SDMX <- "T18_SDMX_INN.txt"
T18_SDMX_RESULT <- "T18_SDMX_INN_RESULT.txt"

TABLENAME <- "T18"

TYPE_ENT <- "INN"
INN_PF <- "_Z"
INDICATOR <- "ENT"

UNIT_MEASURE <- "PN"
UNIT_MULT <- "0"
DECIMALS <- "0"

expression <- c("is.na(INN$COOP_RND) == FALSE & INN$COOP_RND == 1", 
                "is.na(INN$COOP_INN_XRND) == FALSE & INN$COOP_INN_XRND == 1", 
                "is.na(INN$COOP_OTH) == FALSE & INN$COOP_OTH == 1", 
                "INN$M092 %in% COOP_RNDINN$M092",
                "INN$M092 %in% COOP_ALL$M092",
                "INN$M092 %in% COOP_PRV$M092",
                "INN$M092 %in% COOP_PRV_CN$M092",
                "INN$M092 %in% COOP_PRV_SUPPL$M092",
                "INN$M092 %in% COOP_PRV_CLCU$M092",
                "INN$M092 %in% COOP_PRV_COMP$M092",
                "INN$M092 %in% COOP_PRV_OTH$M092",
                "INN$M092 %in% COOP_EG$M092",
                "INN$M092 %in% COOP_UNIV$M092",
                "INN$M092 %in% COOP_GOV_RI$M092",
                "INN$M092 %in% COOP_PUB_CLCU$M092",
                "INN$M092 %in% COOP_NPO$M092",
                "INN$M092 %in% COOP_NAT$M092",
                "INN$M092 %in% COOP_NNAT$M092",
                "INN$M092 %in% COOP_EU_EFTA$M092",
                "INN$M092 %in% COOP_NEU_NEFTA$M092",
                "is.na(INN$COOP_PRV_CN_NAT) == FALSE & INN$COOP_PRV_CN_NAT == 1", 
                "is.na(INN$COOP_PRV_SUPPL_NAT) == FALSE & INN$COOP_PRV_SUPPL_NAT == 1", 
                "is.na(INN$COOP_PRV_CLCU_NAT) == FALSE & INN$COOP_PRV_CLCU_NAT == 1", 
                "is.na(INN$COOP_PRV_COMP_NAT) == FALSE & INN$COOP_PRV_COMP_NAT == 1", 
                "is.na(INN$COOP_PRV_OTH_NAT) == FALSE & INN$COOP_PRV_OTH_NAT == 1", 
                "is.na(INN$COOP_EG_NAT) == FALSE & INN$COOP_EG_NAT == 1", 
                "is.na(INN$COOP_UNIV_NAT) == FALSE & INN$COOP_UNIV_NAT == 1", 
                "is.na(INN$COOP_GOV_RI_NAT) == FALSE & INN$COOP_GOV_RI_NAT == 1", 
                "is.na(INN$COOP_PUB_CLCU_NAT) == FALSE & INN$COOP_PUB_CLCU_NAT == 1", 
                "is.na(INN$COOP_NPO_NAT) == FALSE & INN$COOP_NPO_NAT == 1", 
                "is.na(INN$COOP_PRV_CN_EU_EFTA) == FALSE & INN$COOP_PRV_CN_EU_EFTA == 1", 
                "is.na(INN$COOP_PRV_SUPPL_EU_EFTA) == FALSE & INN$COOP_PRV_SUPPL_EU_EFTA == 1", 
                "is.na(INN$COOP_PRV_CLCU_EU_EFTA) == FALSE & INN$COOP_PRV_CLCU_EU_EFTA == 1", 
                "is.na(INN$COOP_PRV_COMP_EU_EFTA) == FALSE & INN$COOP_PRV_COMP_EU_EFTA == 1", 
                "is.na(INN$COOP_PRV_OTH_EU_EFTA) == FALSE & INN$COOP_PRV_OTH_EU_EFTA == 1", 
                "is.na(INN$COOP_EG_EU_EFTA) == FALSE & INN$COOP_EG_EU_EFTA == 1", 
                "is.na(INN$COOP_UNIV_EU_EFTA) == FALSE & INN$COOP_UNIV_EU_EFTA == 1", 
                "is.na(INN$COOP_GOV_RI_EU_EFTA) == FALSE & INN$COOP_GOV_RI_EU_EFTA == 1", 
                "is.na(INN$COOP_PUB_CLCU_EU_EFTA) == FALSE & INN$COOP_PUB_CLCU_EU_EFTA == 1", 
                "is.na(INN$COOP_NPO_EU_EFTA) == FALSE & INN$COOP_NPO_EU_EFTA == 1", 
                "is.na(INN$COOP_PRV_CN_NEU_NEFTA) == FALSE & INN$COOP_PRV_CN_NEU_NEFTA == 1", 
                "is.na(INN$COOP_PRV_SUPPL_NEU_NEFTA) == FALSE & INN$COOP_PRV_SUPPL_NEU_NEFTA == 1", 
                "is.na(INN$COOP_PRV_CLCU_NEU_NEFTA) == FALSE & INN$COOP_PRV_CLCU_NEU_NEFTA == 1", 
                "is.na(INN$COOP_PRV_COMP_NEU_NEFTA) == FALSE & INN$COOP_PRV_COMP_NEU_NEFTA == 1", 
                "is.na(INN$COOP_PRV_OTH_NEU_NEFTA) == FALSE & INN$COOP_PRV_OTH_NEU_NEFTA == 1", 
                "is.na(INN$COOP_EG_NEU_NEFTA) == FALSE & INN$COOP_EG_NEU_NEFTA == 1", 
                "is.na(INN$COOP_UNIV_NEU_NEFTA) == FALSE & INN$COOP_UNIV_NEU_NEFTA == 1", 
                "is.na(INN$COOP_GOV_RI_NEU_NEFTA) == FALSE & INN$COOP_GOV_RI_NEU_NEFTA == 1", 
                "is.na(INN$COOP_PUB_CLCU_NEU_NEFTA) == FALSE & INN$COOP_PUB_CLCU_NEU_NEFTA == 1", 
                "is.na(INN$COOP_NPO_NEU_NEFTA) == FALSE & INN$COOP_NPO_NEU_NEFTA == 1", 
                "INN$M092 %in% COOP_PRV_NAT$M092",
                "is.na(INN$COOP_NPRV_NAT) == FALSE & INN$COOP_NPRV_NAT == 1", 
                "is.na(INN$COOP_PRV_EU_EFTA) == FALSE & INN$COOP_PRV_EU_EFTA == 1", 
                "is.na(INN$COOP_NPRV_EU_EFTA) == FALSE & INN$COOP_NPRV_EU_EFTA == 1", 
                "is.na(INN$COOP_PRV_NEU_NEFTA) == FALSE & INN$COOP_PRV_NEU_NEFTA == 1", 
                "is.na(INN$COOP_NPRV_NEU_NEFTA) == FALSE & INN$COOP_NPRV_NEU_NEFTA == 1",
                "INN$M092 %in% COOP_NPRV$M092")

expression2 <- c("COOP_RND", 
                 "COOP_INN_XRND", 
                 "COOP_OTH", 
                 "COOP_RNDINN", 
                 "COOP_ALL", 
                 "COOP_PRV", 
                 "COOP_PRV_CN", 
                 "COOP_PRV_SUPPL", 
                 "COOP_PRV_CLCU", 
                 "COOP_PRV_COMP", 
                 "COOP_PRV_OTH", 
                 "COOP_EG", 
                 "COOP_UNIV", 
                 "COOP_GOV_RI", 
                 "COOP_PUB_CLCU", 
                 "COOP_NPO", 
                 "COOP_NAT", 
                 "COOP_NNAT", 
                 "COOP_EU_EFTA", 
                 "COOP_NEU_NEFTA", 
                 "COOP_PRV_CN_NAT", 
                 "COOP_PRV_SUPPL_NAT", 
                 "COOP_PRV_CLCU_NAT", 
                 "COOP_PRV_COMP_NAT", 
                 "COOP_PRV_OTH_NAT", 
                 "COOP_EG_NAT", 
                 "COOP_UNIV_NAT", 
                 "COOP_GOV_RI_NAT", 
                 "COOP_PUB_CLCU_NAT", 
                 "COOP_NPO_NAT", 
                 "COOP_PRV_CN_EU_EFTA", 
                 "COOP_PRV_SUPPL_EU_EFTA", 
                 "COOP_PRV_CLCU_EU_EFTA", 
                 "COOP_PRV_COMP_EU_EFTA", 
                 "COOP_PRV_OTH_EU_EFTA", 
                 "COOP_EG_EU_EFTA", 
                 "COOP_UNIV_EU_EFTA", 
                 "COOP_GOV_RI_EU_EFTA", 
                 "COOP_PUB_CLCU_EU_EFTA", 
                 "COOP_NPO_EU_EFTA", 
                 "COOP_PRV_CN_NEU_NEFTA", 
                 "COOP_PRV_SUPPL_NEU_NEFTA", 
                 "COOP_PRV_CLCU_NEU_NEFTA", 
                 "COOP_PRV_COMP_NEU_NEFTA", 
                 "COOP_PRV_OTH_NEU_NEFTA", 
                 "COOP_EG_NEU_NEFTA", 
                 "COOP_UNIV_NEU_NEFTA", 
                 "COOP_GOV_RI_NEU_NEFTA", 
                 "COOP_PUB_CLCU_NEU_NEFTA", 
                 "COOP_NPO_NEU_NEFTA",
                 "COOP_PRV_NAT", 
                 "COOP_NPRV_NAT", 
                 "COOP_PRV_EU_EFTA", 
                 "COOP_NPRV_EU_EFTA", 
                 "COOP_PRV_NEU_NEFTA", 
                 "COOP_NPRV_NEU_NEFTA",
                 "COOP_NPRV")

cat(paste("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS", sep = ";"), sep = "\n", file = T18_SDMX, append = FALSE)
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
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = T18_SDMX, append = TRUE)
      
    }
    
  }
}

TXTData <- file(description = T18_SDMX, open = "r")
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
    insert_Total(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  TXTData <- file(description = T18_SDMX, open = "r")
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
  write.table(result_Aggregate, T18_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)
  
  #Összetett értékek számítása több ACTIVITY kód alapján az összes vállalatcsoportra
  ACTIVITY <- "A"
  NUMBER_EMPL <- "_T"
  insert_A(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "B"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_B(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "C"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_C(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "C10T12"
  insert_C10T12(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "C13T15"
  insert_C13T15(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "C16T18"
  insert_C16T18(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  #ACTIVITY <- "C19_20"
  #insert_C19T20(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "C19T21"
  insert_C19T21(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  #ACTIVITY <- "C19T22"
  #insert_C19T22(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "C22_23"
  insert_C22T23(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "C24_25"
  insert_C24T25(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "C26T28"
  insert_C26T28(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "C29_30"
  insert_C29T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  #ACTIVITY <- "C25T30"
  #insert_C25T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "C31T33"
  insert_C31T33(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "D"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_D(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "E"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_E(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "E36_37"
  insert_E36T37(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "E38_39"
  insert_E38T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  #ACTIVITY <- "E37T39"
  #insert_E37T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "G"
  insert_G(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "G46"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "H"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_H(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "F"
  insert_F(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "H49T51"
  insert_H49T51(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "H52_53"
  insert_H52T53(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "I"
  insert_I(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "J"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_J(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "J58T60"
  insert_J58T60(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "J61T63"
  insert_J61T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  #ACTIVITY <- "J62_63"
  #insert_J62T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "K"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_K(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "M"
  insert_M(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "M71T73"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_M71T73(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "N"
  insert_N(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "BTE"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_BTE(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "GTN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_GTN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "G46TM73_INN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46TM73_INN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "_T"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert__T(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
}

TXTData <- file(description = T18_SDMX, open = "r")
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
write.table(result_Aggregate, T18_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)

expression <- c("is.na(NINN$COOP_RND) == FALSE & NINN$COOP_RND == 1", 
                "is.na(NINN$COOP_INN_XRND) == FALSE & NINN$COOP_INN_XRND == 1", 
                "is.na(NINN$COOP_OTH) == FALSE & NINN$COOP_OTH == 1", 
                "NINN$M092 %in% COOP_RNDINN$M092",
                "NINN$M092 %in% COOP_ALL$M092",
                "NINN$M092 %in% COOP_PRV$M092",
                "NINN$M092 %in% COOP_PRV_CN$M092",
                "NINN$M092 %in% COOP_PRV_SUPPL$M092",
                "NINN$M092 %in% COOP_PRV_CLCU$M092",
                "NINN$M092 %in% COOP_PRV_COMP$M092",
                "NINN$M092 %in% COOP_PRV_OTH$M092",
                "NINN$M092 %in% COOP_EG$M092",
                "NINN$M092 %in% COOP_UNIV$M092",
                "NINN$M092 %in% COOP_GOV_RI$M092",
                "NINN$M092 %in% COOP_PUB_CLCU$M092",
                "NINN$M092 %in% COOP_NPO$M092",
                "NINN$M092 %in% COOP_NAT$M092",
                "NINN$M092 %in% COOP_NNAT$M092",
                "NINN$M092 %in% COOP_EU_EFTA$M092",
                "NINN$M092 %in% COOP_NEU_NEFTA$M092",
                "is.na(NINN$COOP_PRV_CN_NAT) == FALSE & NINN$COOP_PRV_CN_NAT == 1", 
                "is.na(NINN$COOP_PRV_SUPPL_NAT) == FALSE & NINN$COOP_PRV_SUPPL_NAT == 1", 
                "is.na(NINN$COOP_PRV_CLCU_NAT) == FALSE & NINN$COOP_PRV_CLCU_NAT == 1", 
                "is.na(NINN$COOP_PRV_COMP_NAT) == FALSE & NINN$COOP_PRV_COMP_NAT == 1", 
                "is.na(NINN$COOP_PRV_OTH_NAT) == FALSE & NINN$COOP_PRV_OTH_NAT == 1", 
                "is.na(NINN$COOP_EG_NAT) == FALSE & NINN$COOP_EG_NAT == 1", 
                "is.na(NINN$COOP_UNIV_NAT) == FALSE & NINN$COOP_UNIV_NAT == 1", 
                "is.na(NINN$COOP_GOV_RI_NAT) == FALSE & NINN$COOP_GOV_RI_NAT == 1", 
                "is.na(NINN$COOP_PUB_CLCU_NAT) == FALSE & NINN$COOP_PUB_CLCU_NAT == 1", 
                "is.na(NINN$COOP_NPO_NAT) == FALSE & NINN$COOP_NPO_NAT == 1", 
                "is.na(NINN$COOP_PRV_CN_EU_EFTA) == FALSE & NINN$COOP_PRV_CN_EU_EFTA == 1", 
                "is.na(NINN$COOP_PRV_SUPPL_EU_EFTA) == FALSE & NINN$COOP_PRV_SUPPL_EU_EFTA == 1", 
                "is.na(NINN$COOP_PRV_CLCU_EU_EFTA) == FALSE & NINN$COOP_PRV_CLCU_EU_EFTA == 1", 
                "is.na(NINN$COOP_PRV_COMP_EU_EFTA) == FALSE & NINN$COOP_PRV_COMP_EU_EFTA == 1", 
                "is.na(NINN$COOP_PRV_OTH_EU_EFTA) == FALSE & NINN$COOP_PRV_OTH_EU_EFTA == 1", 
                "is.na(NINN$COOP_EG_EU_EFTA) == FALSE & NINN$COOP_EG_EU_EFTA == 1", 
                "is.na(NINN$COOP_UNIV_EU_EFTA) == FALSE & NINN$COOP_UNIV_EU_EFTA == 1", 
                "is.na(NINN$COOP_GOV_RI_EU_EFTA) == FALSE & NINN$COOP_GOV_RI_EU_EFTA == 1", 
                "is.na(NINN$COOP_PUB_CLCU_EU_EFTA) == FALSE & NINN$COOP_PUB_CLCU_EU_EFTA == 1", 
                "is.na(NINN$COOP_NPO_EU_EFTA) == FALSE & NINN$COOP_NPO_EU_EFTA == 1", 
                "is.na(NINN$COOP_PRV_CN_NEU_NEFTA) == FALSE & NINN$COOP_PRV_CN_NEU_NEFTA == 1", 
                "is.na(NINN$COOP_PRV_SUPPL_NEU_NEFTA) == FALSE & NINN$COOP_PRV_SUPPL_NEU_NEFTA == 1", 
                "is.na(NINN$COOP_PRV_CLCU_NEU_NEFTA) == FALSE & NINN$COOP_PRV_CLCU_NEU_NEFTA == 1", 
                "is.na(NINN$COOP_PRV_COMP_NEU_NEFTA) == FALSE & NINN$COOP_PRV_COMP_NEU_NEFTA == 1", 
                "is.na(NINN$COOP_PRV_OTH_NEU_NEFTA) == FALSE & NINN$COOP_PRV_OTH_NEU_NEFTA == 1", 
                "is.na(NINN$COOP_EG_NEU_NEFTA) == FALSE & NINN$COOP_EG_NEU_NEFTA == 1", 
                "is.na(NINN$COOP_UNIV_NEU_NEFTA) == FALSE & NINN$COOP_UNIV_NEU_NEFTA == 1", 
                "is.na(NINN$COOP_GOV_RI_NEU_NEFTA) == FALSE & NINN$COOP_GOV_RI_NEU_NEFTA == 1", 
                "is.na(NINN$COOP_PUB_CLCU_NEU_NEFTA) == FALSE & NINN$COOP_PUB_CLCU_NEU_NEFTA == 1", 
                "is.na(NINN$COOP_NPO_NEU_NEFTA) == FALSE & NINN$COOP_NPO_NEU_NEFTA == 1", 
                "NINN$M092 %in% COOP_PRV_NAT$M092",
                "is.na(NINN$COOP_NPRV_NAT) == FALSE & NINN$COOP_NPRV_NAT == 1", 
                "is.na(NINN$COOP_PRV_EU_EFTA) == FALSE & NINN$COOP_PRV_EU_EFTA == 1", 
                "is.na(NINN$COOP_NPRV_EU_EFTA) == FALSE & NINN$COOP_NPRV_EU_EFTA == 1", 
                "is.na(NINN$COOP_PRV_NEU_NEFTA) == FALSE & NINN$COOP_PRV_NEU_NEFTA == 1", 
                "is.na(NINN$COOP_NPRV_NEU_NEFTA) == FALSE & NINN$COOP_NPRV_NEU_NEFTA == 1",
                "NINN$M092 %in% COOP_NPRV$M092")

T18_SDMX <- "T18_SDMX_NINN.txt"
T18_SDMX_RESULT <- "T18_SDMX_NINN_RESULT.txt"

TYPE_ENT <- "NINN"

cat(paste("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS", sep = ";"), sep = "\n", file = T18_SDMX, append = FALSE)
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
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = T18_SDMX, append = TRUE)
      
    }
  }
  
}

TXTData <- file(description = T18_SDMX, open = "r")
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
    insert_Total(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  TXTData <- file(description = T18_SDMX, open = "r")
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
  write.table(result_Aggregate, T18_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)
  
  #Összetett értékek számítása több ACTIVITY kód alapján az összes vállalatcsoportra
  ACTIVITY <- "A"
  NUMBER_EMPL <- "_T"
  insert_A(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "B"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_B(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "C"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_C(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "C10T12"
  insert_C10T12(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "C13T15"
  insert_C13T15(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "C16T18"
  insert_C16T18(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  #ACTIVITY <- "C19_20"
  #insert_C19T20(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "C19T21"
  insert_C19T21(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  #ACTIVITY <- "C19T22"
  #insert_C19T22(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "C22_23"
  insert_C22T23(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "C24_25"
  insert_C24T25(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "C26T28"
  insert_C26T28(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "C29_30"
  insert_C29T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  #ACTIVITY <- "C25T30"
  #insert_C25T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "C31T33"
  insert_C31T33(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "D"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_D(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "E"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_E(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "E36_37"
  insert_E36T37(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "E38_39"
  insert_E38T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  #ACTIVITY <- "E37T39"
  #insert_E37T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "G"
  insert_G(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "G46"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "H"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_H(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "F"
  insert_F(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "H49T51"
  insert_H49T51(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "H52_53"
  insert_H52T53(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "I"
  insert_I(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "J"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_J(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "J58T60"
  insert_J58T60(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "J61T63"
  insert_J61T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  #ACTIVITY <- "J62_63"
  #insert_J62T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "K"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_K(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "M"
  insert_M(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "M71T73"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_M71T73(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "N"
  insert_N(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
  
  ACTIVITY <- "BTE"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_BTE(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "GTN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_GTN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "G46TM73_INN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46TM73_INN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
  ACTIVITY <- "_T"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert__T(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T18_SDMX)
    
  }
  
}

TXTData <- file(description = T18_SDMX, open = "r")
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
write.table(result_Aggregate, T18_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)


T18_SDMX <- "T18_SDMX_INN.txt"
T18_SDMX_RESULT <- "T18_SDMX_ALL_RESULT.txt"
T18_SDMX_FINAL_RESULT <- "T18_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T18_SDMX, open = "r")
line <- readLines(con = TXTData)
result_Aggregate <- data.frame()
result_T18 <- data.frame()

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

result_T18 <- rbind(result_T18, result_Aggregate)

T18_SDMX <- "T18_SDMX_NINN.txt"
TXTData <- file(description = T18_SDMX, open = "r")
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

result_T18 <- rbind(result_T18, result_Aggregate)

result_T18$OBS_VALUE <- as.character(result_T18$OBS_VALUE)
result_T18[, "OBS_VALUE"] <- gsub("\\.", ",", result_T18[, "OBS_VALUE"])
write.table(result_T18, T18_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)

result_T18[, "OBS_VALUE"] <- gsub(",", "\\.", result_T18[, "OBS_VALUE"])
result_T18$OBS_VALUE <- as.numeric(result_T18$OBS_VALUE)
result_T18_DT <- data.table(result_T18)
result_T18_DT <- result_T18_DT[, .(SUM_OBS_VALUE=sum(OBS_VALUE)), by = "DATAFLOW,FREQ,TIME_PERIOD,REF_AREA,TABLENAME,ACTIVITY,NUMBER_EMPL,INN_PF,INDICATOR,CIS_INDICATOR,UNIT_MEASURE,UNIT_MULT,DECIMALS"]
setnames(result_T18_DT, "SUM_OBS_VALUE", "OBS_VALUE")

result_T18_DT_FINAL <- cbind(result_T18_DT[, 1:7], stringsAsFactors = FALSE)
result_T18_DT_FINAL$TYPE_ENT <- "_T"
result_T18_DT_FINAL <- cbind(result_T18_DT_FINAL, result_T18_DT[, c(8:10, 14, 11:13)], stringsAsFactors = FALSE)
result_T18_DT_FINAL$OBS_STATUS <- ""
result_T18_DT_FINAL$OBS_STATUS_1 <- ""
result_T18_DT_FINAL$CONF_STATUS <- ""
result_T18_DT_FINAL$COMMENT_OBS <- ""
result_T18_DT_FINAL <- as.data.frame(result_T18_DT_FINAL)
result_T18_DT_FINAL$OBS_VALUE <- as.character(result_T18_DT_FINAL$OBS_VALUE)
result_T18_DT_FINAL[, "OBS_VALUE"] <- gsub("\\.", ",", result_T18_DT_FINAL[, "OBS_VALUE"])
result_T18$OBS_VALUE <- as.character(result_T18$OBS_VALUE)
result_T18[, "OBS_VALUE"] <- gsub("\\.", ",", result_T18[, "OBS_VALUE"])
result_T18$COMMENT_OBS <- ""

result_T18_DT_FINAL[result_T18_DT_FINAL$ACTIVITY == "A" | result_T18_DT_FINAL$ACTIVITY == "A01" | result_T18_DT_FINAL$ACTIVITY == "A02" | result_T18_DT_FINAL$ACTIVITY == "A03" | result_T18_DT_FINAL$ACTIVITY == "F" | result_T18_DT_FINAL$ACTIVITY == "G45" | result_T18_DT_FINAL$ACTIVITY == "G47" | result_T18_DT_FINAL$ACTIVITY == "I" | result_T18_DT_FINAL$ACTIVITY == "L" | result_T18_DT_FINAL$ACTIVITY == "M69" | result_T18_DT_FINAL$ACTIVITY == "M70" | result_T18_DT_FINAL$ACTIVITY == "M74" | result_T18_DT_FINAL$ACTIVITY == "M75" | result_T18_DT_FINAL$ACTIVITY == "N" | result_T18_DT_FINAL$ACTIVITY == "N77" | result_T18_DT_FINAL$ACTIVITY == "N78" | result_T18_DT_FINAL$ACTIVITY == "N79" | result_T18_DT_FINAL$ACTIVITY == "N80" | result_T18_DT_FINAL$ACTIVITY == "N81" | result_T18_DT_FINAL$ACTIVITY == "N82", "OBS_VALUE"] <- ""
result_T18[result_T18$ACTIVITY == "A" | result_T18$ACTIVITY == "A01" | result_T18$ACTIVITY == "A02" | result_T18$ACTIVITY == "A03" | result_T18$ACTIVITY == "F" | result_T18$ACTIVITY == "G45" | result_T18$ACTIVITY == "G47" | result_T18$ACTIVITY == "I" | result_T18$ACTIVITY == "L" | result_T18$ACTIVITY == "M69" | result_T18$ACTIVITY == "M70" | result_T18$ACTIVITY == "M74" | result_T18$ACTIVITY == "M75" | result_T18$ACTIVITY == "N" | result_T18$ACTIVITY == "N77" | result_T18$ACTIVITY == "N78" | result_T18$ACTIVITY == "N79" | result_T18$ACTIVITY == "N80" | result_T18$ACTIVITY == "N81" | result_T18$ACTIVITY == "N82", "OBS_VALUE"] <- ""
result_T18_DT_FINAL <- subset(result_T18_DT_FINAL, ACTIVITY != "GTN" | (ACTIVITY == "GTN" & (NUMBER_EMPL == "_T")))
result_T18 <- subset(result_T18, ACTIVITY != "GTN" | (ACTIVITY == "GTN" & (NUMBER_EMPL == "_T")))
result_T18_DT_FINAL <- subset(result_T18_DT_FINAL, ACTIVITY != "G" & ACTIVITY != "M")
result_T18 <- subset(result_T18, ACTIVITY != "G" & ACTIVITY != "M")
result_T18_DT_FINAL <- subset(result_T18_DT_FINAL, ((CIS_INDICATOR == "COOP_ALL" | CIS_INDICATOR == "COOP_OTH") & (TYPE_ENT == "_T" | TYPE_ENT == "INN" | TYPE_ENT == "NINN")) | ((CIS_INDICATOR != "COOP_ALL" & CIS_INDICATOR != "COOP_OTH") & TYPE_ENT == "INN"))
result_T18 <- subset(result_T18, ((CIS_INDICATOR == "COOP_ALL" | CIS_INDICATOR == "COOP_OTH") & (TYPE_ENT == "_T" | TYPE_ENT == "INN" | TYPE_ENT == "NINN")) | ((CIS_INDICATOR != "COOP_ALL" & CIS_INDICATOR != "COOP_OTH") & TYPE_ENT == "INN"))

write.table(rbind(result_T18_DT_FINAL, result_T18), T18_SDMX_FINAL_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)

set_ENT_Profile("as.numeric(COOP_ALL)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_EG)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_EG_EU_EFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_EG_NAT)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_EG_NEU_NEFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_EU_EFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_GOV_RI)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_GOV_RI_EU_EFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_GOV_RI_NAT)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_GOV_RI_NEU_NEFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_INN_XRND)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_NAT)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_NEU_NEFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_NNAT)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_NPO)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_NPO_EU_EFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_NPO_NAT)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_NPO_NEU_NEFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_NPRV)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_NPRV_EU_EFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_NPRV_NAT)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_NPRV_NEU_NEFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_OTH)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_CLCU)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_CLCU_EU_EFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_CLCU_NAT)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_CLCU_NEU_NEFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_CN)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_CN_EU_EFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_CN_NAT)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_CN_NEU_NEFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_COMP)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_COMP_EU_EFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_COMP_NAT)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_COMP_NEU_NEFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_EU_EFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_NAT)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_NEU_NEFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_OTH)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_OTH_EU_EFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_OTH_NAT)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_OTH_NEU_NEFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_SUPPL)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_SUPPL_EU_EFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_SUPPL_NAT)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PRV_SUPPL_NEU_NEFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PUB_CLCU)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PUB_CLCU_EU_EFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PUB_CLCU_NAT)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_PUB_CLCU_NEU_NEFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_RND)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_RNDINN)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_UNIV)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_UNIV_EU_EFTA)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_UNIV_NAT)", "T18", T18_SDMX_FINAL_RESULT)
set_ENT_Profile("as.numeric(COOP_UNIV_NEU_NEFTA)", "T18", T18_SDMX_FINAL_RESULT)