source("Functions_Standard.R")
library("rlang")
library("tidytable")

T34_SDMX_FINAL_RESULT <- "T34_SDMX_ALL_FINAL_RESULT.txt"

TABLENAME <- "T34"
REF_AREA <- "HU"
TYPE_ENT <- "INN"
INN_PF <- "_Z"
INDICATOR <- "ENT"

UNIT_MEASURE <- "PN"
UNIT_MULT <- "0"
DECIMALS <- "0"

expression <- c("is.na(INN$INNO_PRD_GD) == FALSE & INN$INNO_PRD_GD == 1",
                "is.na(INN$INNO_PRD_SERV) == FALSE & INN$INNO_PRD_SERV == 1",
                "INN$M092 %in% PRD_NEW_MKT$M092",
                "INN$M092 %in% PRD_NEW_ENT$M092",
                "is.na(INN$INNO_PCS_PRD) == FALSE & INN$INNO_PCS_PRD == 1",
                "is.na(INN$INNO_PCS_LOG) == FALSE & INN$INNO_PCS_LOG == 1",
                "is.na(INN$INNO_PCS_COMM) == FALSE & INN$INNO_PCS_COMM == 1",
                "is.na(INN$INNO_PCS_ACCT) == FALSE & INN$INNO_PCS_ACCT == 1",
                "is.na(INN$INNO_PCS_OPROC_EXTREL) == FALSE & INN$INNO_PCS_OPROC_EXTREL == 1",
                "is.na(INN$INNO_PCS_WR_DEC_HRM ) == FALSE & INN$INNO_PCS_WR_DEC_HRM  == 1",
                "is.na(INN$INNO_PCS_SLS_SERV) == FALSE & INN$INNO_PCS_SLS_SERV == 1",
                "is.na(INN$INNA_IH_RND) == FALSE & INN$INNA_IH_RND == 1",
                "is.na(INN$INNA_IH_RND_CONT) == FALSE & INN$INNA_IH_RND_CONT == 1",
                "is.na(INN$INNA_IH_RND_OCC) == FALSE & INN$INNA_IH_RND_OCC == 1",
                "is.na(INN$INNA_RND_CONTR_OUT) == FALSE & INN$INNA_RND_CONTR_OUT == 1",
                "is.na(INN$INNA_ONGO) == FALSE & INN$INNA_ONGO == 1",
                "is.na(INN$INNA_ABDN) == FALSE & INN$INNA_ABDN == 1",
                "is.na(INN$INNA_COMPL) == FALSE & INN$INNA_COMPL == 1",
                "INN$M092 %in% INNO_PRD$M092",
                "INN$M092 %in% PRDBPCS_COMPL$M092",
                "INN$M092 %in% PRD_NEW_ENT_ONL$M092",
                "INN$M092 %in% INNA_RND_OUT_N_IH$M092")

expression2 <- c("INNO_PRD_GD", 
                  "INNO_PRD_SERV", 
                  "PRD_NEW_MKT", 
                  "PRD_NEW_ENT", 
                  "INNO_PCS_PRD", 
                  "INNO_PCS_LOG", 
                  "INNO_PCS_COMM", 
                  "INNO_PCS_ACCT", 
                  "INNO_PCS_OPROC_EXTREL", 
                  "INNO_PCS_WR_DEC_HRM", 
                  "INNO_PCS_SLS_SERV", 
                  "INNA_IH_RND", 
                  "INNA_IH_RND_CONT", 
                  "INNA_IH_RND_OCC", 
                  "INNA_RND_CONTR_OUT", 
                  "INNA_ONGO", 
                  "INNA_ABDN", 
                  "INNA_COMPL", 
                  "INNO_PRD", 
                  "PRDBPCS_COMPL", 
                  "PRD_NEW_ENT_ONL", 
                  "INNA_RND_OUT_N_IH")

Aggregate <- data.table(matrix(ncol = 19, nrow = 0))
names(Aggregate) <- c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
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
      Aggregate <- rbind(Aggregate, list(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS), ignore.attr = TRUE)
      
    }
    
  }
}

NUMBER_EMPL <- "_T"
for(num in 1:length(expression)){
  
  CIS_INDICATOR <- expression2[num]
  
  for(j in 1:length(ACTIVITY_LIST)){
    
    ACTIVITY <- ACTIVITY_LIST[j]
    insert_Total(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
    
  }
  
  #Összetett értékek számítása több ACTIVITY kód alapján az összes vállalatcsoportra
  ACTIVITY <- "A"
  NUMBER_EMPL <- "_T"
  insert_A(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  ACTIVITY <- "B"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_B(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
    
  }
  
  ACTIVITY <- "C"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_C(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
    
  }
  
  ACTIVITY <- "C10T12"
  insert_C10T12(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  ACTIVITY <- "C13T15"
  insert_C13T15(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  ACTIVITY <- "C16T18"
  insert_C16T18(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  #ACTIVITY <- "C19_20"
  #insert_C19T20(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  ACTIVITY <- "C19T21"
  insert_C19T21(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  #ACTIVITY <- "C19T22"
  #insert_C19T22(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  ACTIVITY <- "C22_23"
  insert_C22T23(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  ACTIVITY <- "C24_25"
  insert_C24T25(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  ACTIVITY <- "C26T28"
  insert_C26T28(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  ACTIVITY <- "C29_30"
  insert_C29T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  #ACTIVITY <- "C25T30"
  #insert_C25T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  ACTIVITY <- "C31T33"
  insert_C31T33(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  ACTIVITY <- "D"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_D(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
    
  }
  
  ACTIVITY <- "E"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_E(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
    
  }
  
  ACTIVITY <- "E36_37"
  insert_E36T37(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  ACTIVITY <- "E38_39"
  insert_E38T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  #ACTIVITY <- "E37T39"
  #insert_E37T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  ACTIVITY <- "G"
  insert_G(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  ACTIVITY <- "G46"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    if (nrow(Aggregate %>% filter(ACTIVITY == "G46" & NUMBER_EMPL == NUMBER_EMPL_LIST[j] & CIS_INDICATOR == expression2[num])) != 0)
      next
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
    
  }
  
  ACTIVITY <- "H"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_H(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
    
  }
  
  ACTIVITY <- "F"
  insert_F(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  ACTIVITY <- "H49T51"
  insert_H49T51(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  ACTIVITY <- "H52_53"
  insert_H52T53(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  ACTIVITY <- "I"
  insert_I(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  ACTIVITY <- "J"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_J(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
    
  }
  
  ACTIVITY <- "J58T60"
  insert_J58T60(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  ACTIVITY <- "J61T63"
  insert_J61T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  #ACTIVITY <- "J62_63"
  #insert_J62T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  ACTIVITY <- "K"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_K(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
    
  }
  
  ACTIVITY <- "M"
  insert_M(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  ACTIVITY <- "M71T73"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_M71T73(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
    
  }
  
  ACTIVITY <- "N"
  insert_N(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
  
  ACTIVITY <- "BTE"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_BTE(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
    
  }
  
  ACTIVITY <- "GTN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_GTN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
    
  }
  
  ACTIVITY <- "G46TM73_INN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46TM73_INN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
    
  }
  
  ACTIVITY <- "_T"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert__T(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR)
    
  }
  
}

Aggregate$OBS_VALUE <- as.numeric(Aggregate$OBS_VALUE)
Aggregate_INN <- Aggregate

Aggregate_INN <- Aggregate_INN[(Aggregate_INN$ACTIVITY != "F41" & Aggregate_INN$ACTIVITY != "F42" & Aggregate_INN$ACTIVITY != "F43" & Aggregate_INN$ACTIVITY != "I55" & Aggregate_INN$ACTIVITY != "I56") & (Aggregate_INN$NUMBER_EMPL == "_T" | Aggregate_INN$ACTIVITY == "M71T73" | Aggregate_INN$ACTIVITY == "K" | Aggregate_INN$ACTIVITY == "J" | Aggregate_INN$ACTIVITY == "H" | Aggregate_INN$ACTIVITY == "G46" | Aggregate_INN$ACTIVITY == "A" | Aggregate_INN$ACTIVITY == "B" | Aggregate_INN$ACTIVITY == "C" | Aggregate_INN$ACTIVITY == "D" | Aggregate_INN$ACTIVITY == "E" | Aggregate_INN$ACTIVITY == "I" | Aggregate_INN$ACTIVITY == "BTE" | Aggregate_INN$ACTIVITY == "GTN" | Aggregate_INN$ACTIVITY == "G46TM73_INN" | Aggregate_INN$ACTIVITY == "_T"), ]

Aggregate_INN[Aggregate_INN$ACTIVITY == "A" | Aggregate_INN$ACTIVITY == "A01" | Aggregate_INN$ACTIVITY == "A02" | Aggregate_INN$ACTIVITY == "A03" | Aggregate_INN$ACTIVITY == "F" | Aggregate_INN$ACTIVITY == "G45" | Aggregate_INN$ACTIVITY == "G47" | Aggregate_INN$ACTIVITY == "I" | Aggregate_INN$ACTIVITY == "L" | Aggregate_INN$ACTIVITY == "M69" | Aggregate_INN$ACTIVITY == "M70" | Aggregate_INN$ACTIVITY == "M74" | Aggregate_INN$ACTIVITY == "M75" | Aggregate_INN$ACTIVITY == "N" | Aggregate_INN$ACTIVITY == "N77" | Aggregate_INN$ACTIVITY == "N78" | Aggregate_INN$ACTIVITY == "N79" | Aggregate_INN$ACTIVITY == "N80" | Aggregate_INN$ACTIVITY == "N81" | Aggregate_INN$ACTIVITY == "N82", "OBS_VALUE"] <- ""
Aggregate_INN <- subset(Aggregate_INN, ACTIVITY != "GTN" | (ACTIVITY == "GTN" & (NUMBER_EMPL == "_T")))
Aggregate_INN <- subset(Aggregate_INN, ACTIVITY != "G" & ACTIVITY != "M")

Aggregate_INN <- as.data.frame(Aggregate_INN)
Aggregate_INN$OBS_VALUE <- as.character(Aggregate_INN$OBS_VALUE)
Aggregate_INN[, "OBS_VALUE"] <- gsub("\\.", ",", Aggregate_INN[, "OBS_VALUE"])
Aggregate_INN$OBS_VALUE[is.na(Aggregate_INN$OBS_VALUE)] <- ""

write.table(Aggregate_INN, T34_SDMX_FINAL_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)



# T34_SDMX <- "T34_SDMX_INN.txt"
# T34_SDMX_RESULT <- "T34_SDMX_ALL_RESULT.txt"
# T34_SDMX_FINAL_RESULT <- "T34_SDMX_ALL_FINAL_RESULT.txt"
# TXTData <- file(description = T34_SDMX, open = "r")
# line <- readLines(con = TXTData)
# result_Aggregate <- data.frame()
# result_T34 <- data.frame()
# 
# for (i in 2:length(line)){
#   values <- strsplit(x = line[i], split = ";");
#   df <- data.frame(matrix(unlist(values), nrow = 1), stringsAsFactors = FALSE)
#   result_Aggregate <- rbind(result_Aggregate, df)
# }
# 
# names(result_Aggregate) = c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS")
# result_Aggregate$COMMENT_OBS <- ""
# result_Aggregate <- result_Aggregate[(result_Aggregate$ACTIVITY != "F41" & result_Aggregate$ACTIVITY != "F42" & result_Aggregate$ACTIVITY != "F43" & result_Aggregate$ACTIVITY != "I55" & result_Aggregate$ACTIVITY != "I56") & (result_Aggregate$NUMBER_EMPL == "_T" | result_Aggregate$ACTIVITY == "M71T73" | result_Aggregate$ACTIVITY == "K" | result_Aggregate$ACTIVITY == "J" | result_Aggregate$ACTIVITY == "H" | result_Aggregate$ACTIVITY == "G46" | result_Aggregate$ACTIVITY == "A" | result_Aggregate$ACTIVITY == "B" | result_Aggregate$ACTIVITY == "C" | result_Aggregate$ACTIVITY == "D" | result_Aggregate$ACTIVITY == "E" | result_Aggregate$ACTIVITY == "I" | result_Aggregate$ACTIVITY == "BTE" | result_Aggregate$ACTIVITY == "GTN" | result_Aggregate$ACTIVITY == "G46TM73_INN" | result_Aggregate$ACTIVITY == "_T"), ]
# result_Aggregate <- unique(result_Aggregate) #G46 miatt
# nrow(result_Aggregate)
# 
# result_T34 <- rbind(result_T34, result_Aggregate)
# 
# result_T34$OBS_VALUE <- as.character(result_T34$OBS_VALUE)
# result_T34[, "OBS_VALUE"] <- gsub("\\.", ",", result_T34[, "OBS_VALUE"])
# write.table(result_T34, T34_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)
# 
# result_T34$COMMENT_OBS <- ""
# 
# result_T34[result_T34$ACTIVITY == "A" | result_T34$ACTIVITY == "A01" | result_T34$ACTIVITY == "A02" | result_T34$ACTIVITY == "A03" | result_T34$ACTIVITY == "F" | result_T34$ACTIVITY == "G45" | result_T34$ACTIVITY == "G47" | result_T34$ACTIVITY == "I" | result_T34$ACTIVITY == "L" | result_T34$ACTIVITY == "M69" | result_T34$ACTIVITY == "M70" | result_T34$ACTIVITY == "M74" | result_T34$ACTIVITY == "M75" | result_T34$ACTIVITY == "N" | result_T34$ACTIVITY == "N77" | result_T34$ACTIVITY == "N78" | result_T34$ACTIVITY == "N79" | result_T34$ACTIVITY == "N80" | result_T34$ACTIVITY == "N81" | result_T34$ACTIVITY == "N82", "OBS_VALUE"] <- ""
# result_T34 <- subset(result_T34, ACTIVITY != "GTN" | (ACTIVITY == "GTN" & (NUMBER_EMPL == "_T")))
# result_T34 <- subset(result_T34, ACTIVITY != "G" & ACTIVITY != "M")
# 
# write.table(result_T34, T34_SDMX_FINAL_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)