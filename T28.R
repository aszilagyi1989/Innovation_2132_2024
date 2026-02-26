source("Functions_Standard.R")
source("Profile_ENT.R")
library("rlang")
library("tidytable")

T28_SDMX_FINAL_RESULT <- "T28_SDMX_ALL_FINAL_RESULT.txt"

TABLENAME <- "T28"
REF_AREA <- "HU"
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

TYPE_ENT <- "NINN"

Aggregate <- data.table(matrix(ncol = 19, nrow = 0))
names(Aggregate) <- c("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS")
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
Aggregate_NINN <- Aggregate

Aggregate_INN <- Aggregate_INN[(Aggregate_INN$ACTIVITY != "F41" & Aggregate_INN$ACTIVITY != "F42" & Aggregate_INN$ACTIVITY != "F43" & Aggregate_INN$ACTIVITY != "I55" & Aggregate_INN$ACTIVITY != "I56") & (Aggregate_INN$NUMBER_EMPL == "_T" | Aggregate_INN$ACTIVITY == "M71T73" | Aggregate_INN$ACTIVITY == "K" | Aggregate_INN$ACTIVITY == "J" | Aggregate_INN$ACTIVITY == "H" | Aggregate_INN$ACTIVITY == "G46" | Aggregate_INN$ACTIVITY == "A" | Aggregate_INN$ACTIVITY == "B" | Aggregate_INN$ACTIVITY == "C" | Aggregate_INN$ACTIVITY == "D" | Aggregate_INN$ACTIVITY == "E" | Aggregate_INN$ACTIVITY == "I" | Aggregate_INN$ACTIVITY == "BTE" | Aggregate_INN$ACTIVITY == "GTN" | Aggregate_INN$ACTIVITY == "G46TM73_INN" | Aggregate_INN$ACTIVITY == "_T"), ]
Aggregate_NINN <- Aggregate_NINN[(Aggregate_NINN$ACTIVITY != "F41" & Aggregate_NINN$ACTIVITY != "F42" & Aggregate_NINN$ACTIVITY != "F43" & Aggregate_NINN$ACTIVITY != "I55" & Aggregate_NINN$ACTIVITY != "I56") & (Aggregate_NINN$NUMBER_EMPL == "_T" | Aggregate_NINN$ACTIVITY == "M71T73" | Aggregate_NINN$ACTIVITY == "K" | Aggregate_NINN$ACTIVITY == "J" | Aggregate_NINN$ACTIVITY == "H" | Aggregate_NINN$ACTIVITY == "G46" | Aggregate_NINN$ACTIVITY == "A" | Aggregate_NINN$ACTIVITY == "B" | Aggregate_NINN$ACTIVITY == "C" | Aggregate_NINN$ACTIVITY == "D" | Aggregate_NINN$ACTIVITY == "E" | Aggregate_NINN$ACTIVITY == "I" | Aggregate_NINN$ACTIVITY == "BTE" | Aggregate_NINN$ACTIVITY == "GTN" | Aggregate_NINN$ACTIVITY == "G46TM73_INN" | Aggregate_NINN$ACTIVITY == "_T"), ]

result_T28_DT <- rbind(Aggregate_INN, Aggregate_NINN)
# result_T28_DT %>% group_by(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, INN_PF, INDICATOR, CIS_INDICATOR, UNIT_MEASURE, UNIT_MULT, DECIMALS) %>% summarise(OBS_VALUE = sum(OBS_VALUE)) -> result_T28_DT
result_T28_DT <- result_T28_DT[, .(SUM_OBS_VALUE=sum(OBS_VALUE)), by = "DATAFLOW,FREQ,TIME_PERIOD,REF_AREA,TABLENAME,ACTIVITY,NUMBER_EMPL,INN_PF,INDICATOR,CIS_INDICATOR,UNIT_MEASURE,UNIT_MULT,DECIMALS"]
colnames(result_T28_DT)[14] <- "OBS_VALUE"

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

result_T28_DT_FINAL[result_T28_DT_FINAL$ACTIVITY == "A" | result_T28_DT_FINAL$ACTIVITY == "A01" | result_T28_DT_FINAL$ACTIVITY == "A02" | result_T28_DT_FINAL$ACTIVITY == "A03" | result_T28_DT_FINAL$ACTIVITY == "F" | result_T28_DT_FINAL$ACTIVITY == "G45" | result_T28_DT_FINAL$ACTIVITY == "G47" | result_T28_DT_FINAL$ACTIVITY == "I" | result_T28_DT_FINAL$ACTIVITY == "L" | result_T28_DT_FINAL$ACTIVITY == "M69" | result_T28_DT_FINAL$ACTIVITY == "M70" | result_T28_DT_FINAL$ACTIVITY == "M74" | result_T28_DT_FINAL$ACTIVITY == "M75" | result_T28_DT_FINAL$ACTIVITY == "N" | result_T28_DT_FINAL$ACTIVITY == "N77" | result_T28_DT_FINAL$ACTIVITY == "N78" | result_T28_DT_FINAL$ACTIVITY == "N79" | result_T28_DT_FINAL$ACTIVITY == "N80" | result_T28_DT_FINAL$ACTIVITY == "N81" | result_T28_DT_FINAL$ACTIVITY == "N82", "OBS_VALUE"] <- ""
result_T28_DT_FINAL <- subset(result_T28_DT_FINAL, ACTIVITY != "GTN" | (ACTIVITY == "GTN" & (NUMBER_EMPL == "_T")))
result_T28_DT_FINAL <- subset(result_T28_DT_FINAL, ACTIVITY != "G" & ACTIVITY != "M")

result_T28_DT_FINAL <- subset(result_T28_DT_FINAL, TYPE_ENT == "_T")

Aggregate_INN[Aggregate_INN$ACTIVITY == "A" | Aggregate_INN$ACTIVITY == "A01" | Aggregate_INN$ACTIVITY == "A02" | Aggregate_INN$ACTIVITY == "A03" | Aggregate_INN$ACTIVITY == "F" | Aggregate_INN$ACTIVITY == "G45" | Aggregate_INN$ACTIVITY == "G47" | Aggregate_INN$ACTIVITY == "I" | Aggregate_INN$ACTIVITY == "L" | Aggregate_INN$ACTIVITY == "M69" | Aggregate_INN$ACTIVITY == "M70" | Aggregate_INN$ACTIVITY == "M74" | Aggregate_INN$ACTIVITY == "M75" | Aggregate_INN$ACTIVITY == "N" | Aggregate_INN$ACTIVITY == "N77" | Aggregate_INN$ACTIVITY == "N78" | Aggregate_INN$ACTIVITY == "N79" | Aggregate_INN$ACTIVITY == "N80" | Aggregate_INN$ACTIVITY == "N81" | Aggregate_INN$ACTIVITY == "N82", "OBS_VALUE"] <- ""
Aggregate_INN <- subset(Aggregate_INN, ACTIVITY != "GTN" | (ACTIVITY == "GTN" & (NUMBER_EMPL == "_T")))
Aggregate_INN <- subset(Aggregate_INN, ACTIVITY != "G" & ACTIVITY != "M")
Aggregate_NINN[Aggregate_NINN$ACTIVITY == "A" | Aggregate_NINN$ACTIVITY == "A01" | Aggregate_NINN$ACTIVITY == "A02" | Aggregate_NINN$ACTIVITY == "A03" | Aggregate_NINN$ACTIVITY == "F" | Aggregate_NINN$ACTIVITY == "G45" | Aggregate_NINN$ACTIVITY == "G47" | Aggregate_NINN$ACTIVITY == "I" | Aggregate_NINN$ACTIVITY == "L" | Aggregate_NINN$ACTIVITY == "M69" | Aggregate_NINN$ACTIVITY == "M70" | Aggregate_NINN$ACTIVITY == "M74" | Aggregate_NINN$ACTIVITY == "M75" | Aggregate_NINN$ACTIVITY == "N" | Aggregate_NINN$ACTIVITY == "N77" | Aggregate_NINN$ACTIVITY == "N78" | Aggregate_NINN$ACTIVITY == "N79" | Aggregate_NINN$ACTIVITY == "N80" | Aggregate_NINN$ACTIVITY == "N81" | Aggregate_NINN$ACTIVITY == "N82", "OBS_VALUE"] <- ""
Aggregate_NINN <- subset(Aggregate_NINN, ACTIVITY != "GTN" | (ACTIVITY == "GTN" & (NUMBER_EMPL == "_T")))
Aggregate_NINN <- subset(Aggregate_NINN, ACTIVITY != "G" & ACTIVITY != "M")

Aggregate_INN <- as.data.frame(Aggregate_INN)
Aggregate_INN$OBS_VALUE <- as.character(Aggregate_INN$OBS_VALUE)
Aggregate_INN[, "OBS_VALUE"] <- gsub("\\.", ",", Aggregate_INN[, "OBS_VALUE"])
Aggregate_INN$OBS_VALUE[is.na(Aggregate_INN$OBS_VALUE)] <- ""

Aggregate_NINN <- as.data.frame(Aggregate_NINN)
Aggregate_NINN$OBS_VALUE <- as.character(Aggregate_NINN$OBS_VALUE)
Aggregate_NINN[, "OBS_VALUE"] <- gsub("\\.", ",", Aggregate_NINN[, "OBS_VALUE"])
Aggregate_NINN$OBS_VALUE[is.na(Aggregate_NINN$OBS_VALUE)] <- ""

write.table(rbind(result_T28_DT_FINAL, Aggregate_INN, Aggregate_NINN), T28_SDMX_FINAL_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)

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