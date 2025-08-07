source("Functions_Standard.R")
source("Profile_ENT.R")
library("rlang")
library("tidytable")

T4_SDMX_FINAL_RESULT <- "T4_SDMX_ALL_FINAL_RESULT.txt"

TABLENAME <- "T04"
REF_AREA <- "HU"
TYPE_ENT <- "INN"
INN_PF <- "_Z"
INDICATOR <- "ENT"

UNIT_MEASURE <- "PN"
UNIT_MULT <- "0"
DECIMALS <- "0"

expression <- c("is.na(INN$STRA_FIMPGS) == FALSE & INN$STRA_FIMPGS == 3", 
                "is.na(INN$STRA_FNWGS) == FALSE & INN$STRA_FNWGS == 3", 
                "is.na(INN$STRA_FLOWP) == FALSE & INN$STRA_FLOWP == 3", 
                "is.na(INN$STRA_FHQUAL) == FALSE & INN$STRA_FHQUAL == 3", 
                "is.na(INN$STRA_FBRGS) == FALSE & INN$STRA_FBRGS == 3", 
                "is.na(INN$STRA_FSMGS) == FALSE & INN$STRA_FSMGS == 3", 
                "is.na(INN$STRA_FESTCUS) == FALSE & INN$STRA_FESTCUS == 3", 
                "is.na(INN$STRA_FNWCUS) == FALSE & INN$STRA_FNWCUS == 3", 
                "is.na(INN$STRA_FSTDGS) == FALSE & INN$STRA_FSTDGS == 3", 
                "is.na(INN$STRA_FCSOL) == FALSE & INN$STRA_FCSOL == 3", 
                "is.na(INN$STRA_FIMPGS) == FALSE & INN$STRA_FIMPGS == 2", 
                "is.na(INN$STRA_FNWGS) == FALSE & INN$STRA_FNWGS == 2", 
                "is.na(INN$STRA_FLOWP) == FALSE & INN$STRA_FLOWP == 2", 
                "is.na(INN$STRA_FHQUAL) == FALSE & INN$STRA_FHQUAL == 2", 
                "is.na(INN$STRA_FBRGS) == FALSE & INN$STRA_FBRGS == 2", 
                "is.na(INN$STRA_FSMGS) == FALSE & INN$STRA_FSMGS == 2", 
                "is.na(INN$STRA_FESTCUS) == FALSE & INN$STRA_FESTCUS == 2", 
                "is.na(INN$STRA_FNWCUS) == FALSE & INN$STRA_FNWCUS == 2", 
                "is.na(INN$STRA_FSTDGS) == FALSE & INN$STRA_FSTDGS == 2", 
                "is.na(INN$STRA_FCSOL) == FALSE & INN$STRA_FCSOL == 2", 
                "is.na(INN$STRA_FIMPGS) == FALSE & INN$STRA_FIMPGS == 1", 
                "is.na(INN$STRA_FNWGS) == FALSE & INN$STRA_FNWGS == 1", 
                "is.na(INN$STRA_FLOWP) == FALSE & INN$STRA_FLOWP == 1", 
                "is.na(INN$STRA_FHQUAL) == FALSE & INN$STRA_FHQUAL == 1", 
                "is.na(INN$STRA_FBRGS) == FALSE & INN$STRA_FBRGS == 1", 
                "is.na(INN$STRA_FSMGS) == FALSE & INN$STRA_FSMGS == 1", 
                "is.na(INN$STRA_FESTCUS) == FALSE & INN$STRA_FESTCUS == 1", 
                "is.na(INN$STRA_FNWCUS) == FALSE & INN$STRA_FNWCUS == 1", 
                "is.na(INN$STRA_FSTDGS) == FALSE & INN$STRA_FSTDGS == 1", 
                "is.na(INN$STRA_FCSOL) == FALSE & INN$STRA_FCSOL == 1", 
                "is.na(INN$STRA_FIMPGS) == FALSE & INN$STRA_FIMPGS == 0", 
                "is.na(INN$STRA_FNWGS) == FALSE & INN$STRA_FNWGS == 0", 
                "is.na(INN$STRA_FLOWP) == FALSE & INN$STRA_FLOWP == 0", 
                "is.na(INN$STRA_FHQUAL) == FALSE & INN$STRA_FHQUAL == 0", 
                "is.na(INN$STRA_FBRGS) == FALSE & INN$STRA_FBRGS == 0", 
                "is.na(INN$STRA_FSMGS) == FALSE & INN$STRA_FSMGS == 0", 
                "is.na(INN$STRA_FESTCUS) == FALSE & INN$STRA_FESTCUS == 0", 
                "is.na(INN$STRA_FNWCUS) == FALSE & INN$STRA_FNWCUS == 0", 
                "is.na(INN$STRA_FSTDGS) == FALSE & INN$STRA_FSTDGS == 0", 
                "is.na(INN$STRA_FCSOL) == FALSE & INN$STRA_FCSOL == 0")

expression2 <- c("STRA_FIMPGS_3", 
                 "STRA_FNWGS_3", 
                 "STRA_FLOWP_3", 
                 "STRA_FHQUAL_3", 
                 "STRA_FBRGS_3", 
                 "STRA_FSMGS_3", 
                 "STRA_FESTCUS_3", 
                 "STRA_FNWCUS_3", 
                 "STRA_FSTDGS_3", 
                 "STRA_FCSOL_3", 
                 "STRA_FIMPGS_2", 
                 "STRA_FNWGS_2", 
                 "STRA_FLOWP_2", 
                 "STRA_FHQUAL_2", 
                 "STRA_FBRGS_2", 
                 "STRA_FSMGS_2", 
                 "STRA_FESTCUS_2", 
                 "STRA_FNWCUS_2", 
                 "STRA_FSTDGS_2", 
                 "STRA_FCSOL_2", 
                 "STRA_FIMPGS_1", 
                 "STRA_FNWGS_1", 
                 "STRA_FLOWP_1", 
                 "STRA_FHQUAL_1", 
                 "STRA_FBRGS_1", 
                 "STRA_FSMGS_1", 
                 "STRA_FESTCUS_1", 
                 "STRA_FNWCUS_1", 
                 "STRA_FSTDGS_1", 
                 "STRA_FCSOL_1", 
                 "STRA_FIMPGS_0", 
                 "STRA_FNWGS_0", 
                 "STRA_FLOWP_0", 
                 "STRA_FHQUAL_0", 
                 "STRA_FBRGS_0", 
                 "STRA_FSMGS_0", 
                 "STRA_FESTCUS_0", 
                 "STRA_FNWCUS_0", 
                 "STRA_FSTDGS_0", 
                 "STRA_FCSOL_0")

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

expression <- c("is.na(NINN$STRA_FIMPGS) == FALSE & NINN$STRA_FIMPGS == 3", 
                "is.na(NINN$STRA_FNWGS) == FALSE & NINN$STRA_FNWGS == 3", 
                "is.na(NINN$STRA_FLOWP) == FALSE & NINN$STRA_FLOWP == 3", 
                "is.na(NINN$STRA_FHQUAL) == FALSE & NINN$STRA_FHQUAL == 3", 
                "is.na(NINN$STRA_FBRGS) == FALSE & NINN$STRA_FBRGS == 3", 
                "is.na(NINN$STRA_FSMGS) == FALSE & NINN$STRA_FSMGS == 3", 
                "is.na(NINN$STRA_FESTCUS) == FALSE & NINN$STRA_FESTCUS == 3", 
                "is.na(NINN$STRA_FNWCUS) == FALSE & NINN$STRA_FNWCUS == 3", 
                "is.na(NINN$STRA_FSTDGS) == FALSE & NINN$STRA_FSTDGS == 3", 
                "is.na(NINN$STRA_FCSOL) == FALSE & NINN$STRA_FCSOL == 3", 
                "is.na(NINN$STRA_FIMPGS) == FALSE & NINN$STRA_FIMPGS == 2", 
                "is.na(NINN$STRA_FNWGS) == FALSE & NINN$STRA_FNWGS == 2", 
                "is.na(NINN$STRA_FLOWP) == FALSE & NINN$STRA_FLOWP == 2", 
                "is.na(NINN$STRA_FHQUAL) == FALSE & NINN$STRA_FHQUAL == 2", 
                "is.na(NINN$STRA_FBRGS) == FALSE & NINN$STRA_FBRGS == 2", 
                "is.na(NINN$STRA_FSMGS) == FALSE & NINN$STRA_FSMGS == 2", 
                "is.na(NINN$STRA_FESTCUS) == FALSE & NINN$STRA_FESTCUS == 2", 
                "is.na(NINN$STRA_FNWCUS) == FALSE & NINN$STRA_FNWCUS == 2", 
                "is.na(NINN$STRA_FSTDGS) == FALSE & NINN$STRA_FSTDGS == 2", 
                "is.na(NINN$STRA_FCSOL) == FALSE & NINN$STRA_FCSOL == 2", 
                "is.na(NINN$STRA_FIMPGS) == FALSE & NINN$STRA_FIMPGS == 1", 
                "is.na(NINN$STRA_FNWGS) == FALSE & NINN$STRA_FNWGS == 1", 
                "is.na(NINN$STRA_FLOWP) == FALSE & NINN$STRA_FLOWP == 1", 
                "is.na(NINN$STRA_FHQUAL) == FALSE & NINN$STRA_FHQUAL == 1", 
                "is.na(NINN$STRA_FBRGS) == FALSE & NINN$STRA_FBRGS == 1", 
                "is.na(NINN$STRA_FSMGS) == FALSE & NINN$STRA_FSMGS == 1", 
                "is.na(NINN$STRA_FESTCUS) == FALSE & NINN$STRA_FESTCUS == 1", 
                "is.na(NINN$STRA_FNWCUS) == FALSE & NINN$STRA_FNWCUS == 1", 
                "is.na(NINN$STRA_FSTDGS) == FALSE & NINN$STRA_FSTDGS == 1", 
                "is.na(NINN$STRA_FCSOL) == FALSE & NINN$STRA_FCSOL == 1", 
                "is.na(NINN$STRA_FIMPGS) == FALSE & NINN$STRA_FIMPGS == 0", 
                "is.na(NINN$STRA_FNWGS) == FALSE & NINN$STRA_FNWGS == 0", 
                "is.na(NINN$STRA_FLOWP) == FALSE & NINN$STRA_FLOWP == 0", 
                "is.na(NINN$STRA_FHQUAL) == FALSE & NINN$STRA_FHQUAL == 0", 
                "is.na(NINN$STRA_FBRGS) == FALSE & NINN$STRA_FBRGS == 0", 
                "is.na(NINN$STRA_FSMGS) == FALSE & NINN$STRA_FSMGS == 0", 
                "is.na(NINN$STRA_FESTCUS) == FALSE & NINN$STRA_FESTCUS == 0", 
                "is.na(NINN$STRA_FNWCUS) == FALSE & NINN$STRA_FNWCUS == 0", 
                "is.na(NINN$STRA_FSTDGS) == FALSE & NINN$STRA_FSTDGS == 0", 
                "is.na(NINN$STRA_FCSOL) == FALSE & NINN$STRA_FCSOL == 0")

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

result_T4_DT <- rbind(Aggregate_INN, Aggregate_NINN)
# result_T4_DT %>% group_by(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, INN_PF, INDICATOR, CIS_INDICATOR, UNIT_MEASURE, UNIT_MULT, DECIMALS) %>% summarise(OBS_VALUE = sum(OBS_VALUE)) -> result_T4_DT
result_T4_DT <- result_T4_DT[, .(SUM_OBS_VALUE=sum(OBS_VALUE)), by = "DATAFLOW,FREQ,TIME_PERIOD,REF_AREA,TABLENAME,ACTIVITY,NUMBER_EMPL,INN_PF,INDICATOR,CIS_INDICATOR,UNIT_MEASURE,UNIT_MULT,DECIMALS"]
colnames(result_T4_DT)[14] <- "OBS_VALUE"

result_T4_DT_FINAL <- cbind(result_T4_DT[, 1:7], stringsAsFactors = FALSE)
result_T4_DT_FINAL$TYPE_ENT <- "_T"
result_T4_DT_FINAL <- cbind(result_T4_DT_FINAL, result_T4_DT[, c(8:10, 14, 11:13)], stringsAsFactors = FALSE)
result_T4_DT_FINAL$OBS_STATUS <- ""
result_T4_DT_FINAL$OBS_STATUS_1 <- ""
result_T4_DT_FINAL$CONF_STATUS <- ""
result_T4_DT_FINAL$COMMENT_OBS <- ""
result_T4_DT_FINAL <- as.data.frame(result_T4_DT_FINAL)
result_T4_DT_FINAL$OBS_VALUE <- as.character(result_T4_DT_FINAL$OBS_VALUE)
result_T4_DT_FINAL[, "OBS_VALUE"] <- gsub("\\.", ",", result_T4_DT_FINAL[, "OBS_VALUE"])

result_T4_DT_FINAL[result_T4_DT_FINAL$ACTIVITY == "A" | result_T4_DT_FINAL$ACTIVITY == "A01" | result_T4_DT_FINAL$ACTIVITY == "A02" | result_T4_DT_FINAL$ACTIVITY == "A03" | result_T4_DT_FINAL$ACTIVITY == "F" | result_T4_DT_FINAL$ACTIVITY == "G45" | result_T4_DT_FINAL$ACTIVITY == "G47" | result_T4_DT_FINAL$ACTIVITY == "I" | result_T4_DT_FINAL$ACTIVITY == "L" | result_T4_DT_FINAL$ACTIVITY == "M69" | result_T4_DT_FINAL$ACTIVITY == "M70" | result_T4_DT_FINAL$ACTIVITY == "M74" | result_T4_DT_FINAL$ACTIVITY == "M75" | result_T4_DT_FINAL$ACTIVITY == "N" | result_T4_DT_FINAL$ACTIVITY == "N77" | result_T4_DT_FINAL$ACTIVITY == "N78" | result_T4_DT_FINAL$ACTIVITY == "N79" | result_T4_DT_FINAL$ACTIVITY == "N80" | result_T4_DT_FINAL$ACTIVITY == "N81" | result_T4_DT_FINAL$ACTIVITY == "N82", "OBS_VALUE"] <- ""
result_T4_DT_FINAL <- subset(result_T4_DT_FINAL, ACTIVITY != "GTN" | (ACTIVITY == "GTN" & (NUMBER_EMPL == "_T")))
result_T4_DT_FINAL <- subset(result_T4_DT_FINAL, ACTIVITY != "G" & ACTIVITY != "M")

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

write.table(rbind(result_T4_DT_FINAL, Aggregate_INN, Aggregate_NINN), T4_SDMX_FINAL_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)

set_ENT_Profile("STRA_FBRGS_0", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FBRGS_1", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FBRGS_2", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FBRGS_3", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FCSOL_0", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FCSOL_1", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FCSOL_2", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FCSOL_3", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FESTCUS_0", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FESTCUS_1", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FESTCUS_2", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FESTCUS_3", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FHQUAL_0", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FHQUAL_1", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FHQUAL_2", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FHQUAL_3", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FIMPGS_0", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FIMPGS_1", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FIMPGS_2", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FIMPGS_3", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FLOWP_0", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FLOWP_1", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FLOWP_2", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FLOWP_3", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FNWCUS_0", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FNWCUS_1", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FNWCUS_2", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FNWCUS_3", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FNWGS_0", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FNWGS_1", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FNWGS_2", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FNWGS_3", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FSMGS_0", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FSMGS_1", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FSMGS_2", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FSMGS_3", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FSTDGS_0", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FSTDGS_1", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FSTDGS_2", "T04", T4_SDMX_FINAL_RESULT)
set_ENT_Profile("STRA_FSTDGS_3", "T04", T4_SDMX_FINAL_RESULT)