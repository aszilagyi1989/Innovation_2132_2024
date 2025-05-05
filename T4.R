source("Functions_Standard.R")
source("Profile_ENT.R")
library("data.table")
library("dplyr")

T4_SDMX <- "T4_SDMX_INN.txt"
T4_SDMX_RESULT <- "T4_SDMX_INN_RESULT.txt"

TABLENAME <- "T04"

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

cat(paste("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS", sep = ";"), sep = "\n", file = T4_SDMX, append = FALSE)
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
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = T4_SDMX, append = TRUE)
    
    }
    
  }
}

TXTData <- file(description = T4_SDMX, open = "r")
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
    insert_Total(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  TXTData <- file(description = T4_SDMX, open = "r")
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
  write.table(result_Aggregate, T4_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)
  
  #Összetett értékek számítása több ACTIVITY kód alapján az összes vállalatcsoportra
  ACTIVITY <- "A"
  NUMBER_EMPL <- "_T"
  insert_A(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "B"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_B(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "C"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_C(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "C10T12"
  insert_C10T12(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "C13T15"
  insert_C13T15(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "C16T18"
  insert_C16T18(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  #ACTIVITY <- "C19_20"
  #insert_C19T20(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "C19T21"
  insert_C19T21(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  #ACTIVITY <- "C19T22"
  #insert_C19T22(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "C22_23"
  insert_C22T23(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "C24_25"
  insert_C24T25(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "C26T28"
  insert_C26T28(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "C29_30"
  insert_C29T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  #ACTIVITY <- "C25T30"
  #insert_C25T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "C31T33"
  insert_C31T33(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "D"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_D(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "E"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_E(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "E36_37"
  insert_E36T37(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "E38_39"
  insert_E38T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  #ACTIVITY <- "E37T39"
  #insert_E37T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "G"
  insert_G(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "G46"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "H"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_H(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "F"
  insert_F(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "H49T51"
  insert_H49T51(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "H52_53"
  insert_H52T53(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "I"
  insert_I(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "J"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_J(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "J58T60"
  insert_J58T60(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "J61T63"
  insert_J61T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  #ACTIVITY <- "J62_63"
  #insert_J62T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "K"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_K(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "M"
  insert_M(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "M71T73"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_M71T73(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "N"
  insert_N(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "BTE"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_BTE(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "GTN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_GTN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "G46TM73_INN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46TM73_INN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "_T"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert__T(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
}

TXTData <- file(description = T4_SDMX, open = "r")
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
write.table(result_Aggregate, T4_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)

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

T4_SDMX <- "T4_SDMX_NINN.txt"
T4_SDMX_RESULT <- "T4_SDMX_NINN_RESULT.txt"

TYPE_ENT <- "NINN"

cat(paste("DATAFLOW", "FREQ", "TIME_PERIOD", "REF_AREA", "TABLENAME", "ACTIVITY", "NUMBER_EMPL", "TYPE_ENT", "INN_PF", "INDICATOR", "CIS_INDICATOR",	"OBS_VALUE", "UNIT_MEASURE", "UNIT_MULT", "DECIMALS",	"OBS_STATUS",	"OBS_STATUS_1",	"CONF_STATUS", "COMMENT_OBS", sep = ";"), sep = "\n", file = T4_SDMX, append = FALSE)
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
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = T4_SDMX, append = TRUE)
    
    }
  }
  
}

TXTData <- file(description = T4_SDMX, open = "r")
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
    insert_Total(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  TXTData <- file(description = T4_SDMX, open = "r")
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
  write.table(result_Aggregate, T4_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)
  
  #Összetett értékek számítása több ACTIVITY kód alapján az összes vállalatcsoportra
  ACTIVITY <- "A"
  NUMBER_EMPL <- "_T"
  insert_A(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "B"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_B(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "C"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_C(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "C10T12"
  insert_C10T12(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "C13T15"
  insert_C13T15(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "C16T18"
  insert_C16T18(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  #ACTIVITY <- "C19_20"
  #insert_C19T20(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "C19T21"
  insert_C19T21(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  #ACTIVITY <- "C19T22"
  #insert_C19T22(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "C22_23"
  insert_C22T23(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "C24_25"
  insert_C24T25(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "C26T28"
  insert_C26T28(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "C29_30"
  insert_C29T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  #ACTIVITY <- "C25T30"
  #insert_C25T30(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "C31T33"
  insert_C31T33(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "D"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_D(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "E"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_E(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "E36_37"
  insert_E36T37(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "E38_39"
  insert_E38T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  #ACTIVITY <- "E37T39"
  #insert_E37T39(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "G"
  insert_G(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "G46"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "H"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_H(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "F"
  insert_F(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "H49T51"
  insert_H49T51(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "H52_53"
  insert_H52T53(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "I"
  insert_I(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "J"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_J(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "J58T60"
  insert_J58T60(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "J61T63"
  insert_J61T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  #ACTIVITY <- "J62_63"
  #insert_J62T63(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "K"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_K(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "M"
  insert_M(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "M71T73"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_M71T73(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "N"
  insert_N(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
  
  ACTIVITY <- "BTE"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_BTE(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "GTN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_GTN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "G46TM73_INN"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert_G46TM73_INN(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
  ACTIVITY <- "_T"
  for(j in 1:length(NUMBER_EMPL_LIST)){
    
    NUMBER_EMPL <- NUMBER_EMPL_LIST[j]
    insert__T(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, T4_SDMX)
    
  }
  
}

TXTData <- file(description = T4_SDMX, open = "r")
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
write.table(result_Aggregate, T4_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)


T4_SDMX <- "T4_SDMX_INN.txt"
T4_SDMX_RESULT <- "T4_SDMX_ALL_RESULT.txt"
T4_SDMX_FINAL_RESULT <- "T4_SDMX_ALL_FINAL_RESULT.txt"
TXTData <- file(description = T4_SDMX, open = "r")
line <- readLines(con = TXTData)
result_Aggregate <- data.frame()
result_T4 <- data.frame()

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

result_T4 <- rbind(result_T4, result_Aggregate)

T4_SDMX <- "T4_SDMX_NINN.txt"
TXTData <- file(description = T4_SDMX, open = "r")
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

result_T4 <- rbind(result_T4, result_Aggregate)


result_T4$OBS_VALUE <- as.character(result_T4$OBS_VALUE)
result_T4[, "OBS_VALUE"] <- gsub("\\.", ",", result_T4[, "OBS_VALUE"])
write.table(result_T4, T4_SDMX_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)

result_T4[, "OBS_VALUE"] <- gsub(",", "\\.", result_T4[, "OBS_VALUE"])
result_T4$OBS_VALUE <- as.numeric(result_T4$OBS_VALUE)
result_T4_DT <- data.table(result_T4)
result_T4_DT <- result_T4_DT[, .(SUM_OBS_VALUE=sum(OBS_VALUE)), by = "DATAFLOW,FREQ,TIME_PERIOD,REF_AREA,TABLENAME,ACTIVITY,NUMBER_EMPL,INN_PF,INDICATOR,CIS_INDICATOR,UNIT_MEASURE,UNIT_MULT,DECIMALS"]
setnames(result_T4_DT, "SUM_OBS_VALUE", "OBS_VALUE")

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
result_T4$OBS_VALUE <- as.character(result_T4$OBS_VALUE)
result_T4[, "OBS_VALUE"] <- gsub("\\.", ",", result_T4[, "OBS_VALUE"])
result_T4$COMMENT_OBS <- ""

result_T4_DT_FINAL[result_T4_DT_FINAL$ACTIVITY == "A" | result_T4_DT_FINAL$ACTIVITY == "A01" | result_T4_DT_FINAL$ACTIVITY == "A02" | result_T4_DT_FINAL$ACTIVITY == "A03" | result_T4_DT_FINAL$ACTIVITY == "F" | result_T4_DT_FINAL$ACTIVITY == "G45" | result_T4_DT_FINAL$ACTIVITY == "G47" | result_T4_DT_FINAL$ACTIVITY == "I" | result_T4_DT_FINAL$ACTIVITY == "L" | result_T4_DT_FINAL$ACTIVITY == "M69" | result_T4_DT_FINAL$ACTIVITY == "M70" | result_T4_DT_FINAL$ACTIVITY == "M74" | result_T4_DT_FINAL$ACTIVITY == "M75" | result_T4_DT_FINAL$ACTIVITY == "N" | result_T4_DT_FINAL$ACTIVITY == "N77" | result_T4_DT_FINAL$ACTIVITY == "N78" | result_T4_DT_FINAL$ACTIVITY == "N79" | result_T4_DT_FINAL$ACTIVITY == "N80" | result_T4_DT_FINAL$ACTIVITY == "N81" | result_T4_DT_FINAL$ACTIVITY == "N82", "OBS_VALUE"] <- ""
result_T4[result_T4$ACTIVITY == "A" | result_T4$ACTIVITY == "A01" | result_T4$ACTIVITY == "A02" | result_T4$ACTIVITY == "A03" | result_T4$ACTIVITY == "F" | result_T4$ACTIVITY == "G45" | result_T4$ACTIVITY == "G47" | result_T4$ACTIVITY == "I" | result_T4$ACTIVITY == "L" | result_T4$ACTIVITY == "M69" | result_T4$ACTIVITY == "M70" | result_T4$ACTIVITY == "M74" | result_T4$ACTIVITY == "M75" | result_T4$ACTIVITY == "N" | result_T4$ACTIVITY == "N77" | result_T4$ACTIVITY == "N78" | result_T4$ACTIVITY == "N79" | result_T4$ACTIVITY == "N80" | result_T4$ACTIVITY == "N81" | result_T4$ACTIVITY == "N82", "OBS_VALUE"] <- ""
result_T4_DT_FINAL <- subset(result_T4_DT_FINAL, ACTIVITY != "GTN" | (ACTIVITY == "GTN" & (NUMBER_EMPL == "_T")))
result_T4 <- subset(result_T4, ACTIVITY != "GTN" | (ACTIVITY == "GTN" & (NUMBER_EMPL == "_T")))
result_T4_DT_FINAL <- subset(result_T4_DT_FINAL, ACTIVITY != "G" & ACTIVITY != "M")
result_T4 <- subset(result_T4, ACTIVITY != "G" & ACTIVITY != "M")

write.table(rbind(result_T4_DT_FINAL, result_T4), T4_SDMX_FINAL_RESULT, sep = ";", quote = FALSE, row.names = FALSE, append = FALSE)

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