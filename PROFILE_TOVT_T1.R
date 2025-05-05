set_TOVT_Profile <- function(column_name, tablename_Profile, save_filename){
  
  PROFILE_I_A <<- PROFILE_I_A %>% mutate(SULYOZOTT_TOVT = eval(parse(text = column_name)) * VGMA001_SULY) 
  PROFILE_I_B <<- PROFILE_I_B %>% mutate(SULYOZOTT_TOVT = eval(parse(text = column_name)) * VGMA001_SULY)
  PROFILE_II_A <<- PROFILE_II_A %>% mutate(SULYOZOTT_TOVT = eval(parse(text = column_name)) * VGMA001_SULY) 
  PROFILE_II_B <<- PROFILE_II_B %>% mutate(SULYOZOTT_TOVT = eval(parse(text = column_name)) * VGMA001_SULY)
  PROFILE_III_A <<- PROFILE_III_A %>% mutate(SULYOZOTT_TOVT = eval(parse(text = column_name)) * VGMA001_SULY) 
  PROFILE_III_B <<- PROFILE_III_B %>% mutate(SULYOZOTT_TOVT = eval(parse(text = column_name)) * VGMA001_SULY)
  PROFILE_IV_A <<- PROFILE_IV_A %>% mutate(SULYOZOTT_TOVT = eval(parse(text = column_name)) * VGMA001_SULY) 
  PROFILE_IV_B <<- PROFILE_IV_B %>% mutate(SULYOZOTT_TOVT = eval(parse(text = column_name)) * VGMA001_SULY)
  PROFILE_V_A <<- PROFILE_V_A %>% mutate(SULYOZOTT_TOVT = eval(parse(text = column_name)) * VGMA001_SULY) 
  PROFILE_V_B <<- PROFILE_V_B %>% mutate(SULYOZOTT_TOVT = eval(parse(text = column_name)) * VGMA001_SULY)
  PROFILE_VI <<- PROFILE_VI %>% mutate(SULYOZOTT_TOVT = eval(parse(text = column_name)) * VGMA001_SULY) 
  PROFILE_VII <<- PROFILE_VII %>% mutate(SULYOZOTT_TOVT = eval(parse(text = column_name)) * VGMA001_SULY)
  
  for(col in 1:ncol(Profile_TOVT)){
    
    for(row in 1:length(filter_M065_RETEG1)){
      
      if (col == 1){
        
        if (row != 4){
          
          Profile_TOVT[row, col] <<- sum(subset(PROFILE_I_A, eval(parse(text = filter_M065_RETEG1[row])), SULYOZOTT_TOVT), na.rm = TRUE)
          
        }else{
          
          Profile_TOVT[row, col] <<- sum(PROFILE_I_A[, "SULYOZOTT_TOVT"], na.rm = TRUE)
          
        }
        
      }
      
      if (col == 2){
        
        if (row != 4){
          
          Profile_TOVT[row, col] <<- sum(subset(PROFILE_I_B, eval(parse(text = filter_M065_RETEG1[row])), SULYOZOTT_TOVT), na.rm = TRUE)
          
        }else{
          
          Profile_TOVT[row, col] <<- sum(PROFILE_I_B[, "SULYOZOTT_TOVT"], na.rm = TRUE)
          
        }
        
      }
      
      if (col == 3){
        
        if (row != 4){
          
          Profile_TOVT[row, col] <<- sum(subset(PROFILE_II_A, eval(parse(text = filter_M065_RETEG1[row])), SULYOZOTT_TOVT), na.rm = TRUE)
          
        }else{
          
          Profile_TOVT[row, col] <<- sum(PROFILE_II_A[, "SULYOZOTT_TOVT"], na.rm = TRUE)
          
        }
        
      }
      
      if (col == 4){
        
        if (row != 4){
          
          Profile_TOVT[row, col] <<- sum(subset(PROFILE_II_B, eval(parse(text = filter_M065_RETEG1[row])), SULYOZOTT_TOVT), na.rm = TRUE)
          
        }else{
          
          Profile_TOVT[row, col] <<- sum(PROFILE_II_B[, "SULYOZOTT_TOVT"], na.rm = TRUE)
          
        }
        
      }
      
      if (col == 5){
        
        if (row != 4){
          
          Profile_TOVT[row, col] <<- sum(subset(PROFILE_III_A, eval(parse(text = filter_M065_RETEG1[row])), SULYOZOTT_TOVT), na.rm = TRUE)
          
        }else{
          
          Profile_TOVT[row, col] <<- sum(PROFILE_III_A[, "SULYOZOTT_TOVT"], na.rm = TRUE)
          
        }
        
      }
      
      if (col == 6){
        
        if (row != 4){
          
          Profile_TOVT[row, col] <<- sum(subset(PROFILE_III_B, eval(parse(text = filter_M065_RETEG1[row])), SULYOZOTT_TOVT), na.rm = TRUE)
          
        }else{
          
          Profile_TOVT[row, col] <<- sum(PROFILE_III_B[, "SULYOZOTT_TOVT"], na.rm = TRUE)
          
        }
        
      }
      
      if (col == 7){
        
        if (row != 4){
          
          Profile_TOVT[row, col] <<- sum(subset(PROFILE_IV_A, eval(parse(text = filter_M065_RETEG1[row])), SULYOZOTT_TOVT), na.rm = TRUE)
          
        }else{
          
          Profile_TOVT[row, col] <<- sum(PROFILE_IV_A[, "SULYOZOTT_TOVT"], na.rm = TRUE)
          
        }
        
      }
      
      if (col == 8){
        
        if (row != 4){
          
          Profile_TOVT[row, col] <<- sum(subset(PROFILE_IV_B, eval(parse(text = filter_M065_RETEG1[row])), SULYOZOTT_TOVT), na.rm = TRUE)
          
        }else{
          
          Profile_TOVT[row, col] <<- sum(PROFILE_IV_B[, "SULYOZOTT_TOVT"], na.rm = TRUE)
          
        }
        
      }
      
      if (col == 9){
        
        if (row != 4){
          
          Profile_TOVT[row, col] <<- sum(subset(PROFILE_V_A, eval(parse(text = filter_M065_RETEG1[row])), SULYOZOTT_TOVT), na.rm = TRUE)
          
        }else{
          
          Profile_TOVT[row, col] <<- sum(PROFILE_V_A[, "SULYOZOTT_TOVT"], na.rm = TRUE)
          
        }
        
      }
      
      if (col == 10){
        
        if (row != 4){
          
          Profile_TOVT[row, col] <<- sum(subset(PROFILE_V_B, eval(parse(text = filter_M065_RETEG1[row])), SULYOZOTT_TOVT), na.rm = TRUE)
          
        }else{
          
          Profile_TOVT[row, col] <<- sum(PROFILE_V_B[, "SULYOZOTT_TOVT"], na.rm = TRUE)
          
        }
        
      }
      
      if (col == 11){
        
        if (row != 4){
          
          Profile_TOVT[row, col] <<- sum(subset(PROFILE_VI, eval(parse(text = filter_M065_RETEG1[row])), SULYOZOTT_TOVT), na.rm = TRUE)
          
        }else{
          
          Profile_TOVT[row, col] <<- sum(PROFILE_VI[, "SULYOZOTT_TOVT"], na.rm = TRUE)
          
        }
        
      }
      
      if (col == 12){
        
        if (row != 4){
          
          Profile_TOVT[row, col] <<- sum(subset(PROFILE_VII, eval(parse(text = filter_M065_RETEG1[row])), SULYOZOTT_TOVT), na.rm = TRUE)
          
        }else{
          
          Profile_TOVT[row, col] <<- sum(PROFILE_VII[, "SULYOZOTT_TOVT"], na.rm = TRUE)
          
        }
        
      }
      
    }
  }
  
  
  TABLENAME <- tablename_Profile
  TYPE_TOVT <- "_Z"
  INDICATOR <- "TOVT"
  CIS_INDICATOR <- "_Z"
  REF_AREA <- "HU"
  UNIT_MEASURE <- "EUR"
  UNIT_MULT <- "3"
  DECIMALS <- "0"
  
  if (CIS_INDICATOR != "REAS_NINN_NNEED" & CIS_INDICATOR != "REAS_NINN_OTH" 
      & CIS_INDICATOR != "REAS_NINN_RESC" & CIS_INDICATOR != "REAS_NINN_RESC_OTH"){
    
    INN_PF <- "INN_ITR"
    TYPE_TOVT <- "INN"
    
    for(j in 1:length(ACTIVITY_PROFILE_LIST)){
      
      ACTIVITY <- ACTIVITY_PROFILE_LIST[j]
      
      for(i in length(NUMBER_EMPL_LIST):1){
        
        NUMBER_TOVT <- NUMBER_EMPL_LIST[i]
        OBS_VALUE <-  gsub("\\.", ",", sum(Profile_TOVT[profile_rows[i + ((j - 1) * 4)], c(1:8)], na.rm = TRUE))
        cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_TOVT, TYPE_TOVT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS, sep = ";"), sep = "\n", file = save_filename, append = TRUE)
        
      }
      
    }
    
  }
  
  
  INN_PF <- "NINN_ITR"
  TYPE_TOVT <- "_T"
  
  for(j in 1:length(ACTIVITY_PROFILE_LIST)){
    
    ACTIVITY <- ACTIVITY_PROFILE_LIST[j]
    
    for(i in length(NUMBER_EMPL_LIST):1){
      
      NUMBER_TOVT <- NUMBER_EMPL_LIST[i]
      OBS_VALUE <-  gsub("\\.", ",", sum(Profile_TOVT[profile_rows[i + ((j - 1) * 4)], c(9:12)], na.rm = TRUE))
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_TOVT, TYPE_TOVT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS, sep = ";"), sep = "\n", file = save_filename, append = TRUE)
      
    }
    
  }
  
  if (CIS_INDICATOR != "REAS_NINN_NNEED" & CIS_INDICATOR != "REAS_NINN_OTH" 
      & CIS_INDICATOR != "REAS_NINN_RESC" & CIS_INDICATOR != "REAS_NINN_RESC_OTH"){
    
    INN_PF <- "INN_CAP"
    TYPE_TOVT <- "INN"
    
    for(j in 1:length(ACTIVITY_PROFILE_LIST)){
      
      ACTIVITY <- ACTIVITY_PROFILE_LIST[j]
      
      for(i in length(NUMBER_EMPL_LIST):1){
        
        NUMBER_TOVT <- NUMBER_EMPL_LIST[i]
        OBS_VALUE <-  gsub("\\.", ",", sum(Profile_TOVT[profile_rows[i + ((j - 1) * 4)], c(1:6, 9:10)], na.rm = TRUE))
        cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_TOVT, TYPE_TOVT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS, sep = ";"), sep = "\n", file = save_filename, append = TRUE)
        
      }
      
    }
    
  }
  
  
  INN_PF <- "INN_NCAP"
  TYPE_TOVT <- "_T"
  
  for(j in 1:length(ACTIVITY_PROFILE_LIST)){
    
    ACTIVITY <- ACTIVITY_PROFILE_LIST[j]
    
    for(i in length(NUMBER_EMPL_LIST):1){
      
      NUMBER_TOVT <- NUMBER_EMPL_LIST[i]
      OBS_VALUE <-  gsub("\\.", ",", sum(Profile_TOVT[profile_rows[i + ((j - 1) * 4)], c(7:8, 11:12)], na.rm = TRUE))
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_TOVT, TYPE_TOVT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS, sep = ";"), sep = "\n", file = save_filename, append = TRUE)
      
    }
    
  }
  
  if (CIS_INDICATOR != "REAS_NINN_NNEED" & CIS_INDICATOR != "REAS_NINN_OTH" 
      & CIS_INDICATOR != "REAS_NINN_RESC" & CIS_INDICATOR != "REAS_NINN_RESC_OTH"){
    
    INN_PF <- "INN_RND"
    TYPE_TOVT <- "INN"
    
    for(j in 1:length(ACTIVITY_PROFILE_LIST)){
      
      ACTIVITY <- ACTIVITY_PROFILE_LIST[j]
      
      for(i in length(NUMBER_EMPL_LIST):1){
        
        NUMBER_TOVT <- NUMBER_EMPL_LIST[i]
        OBS_VALUE <-  gsub("\\.", ",", sum(Profile_TOVT[profile_rows[i + ((j - 1) * 4)], c(1, 3, 5, 7, 9)], na.rm = TRUE))
        cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_TOVT, TYPE_TOVT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS, sep = ";"), sep = "\n", file = save_filename, append = TRUE)
        
      }
      
    }
    
  }
  
  
  INN_PF <- "INN_NRND"
  TYPE_TOVT <- "_T"
  
  for(j in 1:length(ACTIVITY_PROFILE_LIST)){
    
    ACTIVITY <- ACTIVITY_PROFILE_LIST[j]
    
    for(i in length(NUMBER_EMPL_LIST):1){
      
      NUMBER_TOVT <- NUMBER_EMPL_LIST[i]
      OBS_VALUE <-  gsub("\\.", ",", sum(Profile_TOVT[profile_rows[i + ((j - 1) * 4)], c(2, 4, 6, 8, 10:12)], na.rm = TRUE))
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_TOVT, TYPE_TOVT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS, sep = ";"), sep = "\n", file = save_filename, append = TRUE)
      
    }
    
  }
  
  
  if (CIS_INDICATOR != "REAS_NINN_NNEED" & CIS_INDICATOR != "REAS_NINN_OTH" 
      & CIS_INDICATOR != "REAS_NINN_RESC" & CIS_INDICATOR != "REAS_NINN_RESC_OTH"){
    
    INN_PF <- "INN_PF1"
    TYPE_TOVT <- "INN"
    
    for(j in 1:length(ACTIVITY_PROFILE_LIST)){
      
      ACTIVITY <- ACTIVITY_PROFILE_LIST[j]
      
      for(i in length(NUMBER_EMPL_LIST):1){
        
        NUMBER_TOVT <- NUMBER_EMPL_LIST[i]
        OBS_VALUE <-  gsub("\\.", ",", sum(Profile_TOVT[profile_rows[i + ((j - 1) * 4)], c(1:2)], na.rm = TRUE))
        cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_TOVT, TYPE_TOVT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS, sep = ";"), sep = "\n", file = save_filename, append = TRUE)
        
      }
      
    }
    
  }
  
  
  if (CIS_INDICATOR != "REAS_NINN_NNEED" & CIS_INDICATOR != "REAS_NINN_OTH" 
      & CIS_INDICATOR != "REAS_NINN_RESC" & CIS_INDICATOR != "REAS_NINN_RESC_OTH"){
    
    INN_PF <- "INN_PF2"
    TYPE_TOVT <- "INN"
    
    for(j in 1:length(ACTIVITY_PROFILE_LIST)){
      
      ACTIVITY <- ACTIVITY_PROFILE_LIST[j]
      
      for(i in length(NUMBER_EMPL_LIST):1){
        
        NUMBER_TOVT <- NUMBER_EMPL_LIST[i]
        OBS_VALUE <-  gsub("\\.", ",", sum(Profile_TOVT[profile_rows[i + ((j - 1) * 4)], c(3:4)], na.rm = TRUE))
        cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_TOVT, TYPE_TOVT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS, sep = ";"), sep = "\n", file = save_filename, append = TRUE)
        
      }
      
    }
    
  }
  
  
  if (CIS_INDICATOR != "FUND_AUT_LOC_REG_RNDINN" & CIS_INDICATOR != "FUND_DEBT_SUCC_RNDINN" 
      & CIS_INDICATOR != "FUND_EQUIT_SUCC_RNDINN" & CIS_INDICATOR != "FUND_EU_HP2020_RNDINN" 
      & CIS_INDICATOR != "FUND_GOV_CTL_RNDINN" & CIS_INDICATOR != "FUND_FOR_RNDINN" 
      & CIS_INDICATOR != "FUND_EU_OTH_RNDINN" & CIS_INDICATOR != "REAS_NMINN_RESC" 
      & CIS_INDICATOR != "REAS_NMINN_RESC_OTH" & CIS_INDICATOR != "REAS_NMINN_OTH" 
      & CIS_INDICATOR != "REAS_NMINN_NNEED" & CIS_INDICATOR != "COOP_EG" 
      & CIS_INDICATOR != "COOP_EG_EU_EFTA" & CIS_INDICATOR != "COOP_EG_NAT" 
      & CIS_INDICATOR != "COOP_EG_NEU_NEFTA" & CIS_INDICATOR != "COOP_EU_EFTA"
      & CIS_INDICATOR != "COOP_GOV_RI" & CIS_INDICATOR != "COOP_GOV_RI_EU_EFTA"
      & CIS_INDICATOR != "COOP_GOV_RI_NAT" & CIS_INDICATOR != "COOP_GOV_RI_NEU_NEFTA"
      & CIS_INDICATOR != "COOP_INN_XRND" & CIS_INDICATOR != "COOP_NAT" 
      & CIS_INDICATOR != "COOP_NEU_NEFTA" & CIS_INDICATOR != "COOP_NNAT"
      & CIS_INDICATOR != "COOP_NPO" & CIS_INDICATOR != "COOP_NPO_EU_EFTA" 
      & CIS_INDICATOR != "COOP_NPO_NAT" & CIS_INDICATOR != "COOP_NPO_NEU_NEFTA"
      & CIS_INDICATOR != "COOP_NPRV" & CIS_INDICATOR != "COOP_NPRV_EU_EFTA" 
      & CIS_INDICATOR != "COOP_NPRV_NAT" & CIS_INDICATOR != "COOP_NPRV_NEU_NEFTA"
      & CIS_INDICATOR != "COOP_PRV" & CIS_INDICATOR != "COOP_PRV_CLCU" 
      & CIS_INDICATOR != "COOP_PRV_CLCU_EU_EFTA" & CIS_INDICATOR != "COOP_PRV_CLCU_NAT"
      & CIS_INDICATOR != "COOP_PRV_CLCU_NEU_NEFTA" & CIS_INDICATOR != "COOP_PRV_CN"
      & CIS_INDICATOR != "COOP_PRV_CN_NAT" & CIS_INDICATOR != "COOP_PRV_CN_EU_EFTA"
      & CIS_INDICATOR != "COOP_PRV_CN_NEU_NEFTA" & CIS_INDICATOR != "COOP_PRV_COMP"
      & CIS_INDICATOR != "COOP_PRV_COMP_EU_EFTA" & CIS_INDICATOR != "COOP_PRV_COMP_NAT"
      & CIS_INDICATOR != "COOP_PRV_COMP_NEU_NEFTA" & CIS_INDICATOR != "COOP_PRV_EU_EFTA"
      & CIS_INDICATOR != "COOP_PRV_NAT" & CIS_INDICATOR != "COOP_PRV_NEU_NEFTA"
      & CIS_INDICATOR != "COOP_PRV_OTH" & CIS_INDICATOR != "COOP_PRV_OTH_EU_EFTA"
      & CIS_INDICATOR != "COOP_PRV_OTH_NAT" & CIS_INDICATOR != "COOP_PRV_OTH_NEU_NEFTA"
      & CIS_INDICATOR != "COOP_PRV_SUPPL" & CIS_INDICATOR != "COOP_PRV_SUPPL_EU_EFTA"
      & CIS_INDICATOR != "COOP_PRV_SUPPL_NAT" & CIS_INDICATOR != "COOP_PRV_SUPPL_NEU_NEFTA"
      & CIS_INDICATOR != "COOP_PUB_CLCU" & CIS_INDICATOR != "COOP_PUB_CLCU_EU_EFTA"
      & CIS_INDICATOR != "COOP_PUB_CLCU_NAT" & CIS_INDICATOR != "COOP_PUB_CLCU_NEU_NEFTA"
      & CIS_INDICATOR != "COOP_RNDINN" & CIS_INDICATOR != "COOP_RND"
      & CIS_INDICATOR != "COOP_UNIV" & CIS_INDICATOR != "COOP_UNIV_NEU_NEFTA" 
      & CIS_INDICATOR != "COOP_UNIV_NAT" & CIS_INDICATOR != "COOP_UNIV_EU_EFTA"){
    
    INN_PF <- "NINN_PF6"
    TYPE_TOVT <- "NINN"
    
    for(j in 1:length(ACTIVITY_PROFILE_LIST)){
      
      ACTIVITY <- ACTIVITY_PROFILE_LIST[j]
      
      for(i in length(NUMBER_EMPL_LIST):1){
        
        NUMBER_TOVT <- NUMBER_EMPL_LIST[i]
        OBS_VALUE <-  gsub("\\.", ",", sum(Profile_TOVT[profile_rows[i + ((j - 1) * 4)], c(11)], na.rm = TRUE))
        cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_TOVT, TYPE_TOVT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS, sep = ";"), sep = "\n", file = save_filename, append = TRUE)
        
      }
      
    }
    
    
    INN_PF <- "NINN_PF7"
    
    for(j in 1:length(ACTIVITY_PROFILE_LIST)){
      
      ACTIVITY <- ACTIVITY_PROFILE_LIST[j]
      
      for(i in length(NUMBER_EMPL_LIST):1){
        
        NUMBER_TOVT <- NUMBER_EMPL_LIST[i]
        OBS_VALUE <-  gsub("\\.", ",", sum(Profile_TOVT[profile_rows[i + ((j - 1) * 4)], c(12)], na.rm = TRUE))
        cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_TOVT, TYPE_TOVT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS, sep = ";"), sep = "\n", file = save_filename, append = TRUE)
        
      }
      
    }
    
  }
  
}