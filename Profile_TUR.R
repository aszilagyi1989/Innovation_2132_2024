set_TUR_Profile <- function(column_name, tablename_Profile, save_filename){
  
  if (column_name != "TUR_ONAT"){
    
    PROFILE_I_A <<- PROFILE_I_A %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) / 100 * TUR22 * VGMA001_SULY) 
    PROFILE_I_B <<- PROFILE_I_B %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) / 100 * TUR22 * VGMA001_SULY)
    PROFILE_II_A <<- PROFILE_II_A %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) / 100 * TUR22 * VGMA001_SULY) 
    PROFILE_II_B <<- PROFILE_II_B %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) / 100 * TUR22 * VGMA001_SULY)
    PROFILE_III_A <<- PROFILE_III_A %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) / 100 * TUR22 * VGMA001_SULY) 
    PROFILE_III_B <<- PROFILE_III_B %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) / 100 * TUR22 * VGMA001_SULY)
    PROFILE_IV_A <<- PROFILE_IV_A %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) / 100 * TUR22 * VGMA001_SULY) 
    PROFILE_IV_B <<- PROFILE_IV_B %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) / 100 * TUR22 * VGMA001_SULY)
    PROFILE_V_A <<- PROFILE_V_A %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) / 100 * TUR22 * VGMA001_SULY) 
    PROFILE_V_B <<- PROFILE_V_B %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) / 100 * TUR22 * VGMA001_SULY)
    PROFILE_VI <<- PROFILE_VI %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) / 100 * TUR22 * VGMA001_SULY) 
    PROFILE_VII <<- PROFILE_VII %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) / 100 * TUR22 * VGMA001_SULY)
  
  }else{
    
    PROFILE_I_A <<- PROFILE_I_A %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) * TUR22 * VGMA001_SULY) 
    PROFILE_I_B <<- PROFILE_I_B %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) * TUR22 * VGMA001_SULY)
    PROFILE_II_A <<- PROFILE_II_A %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) * TUR22 * VGMA001_SULY) 
    PROFILE_II_B <<- PROFILE_II_B %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) * TUR22 * VGMA001_SULY)
    PROFILE_III_A <<- PROFILE_III_A %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) * TUR22 * VGMA001_SULY) 
    PROFILE_III_B <<- PROFILE_III_B %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) * TUR22 * VGMA001_SULY)
    PROFILE_IV_A <<- PROFILE_IV_A %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) * TUR22 * VGMA001_SULY) 
    PROFILE_IV_B <<- PROFILE_IV_B %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) * TUR22 * VGMA001_SULY)
    PROFILE_V_A <<- PROFILE_V_A %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) * TUR22 * VGMA001_SULY) 
    PROFILE_V_B <<- PROFILE_V_B %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) * TUR22 * VGMA001_SULY)
    PROFILE_VI <<- PROFILE_VI %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) * TUR22 * VGMA001_SULY) 
    PROFILE_VII <<- PROFILE_VII %>% mutate(SULYOZOTT_TUR = eval_tidy(parse_expr(column_name)) * TUR22 * VGMA001_SULY)
    
  }
  
  for(col in 1:ncol(Profile_TUR)){

    for(row in 1:length(filter_M065_RETEG1)){
      
      if (col == 1){

        if (row != 4){
          
          Profile_TUR[row, col] <<- sum(subset(PROFILE_I_A, eval_tidy(parse_expr(filter_M065_RETEG1[row])), SULYOZOTT_TUR), na.rm = TRUE)

        }else{
          
          Profile_TUR[row, col] <<- sum(PROFILE_I_A[, "SULYOZOTT_TUR"], na.rm = TRUE)

        }
        
      }
      
      if (col == 2){
        
        if (row != 4){
          
          Profile_TUR[row, col] <<- sum(subset(PROFILE_I_B, eval_tidy(parse_expr(filter_M065_RETEG1[row])), SULYOZOTT_TUR), na.rm = TRUE)
          
        }else{
          
          Profile_TUR[row, col] <<- sum(PROFILE_I_B[, "SULYOZOTT_TUR"], na.rm = TRUE)
          
        }
        
      }
      
      if (col == 3){
        
        if (row != 4){
          
          Profile_TUR[row, col] <<- sum(subset(PROFILE_II_A, eval_tidy(parse_expr(filter_M065_RETEG1[row])), SULYOZOTT_TUR), na.rm = TRUE)
          
        }else{
          
          Profile_TUR[row, col] <<- sum(PROFILE_II_A[, "SULYOZOTT_TUR"], na.rm = TRUE)
          
        }
        
      }
      
      if (col == 4){
        
        if (row != 4){
          
          Profile_TUR[row, col] <<- sum(subset(PROFILE_II_B, eval_tidy(parse_expr(filter_M065_RETEG1[row])), SULYOZOTT_TUR), na.rm = TRUE)
          
        }else{
          
          Profile_TUR[row, col] <<- sum(PROFILE_II_B[, "SULYOZOTT_TUR"], na.rm = TRUE)
          
        }
        
      }
      
      if (col == 5){
        
        if (row != 4){
          
          Profile_TUR[row, col] <<- sum(subset(PROFILE_III_A, eval_tidy(parse_expr(filter_M065_RETEG1[row])), SULYOZOTT_TUR), na.rm = TRUE)
          
        }else{
          
          Profile_TUR[row, col] <<- sum(PROFILE_III_A[, "SULYOZOTT_TUR"], na.rm = TRUE)
          
        }
        
      }
      
      if (col == 6){
        
        if (row != 4){
          
          Profile_TUR[row, col] <<- sum(subset(PROFILE_III_B, eval_tidy(parse_expr(filter_M065_RETEG1[row])), SULYOZOTT_TUR), na.rm = TRUE)
          
        }else{
          
          Profile_TUR[row, col] <<- sum(PROFILE_III_B[, "SULYOZOTT_TUR"], na.rm = TRUE)
          
        }
        
      }
      
      if (col == 7){
        
        if (row != 4){
          
          Profile_TUR[row, col] <<- sum(subset(PROFILE_IV_A, eval_tidy(parse_expr(filter_M065_RETEG1[row])), SULYOZOTT_TUR), na.rm = TRUE)
          
        }else{
          
          Profile_TUR[row, col] <<- sum(PROFILE_IV_A[, "SULYOZOTT_TUR"], na.rm = TRUE)
          
        }
        
      }
      
      if (col == 8){
        
        if (row != 4){
          
          Profile_TUR[row, col] <<- sum(subset(PROFILE_IV_B, eval_tidy(parse_expr(filter_M065_RETEG1[row])), SULYOZOTT_TUR), na.rm = TRUE)
          
        }else{
          
          Profile_TUR[row, col] <<- sum(PROFILE_IV_B[, "SULYOZOTT_TUR"], na.rm = TRUE)
          
        }
        
      }
      
      if (col == 9){
        
        if (row != 4){
          
          Profile_TUR[row, col] <<- sum(subset(PROFILE_V_A, eval_tidy(parse_expr(filter_M065_RETEG1[row])), SULYOZOTT_TUR), na.rm = TRUE)
          
        }else{
          
          Profile_TUR[row, col] <<- sum(PROFILE_V_A[, "SULYOZOTT_TUR"], na.rm = TRUE)
          
        }
        
      }
      
      if (col == 10){
        
        if (row != 4){
          
          Profile_TUR[row, col] <<- sum(subset(PROFILE_V_B, eval_tidy(parse_expr(filter_M065_RETEG1[row])), SULYOZOTT_TUR), na.rm = TRUE)
          
        }else{
          
          Profile_TUR[row, col] <<- sum(PROFILE_V_B[, "SULYOZOTT_TUR"], na.rm = TRUE)
          
        }
        
      }
      
      if (col == 11){
        
        if (row != 4){
          
          Profile_TUR[row, col] <<- sum(subset(PROFILE_VI, eval_tidy(parse_expr(filter_M065_RETEG1[row])), SULYOZOTT_TUR), na.rm = TRUE)
          
        }else{
          
          Profile_TUR[row, col] <<- sum(PROFILE_VI[, "SULYOZOTT_TUR"], na.rm = TRUE)
          
        }
        
      }
      
      if (col == 12){
        
        if (row != 4){
          
          Profile_TUR[row, col] <<- sum(subset(PROFILE_VII, eval_tidy(parse_expr(filter_M065_RETEG1[row])), SULYOZOTT_TUR), na.rm = TRUE)
          
        }else{
          
          Profile_TUR[row, col] <<- sum(PROFILE_VII[, "SULYOZOTT_TUR"], na.rm = TRUE)
          
        }
        
      }
      
    }
  }

  TABLENAME <- tablename_Profile
  TYPE_ENT <- "_Z"
  INDICATOR <- "TOVT"
  CIS_INDICATOR <- gsub("as.numeric|[()]", "", column_name)
  REF_AREA <- "HU"
  UNIT_MEASURE <- "EUR"
  UNIT_MULT <- "3"
  DECIMALS <- "0"
  
  
  INN_PF <- "INN_ITR"
  TYPE_ENT <- "INN"
  
  for(j in 1:length(ACTIVITY_PROFILE_LIST)){
    
    ACTIVITY <- ACTIVITY_PROFILE_LIST[j]
    
    for(i in length(NUMBER_EMPL_LIST):1){

      NUMBER_EMPL <- NUMBER_EMPL_LIST[i]
      OBS_VALUE <-  gsub("\\.", ",", sum(Profile_TUR[profile_rows[i + ((j - 1) * 4)], c(1:8)], na.rm = TRUE))
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS, sep = ";"), sep = "\n", file = save_filename, append = TRUE)

    }
    
  }
  
  
  INN_PF <- "NINN_ITR"
  TYPE_ENT <- "_T"
  
  for(j in 1:length(ACTIVITY_PROFILE_LIST)){
    
    ACTIVITY <- ACTIVITY_PROFILE_LIST[j]
    
    for(i in length(NUMBER_EMPL_LIST):1){
      
      NUMBER_EMPL <- NUMBER_EMPL_LIST[i]
      OBS_VALUE <-  gsub("\\.", ",", sum(Profile_TUR[profile_rows[i + ((j - 1) * 4)], c(9:12)], na.rm = TRUE))
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS, sep = ";"), sep = "\n", file = save_filename, append = TRUE)
      
    }
    
  }
  
  
  INN_PF <- "INN_CAP"
  TYPE_ENT <- "INN"
  
  for(j in 1:length(ACTIVITY_PROFILE_LIST)){
    
    ACTIVITY <- ACTIVITY_PROFILE_LIST[j]
    
    for(i in length(NUMBER_EMPL_LIST):1){
      
      NUMBER_EMPL <- NUMBER_EMPL_LIST[i]
      OBS_VALUE <-  gsub("\\.", ",", sum(Profile_TUR[profile_rows[i + ((j - 1) * 4)], c(1:6, 9:10)], na.rm = TRUE))
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS, sep = ";"), sep = "\n", file = save_filename, append = TRUE)
      
    }
    
  }
  
  
  INN_PF <- "INN_NCAP"
  TYPE_ENT <- "_T"
  
  for(j in 1:length(ACTIVITY_PROFILE_LIST)){
    
    ACTIVITY <- ACTIVITY_PROFILE_LIST[j]
    
    for(i in length(NUMBER_EMPL_LIST):1){
      
      NUMBER_EMPL <- NUMBER_EMPL_LIST[i]
      OBS_VALUE <-  gsub("\\.", ",", sum(Profile_TUR[profile_rows[i + ((j - 1) * 4)], c(7:8, 11:12)], na.rm = TRUE))
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS, sep = ";"), sep = "\n", file = save_filename, append = TRUE)
      
    }
    
  }
  
  
  INN_PF <- "INN_RND"
  TYPE_ENT <- "INN"
  
  for(j in 1:length(ACTIVITY_PROFILE_LIST)){
    
    ACTIVITY <- ACTIVITY_PROFILE_LIST[j]
    
    for(i in length(NUMBER_EMPL_LIST):1){
      
      NUMBER_EMPL <- NUMBER_EMPL_LIST[i]
      OBS_VALUE <-  gsub("\\.", ",", sum(Profile_TUR[profile_rows[i + ((j - 1) * 4)], c(1, 3, 5, 7, 9)], na.rm = TRUE))
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS, sep = ";"), sep = "\n", file = save_filename, append = TRUE)
      
    }
    
  }
  
  
  INN_PF <- "INN_NRND"
  TYPE_ENT <- "_T"
  
  for(j in 1:length(ACTIVITY_PROFILE_LIST)){
    
    ACTIVITY <- ACTIVITY_PROFILE_LIST[j]
    
    for(i in length(NUMBER_EMPL_LIST):1){
      
      NUMBER_EMPL <- NUMBER_EMPL_LIST[i]
      OBS_VALUE <-  gsub("\\.", ",", sum(Profile_TUR[profile_rows[i + ((j - 1) * 4)], c(2, 4, 6, 8, 10:12)], na.rm = TRUE))
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS, sep = ";"), sep = "\n", file = save_filename, append = TRUE)
      
    }
    
  }
  
  #Ezek rosszak voltak a T10-ben, mert a TYPE_ENT-nél INN helyett _T volt
  INN_PF <- "INN_PF1"
  TYPE_ENT <- "INN"
  
  for(j in 1:length(ACTIVITY_PROFILE_LIST)){
    
    ACTIVITY <- ACTIVITY_PROFILE_LIST[j]
    
    for(i in length(NUMBER_EMPL_LIST):1){
      
      NUMBER_EMPL <- NUMBER_EMPL_LIST[i]
      OBS_VALUE <-  gsub("\\.", ",", sum(Profile_TUR[profile_rows[i + ((j - 1) * 4)], c(1:2)], na.rm = TRUE))
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS, sep = ";"), sep = "\n", file = save_filename, append = TRUE)
      
    }
    
  }
  
  
  INN_PF <- "INN_PF2"
  TYPE_ENT <- "INN"
  
  for(j in 1:length(ACTIVITY_PROFILE_LIST)){
    
    ACTIVITY <- ACTIVITY_PROFILE_LIST[j]
    
    for(i in length(NUMBER_EMPL_LIST):1){
      
      NUMBER_EMPL <- NUMBER_EMPL_LIST[i]
      OBS_VALUE <-  gsub("\\.", ",", sum(Profile_TUR[profile_rows[i + ((j - 1) * 4)], c(3:4)], na.rm = TRUE))
      cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS, sep = ";"), sep = "\n", file = save_filename, append = TRUE)
      
    }
    
  }
  
  
  if (tablename_Profile != "T10"){
    
    INN_PF <- "NINN_PF6"
    TYPE_ENT <- "NINN"
    
    for(j in 1:length(ACTIVITY_PROFILE_LIST)){
      
      ACTIVITY <- ACTIVITY_PROFILE_LIST[j]
      
      for(i in length(NUMBER_EMPL_LIST):1){
        
        NUMBER_EMPL <- NUMBER_EMPL_LIST[i]
        OBS_VALUE <-  gsub("\\.", ",", sum(Profile_TUR[profile_rows[i + ((j - 1) * 4)], c(11)], na.rm = TRUE))
        cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS, sep = ";"), sep = "\n", file = save_filename, append = TRUE)
        
      }
      
    }
    
    
    INN_PF <- "NINN_PF7"
    
    for(j in 1:length(ACTIVITY_PROFILE_LIST)){
      
      ACTIVITY <- ACTIVITY_PROFILE_LIST[j]
      
      for(i in length(NUMBER_EMPL_LIST):1){
        
        NUMBER_EMPL <- NUMBER_EMPL_LIST[i]
        OBS_VALUE <-  gsub("\\.", ",", sum(Profile_TUR[profile_rows[i + ((j - 1) * 4)], c(12)], na.rm = TRUE))
        cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS, sep = ";"), sep = "\n", file = save_filename, append = TRUE)
        
      }
      
    }
    
  }
  
}