#egyszerű értékek számolása a háromféle vállalat típusra: E10T49, E50T249 és E_GE250 külön-külön
get_NACE <- function(M0581_2Jegyen) {
  
  if (as.integer(M0581_2Jegyen) >= 1 & as.integer(M0581_2Jegyen) <= 3){
    return(paste("A", M0581_2Jegyen, sep = ""))
  }
  
  if (as.integer(M0581_2Jegyen) >= 5 & as.integer(M0581_2Jegyen) <= 9){
    return(paste("B", M0581_2Jegyen, sep = ""))
  }
  
  if (as.integer(M0581_2Jegyen) >= 10 & as.integer(M0581_2Jegyen) <= 33){
    return(paste("C", M0581_2Jegyen, sep = ""))
  }
  
  if (as.integer(M0581_2Jegyen) >= 351 & as.integer(M0581_2Jegyen) <= 353){
    return(paste("D", M0581_2Jegyen, sep = ""))
  }
  
  if (as.integer(M0581_2Jegyen) >= 36 & as.integer(M0581_2Jegyen) <= 39){
    return(paste("E", M0581_2Jegyen, sep = ""))
  }
  
  if (as.integer(M0581_2Jegyen) >= 41 & as.integer(M0581_2Jegyen) <= 43){
    return(paste("F", M0581_2Jegyen, sep = ""))
  }
  
  if (as.integer(M0581_2Jegyen) >= 45 & as.integer(M0581_2Jegyen) <= 47){
    return(paste("G", M0581_2Jegyen, sep = ""))
  }
  
  if (as.integer(M0581_2Jegyen) >= 49 & as.integer(M0581_2Jegyen) <= 53){
    return(paste("H", M0581_2Jegyen, sep = ""))
  }
  
  if (as.integer(M0581_2Jegyen) >= 55 & as.integer(M0581_2Jegyen) <= 56){
    return(paste("I", M0581_2Jegyen, sep = ""))
  }
  
  if (as.integer(M0581_2Jegyen) >= 58 & as.integer(M0581_2Jegyen) <= 63){
    return(paste("J", M0581_2Jegyen, sep = ""))
  }
  
  if (as.integer(M0581_2Jegyen) >= 64 & as.integer(M0581_2Jegyen) <= 66){
    return(paste("K", M0581_2Jegyen, sep = ""))
  }
  
  if (as.integer(M0581_2Jegyen) == 68){
    return("L")
  }
  
  if (as.integer(M0581_2Jegyen) >= 69 & as.integer(M0581_2Jegyen) <= 75){
    return(paste("M", M0581_2Jegyen, sep = ""))
  }
  
  if (as.integer(M0581_2Jegyen) >= 77 & as.integer(M0581_2Jegyen) <= 82){
    return(paste("N", M0581_2Jegyen, sep = ""))
  }
  
  return("Missing")
}


insert_Total <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {

  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$ACTIVITY == ACTIVITY & Aggregate$CIS_INDICATOR == CIS_INDICATOR, "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_M71T73 <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  if(NUMBER_EMPL != "_T"){
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$NUMBER_EMPL == NUMBER_EMPL & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "M71" | Aggregate$ACTIVITY == "M72" | Aggregate$ACTIVITY == "M73"), "OBS_VALUE"]
    
  }else{
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "M71" | Aggregate$ACTIVITY == "M72" | Aggregate$ACTIVITY == "M73"), "OBS_VALUE"]
    
  }
  
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)

}


insert_N <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "N77" | Aggregate$ACTIVITY == "N78" | Aggregate$ACTIVITY == "N79" | Aggregate$ACTIVITY == "N80" | Aggregate$ACTIVITY == "N81" | Aggregate$ACTIVITY == "N82"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_M <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "M69" | Aggregate$ACTIVITY == "M70" | Aggregate$ACTIVITY == "M71" | Aggregate$ACTIVITY == "M72" | Aggregate$ACTIVITY == "M73" | Aggregate$ACTIVITY == "M74" | Aggregate$ACTIVITY == "M75"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_J61T63 <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "J61" | Aggregate$ACTIVITY == "J62" | Aggregate$ACTIVITY == "J63"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)

}


insert_J58T60 <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "J58" | Aggregate$ACTIVITY == "J59" | Aggregate$ACTIVITY == "J60"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)

}


insert_K <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  if(NUMBER_EMPL != "_T"){
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$NUMBER_EMPL == NUMBER_EMPL & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "K64" | Aggregate$ACTIVITY == "K65" | Aggregate$ACTIVITY == "K66"), "OBS_VALUE"]
    
  }else{
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "K64" | Aggregate$ACTIVITY == "K65" | Aggregate$ACTIVITY == "K66"), "OBS_VALUE"]
    
  }
  
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_J <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  if(NUMBER_EMPL != "_T"){
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$NUMBER_EMPL == NUMBER_EMPL & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "J58" | Aggregate$ACTIVITY == "J59" | Aggregate$ACTIVITY == "J60" | Aggregate$ACTIVITY == "J61" | Aggregate$ACTIVITY == "J62" | Aggregate$ACTIVITY == "J63"), "OBS_VALUE"]
    
  }else{
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "J58" | Aggregate$ACTIVITY == "J59" | Aggregate$ACTIVITY == "J60" | Aggregate$ACTIVITY == "J61" | Aggregate$ACTIVITY == "J62" | Aggregate$ACTIVITY == "J63"), "OBS_VALUE"]
    
  }
  
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_H52T53 <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "H52" | Aggregate$ACTIVITY == "H53"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)

}


insert_H49T51 <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "H49" | Aggregate$ACTIVITY == "H50" | Aggregate$ACTIVITY == "H51"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)

}


insert_H <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  if(NUMBER_EMPL != "_T"){
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$NUMBER_EMPL == NUMBER_EMPL & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "H49" | Aggregate$ACTIVITY == "H50" | Aggregate$ACTIVITY == "H51" | Aggregate$ACTIVITY == "H52" | Aggregate$ACTIVITY == "H53"), "OBS_VALUE"]
    
  }else{
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "H49" | Aggregate$ACTIVITY == "H50" | Aggregate$ACTIVITY == "H51" | Aggregate$ACTIVITY == "H52" | Aggregate$ACTIVITY == "H53"), "OBS_VALUE"]
    
  }
  
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_I <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "I55" | Aggregate$ACTIVITY == "I56"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_G46 <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  if(NUMBER_EMPL != "_T"){
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$NUMBER_EMPL == NUMBER_EMPL & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "G46"), "OBS_VALUE"]
    
  }else{
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "G46"), "OBS_VALUE"]
    
  }
  
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_G <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "G45" | Aggregate$ACTIVITY == "G46" | Aggregate$ACTIVITY == "G47"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_A <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "A01" | Aggregate$ACTIVITY == "A02" | Aggregate$ACTIVITY == "A03"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_B <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  if(NUMBER_EMPL != "_T"){
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$NUMBER_EMPL == NUMBER_EMPL & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "B05" | Aggregate$ACTIVITY == "B06" | Aggregate$ACTIVITY == "B07"| Aggregate$ACTIVITY == "B08" | Aggregate$ACTIVITY == "B09"), "OBS_VALUE"]
    
  }else{
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "B05" | Aggregate$ACTIVITY == "B06" | Aggregate$ACTIVITY == "B07"| Aggregate$ACTIVITY == "B08" | Aggregate$ACTIVITY == "B09"), "OBS_VALUE"]
    
  }
  
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_C <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  if(NUMBER_EMPL != "_T"){
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$NUMBER_EMPL == NUMBER_EMPL & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "C10" | Aggregate$ACTIVITY == "C11" | Aggregate$ACTIVITY == "C12"| Aggregate$ACTIVITY == "C13" | Aggregate$ACTIVITY == "C14" | Aggregate$ACTIVITY == "C15" | Aggregate$ACTIVITY == "C16" | Aggregate$ACTIVITY == "C17" | Aggregate$ACTIVITY == "C18" | Aggregate$ACTIVITY == "C19" | Aggregate$ACTIVITY == "C20" | Aggregate$ACTIVITY == "C21" | Aggregate$ACTIVITY == "C22" | Aggregate$ACTIVITY == "C23" | Aggregate$ACTIVITY == "C24" | Aggregate$ACTIVITY == "C25" | Aggregate$ACTIVITY == "C26" | Aggregate$ACTIVITY == "C27" | Aggregate$ACTIVITY == "C28" | Aggregate$ACTIVITY == "C29" | Aggregate$ACTIVITY == "C30" | Aggregate$ACTIVITY == "C31" | Aggregate$ACTIVITY == "C32" | Aggregate$ACTIVITY == "C33"), "OBS_VALUE"]
    
  }else{
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "C10" | Aggregate$ACTIVITY == "C11" | Aggregate$ACTIVITY == "C12"| Aggregate$ACTIVITY == "C13" | Aggregate$ACTIVITY == "C14" | Aggregate$ACTIVITY == "C15" | Aggregate$ACTIVITY == "C16" | Aggregate$ACTIVITY == "C17" | Aggregate$ACTIVITY == "C18" | Aggregate$ACTIVITY == "C19" | Aggregate$ACTIVITY == "C20" | Aggregate$ACTIVITY == "C21" | Aggregate$ACTIVITY == "C22" | Aggregate$ACTIVITY == "C23" | Aggregate$ACTIVITY == "C24" | Aggregate$ACTIVITY == "C25" | Aggregate$ACTIVITY == "C26" | Aggregate$ACTIVITY == "C27" | Aggregate$ACTIVITY == "C28" | Aggregate$ACTIVITY == "C29" | Aggregate$ACTIVITY == "C30" | Aggregate$ACTIVITY == "C31" | Aggregate$ACTIVITY == "C32" | Aggregate$ACTIVITY == "C33"), "OBS_VALUE"]
    
  }
  
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_C10T12 <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "C10" | Aggregate$ACTIVITY == "C11" | Aggregate$ACTIVITY == "C12"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_C13T15 <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "C13" | Aggregate$ACTIVITY == "C14" | Aggregate$ACTIVITY == "C15"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_C16T18 <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "C16" | Aggregate$ACTIVITY == "C17" | Aggregate$ACTIVITY == "C18"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_C19T21 <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "C19" | Aggregate$ACTIVITY == "C20" | Aggregate$ACTIVITY == "C21"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_C19T22 <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "C19" | Aggregate$ACTIVITY == "C20" | Aggregate$ACTIVITY == "C21" | Aggregate$ACTIVITY == "C22"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_C22T23 <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "C22" | Aggregate$ACTIVITY == "C23"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_C24T25 <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "C24" | Aggregate$ACTIVITY == "C25"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_C26T28 <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "C26" | Aggregate$ACTIVITY == "C27" | Aggregate$ACTIVITY == "C28"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_C29T30 <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "C29" | Aggregate$ACTIVITY == "C30"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_C31T33 <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "C31" | Aggregate$ACTIVITY == "C32" | Aggregate$ACTIVITY == "C33"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_C25T30 <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "C25" | Aggregate$ACTIVITY == "C26" | Aggregate$ACTIVITY == "C27" | Aggregate$ACTIVITY == "C28" | Aggregate$ACTIVITY == "C29" | Aggregate$ACTIVITY == "C30"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_D <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  if(NUMBER_EMPL != "_T"){
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$NUMBER_EMPL == NUMBER_EMPL & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "D351" | Aggregate$ACTIVITY == "D352" | Aggregate$ACTIVITY == "D353"), "OBS_VALUE"]
    
  }else{
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "D351" | Aggregate$ACTIVITY == "D352" | Aggregate$ACTIVITY == "D353"), "OBS_VALUE"]
    
  }
  
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_E <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  if(NUMBER_EMPL != "_T"){
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$NUMBER_EMPL == NUMBER_EMPL & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "E36" | Aggregate$ACTIVITY == "E37" | Aggregate$ACTIVITY == "E38"  | Aggregate$ACTIVITY == "E39"), "OBS_VALUE"]
    
  }else{
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "E36" | Aggregate$ACTIVITY == "E37" | Aggregate$ACTIVITY == "E38"  | Aggregate$ACTIVITY == "E39"), "OBS_VALUE"]
    
  }
  
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_E36T37 <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "E36" | Aggregate$ACTIVITY == "E37"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_E38T39 <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "E38" | Aggregate$ACTIVITY == "E39"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_E37T39 <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "E37" | Aggregate$ACTIVITY == "E38" | Aggregate$ACTIVITY == "E39"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_F <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "F41" | Aggregate$ACTIVITY == "F42" | Aggregate$ACTIVITY == "F43"), "OBS_VALUE"]
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_BTE <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  if(NUMBER_EMPL != "_T"){
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$NUMBER_EMPL == NUMBER_EMPL & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "B05" | Aggregate$ACTIVITY == "B06" | Aggregate$ACTIVITY == "B07"| Aggregate$ACTIVITY == "B08" | Aggregate$ACTIVITY == "B09" | Aggregate$ACTIVITY == "C10" | Aggregate$ACTIVITY == "C11" | Aggregate$ACTIVITY == "C12"| Aggregate$ACTIVITY == "C13" | Aggregate$ACTIVITY == "C14" | Aggregate$ACTIVITY == "C15" | Aggregate$ACTIVITY == "C16" | Aggregate$ACTIVITY == "C17" | Aggregate$ACTIVITY == "C18" | Aggregate$ACTIVITY == "C19" | Aggregate$ACTIVITY == "C20" | Aggregate$ACTIVITY == "C21" | Aggregate$ACTIVITY == "C22" | Aggregate$ACTIVITY == "C23" | Aggregate$ACTIVITY == "C24" | Aggregate$ACTIVITY == "C25" | Aggregate$ACTIVITY == "C26" | Aggregate$ACTIVITY == "C27" | Aggregate$ACTIVITY == "C28" | Aggregate$ACTIVITY == "C29" | Aggregate$ACTIVITY == "C30" | Aggregate$ACTIVITY == "C31" | Aggregate$ACTIVITY == "C32" | Aggregate$ACTIVITY == "C33" | Aggregate$ACTIVITY == "D351" | Aggregate$ACTIVITY == "D352" | Aggregate$ACTIVITY == "D353" | Aggregate$ACTIVITY == "E36" | Aggregate$ACTIVITY == "E37" | Aggregate$ACTIVITY == "E38"  | Aggregate$ACTIVITY == "E39"), "OBS_VALUE"]
    
  }else{
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "B05" | Aggregate$ACTIVITY == "B06" | Aggregate$ACTIVITY == "B07"| Aggregate$ACTIVITY == "B08" | Aggregate$ACTIVITY == "B09" | Aggregate$ACTIVITY == "C10" | Aggregate$ACTIVITY == "C11" | Aggregate$ACTIVITY == "C12"| Aggregate$ACTIVITY == "C13" | Aggregate$ACTIVITY == "C14" | Aggregate$ACTIVITY == "C15" | Aggregate$ACTIVITY == "C16" | Aggregate$ACTIVITY == "C17" | Aggregate$ACTIVITY == "C18" | Aggregate$ACTIVITY == "C19" | Aggregate$ACTIVITY == "C20" | Aggregate$ACTIVITY == "C21" | Aggregate$ACTIVITY == "C22" | Aggregate$ACTIVITY == "C23" | Aggregate$ACTIVITY == "C24" | Aggregate$ACTIVITY == "C25" | Aggregate$ACTIVITY == "C26" | Aggregate$ACTIVITY == "C27" | Aggregate$ACTIVITY == "C28" | Aggregate$ACTIVITY == "C29" | Aggregate$ACTIVITY == "C30" | Aggregate$ACTIVITY == "C31" | Aggregate$ACTIVITY == "C32" | Aggregate$ACTIVITY == "C33" | Aggregate$ACTIVITY == "D351" | Aggregate$ACTIVITY == "D352" | Aggregate$ACTIVITY == "D353" | Aggregate$ACTIVITY == "E36" | Aggregate$ACTIVITY == "E37" | Aggregate$ACTIVITY == "E38"  | Aggregate$ACTIVITY == "E39"), "OBS_VALUE"]
    
  }
  
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_GTN <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  if(NUMBER_EMPL != "_T"){
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$NUMBER_EMPL == NUMBER_EMPL & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "G45" | Aggregate$ACTIVITY == "G46" | Aggregate$ACTIVITY == "G47" | Aggregate$ACTIVITY == "H49" | Aggregate$ACTIVITY == "H50" | Aggregate$ACTIVITY == "H51" | Aggregate$ACTIVITY == "H52" | Aggregate$ACTIVITY == "H53" | Aggregate$ACTIVITY == "I55" | Aggregate$ACTIVITY == "I56" | Aggregate$ACTIVITY == "J58" | Aggregate$ACTIVITY == "J59" | Aggregate$ACTIVITY == "J60" | Aggregate$ACTIVITY == "J61" | Aggregate$ACTIVITY == "J62" | Aggregate$ACTIVITY == "J63" | Aggregate$ACTIVITY == "K64" | Aggregate$ACTIVITY == "K65" | Aggregate$ACTIVITY == "K66" | Aggregate$ACTIVITY == "L" | Aggregate$ACTIVITY == "M69" | Aggregate$ACTIVITY == "M70" | Aggregate$ACTIVITY == "M71" | Aggregate$ACTIVITY == "M72" | Aggregate$ACTIVITY == "M73" | Aggregate$ACTIVITY == "M74" | Aggregate$ACTIVITY == "M75" | Aggregate$ACTIVITY == "N77" | Aggregate$ACTIVITY == "N78" | Aggregate$ACTIVITY == "N79" | Aggregate$ACTIVITY == "N80" | Aggregate$ACTIVITY == "N81" | Aggregate$ACTIVITY == "N82"), "OBS_VALUE"]
    
  }else{
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "G45" | Aggregate$ACTIVITY == "G46" | Aggregate$ACTIVITY == "G47" | Aggregate$ACTIVITY == "H49" | Aggregate$ACTIVITY == "H50" | Aggregate$ACTIVITY == "H51" | Aggregate$ACTIVITY == "H52" | Aggregate$ACTIVITY == "H53" | Aggregate$ACTIVITY == "I55" | Aggregate$ACTIVITY == "I56" | Aggregate$ACTIVITY == "J58" | Aggregate$ACTIVITY == "J59" | Aggregate$ACTIVITY == "J60" | Aggregate$ACTIVITY == "J61" | Aggregate$ACTIVITY == "J62" | Aggregate$ACTIVITY == "J63" | Aggregate$ACTIVITY == "K64" | Aggregate$ACTIVITY == "K65" | Aggregate$ACTIVITY == "K66" | Aggregate$ACTIVITY == "L" | Aggregate$ACTIVITY == "M69" | Aggregate$ACTIVITY == "M70" | Aggregate$ACTIVITY == "M71" | Aggregate$ACTIVITY == "M72" | Aggregate$ACTIVITY == "M73" | Aggregate$ACTIVITY == "M74" | Aggregate$ACTIVITY == "M75" | Aggregate$ACTIVITY == "N77" | Aggregate$ACTIVITY == "N78" | Aggregate$ACTIVITY == "N79" | Aggregate$ACTIVITY == "N80" | Aggregate$ACTIVITY == "N81" | Aggregate$ACTIVITY == "N82"), "OBS_VALUE"]
    
  }
  
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert_G46TM73_INN <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  if(NUMBER_EMPL != "_T"){
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$NUMBER_EMPL == NUMBER_EMPL & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "G46" | Aggregate$ACTIVITY == "H49" | Aggregate$ACTIVITY == "H50" | Aggregate$ACTIVITY == "H51" | Aggregate$ACTIVITY == "H52" | Aggregate$ACTIVITY == "H53" | Aggregate$ACTIVITY == "J58" | Aggregate$ACTIVITY == "J59" | Aggregate$ACTIVITY == "J60" | Aggregate$ACTIVITY == "J61" | Aggregate$ACTIVITY == "J62" | Aggregate$ACTIVITY == "J63" | Aggregate$ACTIVITY == "K64" | Aggregate$ACTIVITY == "K65" | Aggregate$ACTIVITY == "K66" | Aggregate$ACTIVITY == "M71" | Aggregate$ACTIVITY == "M72" | Aggregate$ACTIVITY == "M73"), "OBS_VALUE"]
    
  }else{
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "G46" | Aggregate$ACTIVITY == "H49" | Aggregate$ACTIVITY == "H50" | Aggregate$ACTIVITY == "H51" | Aggregate$ACTIVITY == "H52" | Aggregate$ACTIVITY == "H53" | Aggregate$ACTIVITY == "J58" | Aggregate$ACTIVITY == "J59" | Aggregate$ACTIVITY == "J60" | Aggregate$ACTIVITY == "J61" | Aggregate$ACTIVITY == "J62" | Aggregate$ACTIVITY == "J63" | Aggregate$ACTIVITY == "K64" | Aggregate$ACTIVITY == "K65" | Aggregate$ACTIVITY == "K66" | Aggregate$ACTIVITY == "M71" | Aggregate$ACTIVITY == "M72" | Aggregate$ACTIVITY == "M73"), "OBS_VALUE"]
    
  }
  
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}


insert__T <- function(TYPE_ENT, ACTIVITY, NUMBER_EMPL, UNIT_MEASURE, INDICATOR, CIS_INDICATOR, SAVE_FILE) {
  
  if(NUMBER_EMPL != "_T"){
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$NUMBER_EMPL == NUMBER_EMPL & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "B05" | Aggregate$ACTIVITY == "B06" | Aggregate$ACTIVITY == "B07"| Aggregate$ACTIVITY == "B08" | Aggregate$ACTIVITY == "B09" | Aggregate$ACTIVITY == "C10" | Aggregate$ACTIVITY == "C11" | Aggregate$ACTIVITY == "C12"| Aggregate$ACTIVITY == "C13" | Aggregate$ACTIVITY == "C14" | Aggregate$ACTIVITY == "C15" | Aggregate$ACTIVITY == "C16" | Aggregate$ACTIVITY == "C17" | Aggregate$ACTIVITY == "C18" | Aggregate$ACTIVITY == "C19" | Aggregate$ACTIVITY == "C20" | Aggregate$ACTIVITY == "C21" | Aggregate$ACTIVITY == "C22" | Aggregate$ACTIVITY == "C23" | Aggregate$ACTIVITY == "C24" | Aggregate$ACTIVITY == "C25" | Aggregate$ACTIVITY == "C26" | Aggregate$ACTIVITY == "C27" | Aggregate$ACTIVITY == "C28" | Aggregate$ACTIVITY == "C29" | Aggregate$ACTIVITY == "C30" | Aggregate$ACTIVITY == "C31" | Aggregate$ACTIVITY == "C32" | Aggregate$ACTIVITY == "C33" | Aggregate$ACTIVITY == "D351" | Aggregate$ACTIVITY == "D352" | Aggregate$ACTIVITY == "D353" | Aggregate$ACTIVITY == "E36" | Aggregate$ACTIVITY == "E37" | Aggregate$ACTIVITY == "E38"  | Aggregate$ACTIVITY == "E39" | Aggregate$ACTIVITY == "G46" | Aggregate$ACTIVITY == "H49" | Aggregate$ACTIVITY == "H50" | Aggregate$ACTIVITY == "H51" | Aggregate$ACTIVITY == "H52" | Aggregate$ACTIVITY == "H53" | Aggregate$ACTIVITY == "J58" | Aggregate$ACTIVITY == "J59" | Aggregate$ACTIVITY == "J60" | Aggregate$ACTIVITY == "J61" | Aggregate$ACTIVITY == "J62" | Aggregate$ACTIVITY == "J63" | Aggregate$ACTIVITY == "K64" | Aggregate$ACTIVITY == "K65" | Aggregate$ACTIVITY == "K66" | Aggregate$ACTIVITY == "M71" | Aggregate$ACTIVITY == "M72" | Aggregate$ACTIVITY == "M73"), "OBS_VALUE"]
    
  }else{
    
    OBS_VALUE <- Aggregate[Aggregate$TYPE_ENT == TYPE_ENT & Aggregate$INDICATOR == INDICATOR & Aggregate$CIS_INDICATOR == CIS_INDICATOR & (Aggregate$ACTIVITY == "B05" | Aggregate$ACTIVITY == "B06" | Aggregate$ACTIVITY == "B07"| Aggregate$ACTIVITY == "B08" | Aggregate$ACTIVITY == "B09" | Aggregate$ACTIVITY == "C10" | Aggregate$ACTIVITY == "C11" | Aggregate$ACTIVITY == "C12"| Aggregate$ACTIVITY == "C13" | Aggregate$ACTIVITY == "C14" | Aggregate$ACTIVITY == "C15" | Aggregate$ACTIVITY == "C16" | Aggregate$ACTIVITY == "C17" | Aggregate$ACTIVITY == "C18" | Aggregate$ACTIVITY == "C19" | Aggregate$ACTIVITY == "C20" | Aggregate$ACTIVITY == "C21" | Aggregate$ACTIVITY == "C22" | Aggregate$ACTIVITY == "C23" | Aggregate$ACTIVITY == "C24" | Aggregate$ACTIVITY == "C25" | Aggregate$ACTIVITY == "C26" | Aggregate$ACTIVITY == "C27" | Aggregate$ACTIVITY == "C28" | Aggregate$ACTIVITY == "C29" | Aggregate$ACTIVITY == "C30" | Aggregate$ACTIVITY == "C31" | Aggregate$ACTIVITY == "C32" | Aggregate$ACTIVITY == "C33" | Aggregate$ACTIVITY == "D351" | Aggregate$ACTIVITY == "D352" | Aggregate$ACTIVITY == "D353" | Aggregate$ACTIVITY == "E36" | Aggregate$ACTIVITY == "E37" | Aggregate$ACTIVITY == "E38"  | Aggregate$ACTIVITY == "E39" | Aggregate$ACTIVITY == "G46" | Aggregate$ACTIVITY == "H49" | Aggregate$ACTIVITY == "H50" | Aggregate$ACTIVITY == "H51" | Aggregate$ACTIVITY == "H52" | Aggregate$ACTIVITY == "H53" | Aggregate$ACTIVITY == "J58" | Aggregate$ACTIVITY == "J59" | Aggregate$ACTIVITY == "J60" | Aggregate$ACTIVITY == "J61" | Aggregate$ACTIVITY == "J62" | Aggregate$ACTIVITY == "J63" | Aggregate$ACTIVITY == "K64" | Aggregate$ACTIVITY == "K65" | Aggregate$ACTIVITY == "K66" | Aggregate$ACTIVITY == "M71" | Aggregate$ACTIVITY == "M72" | Aggregate$ACTIVITY == "M73"), "OBS_VALUE"]
    
  }
  
  OBS_VALUE <- sum(as.numeric(OBS_VALUE), na.rm = TRUE)
  cat(paste(DATAFLOW, FREQ, TIME_PERIOD, REF_AREA, TABLENAME, ACTIVITY, NUMBER_EMPL, TYPE_ENT, INN_PF, INDICATOR, CIS_INDICATOR,	OBS_VALUE, UNIT_MEASURE, UNIT_MULT, DECIMALS,	OBS_STATUS,	OBS_STATUS_1,	CONF_STATUS, COMMENT_OBS, sep = ";"), sep = "\n", file = SAVE_FILE, append = TRUE)
  
}