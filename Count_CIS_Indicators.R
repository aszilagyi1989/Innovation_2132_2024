DATAFLOW <- "ESTAT:CIS_OM4_2(2.1)"
FREQ <- "A"
TIME_PERIOD <- "2022"
REF_AREA <- "HU"
OBS_STATUS <- ""
OBS_STATUS_1 <- ""	
CONF_STATUS <- ""
COMMENT_OBS <- ""

ACTIVITY_LIST <- c("A01", "A02", "A03", "B05", "B06", "B07", "B08", "B09", 
                   "C10", "C11", "C12", "C13", "C14", "C15", "C16", "C17", 
                   "C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", 
                   "C26", "C27", "C28", "C29", "C30", "C31", "C32", "C33",
                   "D351", "D352", "D353", "E36", "E37", "E38", "E39", 
                   "F41", "F42", "F43", "G45", "G46", "G47", 
                   "H49", "H50", "H51", "H52", "H53", "I55", "I56", 
                   "J58", "J59", "J60", "J61", "J62", "J63", 
                   "K64", "K65", "K66", "L", 
                   "M69", "M70", "M71", "M72", "M73", "M74", "M75", 
                   "N77", "N78", "N79", "N80", "N81", "N82")

NUMBER_EMPL_LIST <- c("E_GE250", "E50T249", "E10T49", "_T")

INDICATOR_LIST <- c("ENT", "EMPL", "TOVT")

#For profiling
filter_M065_RETEG1 <- c("M065_RETEG1 == 'KI'", "M065_RETEG1 == 'KO'", "M065_RETEG1 == 'BI'", "",
                        "(M0691 == 'B' | M0691 == 'C' | M0691 == 'D' | M0691 == 'E') & M065_RETEG1 == 'KI'",
                        "(M0691 == 'B' | M0691 == 'C' | M0691 == 'D' | M0691 == 'E') & M065_RETEG1 == 'KO'",
                        "(M0691 == 'B' | M0691 == 'C' | M0691 == 'D' | M0691 == 'E') & M065_RETEG1 == 'BI'",
                        "(M0691 == 'B' | M0691 == 'C' | M0691 == 'D' | M0691 == 'E')",
                        "(M0691 != 'B' & M0691 != 'C' & M0691 != 'D' & M0691 != 'E') & M065_RETEG1 == 'KI'",
                        "(M0691 != 'B' & M0691 != 'C' & M0691 != 'D' & M0691 != 'E') & M065_RETEG1 == 'KO'",
                        "(M0691 != 'B' & M0691 != 'C' & M0691 != 'D' & M0691 != 'E') & M065_RETEG1 == 'BI'",
                        "(M0691 != 'B' & M0691 != 'C' & M0691 != 'D' & M0691 != 'E')")

#For profiling
first_rows <- c(3, 2, 1, 4)
middle_rows <- c(7, 6, 5, 8)
last_rows <- c(11, 10, 9, 12)
profile_rows <- c(3, 2, 1, 4, 7, 6, 5, 8, 11, 10, 9, 12)

#For profiling
ACTIVITY_PROFILE_LIST <- c("_T", "BTE", "G46TM73_INN")

#For regional
REF_AREA_LIST <- c("HU", "HU1", "HU2", "HU3", "HU11", "HU12", "HU21", "HU22", "HU23", "HU31", "HU32", "HU33", "HUZ", "HUZZ")

#4 - INN
INN <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], INNO_PRD_GD == 1 | INNO_PRD_SERV == 1 | INNO_PCS_PRD == 1 | INNO_PCS_LOG == 1 | INNO_PCS_COMM == 1 | INNO_PCS_ACCT == 1 | INNO_PCS_OPROC_EXTREL == 1 | INNO_PCS_WR_DEC_HRM == 1 | INNO_PCS_SLS_SERV == 1 | INNA_COMPL == 1 | INNA_ONGO == 1 | INNA_ABDN == 1 | INNA_IH_RND == 1 | INNA_RND_CONTR_OUT == 1)
dim(INN) #2488 sor és 357 oszlop

#5 - PRD_ONL
PRD_ONL <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], (INNO_PRD_GD == 1 | INNO_PRD_SERV == 1) & (INNO_PCS_PRD != 1 & INNO_PCS_LOG != 1 & INNO_PCS_COMM != 1 & INNO_PCS_ACCT != 1 & INNO_PCS_OPROC_EXTREL != 1 & INNO_PCS_WR_DEC_HRM != 1 & INNO_PCS_SLS_SERV != 1))
dim(PRD_ONL) #388 sor és 357 oszlop

#6 - BPCS_ONL
BPCS_ONL <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], (INNO_PRD_GD != 1 & INNO_PRD_SERV != 1) & (INNO_PCS_PRD == 1 | INNO_PCS_LOG == 1 | INNO_PCS_COMM == 1 | INNO_PCS_ACCT == 1 | INNO_PCS_OPROC_EXTREL == 1 | INNO_PCS_WR_DEC_HRM == 1 | INNO_PCS_SLS_SERV == 1))
dim(BPCS_ONL) #668 sor és 357 oszlop

#7 - PRD_BPCS_ONL
PRD_BPCS_ONL <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], (INNO_PRD_GD == 1 | INNO_PRD_SERV == 1) & (INNO_PCS_PRD == 1 | INNO_PCS_LOG == 1 | INNO_PCS_COMM == 1 | INNO_PCS_ACCT == 1 | INNO_PCS_OPROC_EXTREL == 1 | INNO_PCS_WR_DEC_HRM == 1 | INNO_PCS_SLS_SERV == 1))
dim(PRD_BPCS_ONL) #1161 sor és 357 oszlop

#9 - NINN
NINN <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], (INNO_PRD_GD != 1 & INNO_PRD_SERV != 1 & INNO_PCS_PRD != 1 & INNO_PCS_LOG != 1 & INNO_PCS_COMM != 1 & INNO_PCS_ACCT != 1 & INNO_PCS_OPROC_EXTREL != 1 & INNO_PCS_WR_DEC_HRM != 1 & INNO_PCS_SLS_SERV != 1) & (INNA_COMPL != 1 & INNA_ONGO != 1 & INNA_ABDN != 1 & INNA_IH_RND != 1 & INNA_RND_CONTR_OUT != 1))
dim(NINN) #5066 sor és 357 oszlop

#10 - INNO_PRD
INNO_PRD <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], INNO_PRD_GD == 1 | INNO_PRD_SERV == 1)
dim(INNO_PRD) #1549 sor és 357 oszlop

#11
INNO_BPCS <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], INNO_PCS_PRD == 1 | INNO_PCS_LOG == 1 | INNO_PCS_COMM == 1 | INNO_PCS_ACCT == 1 | INNO_PCS_OPROC_EXTREL == 1 | INNO_PCS_WR_DEC_HRM == 1 | INNO_PCS_SLS_SERV == 1)
dim(INNO_BPCS) #1829 sor és 357 oszlop

#12
PRD_NEW_MKT <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], (INNO_PRD_GD == 1 | INNO_PRD_SERV == 1) & INNO_PRD_NEW_MKT == 1)
dim(PRD_NEW_MKT) #687 sor és 357 oszlop

#13
PRD_NEW_ENT <-subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], (INNO_PRD_GD == 1 | INNO_PRD_SERV == 1) & INNO_PRD_NEW_ENT == 1)
dim(PRD_NEW_ENT) #1339 sor és 357 oszlop

#58
INNA_RND_OUT_N_IH <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], INNA_IH_RND == 0 & INNA_RND_CONTR_OUT == 1)
dim(INNA_RND_OUT_N_IH) #95 sor és 357 oszlop

#82 - INN_X_RND
INN_X_RND <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], (INNO_PRD_GD == 1 | INNO_PRD_SERV == 1 | INNO_PCS_PRD == 1 | INNO_PCS_LOG == 1 | INNO_PCS_COMM == 1 | INNO_PCS_ACCT == 1 | INNO_PCS_OPROC_EXTREL == 1 | INNO_PCS_WR_DEC_HRM == 1 | INNO_PCS_SLS_SERV == 1 | INNA_COMPL == 1 | INNA_ONGO == 1 | INNA_ABDN == 1) & (INNA_IH_RND != 1 & INNA_RND_CONTR_OUT != 1))
dim(INN_X_RND) #1258 sor és 357 oszlop

#83
PRD_NEW_ENT_ONL <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], (INNO_PRD_GD == 1 | INNO_PRD_SERV == 1) & (INNO_PRD_NEW_ENT == 1 & INNO_PRD_NEW_MKT == 0))
dim(PRD_NEW_ENT_ONL) #862 sor és 357 oszlop

PRDBPCS_COMPL <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], INNO_PRD_GD == 1 | INNO_PRD_SERV == 1 | INNO_PCS_PRD == 1 | INNO_PCS_LOG == 1 | INNO_PCS_COMM == 1 | INNO_PCS_ACCT == 1 | INNO_PCS_OPROC_EXTREL == 1 | INNO_PCS_WR_DEC_HRM == 1 | INNO_PCS_SLS_SERV == 1 | INNA_COMPL == 1)
dim(PRDBPCS_COMPL) #2261 sor és 357 oszlop

INNA_COMPL_NOT_ZERO <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], INNA_COMPL == 1)
dim(INNA_COMPL_NOT_ZERO) #178 sor és 357 oszlop

INNA_ONGO_NOT_ZERO <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], INNA_ONGO == 1)
dim(INNA_ONGO_NOT_ZERO) #799 sor és 357 oszlop

INNA_ABDN_NOT_ZERO <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], INNA_ABDN == 1)
dim(INNA_ABDN_NOT_ZERO) #282 sor és 357 oszlop

INNA_IH_RND_NOT_ZERO <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], INNA_IH_RND == 1)
dim(INNA_IH_RND_NOT_ZERO) #1135 sor és 357 oszlop

INNA_IH_RND_CONT_NOT_ZERO <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], INNA_IH_RND_CONT == 1)
dim(INNA_IH_RND_CONT_NOT_ZERO) #700 sor és 357 oszlop

INNA_IH_RND_OCC_NOT_ZERO <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], INNA_IH_RND_OCC == 1)
dim(INNA_IH_RND_OCC_NOT_ZERO) #435 sor és 357 oszlop

INNA_RND_CONTR_OUT_NOT_ZERO <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], INNA_RND_CONTR_OUT == 1)
dim(INNA_RND_CONTR_OUT_NOT_ZERO) #340 sor és 357 oszlop

INNO_PRD_GD_NOT_ZERO <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], INNO_PRD_GD == 1)
dim(INNO_PRD_GD_NOT_ZERO) #1170 sor és 357 oszlop

INNO_PRD_SERV_NOT_ZERO <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], INNO_PRD_SERV == 1)
dim(INNO_PRD_SERV_NOT_ZERO) #817 sor és 357 oszlop

INNO_PCS_PRD_NOT_ZERO <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], INNO_PCS_PRD == 1)
dim(INNO_PCS_PRD_NOT_ZERO) #1038 sor és 357 oszlop

INNO_PCS_LOG_NOT_ZERO <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], INNO_PCS_LOG == 1)
dim(INNO_PCS_LOG_NOT_ZERO) #562 sor és 357 oszlop

INNO_PCS_COMM_NOT_ZERO <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], INNO_PCS_COMM == 1)
dim(INNO_PCS_COMM_NOT_ZERO) #1078 sor és 357 oszlop

INNO_PCS_ACCT_NOT_ZERO <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], INNO_PCS_ACCT == 1)
dim(INNO_PCS_ACCT_NOT_ZERO) #888 sor és 357 oszlop

INNO_PCS_OPROC_EXTREL_NOT_ZERO <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], INNO_PCS_OPROC_EXTREL == 1)
dim(INNO_PCS_OPROC_EXTREL_NOT_ZERO) #710 sor és 357 oszlop

INNO_PCS_WR_DEC_HRM_NOT_ZERO <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], INNO_PCS_WR_DEC_HRM == 1)
dim(INNO_PCS_WR_DEC_HRM_NOT_ZERO) #737 sor és 357 oszlop

INNO_PCS_SLS_SERV_NOT_ZERO <- subset(worksheet_01[order(worksheet_01$M065_RETEG1, worksheet_01$M0581_2J), ], INNO_PCS_SLS_SERV == 1)
dim(INNO_PCS_SLS_SERV_NOT_ZERO) #734 sor és 357 oszlop

#T10
#55
TUR_PRD_NEW_MKT <- subset(worksheet_01, INNO_PRD_NEW_MKT == 1, c("M092", "INNO_PRD_NEW_MKT", "TUR_PRD_NEW_MKT", "TUR22", "VGMA001_SULY"))
dim(TUR_PRD_NEW_MKT) #687 sor és 5 oszlop

#56
TUR_PRD_NEW_ENT <- subset(worksheet_01, INNO_PRD_NEW_ENT == 1, c("M092", "INNO_PRD_NEW_ENT", "TUR_PRD_NEW_ENT", "TUR22", "VGMA001_SULY"))
dim(TUR_PRD_NEW_ENT) #1339 sor és 5 oszlop

#T11
#80
DEVE_PRD_BPCS <- subset(worksheet_01, DEVE_PRD_ENT == 1 | DEVE_PRD_ENT_OTH == 1 | DEVE_PCS_ENT == 1 | DEVE_PCS_ENT_OTH == 1, c("M092", "DEVE_PRD_ENT", "DEVE_PRD_ENT_OTH", "DEVE_PCS_ENT", "DEVE_PCS_ENT_OTH"))
dim(DEVE_PRD_BPCS) #1852 sor és 5 oszlop

#T14
#57
EXP_INNO_RND <- subset(worksheet_01, (EXP_INNO_RND_IH != 0 | EXP_INNO_RND_CONTR_OUT != 0), c("M092", "EXP_INNO_RND_IH", "EXP_INNO_RND_CONTR_OUT"))
dim(EXP_INNO_RND) #1122 sor és 3 oszlop

#59
EXP_INNO_TOTAL <- subset(worksheet_01, (EXP_INNO_RND_IH != 0 | EXP_INNO_RND_CONTR_OUT != 0 | EXP_INNO_INN_XRND != 0), c("M092", "EXP_INNO_RND_IH", "EXP_INNO_RND_CONTR_OUT", "EXP_INNO_INN_XRND"))
dim(EXP_INNO_TOTAL) #2064 sor és 4 oszlop

#64
ENT_INNO_RND_IH <- subset(worksheet_01, EXP_INNO_RND_IH != 0, c("M092", "EXP_INNO_RND_IH"))
dim(ENT_INNO_RND_IH) #1074 sor és 2 oszlop

#65
ENT_INNO_RND_CONTR_OUT <- subset(worksheet_01, EXP_INNO_RND_CONTR_OUT != 0, c("M092", "EXP_INNO_RND_CONTR_OUT"))
dim(ENT_INNO_RND_CONTR_OUT) #195 sor és 2 oszlop

#66
ENT_INNO_INN_XRND <- subset(worksheet_01, EXP_INNO_INN_XRND != 0, c("M092", "EXP_INNO_INN_XRND"))
dim(ENT_INNO_INN_XRND) #1280 sor és 2 oszlop

#67
ENT_INNO_TOTAL <- subset(worksheet_01, EXP_INNO_RND_IH != 0 | EXP_INNO_RND_CONTR_OUT != 0 | EXP_INNO_INN_XRND != 0, c("M092", "EXP_INNO_RND_IH", "EXP_INNO_RND_CONTR_OUT", "EXP_INNO_INN_XRND"))
dim(ENT_INNO_TOTAL) #2064 sor és 4 oszlop

#68
ENT_INNO_RND <- subset(worksheet_01, EXP_INNO_RND_IH != 0 | EXP_INNO_RND_CONTR_OUT != 0, c("M092", "EXP_INNO_RND_IH", "EXP_INNO_RND_CONTR_OUT"))
dim(ENT_INNO_RND) #1122 sor és 3 oszlop

#T16
#52
FUND_SOURCE <- subset(worksheet_01, FUND_AUT_LOC_REG == 1 | FUND_GOV_CTL == 1 | FUND_EU_HP2020 == 1 | FUND_EU_OTH == 1, c("M092", "FUND_AUT_LOC_REG", "FUND_GOV_CTL", "FUND_EU_HP2020", "FUND_EU_OTH"))
dim(FUND_SOURCE) #2577 sor és 5 oszlop

#53
FUND_FOR_RNDINN <- subset(worksheet_01, FUND_AUT_LOC_REG_RNDINN == 1 | FUND_GOV_CTL_RNDINN == 1 | FUND_EU_HP2020_RNDINN == 1 | FUND_EU_OTH_RNDINN == 1, c("M092", "FUND_AUT_LOC_REG_RNDINN", "FUND_GOV_CTL_RNDINN", "FUND_EU_HP2020_RNDINN", "FUND_EU_OTH_RNDINN"))
dim(FUND_FOR_RNDINN) #799 sor és 5 oszlop

#T18
#31
COOP_RNDINN <- subset(worksheet_01, COOP_RND == 1 | COOP_INN_XRND == 1, c("M092", "COOP_RND", "COOP_INN_XRND"))
dim(COOP_RNDINN) #973 sor és 3 oszlop

#32
COOP_ALL <- subset(worksheet_01, COOP_RND == 1 | COOP_INN_XRND == 1 | COOP_OTH == 1, c("M092", "COOP_RND", "COOP_INN_XRND", "COOP_OTH"))
dim(COOP_ALL) #2017 sor és 4 oszlop

#33
COOP_PRV <- subset(worksheet_01, COOP_PRV_CN_NAT == 1 | COOP_PRV_CN_EU_EFTA == 1 | COOP_PRV_CN_NEU_NEFTA == 1 | COOP_PRV_SUPPL_NAT == 1 | COOP_PRV_SUPPL_EU_EFTA == 1 | COOP_PRV_SUPPL_NEU_NEFTA == 1 | COOP_PRV_CLCU_NAT == 1 | COOP_PRV_CLCU_EU_EFTA == 1 | COOP_PRV_CLCU_NEU_NEFTA == 1 | COOP_PRV_COMP_NAT == 1 | COOP_PRV_COMP_EU_EFTA == 1 | COOP_PRV_COMP_NEU_NEFTA == 1 | COOP_PRV_OTH_NAT == 1 | COOP_PRV_OTH_EU_EFTA == 1 | COOP_PRV_OTH_NEU_NEFTA == 1 | COOP_EG_NAT == 1 | COOP_EG_EU_EFTA == 1 | COOP_EG_NEU_NEFTA == 1, c("M092", "COOP_PRV_CN_NAT", "COOP_PRV_CN_EU_EFTA", "COOP_PRV_CN_NEU_NEFTA", "COOP_PRV_SUPPL_NAT", "COOP_PRV_SUPPL_EU_EFTA", "COOP_PRV_SUPPL_NEU_NEFTA", "COOP_PRV_CLCU_NAT", "COOP_PRV_CLCU_EU_EFTA", "COOP_PRV_CLCU_NEU_NEFTA", "COOP_PRV_COMP_NAT", "COOP_PRV_COMP_EU_EFTA", "COOP_PRV_COMP_NEU_NEFTA", "COOP_PRV_OTH_NAT", "COOP_PRV_OTH_EU_EFTA", "COOP_PRV_OTH_NEU_NEFTA"))
dim(COOP_PRV) #936 sor és 16 oszlop

#34
COOP_PRV_CN <- subset(worksheet_01, COOP_PRV_CN_NAT == 1 | COOP_PRV_CN_EU_EFTA == 1 | COOP_PRV_CN_NEU_NEFTA == 1, c("M092", "COOP_PRV_CN_NAT", "COOP_PRV_CN_EU_EFTA", "COOP_PRV_CN_NEU_NEFTA"))
dim(COOP_PRV_CN) #482 sor és 4 oszlop

#35
COOP_PRV_SUPPL <- subset(worksheet_01, COOP_PRV_SUPPL_NAT == 1 | COOP_PRV_SUPPL_EU_EFTA == 1 | COOP_PRV_SUPPL_NEU_NEFTA == 1, c("M092", "COOP_PRV_SUPPL_NAT", "COOP_PRV_SUPPL_EU_EFTA", "COOP_PRV_SUPPL_NEU_NEFTA"))
dim(COOP_PRV_SUPPL) #549 sor és 4 oszlop

#36
COOP_PRV_CLCU <- subset(worksheet_01, COOP_PRV_CLCU_NAT == 1 | COOP_PRV_CLCU_EU_EFTA == 1 | COOP_PRV_CLCU_NEU_NEFTA == 1, c("M092", "COOP_PRV_CLCU_NAT", "COOP_PRV_CLCU_EU_EFTA", "COOP_PRV_CLCU_NEU_NEFTA"))
dim(COOP_PRV_CLCU) #333 sor és 4 oszlop

#37
COOP_PRV_COMP <- subset(worksheet_01, COOP_PRV_COMP_NAT == 1 | COOP_PRV_COMP_EU_EFTA == 1 | COOP_PRV_COMP_NEU_NEFTA == 1, c("M092", "COOP_PRV_COMP_NAT", "COOP_PRV_COMP_EU_EFTA", "COOP_PRV_COMP_NEU_NEFTA"))
dim(COOP_PRV_COMP) #140 sor és 4 oszlop

#38
COOP_PRV_OTH <- subset(worksheet_01, COOP_PRV_OTH_NAT == 1 | COOP_PRV_OTH_EU_EFTA == 1 | COOP_PRV_OTH_NEU_NEFTA == 1, c("M092", "COOP_PRV_OTH_NAT", "COOP_PRV_OTH_EU_EFTA", "COOP_PRV_OTH_NEU_NEFTA"))
dim(COOP_PRV_OTH) #360 sor és 4 oszlop

#39
COOP_EG <- subset(worksheet_01, COOP_EG_NAT == 1 | COOP_EG_EU_EFTA == 1 | COOP_EG_NEU_NEFTA == 1, c("M092", "COOP_EG_NAT", "COOP_EG_EU_EFTA", "COOP_EG_NEU_NEFTA"))
dim(COOP_EG) #320 sor és 4 oszlop

#40
COOP_UNIV <- subset(worksheet_01, COOP_UNIV_NAT == 1 | COOP_UNIV_EU_EFTA == 1 | COOP_UNIV_NEU_NEFTA == 1, c("M092", "COOP_UNIV_NAT", "COOP_UNIV_EU_EFTA", "COOP_UNIV_NEU_NEFTA"))
dim(COOP_UNIV) #364 sor és 4 oszlop

#41
COOP_GOV_RI <- subset(worksheet_01, COOP_GOV_RI_NAT == 1 | COOP_GOV_RI_EU_EFTA == 1 | COOP_GOV_RI_NEU_NEFTA == 1, c("M092", "COOP_GOV_RI_NAT", "COOP_GOV_RI_EU_EFTA", "COOP_GOV_RI_NEU_NEFTA"))
dim(COOP_GOV_RI) #92 sor és 4 oszlop

#42
COOP_PUB_CLCU <- subset(worksheet_01, COOP_PUB_CLCU_NAT == 1 | COOP_PUB_CLCU_EU_EFTA == 1 | COOP_PUB_CLCU_NEU_NEFTA == 1, c("M092", "COOP_PUB_CLCU_NAT", "COOP_PUB_CLCU_EU_EFTA", "COOP_PUB_CLCU_NEU_NEFTA"))
dim(COOP_PUB_CLCU) #94 sor és 4 oszlop

#43
COOP_NPO <- subset(worksheet_01, COOP_NPO_NAT == 1 | COOP_NPO_EU_EFTA == 1 | COOP_NPO_NEU_NEFTA == 1, c("M092", "COOP_NPO_NAT", "COOP_NPO_EU_EFTA", "COOP_NPO_NEU_NEFTA"))
dim(COOP_NPO) #71 sor és 4 oszlop

#60
COOP_NAT <- subset(worksheet_01, COOP_PRV_CN_NAT == 1 | COOP_PRV_SUPPL_NAT == 1 | COOP_PRV_CLCU_NAT == 1 | COOP_PRV_COMP_NAT == 1 | COOP_PRV_OTH_NAT == 1 | COOP_EG_NAT == 1 | COOP_UNIV_NAT == 1 | COOP_GOV_RI_NAT == 1 | COOP_PUB_CLCU_NAT == 1 | COOP_NPO_NAT == 1, c("M092", "COOP_PRV_CN_NAT", "COOP_PRV_SUPPL_NAT", "COOP_PRV_CLCU_NAT", "COOP_PRV_COMP_NAT", "COOP_PRV_OTH_NAT", "COOP_EG_NAT", "COOP_UNIV_NAT", "COOP_GOV_RI_NAT", "COOP_PUB_CLCU_NAT", "COOP_NPO_NAT"))
dim(COOP_NAT) #887 sor és 11 oszlop

#44
COOP_NNAT <- subset(worksheet_01, COOP_PRV_CN_EU_EFTA == 1 | COOP_PRV_CN_NEU_NEFTA == 1 | COOP_PRV_SUPPL_EU_EFTA == 1 | COOP_PRV_SUPPL_NEU_NEFTA == 1 | COOP_PRV_CLCU_EU_EFTA == 1 | COOP_PRV_CLCU_NEU_NEFTA == 1 | COOP_PRV_COMP_EU_EFTA == 1 | COOP_PRV_COMP_NEU_NEFTA == 1 | COOP_PRV_OTH_EU_EFTA == 1 | COOP_PRV_OTH_NEU_NEFTA == 1 | COOP_EG_EU_EFTA == 1 | COOP_EG_NEU_NEFTA == 1 | COOP_UNIV_EU_EFTA == 1 | COOP_UNIV_NEU_NEFTA == 1 | COOP_GOV_RI_EU_EFTA == 1 | COOP_GOV_RI_NEU_NEFTA == 1 | COOP_PUB_CLCU_EU_EFTA == 1 | COOP_PUB_CLCU_NEU_NEFTA == 1 | COOP_NPO_EU_EFTA == 1 | COOP_NPO_NEU_NEFTA == 1, c("M092", "COOP_PRV_CN_EU_EFTA", "COOP_PRV_CN_NEU_NEFTA", "COOP_PRV_SUPPL_EU_EFTA", "COOP_PRV_SUPPL_NEU_NEFTA", "COOP_PRV_CLCU_EU_EFTA", "COOP_PRV_CLCU_NEU_NEFTA", "COOP_PRV_COMP_EU_EFTA", "COOP_PRV_COMP_NEU_NEFTA", "COOP_PRV_OTH_EU_EFTA", "COOP_PRV_OTH_NEU_NEFTA", "COOP_EG_EU_EFTA", "COOP_EG_NEU_NEFTA", "COOP_UNIV_EU_EFTA", "COOP_UNIV_NEU_NEFTA", "COOP_GOV_RI_EU_EFTA", "COOP_GOV_RI_NEU_NEFTA", "COOP_PUB_CLCU_EU_EFTA", "COOP_PUB_CLCU_NEU_NEFTA", "COOP_NPO_EU_EFTA", "COOP_NPO_NEU_NEFTA"))
dim(COOP_NNAT) #488 sor és 21 oszlop

#45
COOP_EU_EFTA <- subset(worksheet_01, COOP_PRV_CN_EU_EFTA == 1 | COOP_PRV_SUPPL_EU_EFTA == 1 | COOP_PRV_CLCU_EU_EFTA == 1 | COOP_PRV_COMP_EU_EFTA == 1 | COOP_PRV_OTH_EU_EFTA == 1 | COOP_EG_EU_EFTA == 1 | COOP_UNIV_EU_EFTA == 1 | COOP_GOV_RI_EU_EFTA == 1 | COOP_PUB_CLCU_EU_EFTA == 1 | COOP_NPO_EU_EFTA == 1, c("M092", "COOP_PRV_CN_EU_EFTA", "COOP_PRV_SUPPL_EU_EFTA", "COOP_PRV_CLCU_EU_EFTA", "COOP_PRV_COMP_EU_EFTA", "COOP_PRV_OTH_EU_EFTA", "COOP_EG_EU_EFTA", "COOP_UNIV_EU_EFTA", "COOP_GOV_RI_EU_EFTA", "COOP_PUB_CLCU_EU_EFTA", "COOP_NPO_EU_EFTA"))
dim(COOP_EU_EFTA) #454 sor és 11 oszlop

#46 
COOP_NEU_NEFTA <- subset(worksheet_01, COOP_PRV_CN_NEU_NEFTA == 1 | COOP_PRV_SUPPL_NEU_NEFTA == 1 | COOP_PRV_CLCU_NEU_NEFTA == 1 | COOP_PRV_COMP_NEU_NEFTA == 1 | COOP_PRV_OTH_NEU_NEFTA == 1 | COOP_EG_NEU_NEFTA == 1 | COOP_UNIV_NEU_NEFTA == 1 | COOP_GOV_RI_NEU_NEFTA == 1 | COOP_PUB_CLCU_NEU_NEFTA == 1 | COOP_NPO_NEU_NEFTA == 1, c("M092", "COOP_PRV_CN_NEU_NEFTA", "COOP_PRV_SUPPL_NEU_NEFTA", "COOP_PRV_CLCU_NEU_NEFTA", "COOP_PRV_COMP_NEU_NEFTA", "COOP_PRV_OTH_NEU_NEFTA", "COOP_EG_NEU_NEFTA", "COOP_UNIV_NEU_NEFTA", "COOP_GOV_RI_NEU_NEFTA", "COOP_PUB_CLCU_NEU_NEFTA", "COOP_NPO_NEU_NEFTA"))
dim(COOP_NEU_NEFTA) #182 sor és 11 oszlop

COOP_PRV_NAT <- subset(worksheet_01, COOP_PRV_CN_NAT == 1 | COOP_PRV_SUPPL_NAT == 1 | COOP_PRV_CLCU_NAT == 1 | COOP_PRV_COMP_NAT == 1 | COOP_PRV_OTH_NAT == 1 | COOP_EG_NAT == 1, c("M092", "COOP_PRV_CN_NAT", "COOP_PRV_SUPPL_NAT", "COOP_PRV_CLCU_NAT", "COOP_PRV_COMP_NAT", "COOP_PRV_OTH_NAT", "COOP_EG_NAT"))
dim(COOP_PRV_NAT) #830 sor és 7 oszlop

COOP_NPRV <- subset(worksheet_01, COOP_UNIV_NAT == 1 | COOP_UNIV_EU_EFTA == 1 | COOP_UNIV_NEU_NEFTA == 1 | COOP_GOV_RI_NAT == 1 | COOP_GOV_RI_EU_EFTA == 1 | COOP_GOV_RI_NEU_NEFTA == 1 | COOP_PUB_CLCU_NAT == 1 | COOP_PUB_CLCU_EU_EFTA == 1 | COOP_PUB_CLCU_NEU_NEFTA == 1 | COOP_NPO_NAT == 1 | COOP_NPO_EU_EFTA == 1 | COOP_NPO_NEU_NEFTA == 1, c("M092", "COOP_UNIV_NAT", "COOP_UNIV_EU_EFTA", "COOP_UNIV_NEU_NEFTA", "COOP_GOV_RI_NAT",  "COOP_GOV_RI_EU_EFTA", "COOP_GOV_RI_NEU_NEFTA", "COOP_PUB_CLCU_NAT", "COOP_PUB_CLCU_EU_EFTA", "COOP_PUB_CLCU_NEU_NEFTA", "COOP_NPO_NAT", "COOP_NPO_EU_EFTA", "COOP_NPO_NEU_NEFTA"))
dim(COOP_NPRV) #420 sor és 13 oszlop

#T22
#17
TUR_EU_EFTA_NOT_ZERO <- subset(worksheet_01, TUR_EU_EFTA != 0, c("M092", "TUR_EU_EFTA"))
dim(TUR_EU_EFTA_NOT_ZERO) #4835 sor és 2 oszlop

#18
TUR_NEU_NEFTA_NOT_ZERO <- subset(worksheet_01, TUR_NEU_NEFTA != 0, c("M092", "TUR_NEU_NEFTA"))
dim(TUR_NEU_NEFTA_NOT_ZERO) #1517 sor és 2 oszlop

#19
TUR_NNAT <- subset(worksheet_01, TUR_EU_EFTA != 0 | TUR_NEU_NEFTA != 0, c("M092", "TUR_EU_EFTA", "TUR_NEU_NEFTA"))
dim(TUR_NNAT) #4964 sor és 3 oszlop

#20
TUR_NAT_NOT_ZERO <- subset(worksheet_01, TUR_NAT != 0, c("M092", "TUR_NAT"))
dim(TUR_NAT_NOT_ZERO) #7218 sor és 2 oszlop

#21
TUR_ONAT <- subset(worksheet_01, TUR_NAT != 0 & TUR_EU_EFTA == 0 & TUR_NEU_NEFTA == 0, c("M092", "TUR_NAT", "TUR_EU_EFTA", "TUR_NEU_NEFTA"))
dim(TUR_ONAT) #2589 sor és 4 oszlop

#69
ENT_EU_EFTA <- subset(worksheet_01, TUR_EU_EFTA != 0, c("M092", "TUR_EU_EFTA"))
dim(ENT_EU_EFTA) #4835 sor és 2 oszlop

#70
ENT_NEU_NEFTA <- subset(worksheet_01, TUR_NEU_NEFTA != 0, c("M092", "TUR_NEU_NEFTA"))
dim(ENT_NEU_NEFTA) #1517 sor és 2 oszlop

#71
ENT_NNAT <- subset(worksheet_01, TUR_EU_EFTA != 0 | TUR_NEU_NEFTA != 0, c("M092", "TUR_EU_EFTA", "TUR_NEU_NEFTA"))
dim(ENT_NNAT) #4964 sor és 3 oszlop

#72
ENT_NAT <- subset(worksheet_01, TUR_NAT != 0, c("M092", "TUR_NAT"))
dim(ENT_NAT) #7218 sor és 2 oszlop

#73
ENT_ONAT <- subset(worksheet_01, TUR_NAT != 0 & TUR_EU_EFTA == 0 & TUR_NEU_NEFTA == 0, c("M092", "TUR_NAT", "TUR_EU_EFTA", "TUR_NEU_NEFTA"))
dim(ENT_ONAT) #2589 sor és 4 oszlop

# #T23
# ENTE_Y_GE2020 <- subset(worksheet_01, as.integer(substr(ENTE_TIME, start = 1, stop = 4)) >= 2020, c("M092", "ENTE_TIME"))
# dim(ENTE_Y_GE2020) #88 sor és 2 oszlop
# 
# ENTE_Y2018T2019 <- subset(worksheet_01, as.integer(substr(ENTE_TIME, start = 1, stop = 4)) == 2018 | as.integer(substr(ENTE_TIME, start = 1, stop = 4)) == 2019, c("M092", "ENTE_TIME"))
# dim(ENTE_Y2018T2019) #185 sor és 2 oszlop
# 
# ENTE_Y2014T2017 <- subset(worksheet_01, as.integer(substr(ENTE_TIME, start = 1, stop = 4)) >= 2014 & as.integer(substr(ENTE_TIME, start = 1, stop = 4)) <= 2017, c("M092", "ENTE_TIME"))
# dim(ENTE_Y2014T2017) #527 sor és 2 oszlop
# 
# ENTE_Y_LE2013 <- subset(worksheet_01, as.integer(substr(ENTE_TIME, start = 1, stop = 4)) <= 2013, c("M092", "ENTE_TIME"))
# dim(ENTE_Y_LE2013) #6754 sor és 2 oszlop

#T24
#74
ENT_TOT_ACQ_MEBTA <- subset(worksheet_01, EXP_TOT_ACQ_MEBTA != 0, c("M092", "EXP_TOT_ACQ_MEBTA"))
dim(ENT_TOT_ACQ_MEBTA) #6762 sor és 2 oszlop

#75
ENT_TOT_MKT <- subset(worksheet_01, EXP_TOT_MKT != 0, c("M092", "EXP_TOT_MKT"))
dim(ENT_TOT_MKT) #3929 sor és 2 oszlop

#76
ENT_TOT_TNG <- subset(worksheet_01, EXP_TOT_TNG != 0, c("M092", "EXP_TOT_TNG"))
dim(ENT_TOT_TNG) #4137 sor és 2 oszlop

#77
ENT_TOT_PRD_DESG <- subset(worksheet_01, EXP_TOT_PRD_DESG != 0, c("M092", "EXP_TOT_PRD_DESG"))
dim(ENT_TOT_PRD_DESG) #729 sor és 2 oszlop

#78
ENT_TOT_SOFT_DBA <- subset(worksheet_01, EXP_TOT_SOFT_DBA != 0, c("M092", "EXP_TOT_SOFT_DBA"))
dim(ENT_TOT_SOFT_DBA) #22347 sor és 2 oszlop

#79
ENT_TOT_IPR <- subset(worksheet_01, EXP_TOT_IPR != 0, c("M092", "EXP_TOT_IPR"))
dim(ENT_TOT_IPR) #724 sor és 2 oszlop

#T25
#22
# ENTGRP_PART <- subset(worksheet_01, ENTGRP_HD_NAT == 1 | ENTGRP_HD_FOR == 1, c("M092", "ENTGRP_HD_NAT", "ENTGRP_HD_FOR"))
# dim(ENTGRP_PART) #2242 sor és 3 oszlop

#T26
#23
INFL_TKNOW <- subset(worksheet_01, INFL_TKNOW_NAT == 1 | INFL_TKNOW_FOR == 1, c("M092", "INFL_TKNOW_NAT", "INFL_TKNOW_FOR"))
dim(INFL_TKNOW) #860 sor és 3 oszlop

#24
INFL_FINRES <- subset(worksheet_01, INFL_FINRES_NAT == 1 | INFL_FINRES_FOR == 1, c("M092", "INFL_FINRES_NAT", "INFL_FINRES_FOR"))
dim(INFL_FINRES) #723 sor és 3 oszlop

#25
INFL_PER <- subset(worksheet_01, INFL_PER_NAT == 1 | INFL_PER_FOR == 1, c("M092", "INFL_PER_NAT", "INFL_PER_FOR"))
dim(INFL_PER) #321 sor és 3 oszlop

#26
INFL_SOURC <- subset(worksheet_01, INFL_SOURC_NAT == 1 | INFL_SOURC_FOR == 1, c("M092", "INFL_SOURC_NAT", "INFL_SOURC_FOR"))
dim(INFL_SOURC) #509 sor és 3 oszlop

#27
OUTFL_TKNOW <- subset(worksheet_01, OUTFL_TKNOW_NAT == 1 | OUTFL_TKNOW_FOR == 1, c("M092", "OUTFL_TKNOW_NAT", "OUTFL_TKNOW_FOR"))
dim(OUTFL_TKNOW) #596 sor és 3 oszlop

#28
OUTFL_FINRES <- subset(worksheet_01, OUTFL_FINRES_NAT == 1 | OUTFL_FINRES_FOR == 1, c("M092", "OUTFL_FINRES_NAT", "OUTFL_FINRES_FOR"))
dim(OUTFL_FINRES) #323 sor és 3 oszlop

#29
OUTFL_PER <- subset(worksheet_01, OUTFL_PER_NAT == 1 | OUTFL_PER_FOR == 1, c("M092", "OUTFL_PER_NAT", "OUTFL_PER_FOR"))
dim(OUTFL_PER) #292 sor és 3 oszlop

#30
OUTFL_SOURC <- subset(worksheet_01, OUTFL_SOURC_NAT == 1 | OUTFL_SOURC_FOR == 1, c("M092", "OUTFL_SOURC_NAT", "OUTFL_SOURC_FOR"))
dim(OUTFL_SOURC) #302 sor és 3 oszlop

#T32
REAS_NMINN_RESC_OTH <- subset(worksheet_01, REAS_NMINN_RESC == 1 | REAS_NMINN_OTH == 1, c("M092", "REAS_NMINN_RESC", "REAS_NMINN_OTH"))
dim(REAS_NMINN_RESC_OTH) #1588 sor és 3 oszlop

REAS_NINN_RESC_OTH <- subset(worksheet_01, REAS_NINN_RESC == 1 | REAS_NINN_OTH == 1, c("M092", "REAS_NINN_RESC", "REAS_NINN_OTH"))
dim(REAS_NINN_RESC_OTH) #1721 sor és 3 oszlop

#T34
# PRD_NEW_MKT <- subset(worksheet_01, (INNO_PRD_GD == 1 | INNO_PRD_SERV == 1) & INNO_PRD_NEW_MKT == 1, c("M092", "INNO_PRD_GD", "INNO_PRD_SERV", "INNO_PRD_NEW_MKT"))
# dim(PRD_NEW_MKT) #687 sor és 4 oszlop # T01-es táblához már elkészült

# PRD_NEW_ENT <- subset(worksheet_01, (INNO_PRD_GD == 1 | INNO_PRD_SERV == 1) & INNO_PRD_NEW_ENT == 1, c("M092", "INNO_PRD_GD", "INNO_PRD_SERV", "INNO_PRD_NEW_ENT"))
# dim(PRD_NEW_ENT) #1339 sor és 4 oszlop # T01-es táblához már elkészült

# INNO_PRD <- subset(worksheet_01, INNO_PRD_GD == 1 | INNO_PRD_SERV == 1, c("M092", "INNO_PRD_GD", "INNO_PRD_SERV"))
# dim(INNO_PRD) #1549 sor és 3 oszlop # T01-es táblához már elkészült

# PRDBPCS_COMPL <- subset(worksheet_01, INNO_PRD_GD == 1 | INNO_PRD_SERV == 1 | INNO_PCS_PRD == 1 | INNO_PCS_LOG == 1 | INNO_PCS_COMM == 1 | INNO_PCS_ACCT == 1 | INNO_PCS_OPROC_EXTREL == 1 | INNO_PCS_WR_DEC_HRM == 1 | INNO_PCS_SLS_SERV == 1 | INNA_COMPL == 1, c("M092", "INNO_PRD_GD", "INNO_PRD_SERV", "INNO_PCS_PRD",  "INNO_PCS_LOG", "INNO_PCS_COMM", "INNO_PCS_ACCT", "INNO_PCS_OPROC_EXTREL", "INNO_PCS_WR_DEC_HRM", "INNO_PCS_SLS_SERV", "INNA_COMPL"))
# dim(PRDBPCS_COMPL) #2261 sor és 11 oszlop # T01-es táblához már elkészült

# PRD_NEW_ENT_ONL <- subset(worksheet_01, (INNO_PRD_GD == 1 | INNO_PRD_SERV == 1) & (INNO_PRD_NEW_ENT == 1 & INNO_PRD_NEW_MKT == 0), c("M092", "INNO_PRD_GD", "INNO_PRD_SERV", "INNO_PRD_NEW_ENT", "INNO_PRD_NEW_MKT"))
# dim(PRD_NEW_ENT_ONL) #862 sor és 5 oszlop # T01-es táblához már elkészült

# INNA_RND_OUT_N_IH <- subset(worksheet_01, INNA_IH_RND == 0 & INNA_RND_CONTR_OUT == 1, c("M092", "INNA_IH_RND", "INNA_RND_CONTR_OUT"))
# dim(INNA_RND_OUT_N_IH) #95 sor és 3 oszlop # T01-es táblához már elkészült