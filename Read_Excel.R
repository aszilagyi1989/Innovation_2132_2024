library("openxlsx")
worksheet_01 <- read.xlsx(paste(getwd(), "/Excel_Files/", "2132_ENG_input_allomany_feldolgozashoz_VEGLEGES_LEGUJJABB.xlsx", sep = ""), sheet = "Munka1")
dim(worksheet_01)#7554 sor és 357 oszlop
#View(worksheet_01)

nrow(worksheet_01[worksheet_01$M065_RETEG1 == "BI", ]) #634
nrow(worksheet_01[worksheet_01$M065_RETEG1 == "KO", ]) #1875
nrow(worksheet_01[worksheet_01$M065_RETEG1 == "KI", ]) #5045

colnames(worksheet_01) <- trimws(colnames(worksheet_01)) #Szóközök eltűntetése az oszlopnevek elejéről és végéről
colnames(worksheet_01) <- gsub(" ", "", colnames(worksheet_01)) #Szóközök eltűntetése az oszlopnevek közepéről pl.: COND_GOUT _AF és ECO_ENO _NSG
#Utóbbi nem sikerült, mert pont lett, ezért:
#colnames(worksheet_01)[41] <- "COND_GOUT_AF"
#colnames(worksheet_01)[204] <- "LEG_IP_PHIC"
#colnames(worksheet_01)[239] <- "ECO_ENO_NSG"


unique(worksheet_01$M065_RETEG1)
unique(worksheet_01$M0581_2J)
#str(worksheet_01, list.len = 100)
options(max.print = 7554)
unique(worksheet_01$M092)

View(worksheet_01[is.na(worksheet_01$TUR22) == TRUE, ])#0 cég üres TUR22 értékkel szerepelt
View(worksheet_01[is.na(worksheet_01$EMP22) == TRUE, ])
View(worksheet_01[is.na(worksheet_01$VGMA001_SULY) == TRUE, ])