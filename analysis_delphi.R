
#read DRESS

DRESS$DI <- NA
DRESS$Median<- NA

for( i in 1:nrow(DRESS)) {
  
  score<-DRESS[i,2:55]
  
  LIPR <- quantile(score, c(0.33), na.rm = TRUE,names = TRUE,type = 6)[[1]]
  UIPR <- quantile(score, c(0.66), na.rm = TRUE,names = TRUE,type = 6)[[1]]
  
  IPR <- UIPR - LIPR
  
  IPRCP <- (UIPR + LIPR) / 2 
  
  Asymmetry_Index <- abs(5-IPRCP)
  
  IPRAS <- 2.35 + (1.5 * Asymmetry_Index)
  
  DI <- IPR/IPRAS
  
  DRESS[i,]$DI <- round(DI,3)
  
  }

#
 for (k in 1:nrow(DRESS)) {
   DRESS[k,]$Median <- round(median(as.numeric(DRESS[k,2:55]), na.rm = TRUE),3)
 }


DRESS$aggrement <- "no disagreement"
DRESS[DRESS$DI>1,]$aggrement <- "disagreement"


DRESS$appropriateness <- "appropriate"
DRESS[DRESS$Median<7,]$appropriateness <- "uncertain"

table(DRESS$aggrement,DRESS$appropriateness)

DRESS$final<- paste(DRESS$aggrement,DRESS$appropriateness)


library("writexl")
write_xlsx(DRESS,"/Users/milad/Documents/delphiDRESS/results.xlsx")

