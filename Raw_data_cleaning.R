# Clean data by state and characterisctics group
# library(RCurl)
# library(lubridate)
# library(purrr)
# library(magrittr)
# library(httr)
# library(psych)
# library(reshape2)
# library(data.table)
# library(tidyverse)
# library(tidyr)
# library(lattice)
# library(plotly)
# library(dplyr)
# library(devtools)
# library(fpc)
# library(bindrcpp)
# library(mgcv)
# library(scales)
# library(Hmisc)
# library(fst)
# library(cdlTools)
# library(totalcensus)
# library(dataRetrieval)

Clean_Data <- function(site1, dat1){
  
  finalsite <- as.data.frame(site1) 
  finalsite$Stabbr <- cdlTools::fips(finalsite$StateCode, to='Abbreviation')
  finalsite$StName <- cdlTools::fips(finalsite$Stabbr, to='Name')
  
 
  finalsite <- finalsite %>% dplyr::filter(LongitudeMeasure < -65 & 
                                             LongitudeMeasure > -130 & 
                                             LatitudeMeasure > 25 &
                                             LatitudeMeasure < 52)

  finaldata <- dat1 %>% left_join(finalsite, by = c("MonitoringLocationIdentifier" = "MonitoringLocationIdentifier"))

  
  finaldata$Year <- substr(finaldata$ActivityStartDate, start = 1, stop = 4)
  finaldata$Month <- as.numeric(substr(finaldata$ActivityStartDate, start = 6, stop = 7))
  finaldata$ResultMeasureValue <- as.numeric(as.character(finaldata$ResultMeasureValue))
  finaldata$DetectionQuantitationLimitMeasure.MeasureValue <- as.numeric(as.character(finaldata$DetectionQuantitationLimitMeasure.MeasureValue))
  #Assigning the negative and NAN reported values as "Not Detented"
   finalSubset1 <- finaldata
  #   mutate(ResultDetectionConditionText2 = ifelse((is.na(ResultMeasureValue) | ResultMeasureValue < 0)  , "Not Detected", "Detected"))
  # Unit conversion and cleaning
  finalSubset1$ResultMeasure.MeasureUnitCode <- gsub(" ", replacement = "", finalSubset1$ResultMeasure.MeasureUnitCode,fixed = TRUE)
  finalSubset1$ResultMeasure.MeasureUnitCode <- gsub(pattern="mg/L", replacement = "mg/l", finalSubset1$ResultMeasure.MeasureUnitCode,fixed = TRUE)
  finalSubset1$ResultMeasure.MeasureUnitCode <- gsub(pattern="ug/L", replacement = "ug/l", finalSubset1$ResultMeasure.MeasureUnitCode,fixed = TRUE)
  finalSubset1$ResultMeasure.MeasureUnitCode <- gsub(pattern="ng/L", replacement = "ng/l", finalSubset1$ResultMeasure.MeasureUnitCode,fixed = TRUE)
  finalSubset1$ResultMeasure.MeasureUnitCode <- gsub(pattern="ng/L", replacement = "ng/l", finalSubset1$ResultMeasure.MeasureUnitCode,fixed = TRUE)
  finalSubset1$ResultMeasure.MeasureUnitCode <- sub("^$", "No Unit", finalSubset1$ResultMeasure.MeasureUnitCode)
  #Result_detection_condition_text2 <- c("Detected", "Not Detected")
  cols.num <- c("LatitudeMeasure","LongitudeMeasure", "WellDepthMeasure.MeasureValue", "WellHoleDepthMeasure.MeasureValue")
  finalSubset1[cols.num] <- sapply(finalSubset1[cols.num], as.numeric)
  #Warning: Error in : Evaluation error: missing values and NaN's not allowed if 'na.rm' is FALSE.
  #109: <Anonymous>
    # finalSubset1=finalSubset1[!is.na(finalSubset1$LongitudeMeasure),]
    # finalSubset1=finalSubset1[!is.na(finalSubset1$LatitudeMeasure),]
  #finalSubset1[is.na(finalSubset1)] <- ""
  return(finalSubset1)
}

# 
# Clean_Data2<- function(site1, dat1){
#   
#   finalsite <- as.data.frame(site1) 
#   finalsite$Stabbr <- fips(finalsite$StateCode, to='Abbreviation')
#   finalsite$StName <- fips(finalsite$Stabbr, to='Name')
#   
#   
#   finalsite <- finalsite %>% dplyr::filter(LongitudeMeasure < -65 & 
#                                              LongitudeMeasure > -130 & 
#                                              LatitudeMeasure > 25 &
#                                              LatitudeMeasure < 52)
#   
#   finaldata <- dat1 %>% left_join(finalsite, by = c("MonitoringLocationIdentifier" = "MonitoringLocationIdentifier"))
#   
#   
#   finaldata$Year <- substr(finaldata$ActivityStartDate, start = 1, stop = 4)
#   finaldata$Month <- as.numeric(substr(finaldata$ActivityStartDate, start = 6, stop = 7))
#   finaldata$ResultMeasureValue <- as.numeric(as.character(finaldata$ResultMeasureValue))
#   finaldata$DetectionQuantitationLimitMeasure.MeasureValue <- as.numeric(as.character(finaldata$DetectionQuantitationLimitMeasure.MeasureValue))
#   #Assigning the negative and NAN reported values as "Not Detented"
#   finalSubset1 <- finaldata %>%
#    # mutate(ResultDetectionConditionText2 = ifelse((is.na(ResultMeasureValue) | ResultMeasureValue < 0)  , "Not Detected", "Detected"))
#   # Unit conversion and cleaning
#   finalSubset1$ResultMeasure.MeasureUnitCode <- gsub(" ", replacement = "", finalSubset1$ResultMeasure.MeasureUnitCode,fixed = TRUE)
#   finalSubset1$ResultMeasure.MeasureUnitCode <- gsub(pattern="mg/L", replacement = "mg/l", finalSubset1$ResultMeasure.MeasureUnitCode,fixed = TRUE)
#   finalSubset1$ResultMeasure.MeasureUnitCode <- gsub(pattern="ug/L", replacement = "ug/l", finalSubset1$ResultMeasure.MeasureUnitCode,fixed = TRUE)
#   finalSubset1$ResultMeasure.MeasureUnitCode <- gsub(pattern="ng/L", replacement = "ng/l", finalSubset1$ResultMeasure.MeasureUnitCode,fixed = TRUE)
#   finalSubset1$ResultMeasure.MeasureUnitCode <- gsub(pattern="ng/L", replacement = "ng/l", finalSubset1$ResultMeasure.MeasureUnitCode,fixed = TRUE)
#   finalSubset1$ResultMeasure.MeasureUnitCode <- sub("^$", "No Unit", finalSubset1$ResultMeasure.MeasureUnitCode)
#   #Result_detection_condition_text2 <- c("Detected", "Not Detected")
#   cols.num <- c("LatitudeMeasure","LongitudeMeasure", "WellDepthMeasure.MeasureValue", "WellHoleDepthMeasure.MeasureValue")
#   finalSubset1[cols.num] <- sapply(finalSubset1[cols.num], as.numeric)
#   #Warning: Error in : Evaluation error: missing values and NaN's not allowed if 'na.rm' is FALSE.
#   #109: <Anonymous>
#   # finalSubset1=finalSubset1[!is.na(finalSubset1$LongitudeMeasure),]
#   # finalSubset1=finalSubset1[!is.na(finalSubset1$LatitudeMeasure),]
#   #finalSubset1[is.na(finalSubset1)] <- ""
#   return(finalSubset1)
# }
#   