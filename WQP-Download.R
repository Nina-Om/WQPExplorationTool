#sampleMedia <- c("Water", "water")
# Characteristictype <- c("Inorganics, Major, Metals", "Inorganics, Major, Non-metals", "Organics, Pesticide", "Pysical)
# siteinfo("Physical", c("37","02"), c("pH","pH, lab"), "01-01-2018" , "02-10-2018")
# Characteristictype ="Physical"; input.states = c("01","04","06","05"); charname = c("pH", "Alkalinity, total"); date1 = "06-01-2019"; date2 = "07-08-2019"
# getWQPData_app$siteInfo
# getWQPData_app$url

# state.code = input.states[1]
library(cdlTools)
getWQPData_app <- function(Characteristictype = "Physical", state.code = c("37","02"), 
                     charname = c("pH","pH, lab"), date1="01-01-2018", date2="11-10-2018") {
   urlCallst <- paste0(paste0("statecode=US%3A", state.code), collapse="&")
   urlCallch <- paste0(paste0("characteristicName=", curlEscape(charname), collapse="&"))
   state.abb <- cdlTools::fips(state.code, to='Abbreviation')
   dates <- format(date1, format="%m-%d-%Y")
   datee <- format(date2, format="%m-%d-%Y")

    urlsites <- paste0("https://www.waterqualitydata.us/Station/search?countrycode=US&",urlCallst,
                       "&minactivities=1&sampleMedia=Water&sampleMedia=water&characteristicType=",
                       curlEscape(Characteristictype),"&",urlCallch,"&startDateLo=",
                       dates,"&startDateHi=",datee,"&mimeType=tsv")
    
    urldata <- paste0("https://www.waterqualitydata.us/Result/search?countrycode=US&",urlCallst,
                      "&minactivities=1&sampleMedia=Water&sampleMedia=water&characteristicType=",
                      curlEscape(Characteristictype),"&",urlCallch,"&startDateLo=",
                      dates,"&startDateHi=",datee,"&mimeType=tsv")
    
    matchReturn <- list(state.code,charname, Characteristictype, dates, datee)
    attr(urldata,"matchReturn") <- matchReturn
    datahead = HEAD(urldata)
    
    Retrieve_Summary <- rbind('Query Time: '=datahead$header$'date',
                       'Total Site Number: '=datahead$header$'total-site-count',
                       'NWIS Site Number: '=datahead$header$'nwis-site-count',
                       'STORET Site Number: '=datahead$header$'storet-site-count',
                       'Total Records: '=datahead$header$'total-result-count',
                       'NWIS Records: '=datahead$header$'nwis-result-count',
                       'STORET Records: '=datahead$header$'storet-result-count')
    
     docs <- getWebServiceData(urlsites)
     headerInfos <- attr(docs, "headerInfo")
     numToBeReturneds <- as.numeric(headerInfos["Total-Site-Count"])
     
       if(!datahead$header$'total-result-count' %in% "0"){
         r=data.frame(Retrieve_Summary)
       } else {
         r=sprintf("The following url returned no data:")
       }
     return(list(r=r,url=urldata[1]))
}


# ret_info=load_clean_WQP(Characteristictype ="Physical", state.code = c("01"), 
#                 charname = c("Alkalinity, total"), date1="05-01-2019" , date2="07-06-2019") 
# by state; all char
load_clean_WQP <- function(Characteristictype, state.code, 
                           charname, date1 , date2) {
  # Characteristictype ="Physical"; state.code = "37"; charname = "Alkalinity, total"; date1 = "01-01-2018"; date2 = "11-10-2018"
  source("Raw_data_cleaning.R")
  state.abb <- cdlTools::fips(state.code, to='Abbreviation')
  urlCallch <- paste0(paste0("characteristicName=",curlEscape(charname),collapse="&"))
     dates <- format(date1, format="%m-%d-%Y")
     datee <- format(date2, format="%m-%d-%Y")
  tryCatch({
    urlsites <- paste0("https://www.waterqualitydata.us/Station/search?countrycode=US&statecode=US%3A",state.code,
                       "&minactivities=1&sampleMedia=Water&sampleMedia=water&characteristicType=",
                       curlEscape(Characteristictype),"&",urlCallch,"&startDateLo=",
                       dates,"&startDateHi=",datee,"&mimeType=tsv")
    
    urldata <- paste0("https://www.waterqualitydata.us/Result/search?countrycode=US&statecode=US%3A",state.code,
                      "&minactivities=1&sampleMedia=Water&sampleMedia=water&characteristicType=",
                      curlEscape(Characteristictype),"&",urlCallch,"&startDateLo=",dates,"&startDateHi=",datee,
                      "&mimeType=csv&zip=yes")
    
    
    # urlsites <- 'https://www.waterqualitydata.us/Station/search?countrycode=US&statecode=US%3A01
    # &minactivities=1&sampleMedia=Water&sampleMedia=water&characteristicType=Physical&characteristicName=
    # pH&startDateLo=05-01-2019&startDateHi=07-06-2019&mimeType=csv&zip=yes'
    # 
    # 
    # urldata <- paste0("https://www.waterqualitydata.us/Result/search?countrycode=US&statecode=US%3A",state.code,
    #                   "&minactivities=1&sampleMedia=Water&sampleMedia=water&characteristicType=",
    #                   curlEscape(Characteristictype),"&characteristicName=",curlEscape(char.name),"&startDateLo=",dates,"&startDateHi=",datee,
    #                   "&mimeType=csv&zip=yes")

     print(urlsites)
     print(urldata)
     site <- data.frame()
     dat <- data.frame()
    
      doc <- getWebServiceData(urlsites)
      site <- try(read.delim(textConnection(doc) ,header=T, sep="\t" , quote = "", stringsAsFactors=FALSE, encoding="UTF-8",
                               fill = TRUE), silent=T)

      # try(read.table(unz(temp1, filename="site.csv"), header=T, sep="," , quote = "\"", stringsAsFactors=FALSE, 
      #                encoding="UTF-8",fill = TRUE), silent=T)
      if (class(site) == "try-error") {
        print(paste0("Caught an error during url read, trying read 'site':", state.abb))
      }
      
      #dat <- importWQP(urldata, FALSE, tz="")
      temp2 <- tempfile()
      download.file(urldata,temp2)
       dat <-  try(read.table(unz(temp2, filename="result.csv"), header=T, sep="," , quote = "\"", stringsAsFactors=FALSE, encoding="UTF-8",fill = TRUE), silent=T)
      unlink(temp2)

      # site <- try(read.delim(retval ,header=T, sep="\t" , quote = "",
      #                        colClasses=c('character'), fill = TRUE), silent=T)
    #dat <- importWQP(urldata, zip=FALSE, tz="America/New_York")
    #write.csv(dat,"dat.csv")
    #actualNumReturned <- nrow(dat)
    
    if (class(dat) == "try-error") {
      print(paste0("Caught an error during url read, trying read 'result':", state.abb))
    }
     sitefile <- tempfile()
     try(write_fst(site, path=paste0(sitefile,"site_",state.abb, "_",Characteristictype,".fst"), compress = 100), silent=T)
     site1 <- read_fst(paste0(sitefile,"site_",state.abb, "_", Characteristictype,".fst"))  
     datfile <- tempfile()
     try(write_fst(dat, path=paste0(datfile,"dat_", state.abb,"_", Characteristictype,".fst"), compress = 100), silent=T)
     dat1 <- try(read_fst(paste0(datfile,"dat_",state.abb, "_", Characteristictype,".fst")), silent=T)
    #join site and data
    cleandata <- Clean_Data(site1=site1, dat1=dat1)
    return(list(site = site1, data = dat1, cleandata = as.data.frame(cleandata), urld=urldata))
  })
     
}
#statecode = c("37","02")
#state.code = statecode[1]
clean_WQP_multistate <- function(Characteristictype, statecode =  input.states, 
                                 charname, date1, date2) {
  data <- data.frame(); data2 <- data.frame(); site2 <- data.frame(); clean2 <- data.frame()

    for (i in 1:length(statecode)) {
    data <- try(load_clean_WQP(Characteristictype, state.code = statecode[i], 
                               charname= charname, date1, date2), silent=T)
     if (class(data) == "try-error") {
      print(paste0("Caught an error during load and clean data:", statecode[i]))
      data = NULL
      }
    clean2 <- rbind(clean2, data$cleandata)   
    site2 <- rbind(site2, data$site)
    data2 <- rbind(data2, data$data)
    }
  
  l <- list(site = site2, data = data2, cleandata = as.data.frame(clean2))
  return(l)
}

# Crop_table <- read.csv("survey_crop_12_18.csv", header = TRUE) ## crop data table from NASS survey
# crop_data <- Crop_table[Crop_table$Commodity %in% c("CORN","COTTON","SOYBEANS","WHEAT","SORGHUM"),]
# crop_data = crop_data[crop_data$County.ANSI]




