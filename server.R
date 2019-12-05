source("Raw_data_cleaning.R")
source("WQP-Download.R")
source("genFunctions.R")
source("Curve-fitting-func.R")

#setwd("/opt/home/u963834/ShinyApps/WQP_SSD_Tool/")
#path2 <- "/opt/home/u963834/Interpolation_WQ/urlread/DatabyState/CleanData/"
path2 <- "./urlread/DatabyState/CleanData/"

featureList1 <- c("ProviderName.x2",
                  "OrganizationFormalName.x",
                  "ActivityMediaSubdivisionName2",
                  "Year",
                  "Stabbr",
                  "ResultAnalyticalMethod.MethodName2",
                  "USGSPCode",
                  "ResultMeasure.MeasureUnitCode2",
                  "ResultSampleFractionText2")

featureList2 <- c("ProviderName.x2",
                  "MonitoringLocationIdentifier",
                  "OrganizationFormalName.x",
                  "ActivityMediaSubdivisionName2",
                  "Year",
                  "Stabbr",
                  "ResultAnalyticalMethod.MethodName2",
                  "USGSPCode",
                  "ResultMeasure.MeasureUnitCode2",
                  "ResultSampleFractionText2")


detect_lim = c("Below Daily Detection Limit","Below Detection Limit","Below Long-term Blank-basd Dt Limit","Below Method Detection Limit",
               "Below Reporting Limit","Below Sample-specific Detect Limit","Below System Detection Limit","Between Inst Detect and Quant Limit",
               "Detected Not Quantified","Present Below Quantification Limit","Not Detected",
               "Not Detected at Detection Limit","Not Detected at Reporting Limit")

colsnames = c("MonitoringLocationIdentifier", "CharacteristicName", "ActivityMediaSubdivisionName2", "ResultMeasure.MeasureUnitCode2",
            "Stabbr", "ResultDetectionConditionText","ResultSampleFractionText2","ResultAnalyticalMethod.MethodName",
            "ResultCommentText", "ActivityCommentText")

ff1 <- list(
  family = "Arial, sans-serif",
  size = 16,
  color = "lightgrey")

ff2 <- list(
  family = "Arial, sans-serif",
  size = 20,
  color = "black")

a <- list(
  titlefont = ff1,
  showticklabels = TRUE,
  tickangle = 0,
  tickfont = ff2,
  showgrid=FALSE)

data_pr <- function(data1, pr) {
  #data2 <- dplyr::filter(data1, CharacteristicName %in% input$char_map)
  data2 <- data1 %>% 
    ungroup() %>%
    dplyr::filter(ResultMeasureValue > 0 & !is.na(ResultMeasureValue))%>%
    dplyr::group_by(CharacteristicName, MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure)
  data3 <- dplyr::summarise(data2, prValues = quantile(ResultMeasureValue, probs= pr), count = n()) 
  # data2$Zscore <- spatialEco::outliers(data2$medValues)
  # data2 <- data2[-which(data2$Zscore>9.9),]
  return(data3)
}
# 
# filelist = read.csv(paste0("CleanData/", "Organics, Pesticide",".csv"))
# filelist=filelist %>% dplyr::select(-X)
# filelist <- dplyr::filter(filelist, !st %in% c("AK","AS","GU","MP","PR","VI","UM","FM","MH","PW","HI","DC"))
# stname = sort(as.character(filelist$stname))
# 
# readchar1 <-  lapply(paste0(path2, "clean_", 
#                             as.character(filelist$st), "_", 
#                             "Organics, Pesticide", ".fst"), 
#                      function(x){read.fst(x, columns =c("CharacteristicName", "Year"))
#                      }) %>% rbindlist  
# 
# 
# charname <- c("", sort(as.character(unique(readchar1$CharacteristicName))))
# charname = "Prometryn"
# characteristics.name <- dplyr::filter(readchar1, CharacteristicName %in% charname)
# yr <- c("", sort(as.numeric(unique(characteristics.name$Year))))
# 
# filtered <- dplyr::filter(filelist, stname %in% stname)
# stlist=filtered$st
# data.f=data.frame()
# 
# for (i in 1:length(stlist)){
# readchar <- read.fst(paste0(path2, "clean_", 
#                             as.character(filelist$st[i]), "_", 
#                             "Organics, Pesticide", ".fst")) 
# data.f1 <- readchar[readchar$CharacteristicName %in% charname,]
# data.f2 <- dplyr::filter(data.f1, data.f1$Year %in% yr)
# data.f <- rbind(data.f, data.f2)
# print(NROW(data.f))
# }
# 
# finalsubset = data.f

function(input, output, session) {
   session$onSessionEnded(function() {
     stopApp()
   })
   
  data_of_click <- reactiveValues(clickedMarker = list())
  
  filelist <- reactive({
   st1 = read.csv(paste0("CleanData/", input$Chartype,".csv"))
   st1 <- dplyr::filter(st1, !st %in% c("AK","AS","GU","MP","PR","VI","UM","FM","MH","PW","HI","DC"))
   st1=st1 %>% dplyr::select(-X)
   print(st1$st)
   return(st1)
  })

  output$stsel <- renderUI({
     #filtered <- dplyr::filter(filelist(), !st %in% c("AK","AS","GU","MP","PR","VI","UM","FM","MH","PW"))
    pickerInput("stname", "State Name", 
                choices= sort(as.character(filelist()$stname)), 
                options= list(`actions-box` = TRUE),multiple = T)
  }) 
  
  f01 <- eventReactive(input$goButton0, {
    filtered = dplyr::filter(filelist(), stname %in% input$stname)
    print(input$stname)
    filtered
  })
  
  f1 <- eventReactive(input$goButton0, {
    readchar1 <-  lapply(paste0(path2, "clean_", 
                               as.character(f01()$st), "_", 
                               input$Chartype, ".fst"), 
                        function(x){read.fst(x, columns =c("CharacteristicName", "Year"))
                        }) %>% rbindlist  
    print(NROW(readchar1))
    as.data.frame(readchar1)
  })

  output$charsel <- renderUI({
   req(f1())
    characteristics.name <- c("", sort(as.character(unique(f1()$CharacteristicName))))
    selectizeInput('charname', 'A.I. Name', 
                   characteristics.name, 
                   multiple = T)
  })
  
  output$yearsel <- renderUI({
    req(input$charname)
    characteristics.name <- dplyr::filter(f1(), CharacteristicName %in% input$charname)
    yr <- c("", sort(as.numeric(unique(characteristics.name$Year))))
    print(yr)
    pickerInput("year", "Year", 
                choices= yr, 
                options= list(`actions-box` = TRUE), multiple = T)
  })

  # output$subdiv.sel <- renderUI({
  #   req(input$year)
  #   st <- input$stname
  #   readdata <-  lapply(paste0(path=path2, "clean_", 
  #                              fips(st, to='Abbreviation'),"_",
  #                              input$Chartype, ".fst"), 
  #                       function(x){read.fst(x, columns =c("CharacteristicName", "Year", "ActivityMediaSubdivisionName"))
  #                       }) %>% rbindlist  
  #   
  #   characteristics.name <- dplyr::filter(readdata, CharacteristicName %in% input$charname)
  #   yr <- characteristics.name <- dplyr::filter(readdata, Year %in% input$year)
  #   subdiv <- c(sort(unique(as.character(yr$ActivityMediaSubdivisionName))))
  #   pickerInput('subdiv', 'Water Type', subdiv, options = list(`actions-box` = TRUE), multiple = T)
  # })

  output$loadsel <- renderUI({
      req(input$charname)
    # print(input$year)
    selectizeInput("load", "Load data from:", c("", "Water Quality Portal", "Local Server"), multiple = F)
  })

  #outputOptions(output, 'data_check', suspendWhenHidden=FALSE)

  finalsubset <- reactive({
    req(input$load)
    if(input$load =="Local Server") {
      
      filterdata4 <- reactive({
        req(input$charname)
        filtered <- dplyr::filter(filelist(), stname %in% input$stname)
        stlist=filtered$st
        data.f=data.frame()
        
        for (i in 1:length(stlist)){
     
        readchar <- read.fst(paste0(path2, "clean_", 
                                   as.character(stlist[i]), "_", 
                                   input$Chartype, ".fst")) 

        data.f1 <- readchar[readchar$CharacteristicName %in% input$charname,]
          data.f2 <- dplyr::filter(data.f1, data.f1$Year %in% input$year)
          
          #data.f2$statename <-  cdlTools::fips(data.f2$StateCode, to='Name')
          
          data.f <- rbind(data.f, data.f2)
          print(NROW(data.f))
        }
        withProgress(message = paste0("Loadind state:" , stlist[i], ", Cummulative records:", NROW(data.f)),
                     detail = 'This may take a while...', value = 0, {
                       for (i in 1:10) {
                         incProgress(1/10)
                         Sys.sleep(0.25)}})
        message = NROW(data.f)
        as.data.frame(data.f)
      })
      
      Output <- filterdata4()
      
    }else{
      
      WQP_data <- reactive({
        withProgress(message = 'Loading Data3',
                     detail = 'This may take a while...', value = 0, {
                       for (i in 1:15) {
                         incProgress(1/15)
                         Sys.sleep(0.25)
                       }
                     })
        
        input.states <-  str_pad(cdlTools::fips(input$stname, to = "FIPS"), 2, pad = "0")
        Outputlist <- clean_WQP_multistate(input$Chartype, input.states, input$charname, input$date1, input$date2)
        Output1 <- as.data.frame(Outputlist$cleandata)
        return(Output1)
      })
      
      Output <- WQP_data()
    }
    print("This is what finalsubset returns")
      return(Output)
  })
  
  ret_info <- eventReactive(input$goButton, {
    withProgress(message = 'Loading Data',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                 })

      input.states <- str_pad(cdlTools::fips(input$stname, to = "FIPS"), 2, pad = "0")
      
      ret <- getWQPData_app(input$Chartype, input.states, input$charname, input$date1, input$date2)
      return(ret)
  })
  
  output$siteinfo <- renderPrint({
    req(input$load)
    print(ret_info()$r)
      })
  
  output$URL <- renderText({
    req(input$load)
    ret_info()$url
  })

  non_detect_method <- reactive({
    method <- switch(input$ND_method,
                     '1' = "Default Values",
                     '2' = "Set non-detections to zero",
                     '3' = "Set non-detections to the Limit of Detection",
                     '4' = "Set non-detections to the 1/2 of Limit of Detection")
    return(method)
  })
  
  flaged_data <- eventReactive(input$goButton2,{
    withProgress(message = 'Loading Data',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                 })
    
    req(finalsubset())
    vec = dplyr::select(finalsubset(), c(-ActivityIdentifier, -ActivityTypeCode))
    du = which(duplicated(vec))
    dup <- finalsubset() %>%
    dplyr::mutate(Duplicated = ifelse(rownames(finalsubset()) %in% as.character(du), "Yes", "No"))
    
    u <- dup %>%
      dplyr::mutate(dtec_condition = ifelse(ResultDetectionConditionText %in% c(detect_lim, "Not Detected"), "ND", "D"))
    
    m <- u %>%
      dplyr::mutate(dt_Missing = ifelse((((is.na(ResultMeasureValue) | ResultMeasureValue < 0) & 
                                             dtec_condition %in% "D")), "Yes", "No"))

 
    um <- m %>%
      dplyr::mutate(u_Missing = ifelse(((is.na(ResultMeasureValue) | ResultMeasureValue <= 0) & 
                                        (dtec_condition %in% "ND") &
                                        (is.na(DetectionQuantitationLimitMeasure.MeasureValue) | 
                                           DetectionQuantitationLimitMeasure.MeasureValue < 0 |
                                           DetectionQuantitationLimitMeasure.MeasureValue == "")), "Yes", "No"))
    
    m2 <- um %>%
      dplyr::mutate(Missing = ifelse((u_Missing %in% "Yes" | dt_Missing %in% "Yes"), "Yes", "No"))
    # Set NULL ActivityMediaSubdivisionName to 'Unknown'
  
    
    
    data2 = m2 %>% dplyr::mutate(ActivityMediaSubdivisionName2 = ifelse(ActivityMediaSubdivisionName %in% "", "Unknown", ActivityMediaSubdivisionName))
  
  return(data2)
  })

  
  output$all_data_table <- DT::renderDataTable(server = T,{
    req(flaged_data())
    dat = flaged_data() %>% 
      dplyr::select(ProviderName.x, OrganizationFormalName.x, MonitoringLocationIdentifier, MonitoringLocationTypeName,
                                                ActivityMediaSubdivisionName2, ActivityStartDate, LongitudeMeasure, LatitudeMeasure,
                                                Year, Stabbr, ResultDetectionConditionText, ResultAnalyticalMethod.MethodName,
                                                CharacteristicName, Month, ResultSampleFractionText, ResultMeasure.MeasureUnitCode, 
                                                ResultMeasureValue, ResultCommentText, ActivityCommentText, 
                                                DetectionQuantitationLimitMeasure.MeasureValue,
                                                DetectionQuantitationLimitMeasure.MeasureUnitCode, USGSPCode)
    DT::datatable(dat,
                  rownames=FALSE,
                  selection = "single", 
                  extensions = c('Buttons', "KeyTable"),
                  options = list(autoWidth = TRUE,
                                 columnDefs = list(list(width = '200px', targets = "_all")),
                    stateSave = TRUE, 
                    dom = 'Bfrtip', 
                    buttons = c('colvis', 'csv', 'excel'),
                    fillContainer=TRUE))
  })
  
  output$dup_table <- DT::renderDataTable(server = T,{
    req(flaged_data())
    dat = flaged_data() %>% 
      dplyr::filter(flaged_data()$Duplicated %in% "Yes" ) %>% 
      dplyr::select(ProviderName.x, OrganizationFormalName.x,MonitoringLocationIdentifier, MonitoringLocationTypeName,
                    ActivityMediaSubdivisionName2, ActivityStartDate, LongitudeMeasure, LatitudeMeasure,
                    Year, Stabbr, ResultDetectionConditionText, ResultAnalyticalMethod.MethodName,
                    CharacteristicName, Month, ResultSampleFractionText, ResultMeasure.MeasureUnitCode, 
                    ResultMeasureValue,
                    DetectionQuantitationLimitMeasure.MeasureValue,
                    DetectionQuantitationLimitMeasure.MeasureUnitCode, USGSPCode)
    DT::datatable(dat,
                  rownames=FALSE,
                  selection = "single", 
                  extensions = c('Buttons', "KeyTable"),
                  options = list( autoWidth = TRUE, columnDefs = list(list(width = '200px', targets = "_all")),
                    stateSave = TRUE, 
                    dom = 'Bfrtip', 
                    buttons = c('colvis', 'csv', 'excel'),
                    fillContainer=TRUE))
  })
  
  output$missing_table <- DT::renderDataTable(server = FALSE,{
    req(flaged_data())
    dat = flaged_data() %>% dplyr::filter(flaged_data()$Missing %in% "Yes") %>% 
      dplyr::select(ProviderName.x, OrganizationFormalName.x,MonitoringLocationIdentifier, MonitoringLocationTypeName,
                      ActivityMediaSubdivisionName2, ActivityStartDate, LongitudeMeasure, LatitudeMeasure,
                      Year, Stabbr, ResultDetectionConditionText, ResultAnalyticalMethod.MethodName,
                      CharacteristicName, Month, ResultSampleFractionText, ResultMeasure.MeasureUnitCode, 
                      ResultMeasureValue,
                      DetectionQuantitationLimitMeasure.MeasureValue,
                      DetectionQuantitationLimitMeasure.MeasureUnitCode, USGSPCode)
    DT::datatable(dat,
                  rownames=FALSE,
                  selection = "single", 
                  extensions = c('Buttons', "KeyTable"),
                  options = list(autoWidth = TRUE,columnDefs = list(list(width = '200px', targets = "_all")),
                    stateSave = TRUE, 
                    dom = 'Bfrtip', 
                    buttons = c('colvis', 'csv', 'excel'),
                    fillContainer=TRUE))
  })

  data2 <- reactive({
    req(flaged_data())
    dat = flaged_data() %>% 
      dplyr::filter(flaged_data()$Duplicated %in% "No") 
    # %>%
    #   dplyr::select(ProviderName.x, OrganizationFormalName.x,MonitoringLocationIdentifier, MonitoringLocationTypeName,
    #                   ActivityMediaSubdivisionName2, ActivityStartDate, LongitudeMeasure, LatitudeMeasure,
    #                   Year, Stabbr, ResultDetectionConditionText, ResultAnalyticalMethod.MethodName,
    #                   CharacteristicName, Month, ResultSampleFractionText, ResultMeasure.MeasureUnitCode, 
    #                   ResultMeasureValue, 
    #                   DetectionQuantitationLimitMeasure.MeasureValue,
    #                   DetectionQuantitationLimitMeasure.MeasureUnitCode, USGSPCode, Duplicated, Missing, dtec_condition)

    if(input$ND_method == 1){
      data2=dat
      data2=data2 %>% dplyr::filter(!data2$ResultDetectionConditionText %in% c(detect_lim,"Not Detected"))
    
      } else if(input$ND_method == 2){
        data2=dat
      data2[data2$ResultDetectionConditionText %in% c(detect_lim,"Not Detected"), "ResultMeasureValue"] <- 0
      data2[data2$ResultDetectionConditionText %in% c(detect_lim,"Not Detected"), "ResultMeasure.MeasureUnitCode"] <- 
        data2[data2$ResultDetectionConditionText %in% c(detect_lim,"Not Detected"), "DetectionQuantitationLimitMeasure.MeasureUnitCode"]
    
      } else if(input$ND_method == 3){
        data2=dat
        data2[data2$ResultDetectionConditionText %in% c(detect_lim,"Not Detected"), "ResultMeasureValue"] <- 
          1*as.numeric(data2[data2$ResultDetectionConditionText %in% c(detect_lim,"Not Detected"), "DetectionQuantitationLimitMeasure.MeasureValue"])
        data2[data2$ResultDetectionConditionText %in% c(detect_lim,"Not Detected"), "ResultMeasure.MeasureUnitCode"] <- 
          data2[data2$ResultDetectionConditionText %in% c(detect_lim,"Not Detected"), "DetectionQuantitationLimitMeasure.MeasureUnitCode"]
      } else {
        data2=dat
      data2[data2$ResultDetectionConditionText %in% c(detect_lim,"Not Detected"), "ResultMeasureValue"] <- 
        0.5*as.numeric(data2[data2$ResultDetectionConditionText %in% c(detect_lim,"Not Detected"), "DetectionQuantitationLimitMeasure.MeasureValue"])
      data2[data2$ResultDetectionConditionText %in% c(detect_lim,"Not Detected"), "ResultMeasure.MeasureUnitCode"] <- 
        data2[data2$ResultDetectionConditionText %in% c(detect_lim,"Not Detected"), "DetectionQuantitationLimitMeasure.MeasureUnitCode"]
    }
    data2 = data2 %>% dplyr::mutate(ResultMeasure.MeasureUnitCode2 = ifelse(ResultMeasure.MeasureUnitCode %in% "", "Unknown", ResultMeasure.MeasureUnitCode))
    data2 = data2 %>% dplyr::mutate(ProviderName.x2 = ifelse(ProviderName.x %in% "", "Unknown", ProviderName.x))
    data2 = data2 %>% dplyr::mutate(ResultSampleFractionText2 = ifelse(ResultSampleFractionText %in% "", "Unknown", ResultSampleFractionText))
    data2 = data2 %>% dplyr::mutate(ResultAnalyticalMethod.MethodName2 = ifelse(ResultAnalyticalMethod.MethodName %in% "", "Unknown", ResultAnalyticalMethod.MethodName))
    data2
  })
  
  output$undetections_table <- DT::renderDataTable(server = T,{
    req(data2())
    data=data2()
    dat = data %>% 
      #dplyr::filter(data$dtec_condition %in% "ND") %>%
      dplyr::select(MonitoringLocationIdentifier, CharacteristicName, ResultMeasureValue,
                    ResultMeasure.MeasureUnitCode,
                    DetectionQuantitationLimitMeasure.MeasureValue,
                    DetectionQuantitationLimitMeasure.MeasureUnitCode,
                    ResultDetectionConditionText, USGSPCode, dtec_condition)
    DT::datatable(dat,
                  rownames=FALSE,
                  selection = "single", 
                  extensions = c('Buttons', "KeyTable"),
                  options = list(autoWidth = TRUE,columnDefs = list(list(width = '200px', targets = "_all")),
                    stateSave = TRUE, 
                    dom = 'Bfrtip', 
                    buttons = c('colvis', 'csv', 'excel'),
                    fillContainer=TRUE))
  })
  
  
  ##################################################################################################
  
  output$group.sel1 <- renderUI({
    selectInput("group1", "Summerise data by:", 
                choices = colsnames,
                selected = "MonitoringLocationIdentifier")})
  
  output$group.sel2 <- renderUI({
    selectInput("group2", "Summerise data by:", 
                choices=colsnames,
                selected="CharacteristicName")})
  
  # flaged_data2 <- eventReactive(input$goButton3,{
  #   req(data2())
  #   withProgress(message = 'Calculating',
  #                detail = 'This may take a while...', value = 0, {
  #                  for (i in 1:5) {
  #                    incProgress(1/5)
  #                    Sys.sleep(0.25)
  #                  }
  #                })
  #   data2()
  # })
  
  plot.df <- reactive({
    req(data2(), input$group1)
    plot.df = data2() %>% dplyr::filter(data2()$Missing %in% "No")
    
    n1 = plot.df[, input$group1]
    n2 = plot.df[, input$group2]
    plot.df <- data.frame(feature1 = n1,
                          feature2 = n2,
                          plot.df[,!names(plot.df) %in% 
                                    c(input$group1, input$group2)])
    write.csv(plot.df,"data.csv")
    plot.df= plot.df %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(plot.df$feature1, plot.df$feature2) %>%
      dplyr::summarise(mean = mean(ResultMeasureValue),
                       sd = sd(ResultMeasureValue), 
                       IQR = IQR(ResultMeasureValue), 
                       min = min(ResultMeasureValue),
                       q10 = quantile(ResultMeasureValue, 0.10),
                       q25 = quantile(ResultMeasureValue, 0.25), 
                       med = median(ResultMeasureValue), 
                       q75 = quantile(ResultMeasureValue, 0.75),
                       q90 = quantile(ResultMeasureValue, 0.9),
                       max = max(ResultMeasureValue), 
                       records = n()
      )
    plot.df
  })
  
  output$summary_table <- DT::renderDataTable(server = T,{
    req(plot.df())
    withProgress(message = 'Calculating',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:5) {
                     incProgress(1/5)
                     Sys.sleep(0.25)
                   }
                   })
   

    DT::datatable(plot.df(),
                  rownames=FALSE,
                  selection = "single", 
                  extensions = c('Buttons', "KeyTable"),
                  options = list(
                    autoWidth = TRUE,
                    columnDefs = list(list(width = '200px', targets = "_all")),
                    stateSave = TRUE, 
                    dom = 'Bfrtip', 
                    buttons = c('colvis', 'csv', 'excel'),
                    fillContainer=TRUE))
    
  })

  output$summary_table2 <- DT::renderDataTable({
    dt = length(flaged_data()[flaged_data()$ResultDetectionConditionText %in% detect_lim, "ResultMeasureValue"])
    mis = length(flaged_data()[flaged_data()$Missing %in% "Yes", "ResultMeasureValue"])
    du = length(flaged_data()[flaged_data()$Duplicated %in% "Yes", "ResultMeasureValue"])
    table = data.frame(rbind("Non Detection:"=dt, "Missing values:"=mis, "Duplicated:"=du))
    names(table) = c("Count")
    DT::datatable(table)
  })
  
  output$downloadData3 <- downloadHandler(
    filename = paste0("Summary.csv"),
    content = function(file) {
      write.csv(plot.df(), file, row.names = FALSE)
    })
  
  
  
  get.boxplot <- function(plot.df, title1){
    p <- ggplot() +
      geom_boxplot(data=plot.df, aes(plot.df[,1], as.numeric(ResultMeasureValue), fill=plot.df[,1])) +
      #geom_jitter(color="grey", alpha=0.3, size=0.9) +
      scale_fill_viridis(discrete=TRUE) +
      theme_ipsum() +
      theme(legend.position="none") +
      ggtitle(title1) +
      coord_flip() +
      xlab("") +
      ylab("log scale") +
      scale_y_log10()
    p
  }
  
  boxplots <- eventReactive(input$goButton11, {
    plot.df = data2() %>% dplyr::filter(data2()$Missing %in% "No")
    n1 = plot.df[, input$group1]
    n2 = plot.df[, input$group2]
    plot.df <- data.frame(feature1 = n1,
                          feature2 = n2,
                          plot.df[,!names(plot.df) %in% 
                                    c(input$group1, input$group2)])

    p = get.boxplot(plot.df, 
                            title1=paste0("Boxplots of ", input$charname, " grouped by ",  input$group1))
    p
  })
  
  output$boxplot <- renderPlot({
    req(boxplots())
    boxplots()
  })
  
  
  datatable1 <- reactive({
    req(flaged_data())
    data=flaged_data()
    data=data[!is.na(as.numeric(data$LongitudeMeasure)),]
    data=data[!is.na(as.numeric(data$LatitudeMeasure)),]
    
    data = as.data.frame(data[,c("ProviderName.x", "OrganizationFormalName.x","MonitoringLocationIdentifier", 
                                 "MonitoringLocationTypeName",
                                 "ActivityMediaSubdivisionName2", "ActivityStartDate",
                                 "LongitudeMeasure", "LatitudeMeasure",
                                 "Stabbr", "CharacteristicName", "USGSPCode")])
    
    data1 = data %>% group_by(ProviderName.x, OrganizationFormalName.x,CharacteristicName, 
                              Stabbr, MonitoringLocationTypeName, ActivityMediaSubdivisionName2,
                              MonitoringLocationIdentifier, USGSPCode,
                              LongitudeMeasure, LatitudeMeasure) %>%
      dplyr::summarise(records=n())
    data2=as.data.frame(data1)
    data2
  })
  
  # site table
  output$site_table <- DT::renderDataTable(
  datatable1(),server = FALSE,
                  caption = htmltools::tags$caption(
                    style = 'caption-side: top; text-align: left;',
                    #'Table 1: ',
                     htmltools::em('Summary of stations information. Use above buttons to save table or display selected columns.')),
                  rownames=FALSE,
                  selection = "single", 
                  extensions = c('Buttons', "KeyTable"),
                  options = list(autoWidth = TRUE, columnDefs = list(list(width = '200px', targets = "_all")),
                                  stateSave = TRUE, 
                                  dom = 'Bfrtip', 
                                  buttons = c('colvis', 'csv', 'excel'),
                                  fillContainer=TRUE))
  

###################################################################################
  output$mymap1 <- renderLeaflet({
    req(datatable1())
    withProgress(message = 'Displaying Data',
                 detail = 'This may take a while...', value = 0, {
                   for (i in 1:5) {
                     incProgress(1/5)
                     Sys.sleep(0.25)
                     #return() # this is shortening the time to return the header info
                   }
                 })
    
    data=datatable1()
    data=data[!is.na(data$LongitudeMeasure),]
    data=data[!is.na(data$LatitudeMeasure),]
    
    leaflet() %>%
      addTiles()  %>%
      setView(lng = -98.5, lat = 39.8, zoom = 4)%>%
      addCircles(data = data,
                       radius = 2000,
                       lat = as.numeric(data$LatitudeMeasure),
                       lng = as.numeric(data$LongitudeMeasure),
                       fillColor = "white",
                       fillOpacity = 1,
                       #color =  ~pal1(medValues),
                       weight = 2,
                       stroke = T,
                       layerId = as.numeric(rownames(data)),
                       popup = ~paste(data$MonitoringLocationIdentifier)
                 )
    })
  
  observeEvent(input$mymap1_marker_click, {
    clickId <- input$mymap1_marker_click$id
    dataTableProxy("site_table") %>%
      selectRows(which(datatable1()$id == clickId)) %>%
      selectPage(which(input$site_table_rows_all == clickId) %/% input$site_table_state$length + 1)
  })

  prev_row <- reactiveVal()
  
  observeEvent(input$site_table_rows_selected, {
    row_selected = datatable1()[input$site_table_rows_selected,]
    proxy <- leafletProxy('mymap1')
    print(row_selected)
    proxy %>%
      addAwesomeMarkers(popup=as.character(row_selected$MonitoringLocationIdentifier),
                        layerId = as.character(row_selected$id),
                        lng=row_selected$LongitudeMeasure,
                        lat=row_selected$LatitudeMeasure)

    # Reset previously selected marker
    if(!is.null(prev_row())){
      proxy %>% clearMarkers() %>% 
        addMarkers(popup=as.character(row_selected$MonitoringLocationIdentifier),
                   layerId = as.character(row_selected$id),
                   lng=row_selected$LongitudeMeasure,
                   lat=row_selected$LatitudeMeasure)
    }
    # set new value to reactiveVal
    prev_row(row_selected)
  })
  
  observeEvent(input$goButton4,{
    proxy <- leafletProxy("mymap1")
    proxy %>%
      clearMarkers() %>%
    clearShapes() 
  })
  


  filteredData_prj <- reactive({ #map
    req(data2())
    data=data2()
    data1=data[!is.na(data$LongitudeMeasure),]
    data1=data[!is.na(data$LatitudeMeasure),]
    coordinates(data1) <- cbind(data1$LongitudeMeasure, data1$LatitudeMeasure)
    nad83 <- "+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"  
    mrc <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
    proj4string(data1) = CRS(nad83)
    data1 <- spTransform(data1, CRS(mrc))
  data1
  })
  
  ## outlier detection, no outlier in filteredData2
  # filteredData2 <- eventReactive(input$display,{
  #   req(filteredData())
  #   filteredData() %>%
  #     group_by(ResultDetectionConditionText2, StateCode , ActivityMediaSubdivisionName)
  #     # nest() %>%
  #     # mutate(data = map(data, outlier_fun)) %>%
  #     # unnest()
  # })
  
  #Save data on PC, filteredData includes the outliers
  output$downloadData <- downloadHandler(
    filename = paste0("WQ_RawData.csv"),
    content = function(file) {
      write.csv(finalsubset(), file, row.names = FALSE)
    })
  
  output$downloadData2 <- downloadHandler(
    filename = paste0("WQ_cleanData.csv"),
    content = function(file) {
      write.csv(data2(), file, row.names = FALSE)
    })
  
  # output$mytable <- DT::renderDataTable({
  #   req(flaged_data())
  #   flaged_data()
  # },
  # filter = 'top',
  # options = list(
  #   dom = 'T<"clear">lfrtip',
  #   autoWidth = TRUE,
  #   columnDefs = list(list(width = '20%', targets = list(2,3,4))),
  #   deferRender=TRUE,
  #   scrollX=TRUE,scrollY=400,
  #   scrollCollapse=TRUE)
  # )
  

  
  # output$summary_tot <- renderTable({
  #   flaged_data() %>%
  #     ungroup() %>%
  #     dplyr::summarise(med = median(ResultMeasureValue), mean=mean(ResultMeasureValue), 
  #                      sd=sd(ResultMeasureValue), IQR = IQR(ResultMeasureValue), min = min(ResultMeasureValue), 
  #                      max=max(ResultMeasureValue), count = n())
  # })
  # 
  # output$n_stations <- renderTable({
  #   req(flaged_data())
  #   length(unique(flaged_data()$MonitoringLocationIdentifier))
  # })
  # 
  # output$n_Outliers <- renderTable({
  #   nrow(dplyr::filter(flaged_data())) - nrow(flaged_data())
  # })
  
  ################################################# section one #################################################
  # list to store the selections for tracking
  
  # output$charmap_sel <- renderUI({
  #   req(f1())
  #   characteristics.name <- c("", sort(as.character(unique(f1()$CharacteristicName))))
  #   selectizeInput('charname', 'A.I. Name', 
  #                  characteristics.name, 
  #                  multiple = T)
  # })
    
    
  output$charmap_sel <- renderUI({
    req(data2())
    characteristics.name <- c("", sort(as.character(unique(data2()$CharacteristicName))))
    print(characteristics.name)
    #selectizeInput('year', 'Year', yr, multiple = T)
    selectInput("charnamemap", "Select A.I. for Map:", choices= characteristics.name, multiple = F)
  })
  
  filteredData_map <- reactive({## Remove zerros and "NA" to plot ECDF
    req(data2(), input$charnamemap)
    data=data2()
    data = dplyr::filter(data2(), CharacteristicName %in% input$charnamemap)
    data = data_pr(data, as.numeric(input$pr))
    data = data[!is.na(data$prValues),]
    data=data[!is.na(data$LongitudeMeasure),]
    data=data[!is.na(data$LatitudeMeasure),]
    data$id <- as.numeric(rownames(data))
    data$secondLocationID <- paste(as.character(data$id), "_selectedLayer", sep="")
    data
  })
  # 
  coordinates1 <- reactive({
    SpatialPointsDataFrame(filteredData_map()[,c('LongitudeMeasure', 'LatitudeMeasure')], filteredData_map())
  })

  ################################################# section two #################################################
  # base map
  output$mymap <- renderLeaflet({
    # withProgress(message = 'Displaying sites',
    #              detail = 'This may take a while...', value = 0, {
    #                for (i in 1:15) {
    #                  incProgress(1/15)
    #                  Sys.sleep(0.25)
    #                }
    #              })
    req(input$charnamemap)
    print(input$charnamemap)
    data=dplyr::filter(filteredData_map(), CharacteristicName %in% input$charnamemap)
    #data=filteredData_map()
    print(data)
    pal1 <- colorNumeric(c("green","red"), data$prValues, na.color = "transparent", reverse = T)
    
    p <- leaflet() %>%
      addTiles()  %>%
      setView(lng = -98.5, lat = 39.8, zoom = 4)%>%
      addCircles(data = data,
                 radius = 2000,
                 lat = as.numeric(data$LatitudeMeasure),
                 lng = as.numeric(data$LongitudeMeasure),
                 fillColor = "white",
                 fillOpacity = 1,
                 color =  ~pal1(prValues),
                 weight = 2,
                 stroke = T,
                 layerId = as.character(data$id),
                 popup = ~paste(round(prValues,3), "_", MonitoringLocationIdentifier),
                 highlightOptions = highlightOptions(color = "mediumseagreen",
                                                     opacity = 1.0,
                                                     weight = 2,
                                                     bringToFront = TRUE)
                 ) %>%
      addLegend(pal= pal1, values = data$prValues,  title= paste0(input$pr*100, "th percentile,", input$charnamemap, " values")) %>%
      addDrawToolbar(
        targetGroup='Selected',polylineOptions=FALSE, markerOptions = FALSE,
        polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0,color = 'white',weight = 3)),
        rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0,color = 'white',weight = 3)),
        #circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0,color = 'white',weight = 3)),
        editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
    p
  })
  
  #color = ~pal4(Zscore), popup = ~paste(round(Zscore,3)
  ############################################### section three #################################################
  observeEvent(input$mymap_draw_new_feature,{
    #Only add new layers for bounded locations. 
    #Find locations inside a polygon, square, or circle drawn with leaflet.extras drawing tools on a Shiny Leaflet map.
    found_in_bounds <- findLocations(shape = input$mymap_draw_new_feature
                                     , location_coordinates = coordinates1() # all map locations
                                     , location_id_colname = "id") #returned locations
    #
    for(id in found_in_bounds){
      if(id %in% data_of_click$clickedMarker){
        # don't add id
      } else {
        # add id
        data_of_click$clickedMarker <- append(data_of_click$clickedMarker, id, 0)
      }
    }
    #

    selected <- subset(filteredData_map(), id %in% 
                         data_of_click$clickedMarker)
    
    proxy <- leafletProxy("mymap")

    proxy %>% addCircles(data = selected,
                         radius = 2000,
                         lat = selected$LatitudeMeasure,
                         lng = selected$LongitudeMeasure,
                         fillColor = "wheat",
                         fillOpacity = 1,
                         color = "mediumseagreen",
                         weight = 3,
                         stroke = T,
                         layerId = as.character(selected$secondLocationID),
                         highlightOptions = highlightOptions(color = "red",
                                                             opacity = 1.0,
                                                             weight = 2,
                                                             bringToFront = TRUE)
                         )

     # output$mytable <- renderDT(
     #   dplyr::filter(data2(), MonitoringLocationIdentifier %in% selected$MonitoringLocationIdentifier)
     #   , server=T, selection = 'none', editable = T)
     
     output$featureInput1 <- renderUI({
       selectInput(inputId = "featureInput1", 
                           label = "Select first feature", 
                           choices = featureList1, 
                           selected = "Year")
       })
     
     output$featureInput2 <- renderUI({selectInput(inputId = "featureInput2", 
                           label = "Select second feature", 
                           choices = featureList2, 
                           selected = "MonitoringLocationIdentifier")
     })

     output$ui <- renderUI({
       datarange <- dplyr::filter(data2(),
                                  MonitoringLocationIdentifier %in% selected$MonitoringLocationIdentifier &
                                    CharacteristicName %in% input$charnamemap)
       numericInput("obs1", "Insert Maximum Value", value = max(datarange$ResultMeasureValue))
     })
     
     p <- eventReactive(input$Plot, {
       
       plot.summ <- 
         dplyr::filter(filteredData_map(), MonitoringLocationIdentifier %in% selected$MonitoringLocationIdentifier)
       
       p = ggplot(plot.summ, aes(prValues), fill = rainbow(10)) +
         geom_histogram(stat = "bin", bins=10, show.legend = TRUE, binwidth =50) +
         theme(plot.title = element_text(size=14, hjust=0), 
               legend.position="none", axis.text=element_text(size=12),
               plot.background = element_rect(fill = "lavender"), 
               axis.title=element_text(size=10)) +
         labs(x = paste0(input$pr *100, "th percentile of measured values"), y ="Frequency")
       
       p
       })

     output$plot_map1 <- renderPlot({
              p()
       })

     # Coupled event 1
     output$plot_map2 <- renderPlotly({
       req(input$featureInput1)
       withProgress(message = 'Plotting ...',
                    detail = 'This may take a while...', value = 0, {
                      for (i in 1:25) {
                        incProgress(1/25)
                        Sys.sleep(0.25)
                      }
                    })
       
       plot.summ3 <- 
         dplyr::filter(data2(), ResultMeasureValue < as.numeric(input$obs1) & 
                         MonitoringLocationIdentifier %in% selected$MonitoringLocationIdentifier & 
                         CharacteristicName %in% input$charnamemap)
       
       plot.summ3 <<- plot.summ3
       
       plot.df <- data.frame(plot.summ3[,input$featureInput1],
                             #plot.summ[,input$featureInput2],
                             plot.summ3$CharacteristicName,
                             plot.summ3$ResultMeasureValue)
       
       colnames(plot.df) <- c("x1", "Class", "conc")
       
       plot_ly(plot.df, x = ~x1, y = ~conc, type = "box", source = "AA", color = ~x1, showlegend = F) %>%
         layout(yaxis = list(title = "Measured Value"),
                xaxis = list(title = input$featureInput1))
        })

     output$plot_map3 <- renderPlotly({
       req(input$featureInput2)
       plot.summ <- dplyr::filter(data2(), ResultMeasureValue < as.numeric(input$obs1) &
                                  MonitoringLocationIdentifier %in% selected$MonitoringLocationIdentifier &
                                    CharacteristicName %in% input$charnamemap)
       plot.summ <<- plot.summ
       plot.df2 <- data.frame(plot.summ[,input$featureInput1],
                             plot.summ[,input$featureInput2],
                             plot.summ$CharacteristicName,
                             plot.summ$ResultMeasureValue)
       
       colnames(plot.df2) <- c("x1", "x2", "Class", "conc")
       
       ggplot(plot.df2, aes(x2, conc, label = x2))+
         geom_boxplot()+ facet_grid(x1 ~ .)+
         theme(strip.text.x = element_text(size=8),
               axis.text.x = element_text(angle = 45),
               strip.text.y = element_text(size=12, face="bold"),
               strip.background = element_rect(colour="red", fill="#CCCCFF"))+
         labs(x = input$featureInput2, y ="Measured value")
         ggplotly()
     })
     # 
     # 
     data11 <- reactive({
       dat= dplyr::filter(data2(), ResultMeasureValue < as.numeric(input$obs1) ,
                          MonitoringLocationIdentifier %in% as.character(selected$MonitoringLocationIdentifier) ,
                          CharacteristicName %in% input$charnamemap)
     })
     
     output$provider.sel <- renderUI({
       prov = c("",unique(as.character(data11()$ProviderName.x2)))
       selectizeInput("provider", "Select Provider", prov, multiple = T)
     })
     
     finaldata111 <- reactive({
       req(input$provider)
       dat = as.data.frame(dplyr::filter(data11(), ProviderName.x2 %in% as.character(input$provider)))
       dat
     })
     
     output$orgname.sel <- renderUI({
       org = c("",unique(as.character(finaldata111()$OrganizationFormalName.x)))
       pickerInput("orgname", "Select Organization", 
                   choices= org, 
                   options= list(`actions-box` = TRUE), multiple = T)
     })

     finaldata121 <- reactive({
       req(input$orgname)
       dat = as.data.frame(dplyr::filter(finaldata111(), OrganizationFormalName.x %in% as.character(input$orgname)))
       dat
     })
     
     output$station.sel <- renderUI({
       station = unique(as.character(finaldata121()$MonitoringLocationIdentifier))
       pickerInput("station", "Select Stations", 
                   choices= station, 
                   options= list(`actions-box` = TRUE),multiple = T)
     })
     # 
     finaldata122 <- reactive({
       req(input$station)
       dat = as.data.frame(dplyr::filter(finaldata121(), MonitoringLocationIdentifier %in% as.character(input$station)))
       dat
     })

     output$subdiv.sel <- renderUI({
       subdiv1 = c("",unique(as.character(finaldata122()$ActivityMediaSubdivisionName2)))
       selectizeInput("subdiv", "Select Type of Water", subdiv1, multiple = T)
     })
     # 
     finaldata12 <- reactive({
       req(finaldata122())
       dat = as.data.frame(dplyr::filter(finaldata122(), ActivityMediaSubdivisionName2 %in% as.character(input$subdiv)))
       dat
     })
     # 
     output$unicode.sel <- renderUI({
       units = c("",unique(as.character(finaldata12()$ResultMeasure.MeasureUnitCode2)))
         selectizeInput("unicode", "Select Unit of Measure", units, multiple = T)
       })

     finaldata13 <- reactive({
       req(finaldata12())
       dat = as.data.frame(dplyr::filter(finaldata12(), ResultMeasure.MeasureUnitCode2 %in% as.character(input$unicode)))
       dat
     })
     
     output$fraction.sel <- renderUI({
       fr = c("",unique(as.character(finaldata13()$ResultSampleFractionText2)))
       selectizeInput("fraction", "Select Sample Fraction", fr, multiple = T)
     })
     
     finaldata14 <- reactive({
       req(input$fraction)
       dat = as.data.frame(dplyr::filter(finaldata13(), ResultSampleFractionText2 %in% as.character(input$fraction)))
       dat
     })
     
     output$method.sel <- renderUI({
       meth = c("",unique(as.character(finaldata14()$ResultAnalyticalMethod.MethodName2)))
       pickerInput("method", "Select Analytical Method", 
                   choices= meth, 
                   options= list(`actions-box` = TRUE), multiple = T)
     })
     
     finaldata15 <- reactive({
       req(input$method)
       dat = as.data.frame(dplyr::filter(finaldata14(), ResultAnalyticalMethod.MethodName2 %in% as.character(input$method)))
       dat
     })
     
     output$final_filtered_data <- DT::renderDataTable({
          req(finaldata15())

       
       # dat= dat %>% dplyr::filter(
       #                            MonitoringLocationIdentifier %in% as.character(input$station), 
       #                             ActivityMediaSubdivisionName2 %in% as.character(input$subdiv),
       #                             ResultMeasure.MeasureUnitCode2 %in% as.character(input$unicode),
       #                             ResultSampleFractionText2 %in% as.character(input$fraction),
       #                             ResultAnalyticalMethod.MethodName2 %in% as.character(input$method))
       
       
       DT::datatable(finaldata15(), 
                     caption = htmltools::tags$caption(
                       style = 'caption-side: top; text-align: left;',
                       #'Table 1: ',
                       htmltools::em('Table of filtered data. Use above buttons to save table or display selected columns.')),
                     rownames=FALSE,
                     selection = "single", 
                     extensions = c('Buttons', "KeyTable"),
                     options = list(                    autoWidth = TRUE,
                                                        columnDefs = list(list(width = '200px', targets = "_all")),
                                                        stateSave = TRUE, 
                                                        dom = 'Bfrtip', 
                                                        buttons = c('colvis', 'csv', 'excel'),
                                                        fillContainer=TRUE))
     })
     
  })#mymap_draw_new_feature
  


  
  ############################################### section four ##################################################
     observeEvent(input$mymap_draw_deleted_features, {
    # loop through list of one or more deleted features/ polygons
    for(feature in input$mymap_draw_deleted_features$features){
      # get ids for locations within the bounding shape

        bounded_layer_ids <- findLocations(shape = feature
                                         , location_coordinates = SpatialPointsDataFrame(filteredData_map()[,c('LongitudeMeasure', 'LatitudeMeasure')], filteredData_map())
                                         , location_id_colname = "secondLocationID")
      # remove second layer representing selected locations
      proxy2 <- leafletProxy("mymap")
      proxy2 %>% removeShape(layerId = as.character(bounded_layer_ids))
      first_layer_ids <- subset(filteredData_map(), secondLocationID %in% bounded_layer_ids)$id
      data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker %in% first_layer_ids]}

    # output$mytable <- renderDT(filteredData_map(), selection = 'none', editable = T, server=T)
     })
  ###################################################################
  # SSD Tool ########################################################
  ###################################################################
  # 1. Data preparation
  SSDdata <- reactive({
    req(input$file1)
    inFile <- input$file1
    Data <- read_excel(inFile$datapath)
    colnames(Data)<- c("X to include","Taxa Grouping","Species", "Concentration")
    Data <- Data[order(Data$Concentration),]
    Data$fraq <- ppoints(Data[[4]], 0.5)
    Data$fraq <- as.numeric(format(round(Data$fraq, 2), nsmall = 2))
    print(head(Data))
    return(as.data.frame(Data))
  })
  
  # output$downTemplate <- downloadHandler(
  #   filename = function(){
  #     paste("Template-Download","xlsx",sep=".")
  #   },
  #   content = function(con){
  #     file.copy("ToxicityData2.xlsx", con)
  #   })
  
  output$downTemplate <- downloadHandler(
    filename = function(){
      paste("Toxicity_Template-Download","xlsx",sep=".")
    },
    content = function(con){
      file.copy("ToxicityData2.xlsx", con)
    })
  
  output$downTemplate2 <- downloadHandler(
    filename = function(){
      paste("Exposure_Template-Download","csv",sep=".")
    },
    content = function(con){
      file.copy("exposuredata.csv", con)
    })

  
  output$SSDtable <- DT::renderDataTable(
    SSDdata())
  
  # Generate a summary of the dataset
  output$summary.tox <- renderPrint({
    toxdata <- cbind(SSDdata()[4], SSDdata()[5])
    summary(toxdata)
  })
  
  # SSD Options
  output$unitselssd <- renderUI({
    selectizeInput("unitssd", "Units (Required)", c("", "ng/l", "ug/l", "mg/l", "lbs/A"), multiple = F)
  })
  
  output$plotsel <- renderUI({
    selectizeInput("plotpos", "Plotting Position ", c("", "Hazen", "Filiben's"), multiple = F)
  })
  
  # Unit Conversion 
  unitfactor <- reactive({
    req(input$unitssd)
    as.character(input$unitssd)
    unifact <- 1
    if(input$unitssd %in% c("mg/l")) {
      unifact <- 0.001}
    else{
      if(input$unitssd %in% c("ng/l")) {
        unifact <- 1000}
    }
    print(unifact)
    return(unifact)
  })
  
  output$samplsizesel <- renderUI({
    selectizeInput("samplsize", "Bootstrap Iterations", c("", 500, 1000, 5000, 10000), multiple = F)
  })
  
  # SSD Calculation
  fit.fun <- eventReactive(input$dist, {
    fitfun <- Curve.fitting(SSDdata(), input$unitssd, input$conf, input$scale, as.numeric(input$exposure), as.numeric(input$samplsize))
    fitfun
  })
  
  SSDfit <- reactive({
    req(fit.fun())
    aa <- fit.fun()$SSD.table
    print(aa[1:6])
    return(aa)
  })
  
  output$FitTable <- DT::renderDataTable({          
    SSDfit() %>% format(digits=2)}, 
    options = list(
      dom = 'T<"clear">lfrtip',
      autoWidth = TRUE,
      columnDefs = list(list(width = '20%', targets = list(2,3,4))),
      deferRender=TRUE,
      scrollX=TRUE,scrollY=400,
      pageLength = 5,
      scrollCollapse=TRUE),
    caption="Species Sensitivity Distributions with upper and lower confidence interval")

  fit.pars <- eventReactive(input$act2, {
    fit.fun()$fit.parameters
  })
  
  # Models parameters
  output$fitpars.table <- DT::renderDataTable(
    fit.pars(), rownames = TRUE)
  
  # GoF tests
  gof.pars <- eventReactive(input$act3, {
    fit.fun()$gof.test
  })
  
  output$gofPars <- DT::renderDataTable(
    datatable(gof.pars() , rownames = TRUE) %>%
      formatRound(columns=c("Anderson-Darling","Kolmogrov-Smirnov","Chi-Squared", "Chisq p-value"), digits=3)
  )
  
  # SSE and MSE
  dev.pars <- eventReactive(input$act4, {
    fit.fun()$sse
  })
  
  output$sse.mse <- DT::renderDataTable(
    dev.pars(), 
    rownames = TRUE)
  
  # HC% and HC50
  hc2 <- eventReactive(input$act5, {
    fit.fun()$df.hc
  })
  
  output$hcTabl <- DT::renderDataTable(
    hc2() %>% format(digits=3))
  
  # Fraction Affected
  fa <- eventReactive(input$dist, {
    fit.fun()$df.fa
  })
  
  #### Plot SSD
  output$modelsel <- renderUI({
    selectizeInput("model", "Select Distribution Name", 
                   c("", "Normal", "Logistic", "Extreme.Value", "Gumbel", "Weibull"), multiple = F)
  })
  
  # "hc" for plot
  hc <- reactive({
    fit.fun()$df.hc
  })
  
  # Data for SSD plots
  distdata <- reactive({
    colname1 <- input$model
    colname3 <- paste0(input$model,".lwr")
    colname4 <- paste0(input$model,".upr")
    data <- cbind(SSDfit()[,colname1], SSDfit()$pro, SSDfit()[,colname3], SSDfit()[,colname4])
    colnames(data) <- c("dat", "pro", "lwr", "upr")
    return(as.data.frame(data))
  })
  
  output$taxasel <- renderUI({
    taxa.group <- c(sort(unique(as.character(SSDdata()$"Taxa Grouping"))))
    pickerInput("taxa", "Add label to plot", choices=taxa.group, options = list(`actions-box` = TRUE),multiple = T)
  })
  
  filteredData3 <- reactive({## Remove zerros and "NA" to plot ECDF
    req(input$file2)
    inFile <- input$file2
    Data <- read.csv(inFile$datapath)
    Data %>%
      dplyr::filter(!is.na (ResultMeasureValue)) %>%
      dplyr::filter(ResultMeasureValue != 0)
    Data
  })

  ## Plots #########################################################################################################
  output$plot_USGS <- renderPlot({
    req(filteredData3())
    ggplot(filteredData3(), aes(ResultMeasureValue)) + 
      stat_ecdf(geom = "point", pad = FALSE, col="dodgerblue") + 
      scale_x_log10(name = paste0("Concentration (ug/l)")) +
      scale_y_continuous(name = "Likelihood of exceeding exposure treshold")+
      ggtitle(paste0("Distribution of ",input$chem, " detects in surface water samples.")) + 
      theme(plot.title = element_text(size=14, hjust=0), legend.position="none", axis.text=element_text(size=12),
            plot.background = element_rect(fill = "lavender"), axis.title=element_text(size=12,face="bold"))
  }, height = 400, width = 600)
  
  plotl <- reactive({
    req(input$model, unitfactor())
    #savedhc <<- hc()
    n <- 6 # count of dist. models + 1
    xint <- as.numeric(c(hc()[[input$model]][[1]], hc()[[input$model]][[4]]))
    x.lim <- c(0.01*min(SSDfit()[2:n]), max(SSDfit()[2:n]))
    print(xint, x.lim)
    
    ggplot() + 
      stat_ecdf(data=filteredData3(), aes(ResultMeasureValue*unitfactor()), geom = "point", pad = FALSE) + 
      stat_ecdf(data = SSDdata(), aes(Concentration) ,geom = "point", pad = FALSE, col="dodgerblue") + 
      geom_line(data=distdata(),  aes(x= dat, y=pro), colour="magenta3") +
      geom_line(data=distdata(), aes(x= lwr, y=pro), colour="magenta3", lty="dotted") +
      geom_line(data=distdata(), aes(x= upr, y=pro), colour="magenta3", lty="dotted") +
       geom_vline(aes(xintercept = xint), linetype = "dashed", colour = "lightslateblue") +
       geom_vline(aes(xintercept = as.numeric(input$exposure)), colour = "lightslateblue") +
       geom_text(aes(x=xint[1], y= 0.75*max(SSDdata()$fraq), label=paste0("HC5=", xint[1])), colour="black", angle=90, text=element_text(size=11)) +
       geom_text(aes(x=xint[2], y= 0.5*max(SSDdata()$fraq), label=paste0("HC50=", xint[2])), colour="black", angle=90, text=element_text(size=11)) +
      geom_text(aes(x=as.numeric(input$exposure), y= 0.25*max(SSDdata()$fraq), label=paste0("FA=", as.numeric(fa()[[input$model]])*100,"%")), 
                colour="black", angle=90, text=element_text(size=11)) +
      geom_text(data = SSDdata()[SSDdata()$"Taxa Grouping" %in% input$taxa,], 
                aes(x = Concentration, y = fraq, label = Species), hjust = 1, size = 4) +
      scale_x_log10(limits=x.lim) +
      #ggtitle(paste0("Water quality concentration (", input$unitssd,"), Toxicity concentration (", input$unitssd, ")")) + 
      theme(plot.title = element_text(size=11, hjust=0), legend.position="none", axis.text=element_text(size=10),
            axis.title=element_text(size=10))+
      labs(x = paste0("Concentration"), y ="Fraction of species affected")
  })
  
  plota <- reactive({
    req(input$model)
    xint <- as.numeric(c(hc()[[input$model]][[1]], hc()[[input$model]][[4]]))
    print(xint)
    ggplot() + 
      stat_ecdf(data=filteredData3(), aes(ResultMeasureValue*unitfactor()), geom = "point", pad = FALSE) + 
      stat_ecdf(data = SSDdata(), aes(Concentration) ,geom = "point", pad = FALSE, col="dodgerblue") + 
      geom_line(data=distdata(),  aes(x= dat, y=pro), colour="magenta3") +
      geom_line(data=distdata(), aes(x= lwr, y=pro), colour="magenta3", lty="dotted") +
      geom_line(data=distdata(), aes(x= upr, y=pro), colour="magenta3", lty="dotted") +
       geom_vline(aes(xintercept = xint), linetype = "dashed", colour = "lightslateblue") +
       geom_vline(aes(xintercept = as.numeric(input$exposure)), colour = "lightslateblue") +
       geom_text(aes(x=xint[1], y= 0.75*max(SSDdata()$fraq), label=paste0("HC5=", xint[1])), colour="black", angle=90, text=element_text(size=11)) +
       geom_text(aes(x=xint[2], y= 0.5*max(SSDdata()$fraq), label=paste0("HC50=", xint[2])), colour="black", angle=90, text=element_text(size=11)) +
      geom_text(aes(x=as.numeric(input$exposure), y= 0.25*max(SSDdata()$fraq), label=paste0("FA=", as.numeric(fa()[[input$model]])*100,"%")), 
                colour="black", angle=90, text=element_text(size=11)) +
      scale_x_discrete(limits=c(-1*max(SSDdata()$Concentration), max(SSDdata()$Concentration))) +
      geom_text(data = SSDdata()[SSDdata()$"Taxa Grouping" %in% input$taxa,], 
                aes(x = Concentration, y = fraq, label = Species), hjust = 1, size = 4) +
      #ggtitle(paste0("Water quality concentration (", input$unitssd,"), Toxicity concentration (", input$unitssd, ")")) + 
      theme(plot.title = element_text(size=11, hjust=0), legend.position="none", axis.text=element_text(size=10),
            axis.title=element_text(size=10))+
      labs(x = paste0("Concentration"), y ="Fraction of species affected")
  })
  
  output$plot <- renderPlot({
    if(input$scale == 2){
      plotl() + ggtitle(paste0("Water quality concentration (", input$unitssd,"), Toxicity concentration (", input$unitssd, ")"))
    }else{
      plota() + ggtitle(paste0("Water quality concentration (", input$unitssd,"), Toxicity concentration (", input$unitssd, ")"))
    }
  })
  ########################################################################################################### 
  #### Generating R Markdown report #########################################################################
  ###########################################################################################################
  wrapper <- function(x, ...) 
  {
    paste(strwrap(x, ...), collapse = "\n")
  }
  
  plot_USGS2 <- reactive({
    req(filteredData3())
    p <- ggplot(filteredData3(), aes(ResultMeasureValue)) + 
      stat_ecdf(geom = "point", pad = FALSE, col="dodgerblue") + 
      scale_x_log10(name = paste0("Concentration (", input$unitssd,")")) +
      scale_y_continuous(name = "Likelihood of exceeding exposure treshold")+
      #labs(caption = "Source: the Lahman baseball database")+
      ggtitle(wrapper("Figure 6. Distribution of ", input$chem, " detects in surface water samples.", width = 100)) + 
      theme(plot.title = element_text(size=11, hjust=0), legend.position="none", axis.text=element_text(size=10),
            axis.title=element_text(size=10))
    return(p)
  })
  
  ssd.plotl <- reactive({
    x.lim <- c(min(SSDfit()[2:6]), max(SSDfit()[2:6]))
    p2 <- ggplot() + 
      stat_ecdf(data = SSDdata(), aes(Concentration) ,geom = "point", pad = FALSE, col="dodgerblue") + 
      geom_text(data = as.data.frame(SSDdata()), 
                aes(x = Concentration, y = fraq, label = Species), hjust = 1, size = 2) +
      geom_line(data=SSDfit(),  aes(x= Normal, y=pro, colour="Normal")) +
      geom_line(data=SSDfit(),  aes(x= Logistic, y=pro, colour="Logistic")) +
      geom_line(data=SSDfit(),  aes(x= Extreme.Value, y=pro, colour="Extreme.Value")) +
      geom_line(data=SSDfit(),  aes(x= Gumbel, y=pro, colour="Gumbel")) +
      geom_line(data=SSDfit(),  aes(x= Weibull, y=pro, colour="Weibull")  ) +
      scale_x_log10(limits=x.lim) +
      scale_color_manual(name = NULL, values = c("Normal"="red", "Logistic"="yellow", "Extreme.Value"="green", 
                                                 "Gumbel"="magenta3", "Weibull"="black")) +
      theme(plot.title = element_text(size=11, hjust=0), axis.text=element_text(size=10),
            axis.title=element_text(size=10), legend.position="right") +
      labs(x = paste0("Concentration (", input$unitssd,")"), y ="Fraction of species affected")
    return(p2)
  })
  
  ssd.plota <- reactive({
    x.lim <- c(min(SSDfit()[2:6]), max(SSDfit()[2:6]))
    p2 <- ggplot() + 
      stat_ecdf(data = SSDdata(), aes(Concentration) ,geom = "point", pad = FALSE, col="dodgerblue") + 
      geom_text(data = as.data.frame(SSDdata()), 
                aes(x = Concentration, y = fraq, label = Species), hjust = 1, size = 2) +
      geom_line(data=SSDfit(),  aes(x= Normal, y=pro, colour="Normal")) +
      geom_line(data=SSDfit(),  aes(x= Logistic, y=pro, colour="Logistic")) +
      geom_line(data=SSDfit(),  aes(x= Extreme.Value, y=pro, colour="Extreme.Value")) +
      geom_line(data=SSDfit(),  aes(x= Gumbel, y=pro, colour="Gumbel")) +
      geom_line(data=SSDfit(),  aes(x= Weibull, y=pro, colour="Weibull")) +
      scale_x_discrete(limits=c(-1*max(SSDdata()$Concentration), max(SSDdata()$Concentration))) +
      scale_color_manual(name = NULL, values = c("Normal"="red", "Logistic"="yellow", "Extreme.Value"="green", 
                                                 "Gumbel"="magenta3", "Weibull"="black"))+
      theme(plot.title = element_text(size=11, hjust=0), axis.text=element_text(size=10),
            axis.title=element_text(size=10), legend.position="right")+
      labs(x = paste0("Concentration (", input$unitssd,")"), y ="Fraction of species affected")
    return(p2)
  })
  
  ssd.plot1 <- reactive({
    if(input$scale == 2){
      ssd.plotl() + ggtitle(wrapper("Figure 7. Model fits visualization of the aquatic plant species sensitivity 
                                    distributions \ndeveloped for prometryn. Dots on the graph represent endpoints 
                                    for individual species, while the lines represent different model fits of the 
                                    data distributions.", width = 100))
    }else{
      ssd.plota() + ggtitle(wrapper("Figure 7. Model fits visualization of the aquatic plant species sensitivity 
                                    distributions \ndeveloped for prometryn. Dots on the graph represent endpoints 
                                    for individual species, while the lines represent different model fits of the 
                                    data distributions.", width = 100))
    } 
    })
  
  ssd.plot2 <- reactive({
    if(input$scale == 2){
      plotl() + ggtitle(wrapper("Figure 8. Distribution of ",input$chem, " detects in surface water samples.", width = 100)) 
    }else{
      plota() + ggtitle(wrapper("Figure 8. Distribution of ",input$chem, " detects in surface water samples.", width = 100)) 
    } 
    })
  
  mrkdwn.gof <- reactive({
    t(fit.fun()$gof.test)
  })
  
  ssd.summary <- reactive({
    rbind(fit.fun()$sse, fit.fun()$df.hc)
  })
  
  output$report <- downloadHandler(
    filename = function() {
      paste('report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'))
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report2.Rmd")
      file.copy("report2.Rmd", tempReport, overwrite = TRUE)
      
     # params <- list(n = input$samplsize)
      
      out <- rmarkdown::render(tempReport, 
                               params = list(m= input$chem),
                               switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
      #rmarkdown::render(tempReport, output_file = file)
    })
  
  }

