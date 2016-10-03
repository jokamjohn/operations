#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# @authors   John Kagga <johnkagga@gmail.com>
#            Cecilia Caroline <nalubegac58@gmail.com>              
#    
#

library(shiny)
library(formattable)
library(DT)
library(shinydashboard)
library(dplyr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
     
    #Function to read the inputs of the uploaded file.
    inputFile <- function(input){
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote, encoding="UTF-8" )
    }
  
    #Show the file raw data.
    output$contents <- renderTable({
      
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      
      inputFile(input)
    })
 
    #Table renders
    #
    #
    #Show only the planned activities.
    output$planned <- renderDataTable({
      
    #Calling the planned Method and giving it the uploaded file instance
    plannedData(inputFile(input))
    
    })
    
    #Show only the un-planned activities.
    output$unplanned <- renderDataTable({
      
      #Calling the un-planned Method and giving it the uploaded file instance
      unPlannedData(inputFile(input))
      
    })
   
    
    #Data manipulations
    #
    #
    #The Planned method
    plannedData <- function(uploadedData){
      
      #Sort the data by name
      orderPlannedData <- ordered_Planned_Data(uploadedData)
      
      datatable(orderPlannedData, options = list(
        columnDefs = list(list(targets = c(2, 3, 5, 6), searchable = FALSE)), pageLength = 200)) %>% 
        formatStyle(
          columns =   'System.status',
          
          backgroundColor = styleEqual(
            c('DONE','NOT DONE', 'IN PROGRESS'), c('green', 'red', 'orange')
          ))
      

    }
    
    output$date <- renderText(input$date)
    output$date1 <- renderText(input$date1)
    output$date2 <- renderText(input$date2)
    output$date3 <- renderText(input$date3)
  
    #The unplanned data function.
      unPlannedData <- function(uploadedData){
      
      #Sort the data by name
      orderedUnplannedData <- ordered_UnPlanned_Data(uploadedData)
      
      #return(orderedUnplannedData)
      
      datatable(orderedUnplannedData, options = list(
        columnDefs = list(list(targets = c(2, 3, 5, 6), searchable = FALSE)), pageLength = 200)) %>% 
        formatStyle(
          columns =   'System.status',
          
          backgroundColor = styleEqual(
            c('DONE','NOT DONE', 'IN PROGRESS'), c('green', 'red', 'orange')
          ))
      
      }
      
      #Data frame functions
      #
      #
      #Make ordered planned data
      ordered_Planned_Data <- function(uploadedData){
        #Get the columns that we need
        columnsWeWant <- getColumnsWeWant(uploadedData)
        #planed man hrs
        N2 <- gsub(",","",as.character(columnsWeWant$TotSum..plan.))
        columnsWeWant$TotSum..plan. <- as.numeric(N2)/input$cost
        columnsWeWant$TotSum..plan. <- round(columnsWeWant$TotSum..plan., digits = 2)
        
        #actual man hrs
        N2 <- gsub(",","",as.character(columnsWeWant$TotSum..actual.))
        columnsWeWant$TotSum..actual. <- as.numeric(N2)/input$cost
        columnsWeWant$TotSum..actual. <- round(  columnsWeWant$TotSum..actual., digits = 2)
        
        #cost
        N2 <- gsub(",","",as.character(columnsWeWant$TotSum..plan.))
        columnsWeWant$cost <- as.numeric(N2)*input$cost
        
        #column rename
        names(columnsWeWant) <-  c("Description","Order","Resourcename","Closuredate","Order.Type","System.status","plannedmanhrs", "actualmanhrs","costs")
        
        #only planned activities
        neworderedData <- subset(columnsWeWant, Order.Type == 'ZPMR'| Order.Type == 'ZPMS' )
        
        orderPlannedData <- neworderedData[order(neworderedData$Resourcename),]
      }
      
      #Make ordered planned data
      ordered_UnPlanned_Data <- function(uploadedData){
        #Get only the columns needed.  
        columnsWeWant <- getColumnsWeWant(uploadedData)
        
        #planed man hrs
        N2 <- gsub(",","",as.character(columnsWeWant$TotSum..plan.))
        columnsWeWant$TotSum..plan. <- as.numeric(N2)/input$cost
        columnsWeWant$TotSum..plan. <- round(columnsWeWant$TotSum..plan., digits = 2)
      
        #actual man hrs
        N2 <- gsub(",","",as.character(columnsWeWant$TotSum..actual.))
        columnsWeWant$TotSum..actual. <- as.numeric(N2)/input$cost
        columnsWeWant$TotSum..actual. <- round(  columnsWeWant$TotSum..actual., digits = 2)
        #cost
        N2 <- gsub(",","",as.character(columnsWeWant$TotSum..plan.))
        columnsWeWant$cost <- as.numeric(N2)*input$cost
        
        
        #column rename
        names(columnsWeWant) <- c("Description","Order","Resourcename","Closuredate","Order.Type","System.status","plannedmanhrs", "actualmanhrs","costs")
        
        #Sort the data by name
        orderPlannedData <- columnsWeWant[order(columnsWeWant$Resourcename),]
        #Order Data to get tasks that are unplanned
        neworderedData <- subset(columnsWeWant, Order.Type == 'ZPMM')
        
        #Sort the data by name
        orderedUnplannedData <- neworderedData[order(neworderedData$Resourcename),]
        
      }
      #utilisation method
      utilisation <- function(uploadedData){
        #Get only the columns needed.  
        
        orderedData<-uploadedData[, c("Changed.by","Description","Order.Type","System.status","TotSum..plan.", "TotSum..actual.", "Resource","Utilization")]
        orderedData <- orderedData[order(orderedData$Changed.by),]
        
        N2 <- gsub(",","",as.character(orderedData$TotSum..actual.))
        orderedData$actualmanhrs <- as.numeric(N2)/input$cost
        orderedData$actualmanhrs <-round(orderedData$actualmanhrs, 2)
          
        
        orderedData <- orderedData[,c( "actualmanhrs", "Changed.by")]  %>%
          
  
       
          group_by(Changed.by )%>%  
          
          
          summarise(totalactualmanhrs = sum(actualmanhrs)) %>%
        datatable(orderedData, options = list( dom = 't'))
        
        
      }
      
      #resource method
      resource <- function(uploadedData){
        #Get only the columns needed.  
        
        orderedData<-uploadedData[, c( "Resource","Utilization")]
       # orderedData <- orderedData[order(orderedData$Resource),]
        orderedData <- na.omit(orderedData)
        orderedData <- orderedData[order(orderedData$Resource),]
        datatable(orderedData, options = list( dom = 't'))
        
       
      }
      
      #resource table
      
      output$Resource <- renderDataTable(
        
        {
          
          resource(inputFile(input))
        }
      )
      
      #utilisation table
      output$Utilisation <- renderDataTable(
        
        {
          
          utilisation(inputFile(input))
        }
      )
      
      #Calculations
      #
      #
      #CI calculation
      #create new dataset
      CICalc <- function(uploadedData){

          ciorderedData <- uploadedData[, c("Order.Type","System.status","TotSum..plan.", "TotSum..actual.","MaintActivType")]
          
          
          #filter to done and by mainactivitytype
          ciorderedData$System.status <- strtrim(ciorderedData$System.status, 4)
          
          ciorderedData$System.status <- replace(ciorderedData$System.status,ciorderedData$System.status == 'TECO', 'DONE')
          ciorderedData$System.status <- replace(ciorderedData$System.status,ciorderedData$System.status == 'REL ', 'NOT DONE')
          ciorderedData$System.status <- replace(ciorderedData$System.status,ciorderedData$System.status == 'CNF ', 'IN PROGRESS')

          
        #filter to done and by mainactivitytype
        ciorderedData$System.status <- strtrim(ciorderedData$System.status, 4)


        cineworderedData <- subset(ciorderedData, MaintActivType =='IMP'| MaintActivType== '5S'| MaintActivType =='RCA' )
        
        done_ciorderedData <- subset(cineworderedData, System.status =='DONE' )
        
        #create actual man hrs column
        N2 <- gsub(",","",as.character(done_ciorderedData$TotSum..actual.))
        done_ciorderedData$actualmanhrs <- as.numeric(N2)/input$cost
        
        cisum <- sum(done_ciorderedData$actualmanhrs)
        (cisum/input$hours)*100
        }
      
      #Get the needed columns and also mark done and un done tasks
      getColumnsWeWant <- function(uploadedData){
        columnsWeWant <- uploadedData[, c("Description","Order","Changed.by","Basic.fin..date","Order.Type","System.status","TotSum..plan.", "TotSum..actual.")]
        
        #Strimming long strings to 4 characters
        columnsWeWant$System.status <- strtrim(columnsWeWant$System.status, 4)
        
        #Marking Done and not done data
       
        columnsWeWant$System.status <- replace(columnsWeWant$System.status,columnsWeWant$System.status == 'TECO', 'DONE')
        columnsWeWant$System.status <- replace(columnsWeWant$System.status,columnsWeWant$System.status == 'REL ', 'NOT DONE')
        columnsWeWant$System.status <- replace(columnsWeWant$System.status,columnsWeWant$System.status == 'CNF ', 'IN PROGRESS')
        
         return(columnsWeWant)
      }
    
   
    
    #Downloading a file
    datasetInput <- reactive({
      switch(input$dataset,
             "Planned Data" = ordered_Planned_Data(inputFile(input)),
             "Un-Planned Data" = ordered_UnPlanned_Data(inputFile(input))
      )
    })
    
    #Download Files
    #
    #
    output$table <- renderTable({
      datasetInput()
    })
    
    output$downloadData <- downloadHandler(
      filename = function() { 
        paste(format(Sys.time(), "%Y-%m-%d %I-%p"),input$dataset, '.csv', sep='') 
      },
      content = function(file) {
        write.csv(datasetInput(), file)
      }
    )
    

      output$atpBox <- renderValueBox({
       
        #filtering to required dataframe
        columnsWeWant <- getColumnsWeWant(inputFile(input))
         #ATP calculation
        
        neworderedData <- subset( columnsWeWant, Order.Type == 'ZPMR'| Order.Type == 'ZPMS' )
        
        sum(neworderedData$System.status =='DONE') 
        length(neworderedData$System.status)
        ATP <- (sum(neworderedData$System.status =='DONE')/length(neworderedData$System.status))*100
        ATP <- round(  ATP, digits = 2)
        
        valueBox(
          paste0(ATP), "ATP", icon = icon("calculator"),
          color = "purple"
        )
      })
      
      output$accuracyBox <- renderValueBox({
        #filtering to required dataframe
        columnsWeWant <- getColumnsWeWant(inputFile(input))
        
        #ACCURACY calculation
        
        neworderedData <- subset( columnsWeWant, Order.Type == 'ZPMR'| Order.Type == 'ZPMS' )
        
        acorderedData <- subset(neworderedData,neworderedData$System.status == 'DONE' )
        N2 <- gsub(",","",as.character(acorderedData$TotSum..actual.))
        acorderedData$actualmanhrs <- as.numeric(N2)/input$cost
        sum(acorderedData$actualmanhrs)
        
        
        N2 <- gsub(",","",as.character(acorderedData$TotSum..plan.))
        acorderedData$planedmanhrs <- as.numeric(N2)/input$cost
        
        sum(acorderedData$planedmanhrs)
        Accuracy <- (sum(acorderedData$actualmanhrs)/sum(acorderedData$planedmanhrs))*100
        
        Accuracy <- round( Accuracy, digits = 2)
        valueBox(
          paste0(Accuracy), "Accuracy", icon = icon("calculator"),
          color = "yellow"
          
        )
      })
      
      output$peBox <- renderValueBox({
        
        #filtering to required dataframe
        columnsWeWant <- getColumnsWeWant(inputFile(input))
        
        #PE calculation
        neworderedData <- subset(columnsWeWant, Order.Type == 'ZPMR'| Order.Type == 'ZPMS' )
        
        
        doneneworderedData <- subset(neworderedData, System.status =='DONE')
        
        N2 <- gsub(",","",as.character(doneneworderedData$TotSum..actual.))
        doneneworderedData$actualmanhrs <- as.numeric(N2)/input$cost
        #PE calculation
        PE <- (sum(doneneworderedData$actualmanhrs)/input$hours)*100
        
        PE <- round( PE, digits = 2)
        
        valueBox(
          paste0(PE), "PE", icon = icon("calculator"),
          color = "blue"
        )
        
      })
      
      output$pmBox <- renderValueBox({
        
        #filtering to required dataframe
        columnsWeWant <- getColumnsWeWant(inputFile(input))
        
        #PM calculation
        zorderedData <- subset(columnsWeWant, Order.Type == 'ZPMS')
        pmlength <-length(zorderedData$Order.Type)
        zplanneddone <- sum(zorderedData$System.status =='DONE')
        
        PMschedulecompliance <- ( zplanneddone/ pmlength)*100

        PMschedulecompliance <- round(PMschedulecompliance, digits = 2)

        valueBox(
          paste0(PMschedulecompliance), "PMschedulecompliance", icon = icon("calculator"),
          color = "red"
        )
      })
      
      output$ciBox <- renderValueBox({
        
        #filtering to required dataframe
        columnsWeWant <- getColumnsWeWant(inputFile(input))
        
        #Make the CI calculation using the CI function
        CI <- CICalc(inputFile(input))
        CI <- round(CI, digits = 2)
        valueBox(
          paste0(CI), "CI", icon = icon("calculator"),
          color = "orange"
        )
        
        
      })
      
      
      
      output$mixBox <- renderValueBox({
        
        #filtering to required dataframe
        columnsWeWant <- getColumnsWeWant(inputFile(input))
        
        mtotalplan <- length(columnsWeWant$Order.Type)
        rorderedData <- subset(columnsWeWant, Order.Type == 'ZPMR')
        sorderedData <- subset(columnsWeWant, Order.Type == 'ZPMS')
        morderedData <- subset(columnsWeWant, Order.Type == 'ZPMM')
        sumzpmr <- length(rorderedData$Order.Type )
        sumzpms <-length(sorderedData$Order.Type )
        sumzpmm <-length(morderedData$Order.Type )
        
        #calculate the maintenance mix and round off result
        ZPMRMIX <- (sumzpmr/mtotalplan)*100
        ZPMRMIX  <-  round(ZPMRMIX, digits = 2)
        
        ZPMSMIX <-(sumzpms/mtotalplan)*100
        ZPMSMIX  <-  round( ZPMSMIX, digits = 2)
        
        ZPMMMIX <- (sumzpmm/mtotalplan)*100
        ZPMMMIX <-  round( ZPMMMIX, digits = 2)
        #making the maintenance ratio with : separator
        
        
        r <- paste0(ZPMRMIX, sep = " : ")
        s <- paste0(ZPMSMIX, sep = " : ")
        m <- paste0(ZPMMMIX, sep = " ")
        valueBox(
          paste0(r,s,m, sep = ""), "Maintance Mix", icon = icon("calculator"),
          color = "green"
        )
        
      })
})
