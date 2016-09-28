#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# @authors   John Kagga <johnkagga@gmail.com>
#            Cecilia Caroline <nalubegac58@gmail.com>  
#   
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
dashboardPage(
  
  # Application title
  dashboardHeader(title = "Uganda Breweries Limited"),
  
  # Sidebar with a slider input for number of bins 
  
    dashboardSidebar(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=','
                     )
                   ),
      radioButtons('quote', 'Quote',
                   c(
                     'Double Quote'='"'
                     )
                   ),
                   numericInput("cost", "Cost value equivalent ", 18823.355),
      numericInput("hours", "Available man hours ", 100)
      
      ),
    
    # Show a plot of the generated distribution
  dashboardBody(
       tabsetPanel(type ="tab",
                
                   tabPanel("Raw Data", tableOutput("contents")),
                  
                  #Planned Data Tab

                   tabPanel("Planned Data",dateInput("date", "Date:", value = Sys.Date()-10), DT::dataTableOutput("planned"),
                            h4("PLEASE ENDEVOUR TO RAISE WORK ORDERS FOR ALL THE JOBS DONE AND TECO IF COMPLETED")
                            ),
                 
                  #Un-planned Data Tab
                   tabPanel("Un-Planned Data",dateInput("date1", "Date:", value = Sys.Date()-10),  DT::dataTableOutput("unplanned"),
                            h4("PLEASE ENDEVOUR TO RAISE WORK ORDERS FOR ALL THE JOBS DONE AND TECO IF COMPLETED")
                            
                            ),
                   
                   #Calcualtion Section

                   tabPanel("Calculation", dateInput("date2", "Date:", value = Sys.Date()-10),

                   fluidRow(
                    
                     # Dynamic valueBoxes
                     valueBoxOutput("atpBox"),
                     
                     valueBoxOutput("accuracyBox"),
                     valueBoxOutput("ciBox")
                   ),
                   fluidRow(
                     
                     # Dynamic valueBoxes
                     valueBoxOutput("pmBox"),
                     
                     valueBoxOutput("peBox"),
                     valueBoxOutput("mixBox")
                    
                   )
                   
                   ),
                   
                  #utilisation tab panel
                  tabPanel("Resource Utilisation", dateInput("date3", "Date:", value = Sys.Date()-10),
                           fluidRow
                           (
                          column(6, DT::dataTableOutput("Utilisation"))  ,
                         column(6, DT::dataTableOutput("Resource"))  
                           )),
                  
                   #Download Panel
                   tabPanel("Download",
                            selectInput("dataset", "Choose a dataset:", 
                                        choices = c("Planned Data", "Un-Planned Data")),
                            downloadButton('downloadData', 'Download')
                   )
                
                   )
    )#End of main panel
  
)
