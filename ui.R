library(shiny)
library(plyr)
library(plotly)
library(dplyr)
library(data.table)

shinyUI(fluidPage(
        titlePanel("Production Control 3G"),
        sidebarLayout(
                
                sidebarPanel(
                        conditionalPanel(
                                'input.dataset === "Display Graph"',
                        
                        dateInput("date", label = ("Select a Date:"), value = "2017-04-21"),
                        sliderInput("sliderTurnos","Select hours:",
                                    0,23, value = c(7,23)), ##input that will be taken
                        radioButtons("plant", label = ("Select a Plant:"), 
                                    choices = c("MTY" = "MTY", "GDL" = "GDL", "QTO" = "QTO", 
                                                   "SALT" = "SALT"),
                                    selected = "MTY"),
                        
                        conditionalPanel(
                        condition = "input.plant == 'GDL'",
                        selectInput("G", label = ("Select a Machine from GDL:"), 
                                    choices = c("W01" = "W01", "W02" = "W02"),
                                    selected = "W01", multiple = FALSE)),
                        
                        conditionalPanel(
                                condition = "input.plant == 'MTY'",
                                selectInput("M", label = ("Select a Machine from MTY:"), 
                                            choices = c("W03" = "W03", "W06" = "W06", "W07" = "W07",
                                                        "W08" = "W08", "W09" = "W09", "W10" = "W10", 
                                                        "W11" = "W11"),
                                            selected = "W11", multiple = FALSE)),
                        
                        conditionalPanel(
                        condition = "input.plant == 'QTO'",
                        selectInput("Q", label = ("Select a Machine from QTO:"), 
                                    choices = c("W04" = "W04"),
                                    selected = "W04", multiple = FALSE)),
                        
                        conditionalPanel(
                        condition = "input.plant == 'SALT'",
                        selectInput("S", label = ("Select a Machine from SALT:"), 
                                    choices = c("W11" = "W11"),
                                    selected = "W11", multiple = FALSE))
                ),
                
                conditionalPanel(
                        'input.dataset === "Summary"',
                        dateRangeInput("date2", label = ("Select a Date:"), 
                                       start = "2017-04-17",
                                       end = "2017-04-23",
                                       format = "yyyy-mm-dd",
                                       weekstart = 1)
                        
                ),
                
                
                conditionalPanel(
                        'input.dataset === "Data OpTime"',
                        ##dateInput("date4", label = ("Select a Date:"), value = "2017-04-21")
                        dateRangeInput("date4", label = ("Select a Date:"),
                                       start = "2017-04-17",
                                       end = "2017-04-23", 
                                       format = "yyyy-mm-dd",
                                       weekstart = 1)
                ),
                
                conditionalPanel(
                        'input.dataset === "Data Turnos"',
                        dateRangeInput("date5", label = ("Select a Date:"),
                                       start = "2017-04-17",
                                       end = "2017-04-23", 
                                       format = "yyyy-mm-dd",
                                       weekstart = 1)
                )
                
                ),
                mainPanel(
                        tabsetPanel(
                                id = 'dataset',
                        tabPanel("Display Graph", plotlyOutput("plot")),
                        tabPanel("Summary",h4("Summary by Machine"),tableOutput("summary"),
                                 h4("Summary by Plant"), tableOutput("summaryplanta")),
                        tabPanel("Data OpTime", tableOutput("data")),
                        tabPanel("Data Turnos", tableOutput("dataturnos"))
                        )
                        
                   
                        
                )
                
        ))
)