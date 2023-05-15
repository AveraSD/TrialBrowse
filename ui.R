#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(reactable)
library(reactablefmtr)
library(bslib)
library(readxl)
library(here)
library(mongolite)
library(jsonlite)
library(httr)
library(glue)
library(tidyverse)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)



ui <- dashboardPage(
  dashboardHeader(title = "TrialMatch",titleWidth = 350),
  dashboardSidebar(
    
    tags$style(HTML("
      .main-sidebarmenu{
        width: 350px;
      }
    ")),
    
    
    sidebarMenu(
      menuItem("Browse", tabName = "browse", icon = icon("search")),

      ####
      useShinyjs(),
      tabItems(
        tabItem(tabName = "browse",
                              selectInput(
                                 inputId = "stageView",
                                 label = "Disease Stages",
                                 choices = c(stageAv$stage),
                                 multiple = TRUE,
                                 width = "100%"
                               ), 
                          
                                    selectInput(
                                      inputId = "disFil",
                                      label = "Cancer Type",
                                      choices = c(diseasAv$code),
                                      multiple = TRUE,
                                      width = "100%"
                                    ), 
                         
                                    selectInput(
                                      inputId = "lineofTxFil",
                                      label = "Line of therapy",

                                      choices = c(lineoftxAv$line_of_therapy),
                                      multiple = T,

                                      width = "100%"

                                    ),
                
                                    selectInput(
                                    inputId = "trialTyxFil",
                                    label = "Trial Type",
                                    choices = c(trialTyAv$JIT),
                                    multiple = T, width = "100%"
                                    ),
                                      selectInput(
                                      inputId = "locaFil",
                                      label = "Locations",
                                      choices = c(locAv$Location),
                                      multiple = T,

                                      width = "100%"

                                    ),
                               
                                    actionButton("loc_fil", "Filter",icon = shiny::icon("filter"),size = "sm",class = "btn-warning",width="50%"),

                          
       #                    ), #fluidrow,
                           br(),
                           br(),

                         
                           
                                    actionButton("reset_btn_browse", "Reset Trials",class = "btn-success", width = "50%"),

                                    actionButton("collapse_btn_browse", "Collapse All",class = "btn-info", width = "50%") ,

  
                           #, #fluidrow



      ####

      collapsible = TRUE

    )#tabitems
    )#tabitem,
    )#sidebar menu
  ),#sidebar
    dashboardBody(
      useShinyjs(),
      tabItems(
      tabItem(tabName = "dashboard",
              h2("Dashboard tab content")) ),
      
      br(),
              reactableOutput("filterbrowse"),
             br(),
             reactableOutput("browsetable"),
           
              theme = bs_theme(version = 5,
                              bootswatch = "cosmo",
                               primary = "#246725")

  )
  
   
)#dashboard page
  
  
  
  
  
  
  
  
  
  
  





































