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
#  dashboardHeader(title = "TrialMatch",titleWidth = 350),
  
  dashboardHeader(title = "TrialMatch", titleWidth = 350,
                  tags$li(class="dropdown",tags$style(HTML(".skin-blue .main-header .navbar, .skin-blue .main-header .logo {background-color: #007852; }")))),
  
  
  dashboardSidebar(collapsed=TRUE,    
    
    # tags$style(HTML("
    #   .main-sidebarmenu{
    #     width: 350px;
    #   }
    # ")),
    # 
    
    #new block
    width = 350, #original was 370
    
    tags$style(HTML('.skin-blue .sidebar { background-color: #D1D3D3  ; width:350px; height: 200vh;}',          
                    #    '.left-side, .main-sidebar {padding-top: 20px}',
                    #     '.skin-blue .sidebar .selectize-control { background-color: #D1D3D3 }',
                    #    '.skin-blue .sidebar .selectize-input { background-color: #D1D3D3 }',
                    '.skin-blue .sidebar .selectize-dropdown { background-color: #D1D3D3  }',
                    #  '.skin-blue .sidebar .sidebar-menu .treeview-menu > li >a {color: black;}',
                    #  '.skin-blue .sidebar h5 {color:black; }',
                    '.skin-blue .control-label { color: black; }',
                    '.skin-blue .sidebar .form-group .control-label {color: black; }',
                    #  '.well {color: white;}',
                    '.well .control-label { color: black;}',
                    '.sidebar .checkbox label {color: black;}',
                    #  '.btn-yellow {background-color: #F8EB60; color: #F8EB60 ;}',
                    #  '.btn-yellow {background-color: #F8EB60;color: black ;}', good yellow put back
                    '.btn-green {background-color: #C4E86B;color: black ;}',
                    '.btn-orange {background-color: #FF7F2F; color: black ;}',
                    '.btn-purple {background-color: #644B78; color: white ;}',
                    '.btn-resetorange {background-color: #FF7F2F; color: black ;}',
                    
                    '.btn-ltgreen {background-color: #72AA85; color: white ;}',
                    '.btn-tan {background-color: #9D9666; color: white ;}',
                    
                    #   '.skin-blue .sidebar .checkbox .control-label { color: black;}' 
    )),
    
    
    
    
    
    
    
    
    
    
    
    sidebarMenu(
      menuItem("Browse", tabName = "browse", icon = icon("search")),

      ####
      useShinyjs(),
      tabItems(
        tabItem(tabName = "browse",
                
                style = "background-color: #D1D3D3;",
                
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
                
                selectInput(
                  inputId = "selcolumns",
                  label = "Column selection",
                  
                  choices = c(colnames(seldiscolumns)),
                  #            choices = c(colnames(browse_tbl),
                  # selected = names(browse_tbl)),
                  
                  
                  
                  multiple = T,
                  
                  width = "100%"
                  
                ),
                
                
                               
                                 #   actionButton("loc_fil", "Filter",icon = shiny::icon("filter"),size = "sm",class = "btn-warning",width="50%"),
                actionButton("loc_fil", "Filter",icon = shiny::icon("filter"),size = "sm",class = "btn-ltgreen",width="50%"),
                
                          
       #                    ), #fluidrow,
                           br(),
                           br(),

                         
                           
                                   # actionButton("reset_btn_browse", "Reset Trials",class = "btn-success", width = "50%"),

                                   # actionButton("collapse_btn_browse", "Collapse All",class = "btn-info", width = "50%") ,

                                      actionButton("reset_btn_browse", "Reset Trials",class = "btn-tan", width = "50%"),
       
                                      actionButton("collapse_btn_browse", "Collapse All",class = "btn-purple", width = "50%") ,
       
                                      checkboxInput("show_closed","show closed trials",value = FALSE),
       
       
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
                               # primary = "#246725"
                              primary = "#D1D3D3")
                              

  )
  
   
)#dashboard page
  
  
  
  
  
  
  
  
  
  
  





































