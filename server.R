#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)
library(shinyjs)
library(dplyr)
library(tidyverse)
library(htmltools)


# Define server logic 
shinyServer(function(input, output,session) {
  
  # first time pass all the trials 
  selecTrial = reactiveValues(comTb=tibble())
  
  # reactive to get the data if any of the buttons are clicked 
  
  observeEvent(input$loc_fil,{
    
    # To stop errors popping up in app if nothing is chosen by default
    shinyjs::hide(id = "browsetable")
    shinyjs::show(id = "filterbrowse")
    
    # selection 
    SelStage = as.list.data.frame(input$stageView)
    checkStageSel = browse_tbl %>% select(NCT, disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";") %>% filter(stage %in% SelStage) %>% select(NCT) %>% distinct()
    print(isTRUE(SelStage))
    print(length(checkStageSel$NCT))
    
    
    SelDise = as.list.data.frame(input$disFil)
    checkDiseSel = browse_tbl %>% select(NCT,disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";") %>% filter(code %in% SelDise) %>% select(NCT) %>% distinct()
   # checkDiseSel = browse_tbl %>% select(NCT,disp_disease1) %>% filter(disp_disease1 %in% SelDise) %>% select(NCT) %>% distinct()
     
    # SelDrug = as.list.data.frame(input$drugFil)
    # checkDrugSel = browse_tbl %>% select(NCT,arms) %>% unnest(arms) %>% filter(drug %in% SelDrug) %>% select(NCT) %>% distinct()
    # 
    SelLineofTx = as.list.data.frame(input$lineofTxFil)
    checklineoftxSel = browse_tbl %>% select(NCT,arms) %>% unnest(arms) %>% separate_rows(line_of_therapy,sep = ";") %>% filter(line_of_therapy %in% SelLineofTx) %>% select(NCT) %>% distinct()
   
    SelLocat = as.list.data.frame(input$locaFil)
    checklocat = browse_tbl %>% select(NCT,Location) %>% filter(Location %in%  SelLocat) %>% select(NCT) %>% distinct()
    
    SelTrialty = as.list.data.frame(input$trialTyxFil) # Ui name
    checktrlTy = browse_tbl %>% select(NCT,JIT) %>% filter(JIT %in%  SelTrialty) %>% select(NCT) %>% distinct()
    # ----------------------------------------------------------------------------------------------------------------------- #
    # part 2 options 
    #with drug
    #if(length(checkStageSel$NCT) >= 1  && length(checkDiseSel$NCT) == 0 && length(checkDrugSel$NCT) == 0 && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) == 0 && length(checktrlTy$NCT) == 0){

    #without drug  
      if(length(checkStageSel$NCT) >= 1  && length(checkDiseSel$NCT) == 0  && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) == 0 && length(checktrlTy$NCT) == 0){  
    
          # in all four options
      completeList = c(unique(checkStageSel$NCT))
      print(completeList)

    }else if(length(checkStageSel$NCT) ==0  && length(checkDiseSel$NCT) >=1  && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) == 0 && length(checktrlTy$NCT) == 0){

      # in all disease options
      completeList = c( unique(checkDiseSel$NCT))
      print(completeList)
      
    }else if(length(checkStageSel$NCT) ==0  && length(checkDiseSel$NCT) == 0  && length(checklineoftxSel$NCT) >=1 && length(checklocat$NCT) == 0  && length(checktrlTy$NCT) == 0){

      # in all Drug options
      completeList =  c(unique(checkDrugSel$NCT))
      print(completeList)


    }else if(length(checkStageSel$NCT) == 0  && length(checkDiseSel$NCT) == 0  && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) >=1  && length(checktrlTy$NCT) == 0){

      # in all line of therapy option
      completeList = c(unique(checklineoftxSel$NCT))
      print(completeList)

    }else if(length(checkStageSel$NCT) == 0  && length(checkDiseSel$NCT) == 0  && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) == 0  && length(checktrlTy$NCT) >=1){
      
      # in all Location option
      completeList = c(unique(checklineoftxSel$NCT))
      
    }else{
      
   #   matchList = c(unique(checkStageSel$NCT), unique(checkDiseSel$NCT), unique(checkDrugSel$NCT), unique(checklineoftxSel$NCT), unique(checklocat$NCT), unique(checktrlTy$NCT) )
      #without drug
      matchList = c(unique(checkStageSel$NCT), unique(checkDiseSel$NCT), unique(checklineoftxSel$NCT), unique(checklocat$NCT), unique(checktrlTy$NCT) )
      
      ntb = as.data.frame(table(matchList))
      maxNb = max(ntb$Freq)
      ntb = ntb %>% filter(Freq >= maxNb )
      completeList = c(ntb$matchList)
      print(completeList)
      
    }


    # ----------------------------------------------------------------------------------------------------------------------- #
   
    
    
    
    filTb = browse_tbl %>% filter(NCT %in% completeList )
   
    
   output$filterbrowse <- renderReactable({
   
         #    reactable(filTb %>% dplyr::select(Protocol, HoldStatus, Phase, Title, Disease, lnOfTherapy, disp_disease1, disp_biomarkers,  Documentation), 
                       reactable(filTb %>% dplyr::select(Protocol, HoldStatus, Phase, Title, Disease, lnOfTherapy, disp_biomarkers),
                      filterable = TRUE,
             #searchable = TRUE,
             resizable = TRUE,
             fullWidth = TRUE,
             defaultColDef = colDef(align = "center"),
             striped = TRUE,
             showSortable = TRUE,
             style = list(minWidth = 800),
             
             
             columns = list( HoldStatus = colDef(name = "Current Status"), lnOfTherapy = colDef(name = "Line of Therapy"), Disease = colDef(name = "Conditions/Disease"),
                       #     disp_biomarkers = colDef(name = "Biomarker"), disp_disease1 = colDef(name = "Cancer Type"), Documentation = colDef(html=TRUE),
                            disp_biomarkers = colDef(name = "Biomarker"), 
                            Title = colDef(name = "Title", minWidth = 300 ,style = list(fontWeight = "bold"))
                            
                            ),
             details = function(index) { 
               
               
               # create tables to be displayed if nested rows are expanded
               htmltools::div(
                
                 # group1: general info
                 reactable(filTb[index, ] %>% select(Link,Documentation,Name,Sponsor,StudyType, Location, TrialLastUpdate),
                           defaultColDef = colDef(align = "center"),
                           columns = list(TrialLastUpdate = colDef(name = "Onsite Last Update"),Link = colDef(html = TRUE,name = "Trial"),Documentation = colDef(html=TRUE))
                 ),
                 
                 # group 3: summary
                 reactable(filTb[index, ] %>%
                             select(Summary)),
                 
                 
                 # group 4: trial Status from .gov
                 reactable(filTb[index, ] %>%
                             select(Status, StatusUpdate, LastUpdate, Gender, MinAge),
                           defaultColDef = colDef(align = "center"),
                           columns = list(Status = colDef(name = "Clinical.gov Status"),
                                          MinAge = colDef(name = "Minimum Age"),
                                          StatusUpdate = colDef(name = "Clinical.gov Verification Date"),
                                          LastUpdate = colDef(name = "Clinical.gov Last Update"))),
                 # group2: cohort info
                 
                
                 reactable(filTb[index, ]$arms[[1]] %>% unnest(biomarker) %>%
                            
                             select(cohortlabel, drug, arm_type,line_of_therapy,arm_hold_status,Selection,summary) %>% distinct(),
                           columns = list(cohortlabel = colDef(name = "Cohort Label"),
                                          drug = colDef(name = "Drug(s)"),
                                          arm_type = colDef(name = "Arm Type"),
                                          
                                          line_of_therapy = colDef(name = "Line of Tx"),
                                          arm_hold_status = colDef(name = "Arm HoldStatus"),
                                          Selection = colDef(name = "Criteria"),
                                          summary = colDef(name = "Biomarker")
                                          
                           )),
                 # group 5: CONDITIONS MENTIONED FROM CLINICALTRIALS.GOV
                 reactable(filTb[index, ] %>%
                             select(Conditions)),
                 
                 # group 3: disease information
              
                 reactable(filTb[index, ]$disp_disease[[1]] %>% select(code, selection,stage))
                  
               )
             }) 
   })
   
  
  })
 
  # Reset button
 observeEvent(input$reset_btn_browse, {
 
   shinyjs::show(id = "browsetable")
   shinyjs::hide(id = "filterbrowse")
   
   updateSelectInput(inputId = "stageView",selected = "")
   updateSelectInput(inputId = "disFil",selected = "")
   updateSelectInput(inputId = "drugFil",selected = "")
   updateSelectInput(inputId = "locaFil",selected = "")
   updateSelectInput(inputId = "lineofTxFil",selected = "")
   
   updateSelectInput(inputId = "selcolumns",selected = "")
   
   
   updateRadioButtons(inputId = "filtercond", selected = character(0))
 })

  ##### BROWSE ########b
  # main display table for BROWSE
 
  output$browsetable <- renderReactable({
  
     selecTrial$comTb = as_tibble(browse_tbl) #original
 #    selecTrial$comTb$comb <- paste(selecTrial$comTb$Protocol,selecTrial$comTb$Documentation,sep = "\n")  #june 5th
     
  # selecTrial$comTb <- selecTrial$comTb %>% mutate(ProtDoc = paste0('<a href="', Documentation, '">', Documentation, '</a>',Protocol, collapse = ":")) #june 5th
                 #          reactable::reactable( selecTrial$comTb %>% dplyr::select(Protocol, HoldStatus, Phase, Title, Disease, lnOfTherapy, disp_disease1, disp_biomarkers, Documentation),    # original
                                                 reactable::reactable( selecTrial$comTb %>% dplyr::select(Protocol,HoldStatus, Phase, Title, Disease, lnOfTherapy, disp_biomarkers), #june 5th             
                                                 filterable = TRUE,
                                                 
                                                 resizable = TRUE,
                                                 fullWidth = TRUE,
                                                 defaultColDef = colDef(align = "center"),
                                                 striped = TRUE,
                                                 showSortable = TRUE,
         
         columns = list(HoldStatus = colDef(name = "Current Status"), lnOfTherapy = colDef(name = "Line of Therapy") ,Disease = colDef(name = "Conditions/Disease"),
          #                     disp_biomarkers = colDef(name = "Biomarker"), disp_disease1 = colDef(name = "Cancer Type"), Documentation = colDef(html=TRUE), #original
                        disp_biomarkers = colDef(name = "Biomarker"),  
     #     comb = colDef(html = TRUE), 
                      Title = colDef(name = "Title", minWidth = 300 ,style = list(fontWeight = "bold"))
                 
                      ),
       
       
                details = function(index) {
                  # create tables to be displayed if nested rows are expanded
                  htmltools::div(
                   
                    # group1: general info
                    reactable(selecTrial$comTb[index, ] %>% select(Link, Documentation, Name,Sponsor,StudyType, Location, TrialLastUpdate),
                              defaultColDef = colDef(align = "center"),
                  #            columns = list(TrialLastUpdate = colDef(name = "Onsite Last Update"),Link = colDef(html = TRUE,name = "Trial")) #original
                  #  ), #original
                    
                    
                    columns = list(TrialLastUpdate = colDef(name = "Onsite Last Update"),Link = colDef(html = TRUE,name = "Trial"),Documentation = colDef(html=TRUE)) #june 6th
                  ), #june 6th
                    
                    # group 3: summary
                    reactable(selecTrial$comTb[index, ] %>%
                                select(Summary)),
                    
                    
                    # group 4: trial Status from .gov
                    reactable(selecTrial$comTb[index, ] %>%
                                select(Status, StatusUpdate, LastUpdate, Gender, MinAge),
                              defaultColDef = colDef(align = "center"),
                              columns = list(Status = colDef(name = "Clinical.gov Status"),
                                             MinAge = colDef(name = "Minimum Age"),
                                             StatusUpdate = colDef(name = "Clinical.gov Verification Date"),
                                             LastUpdate = colDef(name = "Clinical.gov Last Update"))),
                    # group2: cohort info
               
                    reactable(selecTrial$comTb[index, ]$arms[[1]] %>% unnest(biomarker) %>%
                               
                                select(cohortlabel, drug, arm_type,line_of_therapy,arm_hold_status,Selection,summary) %>% distinct(),
                              columns = list(cohortlabel = colDef(name = "Cohort Label"),
                                             drug = colDef(name = "Drug(s)"),
                                             arm_type = colDef(name = "Arm Type"),
                                             
                                             line_of_therapy = colDef(name = "Line of Tx"),
                                             arm_hold_status = colDef(name = "Arm HoldStatus"),
                                             Selection = colDef(name = "Criteria"),
                                             summary = colDef(name = "Biomarker")
                                             
                              )),
                    # group 5: CONDITIONS MENTIONED FROM .GOV
                    reactable(selecTrial$comTb[index, ] %>%
                                select(Conditions)),
                    
                    # group 3: disease information
                    
                   
                    reactable(selecTrial$comTb[index, ]$disp_disease[[1]] %>% select(code, selection,stage))
                    
                    
                  )
                })

  })
  
  # collapse button
  observeEvent(input$collapse_btn_browse, {
    updateReactable("browsetable", expanded = FALSE)
  })
  
  
  observeEvent(input$collapse_btn_browse, {
    updateReactable("filterbrowse", expanded = FALSE)
  })
  
  

})






