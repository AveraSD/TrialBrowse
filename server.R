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
  #  checkStageSel = browse_tbl %>% select(NCT, disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";") %>% filter(stage %in% SelStage) %>% select(NCT) %>% distinct()
    
   # checkStageSel = browse_tbl %>% select(NCT, disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";") %>% filter(str_detect(stage, paste(SelStage,collapse = "|"))) %>% select(NCT) %>% distinct()
    
    #add more formatting
    checkStageSel = browse_tbl %>% select(NCT, disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";") %>% mutate(stage = trimws(stage), stage = tolower(stage)) %>% filter(str_detect(stage, paste0("\\b",SelStage,"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
    print(isTRUE(SelStage))
    print(length(checkStageSel$NCT))
    
    
    SelDise = as.list.data.frame(input$disFil)
  #  checkDiseSel = browse_tbl %>% select(NCT,disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";") %>% filter(code %in% SelDise) %>% select(NCT) %>% distinct()
    
    SelDise1<- as.list.data.frame(gsub("\\([^()]*\\)","",SelDise))
    
    SelDise1<- trimws(SelDise1)
    print(SelDise1)
    
    reactive_data <- reactive({
      # add more search keywords
      if(SelDise1 == "Lung Cancer" | SelDise1 == "Lung" | SelDise1 == "Lung (LUNG)") {
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Lung","Lung Cancer","Lung (LUNG)"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }else if(SelDise1 == "Breast Cancer" | SelDise1 == "Breast" | SelDise1 == "Breast (BREAST)"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Breast","Breast Cancer","Breast (BREAST)"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      } else if(SelDise1 == "Small Cell Lung Cancer"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("^",c("Small Cell Lung Cancer"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      } else if(SelDise1 == "Skin" | SelDise1 == "Melanoma" | SelDise1 == "Skin (SKIN)"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Skin","Melanoma","Skin (SKIN)" ),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }else if(SelDise1 == "Ovary/Fallopian Tube" | SelDise1 == "Ovarian Cancer" | SelDise1 == "Ovary/Fallopian Tube (OVARY)"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Ovary","Ovarian Cancer","Ovary/Fallopian Tube (OVARY)"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "Lymphoid Neoplasm"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Lymphoid","Lymphoma"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "Myeloid Neoplasm"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Myeloid", "Myeloma"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      
      
      else if(SelDise1 == "Cervix" | SelDise1 == "Cervix (CERVIX)"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Cervix","Cervical","Cervix (CERVIX)"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "Peritoneum" | SelDise1 == "Peritoneum (PERITONEUM)"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Peritoneum","Peritoneal","Peritoneum (PERITONEUM)"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "Pancreas" | SelDise1 == "Pancreatic Cancer" | SelDise1 == "Pancreas (PANCREAS)" ){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Pancreas","Pancreatic","Pancreas (PANCREAS)"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "Bladder/Urinary Tract" | SelDise1 == "Bladder Cancer" | SelDise1 == "Bladder/Urinary Tract (BLADDER)"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Bladder","Bladder Cancer","Bladder/Urinary Tract (BLADDER)"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "Soft Tissue" | SelDise1 == "Sarcoma" | SelDise1 == "DSRCT"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Soft Tissue","Sarcoma","DSRCT"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "Liver" | SelDise1 == "Liver Cancer" | SelDise1 == "Liver (LIVER)"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Liver","Hepatocellular carcinoma","Liver (LIVER)"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "Head and Neck" | SelDise1 == "Head and Neck Cancer" | SelDise1 == "Head and Neck (HEAD_NECK)"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Head and Neck","Hypopharynx", "Larynx", "Oral", "Head and Neck Squamous","Head and Neck (HEAD_NECK)"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "Uterus" | SelDise1 == "Uterus (UTERUS)"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Uterus","Uterine", "Mullerian", "Endometrial", "Uterus (UTERUS)"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "CNS/Brain" | SelDise1 == "Brain Cancer" | SelDise1 == "CNS/Brain (BRAIN)"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("CNS/Brain","Glioma","Glioblastoma","Glioblastoma Multiforme","CNS/Brain (BRAIN)"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "Esophagus/Stomach" | SelDise1 == "Esophagus/Stomach Cancer" | SelDise1 == "Esophagus/Stomach (STOMACH)"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Esophagus","Stomach", "Esophageal", "Esophagogastric", "Gastroesophageal", "Esophagus/Stomach (STOMACH)"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "Solid Tumors"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Uterus","Cervix", "Vulva", "Vagina","Brain", "Bowel","Lung","Breast","Bone", "Skin","Biliary Tract",
                                                                                                                     "Ovary","Fallopian Tube","Esophagus", "Stomach","Pancreas","Kidney","Head and Neck","Soft Tissue","Rectal","Oropharynx",
                                                                                                                     "Lymphoid", "Colorectal","Glioblastoma","Melanoma","Colorectal","Pancreatic","Gall bladder","Non-Small Cell Lung",
                                                                                                                     "Hepatocellular Carcinoma","Glioma","Ovarian","Gliosarcoma","Gastroesophageal","Small Cell Lung","Renal","Esophageal",
                                                                                                                     "Sarcoma", "Lymphoma","Endometrial","Mullerian","Uterine","Bile","Myelofibrosis","DSRCT","Uveal", "Thymic", 
                                                                                                                     "Prostate","Bladder","Cervical","Liver","Peritoneum", "Colon", "Thyroid","Solid Tumors"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }
      else if(SelDise1 == "Prostate" | SelDise1 == "Prostate Cancer" | SelDise1 == "Prostate (PROSTATE)"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Prostate","Prostate Cancer", "Prostate (PROSTATE)"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()
      }  else if(SelDise1 == "Colorectal Cancer" | SelDise1 == "Bowel" | SelDise1 == "Bowel (BOWEL)"){
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",c("Colon","Colorectal Cancer", "Colorectal", "Bowel","Rectal", "Bowel (BOWEL)"),"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()                                                                                                               
      }
      else {
        data <- browse_tbl %>% select(NCT,disp_disease1) %>%  filter(str_detect(trimws(disp_disease1),paste0("\\b",SelDise1,"\\b",collapse = "|"))) %>% select(NCT) %>% distinct()   #works final with b
      }
      
      return(data)
    })
    checkDiseSel = reactive_data()
    
    
    
    
    
    
    
    
    
   # checkDiseSel = browse_tbl %>% select(NCT,disp_disease1) %>% filter(disp_disease1 %in% SelDise) %>% select(NCT) %>% distinct()
     
    # SelDrug = as.list.data.frame(input$drugFil)
    # checkDrugSel = browse_tbl %>% select(NCT,arms) %>% unnest(arms) %>% filter(drug %in% SelDrug) %>% select(NCT) %>% distinct()
    # 
    SelLineofTx = as.list.data.frame(input$lineofTxFil)
    checklineoftxSel = browse_tbl %>% select(NCT,arms) %>% unnest(arms) %>% separate_rows(line_of_therapy,sep = ";") %>% filter(line_of_therapy %in% SelLineofTx) %>% select(NCT) %>% distinct()
   
    SelLocat = as.list.data.frame(input$locaFil)
  #  checklocat = browse_tbl %>% select(NCT,Location) %>% filter(Location %in%  SelLocat) %>% select(NCT) %>% distinct()
    
    # add seperate_rows
    checklocat = browse_tbl %>% select(NCT,Location) %>% separate_rows(Location,sep = "\\s*;s*") %>% filter(str_detect(Location, paste(SelLocat, collapse = "|"))) %>% select(NCT) %>% distinct()
    
    
    SelTrialty = as.list.data.frame(input$trialTyxFil) # Ui name
  #  checktrlTy = browse_tbl %>% select(NCT,JIT) %>% filter(JIT %in%  SelTrialty) %>% select(NCT) %>% distinct()
    checktrlTy = browse_tbl %>% select(NCT,JIT) %>% filter(str_detect(JIT, paste(SelTrialty,collapse = "|"))) %>% select(NCT) %>% distinct()
    
    # ----------------------------------------------------------------------------------------------------------------------- #
    # part 2 options 
    #with drug
    #if(length(checkStageSel$NCT) >= 1  && length(checkDiseSel$NCT) == 0 && length(checkDrugSel$NCT) == 0 && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) == 0 && length(checktrlTy$NCT) == 0){

    #without drug  
     
    
   #   if(length(checkStageSel$NCT) >= 1  && length(checkDiseSel$NCT) == 0  && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) == 0 && length(checktrlTy$NCT) == 0){  
   #  
   #        # in all four options
   #    completeList = c(unique(checkStageSel$NCT))
   #    print(completeList)
   # 
   #  }else if(length(checkStageSel$NCT) ==0  && length(checkDiseSel$NCT) >=1  && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) == 0 && length(checktrlTy$NCT) == 0){
   # 
   #    # in all disease options
   #    completeList = c( unique(checkDiseSel$NCT))
   #    print(completeList)
   #    
   #  }else if(length(checkStageSel$NCT) ==0  && length(checkDiseSel$NCT) == 0  && length(checklineoftxSel$NCT) >=1 && length(checklocat$NCT) == 0  && length(checktrlTy$NCT) == 0){
   # 
   #    # in all Drug options
   #    completeList =  c(unique(checkDrugSel$NCT))
   #    print(completeList)
   # 
   # 
   #  }else if(length(checkStageSel$NCT) == 0  && length(checkDiseSel$NCT) == 0  && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) >=1  && length(checktrlTy$NCT) == 0){
   # 
   #    # in all line of therapy option
   #    completeList = c(unique(checklineoftxSel$NCT))
   #    print(completeList)
   # 
   #  }else if(length(checkStageSel$NCT) == 0  && length(checkDiseSel$NCT) == 0  && length(checklineoftxSel$NCT) == 0 && length(checklocat$NCT) == 0  && length(checktrlTy$NCT) >=1){
   #    
   #    # in all Location option
   #    completeList = c(unique(checklineoftxSel$NCT))
   #    
   #  }else{
   #    
   # #   matchList = c(unique(checkStageSel$NCT), unique(checkDiseSel$NCT), unique(checkDrugSel$NCT), unique(checklineoftxSel$NCT), unique(checklocat$NCT), unique(checktrlTy$NCT) )
   #    #without drug
   #    matchList = c(unique(checkStageSel$NCT), unique(checkDiseSel$NCT), unique(checklineoftxSel$NCT), unique(checklocat$NCT), unique(checktrlTy$NCT) )
   #    
   #    ntb = as.data.frame(table(matchList))
   #    maxNb = max(ntb$Freq)
   #    ntb = ntb %>% filter(Freq >= maxNb )
   #    completeList = c(ntb$matchList)
   #    print(completeList)
   #    
   #  }


    get_intersection <- function(result_objects) {
      # Extract NCT values from result objects
      
      non_null_results<- lapply(result_objects, function(result)
      {
        if(nrow(result) > 0) result$NCT else NULL
      }
      )
      
      non_null_results <- Filter(Negate(is.null),non_null_results)
      
      #original block
      if(length(non_null_results) == 0){
        return(character(0))
        
      }
    
      intersection_nct <- Reduce(intersect, non_null_results)
      return(intersection_nct)
    }
    
    result_objects <- list(checkStageSel, checkDiseSel, checklineoftxSel, checklocat, checktrlTy)
    
    # Get the intersection of NCT values
    intersection_nct <- get_intersection(result_objects)
    
    # Print the intersection NCT values
    print(intersection_nct)
    
    completeList = intersection_nct 
      
    
    # ----------------------------------------------------------------------------------------------------------------------- #
   
    
    
    
    filTb = browse_tbl %>% filter(NCT %in% completeList )
   
    
    filt_data_initial_filtered<- reactive({
      if(input$show_closed){
        if(length(input$selcolumns > 0)){
          #    filTb[filTb$HoldStatus != "open",input$selcolumns] commented oct'25th
          filTb[filTb$HoldStatus == "closed",input$selcolumns]
        }
        else{
          #  filTb[filTb$HoldStatus != "open",] %>% dplyr::select(Protocol, HoldStatus, filtopencohort, Phase, Title, Conditions, lnOfTherapy, disp_biomarkers) 
          #commented oct' 25 filTb[filTb$HoldStatus != "open",] %>% dplyr::select(Protocol, HoldStatus, Phase, Title, Diseasecat, Conditions, stages, disp_biomarkers)
          # filTb[filTb$HoldStatus == "closed",] %>% dplyr::select(Protocol, PrincipalInvestigator, HoldStatus, Phase, Title, Diseasecat, Conditions, stages, disp_biomarkers) #previous display order
          
          #changing display order
          #   filTb[filTb$HoldStatus == "closed",] %>% dplyr::select(Protocol, Title, Phase, stages, Conditions,  HoldStatus, Diseasecat, PrincipalInvestigator,  disp_biomarkers)
          #changing display columns
          filTb[filTb$HoldStatus == "closed",] %>% dplyr::select(Protocol, HoldStatus, Phase, Title, Disease, lnOfTherapy, disp_biomarkers)
        }
        
        
      }
      else{
        if(length(input$selcolumns > 0)){
          #  filTb[filTb$HoldStatus == "open",input$selcolumns] commented oct' 25th
          filTb[filTb$HoldStatus != "closed",input$selcolumns]
        }
        else{
          #   filTb[filTb$HoldStatus == "open",] %>% dplyr::select(Protocol, HoldStatus, filtopencohort, Phase, Title, Conditions, lnOfTherapy, disp_biomarkers) 
          # commented oct 25th..   filTb[filTb$HoldStatus == "open",] %>% dplyr::select(Protocol, HoldStatus, Phase, Title, Diseasecat, Conditions, stages, disp_biomarkers)
          #   filTb[filTb$HoldStatus != "closed",] %>% dplyr::select(Protocol, PrincipalInvestigator, HoldStatus, Phase, Title, Diseasecat, Conditions, stages, disp_biomarkers) #previous display order
          
          #changing display order
          # filTb[filTb$HoldStatus != "closed",] %>% dplyr::select(Protocol, Title, Phase, stages, Conditions,  HoldStatus, Diseasecat, PrincipalInvestigator,  disp_biomarkers)
          #changing display columns
          filTb[filTb$HoldStatus != "closed",] %>% dplyr::select(Protocol, HoldStatus, Phase, Title, Disease, lnOfTherapy, disp_biomarkers)
        } 
        
      }
    })
    
    
    expandable_data_filt <- reactive({
      if (input$show_closed) {
        #filTb[!filTb$HoldStatus %in% "open", ] commented oct' 25th
        filTb[filTb$HoldStatus %in% "closed", ]
      } else {
        #  filTb[filTb$HoldStatus == "open", ] commented oct' 25th
        filTb[!filTb$HoldStatus == "closed", ]
      }
    })
    
    
    
    
    
    
   output$filterbrowse <- renderReactable({
   
         #    reactable(filTb %>% dplyr::select(Protocol, HoldStatus, Phase, Title, Disease, lnOfTherapy, disp_disease1, disp_biomarkers,  Documentation), 
                      # reactable(filTb %>% dplyr::select(Protocol, HoldStatus, Phase, Title, Disease, lnOfTherapy, disp_biomarkers),
                                 
                                 
                                 reactable(filt_data_initial_filtered() ,          
                                 
                      filterable = TRUE,
             searchable = TRUE,
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
        #         reactable(filTb[index, ] %>% select(Link,Documentation,Name,Sponsor,StudyType, Location, TrialLastUpdate),
                           
                 reactable(expandable_data_filt()[index, ] %>% select(Link,Documentation,Name,Sponsor,StudyType, Location, TrialLastUpdate),        
                           
                           defaultColDef = colDef(align = "center"),
                           columns = list(TrialLastUpdate = colDef(name = "Onsite Last Update"),Link = colDef(html = TRUE,name = "Trial"),Documentation = colDef(html=TRUE))
                 ),
                 
                 # group 3: summary
       #          reactable(filTb[index, ] %>%
       
       reactable(expandable_data_filt()[index, ] %>%
                             select(Summary)),
                 
                 
                 # group 4: trial Status from .gov
         #        reactable(filTb[index, ] %>%
                             
                             
       reactable(expandable_data_filt()[index, ] %>%
                             select(Status, StatusUpdate, LastUpdate, Gender, MinAge),
                           defaultColDef = colDef(align = "center"),
                           columns = list(Status = colDef(name = "Clinical.gov Status"),
                                          MinAge = colDef(name = "Minimum Age"),
                                          StatusUpdate = colDef(name = "Clinical.gov Verification Date"),
                                          LastUpdate = colDef(name = "Clinical.gov Last Update"))),
                 # group2: cohort info
                 
                
            #     reactable(filTb[index, ]$arms[[1]] %>% unnest(biomarker) %>%
                             
                             
                             reactable(expandable_data_filt()[index, ]$arms[[1]] %>% unnest(biomarker) %>%     
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
          #       reactable(filTb[index, ] %>%
                             
       reactable(expandable_data_filt()[index, ] %>%
                             select(Conditions)),
                 
                 # group 3: disease information
              
                 #reactable(filTb[index, ]$disp_disease[[1]] %>% select(code, selection,stage))
       
       
       reactable(expandable_data_filt()[index, ]$disp_disease[[1]] %>% select(code, selection,stage)) 
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
   
   updateSelectInput(inputId = "trialTyxFil",selected = "")
   updateSelectInput(inputId = "selcolumns",selected = "")
   
   
   updateRadioButtons(inputId = "filtercond", selected = character(0))
   
   updateCheckboxInput(inputId = "show_closed", value = FALSE)
 })

  ##### BROWSE ########b
  # main display table for BROWSE
 
 browse_data_initial_filtered<- reactive({
   if(input$show_closed){
     if(length(input$selcolumns) > 0){
       #  selecTrial$comTb[selecTrial$comTb$HoldStatus!="open",input$selcolumns] commented oct 25th
       selecTrial$comTb[selecTrial$comTb$HoldStatus=="closed",input$selcolumns]
     }
     else{
       #   selecTrial$comTb[selecTrial$comTb$HoldStatus!="open",] %>% dplyr::select(Protocol, HoldStatus, filtopencohort, Diseasecat, Phase, Title, Conditions, stages, lnOfTherapy, disp_biomarkers)
       # commented oct 25th   selecTrial$comTb[selecTrial$comTb$HoldStatus!="open",] %>% dplyr::select(Protocol, HoldStatus, Diseasecat, Phase, Title, Conditions, stages, disp_biomarkers)
       #   selecTrial$comTb[selecTrial$comTb$HoldStatus=="closed",] %>% dplyr::select(Protocol, PrincipalInvestigator, HoldStatus, Diseasecat, Phase, Title, Conditions, stages, disp_biomarkers) #previous display order
       
       #changing display order
       #  selecTrial$comTb[selecTrial$comTb$HoldStatus=="closed",] %>% dplyr::select(Protocol, Title, Phase, stages, Conditions,  HoldStatus, Diseasecat, PrincipalInvestigator,  disp_biomarkers)
       #changing display columns
     #  selecTrial$comTb[selecTrial$comTb$HoldStatus=="closed",] %>% dplyr::select(Protocol, HoldStatus, Phase, Title, Disease, disp_disease1, lnOfTherapy, disp_biomarkers)
       selecTrial$comTb[selecTrial$comTb$HoldStatus=="closed",] %>% dplyr::select(Protocol, HoldStatus, Phase, Title, disp_disease1, lnOfTherapy, disp_biomarkers)
     }
   } # if closing for show_closed
   else
   { 
     if(length(input$selcolumns) > 0){
       #   selecTrial$comTb[selecTrial$comTb$HoldStatus=="open",input$selcolumns] commented oct25th
       selecTrial$comTb[selecTrial$comTb$HoldStatus!="closed",input$selcolumns]
     }
     else {
       
       #    selecTrial$comTb[selecTrial$comTb$HoldStatus=="open",] %>% dplyr::select(Protocol, HoldStatus, filtopencohort, Diseasecat, Phase, Title, Conditions, stages, lnOfTherapy, disp_biomarkers)
       #    selecTrial$comTb[selecTrial$comTb$HoldStatus=="open",] %>% dplyr::select(Protocol, HoldStatus, Diseasecat, Phase, Title, Conditions, stages, disp_biomarkers)
       #  selecTrial$comTb[selecTrial$comTb$HoldStatus!="closed",] %>% dplyr::select(Protocol, PrincipalInvestigator, HoldStatus, Diseasecat, Phase, Title, Conditions, stages, disp_biomarkers) #previous display order
       
       # changing display order
       # selecTrial$comTb[selecTrial$comTb$HoldStatus!="closed",] %>% dplyr::select(Protocol, Title, Phase, stages, Conditions,  HoldStatus, Diseasecat, PrincipalInvestigator,  disp_biomarkers)
       #changing display columns
  #     selecTrial$comTb[selecTrial$comTb$HoldStatus!="closed",] %>% dplyr::select(Protocol, HoldStatus, Phase, Title, Disease, disp_disease1, lnOfTherapy, disp_biomarkers)
       selecTrial$comTb[selecTrial$comTb$HoldStatus!="closed",] %>% dplyr::select(Protocol, HoldStatus, Phase, Title, disp_disease1, lnOfTherapy, disp_biomarkers)
        }
     
     
   }# else - for this - closing 
   
 }) #reactive
 
 #this is for expandable rows displaying the correct rows for open and close trials
 expandable_data <- reactive({
   if (input$show_closed) {
     #  selecTrial$comTb[!selecTrial$comTb$HoldStatus %in% "open", ] commented oct 25th
     selecTrial$comTb[selecTrial$comTb$HoldStatus %in% "closed", ]
   } else {
     #  selecTrial$comTb[selecTrial$comTb$HoldStatus == "open", ] commented oct 25th
     selecTrial$comTb[!selecTrial$comTb$HoldStatus == "closed", ]
   }
 })
 
 
 
 
 
 
 
 
 
  output$browsetable <- renderReactable({
  
     selecTrial$comTb = as_tibble(browse_tbl) #original
     
     
 #    selecTrial$comTb$comb <- paste(selecTrial$comTb$Protocol,selecTrial$comTb$Documentation,sep = "\n")  #june 5th
     
  # selecTrial$comTb <- selecTrial$comTb %>% mutate(ProtDoc = paste0('<a href="', Documentation, '">', Documentation, '</a>',Protocol, collapse = ":")) #june 5th
                 #          reactable::reactable( selecTrial$comTb %>% dplyr::select(Protocol, HoldStatus, Phase, Title, Disease, lnOfTherapy, disp_disease1, disp_biomarkers, Documentation),    # original
                                                 
  #  reactable::reactable( selecTrial$comTb %>% dplyr::select(Protocol,HoldStatus, Phase, Title, Disease, lnOfTherapy, disp_biomarkers), #june 5th    original         
                                                 
                          reactable::reactable(browse_data_initial_filtered(),                                                                  
                                               searchable = TRUE,
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
                #    reactable(selecTrial$comTb[index, ] %>% select(Link, Documentation, Name,Sponsor,StudyType, Location, TrialLastUpdate),
                              
                              reactable(expandable_data()[index, ]  %>% select(Link, Documentation, Name,Sponsor,StudyType, Location, TrialLastUpdate),          
                              
                              
                              defaultColDef = colDef(align = "center"),
                  #            columns = list(TrialLastUpdate = colDef(name = "Onsite Last Update"),Link = colDef(html = TRUE,name = "Trial")) #original
                  #  ), #original
                    
                    
                    columns = list(TrialLastUpdate = colDef(name = "Onsite Last Update"),Link = colDef(html = TRUE,name = "Trial"),Documentation = colDef(html=TRUE)) #june 6th
                  ), #june 6th
                    
                    # group 3: summary
              #      reactable(selecTrial$comTb[index, ] %>%
                                reactable(expandable_data()[index, ]  %>%
                                
                                select(Summary)),
                    
                    
                    # group 4: trial Status from .gov
                  #  reactable(selecTrial$comTb[index, ] %>%
                                
                                reactable(expandable_data()[index, ]  %>%
                                select(Status, StatusUpdate, LastUpdate, Gender, MinAge),
                              defaultColDef = colDef(align = "center"),
                              columns = list(Status = colDef(name = "Clinical.gov Status"),
                                             MinAge = colDef(name = "Minimum Age"),
                                             StatusUpdate = colDef(name = "Clinical.gov Verification Date"),
                                             LastUpdate = colDef(name = "Clinical.gov Last Update"))),
                    # group2: cohort info
               
                  #  reactable(selecTrial$comTb[index, ]$arms[[1]] %>% unnest(biomarker) %>%
                               
                                reactable(expandable_data()[index, ] $arms[[1]] %>% unnest(biomarker) %>%
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
             #       reactable(selecTrial$comTb[index, ] %>%
                                
                                reactable(expandable_data()[index, ] %>%
                                select(Conditions)),
                    
                    # group 3: disease information
                    
                   
              #      reactable(selecTrial$comTb[index, ]$disp_disease[[1]] %>% select(code, selection,stage))
                    
             reactable(expandable_data()[index, ]$disp_disease[[1]] %>% select(code, selection,stage))
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






