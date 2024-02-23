library(tidyverse)
library(here)
library(jsonlite)
library(glue)
library(dplyr)
## parse all trial.full.ndjson and curate relevant info for a basic summary table

## reactive table 


parseTrials <- function(jsonfile) {
  
  trial <- fromJSON(jsonfile)

  # pulling out trial arms
  arm_groups = tibble(cohortlabel = trial$query$arm[[1]]$cohortlabel,
                      drug = trial$query$arm[[1]]$drug,
                      arm_type = trial$query$arm[[1]]$arm_type,
                      line_of_therapy = trial$query$arm[[1]]$line_of_therapy,
                      arm_hold_status = trial$query$arm[[1]]$arm_hold_status,
                      biomarker = trial$query$arm[[1]]$biomarker)
                      
         parsedTrial <- tibble(

    # info
    Protocol = trial$info$Protocol_No,
    NCT = trial$info$NCT,
    JIT = trial$info$jit,
    
    Name = trial$info$trial_name,

    # disease
    Disease = trial$disease$summary,
  
  disp_disease = list(disp_disease = trial$disease$details[[1]]),
  
    # query - general
  Title = trial$query$title,
    Status = trial$query$current_status,
    StatusUpdate = trial$query$status_verif_date,
    Sponsor = trial$query$sponsor,
    Summary = trial$query$brief_summary,
    Conditions = trial$query$conditions,
    Phase = trial$query$phase,
    StudyType = trial$query$type,
    MinAge = if(trial$query$min_age %>% is_empty()) {
      min_age = "Not Available"
    } else
    {
      trial$query$min_age
    },
    Gender = trial$query$gender,
    Link = trial$query$link,
 
    LastUpdate = trial$query$last_update_date,

    # query - cohorts w/ drug and biomarker information
    arms = list(arms = trial$query$arm[[1]] %>% unnest(biomarker)),

    # query - biomarkers only for display table
    disp_biomarkers = trial$query$arm[[1]]$biomarker %>%
    bind_rows() %>%
    select(summary) %>% distinct() %>% unlist() %>% na.omit() %>%
    paste0(collapse = " | "),
   
    HoldStatus = trial$query$trial_hold_status,
    Documentation = trial$query$docs,
 TrialLastUpdate = trial$query$doclastupdate,
 Location = trial$query$locations

  )
  return(parsedTrial)

 }             


###############################################

## read in trials from database to use for browse
# create the equivalent of the 'result' tibble


loadDbData <- function() {
  
  db <- mongolite::mongo(collection = "ClinicalTrials", 
              db = "aci", 
              url = db_url)
  
  
  
  
  # aggregate tibble
  db_tbl <- db$aggregate()[,2:4] %>% 
    unnest(cols = c(info, disease, query))
  
  
  db_tbl <- db_tbl %>% mutate(NameProtocol = glue("{trial_name} : {Protocol_No}", .sep=";")) %>% rename(
                               
                             #   %>% rename( 
    # info
   "Protocol" = Protocol_No,
    "JIT" = jit,
    "Name" = trial_name,
    
    # disease
    "Disease" = summary,
    
    # query - general
   "Title" = title,
    "Status" = current_status,
    "StatusUpdate" = status_verif_date,
    "Sponsor" = sponsor,
    "Summary" = brief_summary,
  
    "Conditions" = conditions,
    "Phase" = phase,
    "StudyType" = type,
   
    "MinAge" = min_age,
    "Gender" = gender,
    "Link" = link,
   
    "LastUpdate" = last_update_date,
    "HoldStatus" = trial_hold_status,
    "Documentation" = docs,
   "Location" = locations, 
   "TrialLastUpdate" = doclastupdate
  )
 
  db_tbl = db_tbl %>% mutate(disp_disease = db_tbl$details)
  db_tbl = db_tbl %>% mutate(disp_disease1 = sapply(db_tbl$details, "[[","code"))
  
  db_tbl = db_tbl %>% mutate(arms = db_tbl$arm)

 db_tbl$disp_biomarkers <- "NA"
 db_tbl$lnOfTherapy <- "NA"
  
  return(db_tbl)
  
}


source(here("R", "read_data.R"))

