library(here)
library(dplyr)
library(tidyverse)
library(shinyjs)
### read in config parameters

## trials data location
t_d_d <- config::get("trial_data_dir")
trial_data_dir <- if_else(t_d_d %>% fs::is_absolute_path(), t_d_d, t_d_d %>% here())

## patient data location
p_d_f <- config::get("pt_data_file") 
pt_data_file <- if_else(p_d_f %>% fs::is_absolute_path(), p_d_f, p_d_f %>% here())

## trial data storage format
storage <- config::get("storage")

if (storage == "json") {
  # create a combined tibble
  trialsfiles <- dir(path = trial_data_dir, pattern = "*.full.ndjson", full.names = T)
  
  result <- trialsfiles %>% map(parseTrials) %>% bind_rows()
  browse_tbl <<- result
  
}

if (storage == "db") {
  # look for active mongod process based on docker status
  docker <- config::get("docker")
  
  if (docker == "yes") {
	#connecting with the service name of mongo
      db_url <<- "mongodb://mongodba:27017/aci"
}
  
  if (docker == "no") {
    
    db_url <<- "mongodb://127.0.0.1:27017/aci"
  }
  
  result <<- loadDbData()
  browse_tbl <<- result
 

}

for (i in 1:nrow(browse_tbl)) {
  # n <- browse_tbl$arm[[i]]$biomarker %>% bind_rows() %>% select(summary) %>% distinct() %>% unlist() %>% na.omit() %>% paste0(collapse = "|")
  # browse_tbl$disp_biomarkers[i] <- n
  n <- browse_tbl$arm[[i]]$biomarker %>% bind_rows() %>% 
    
    #filtering for biomarkers only that belong to inclusion criteria
    # select(`Selection`,summary) %>% filter(`Selection` == "include") %>%  ## previous working
    
    #filtering for biomarkers only that belong to inclusion and exclusion criteria
    select(`Selection`,summary) %>% filter(`Selection` == "include" | `Selection` == "exclude") %>%
    
    select(summary) %>% distinct() %>% unlist() %>% na.omit() %>% paste0(collapse = "|")
  
  
  browse_tbl$disp_biomarkers[i] <- n
  
  
  
  
  
}

for (e in 1:nrow(browse_tbl)) {
  se <- browse_tbl$arm[[e]] %>% bind_rows()%>% separate_rows(line_of_therapy,sep =";") %>% select(line_of_therapy) %>% distinct() %>% unlist() %>% na.omit() %>% paste0(collapse = " | ")
  browse_tbl$lnOfTherapy[e] <- se
}


# Make dataframe for each of the filtration criteria - for Drug, cancer type, stage, location and line of therapy 

#drugAv = browse_tbl %>% select(arms) %>% unnest(arms) %>% select(drug) %>% distinct()
diseasAv = browse_tbl %>% select(disp_disease) %>% unnest(disp_disease) %>% select(code) %>% distinct()

#stageAv = browse_tbl %>% select(disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";") %>% select(stage) %>% distinct()

stageAv = browse_tbl %>% select(disp_disease) %>% unnest(disp_disease) %>% separate_rows(stage,sep = ";") %>% mutate(stage=trimws(stage)) %>% distinct()
#locAv = browse_tbl %>% select(Location) %>% distinct()

#trialTyAv = browse_tbl %>% select(JIT) %>% distinct()

locAv = browse_tbl %>% select(Location) %>% separate_rows(Location,sep=",") %>% mutate(Location=trimws(Location)) %>% distinct()
trialTyAv = browse_tbl %>% select(JIT) %>%  mutate(JIT =trimws(JIT)) %>% distinct()


lineoftxAv = browse_tbl %>% select(arms) %>% unnest(arms) %>% separate_rows(line_of_therapy,sep = c(";")) %>% select(line_of_therapy) %>% distinct() 

seldiscolumns<- browse_tbl %>% select(Protocol, JIT, Title, Summary, Phase, Title, HoldStatus, Conditions, lnOfTherapy, disp_biomarkers)