#' ---
#' title: VBD surveys 
#' subtitle: Load data
#' author: jriou
#' date: 2025-06-10
#' ---

sh_001_load = function(){
  
  # Stakeholder survey
  dat0 = suppressMessages(readr::read_csv2(file.path(controls$data_path,"NCCSimpactsStakehold_DATA_2025-06-10_1142.csv"))) 
  
  # Supplementary info (canton and function)
  sup = suppressMessages(readr::read_csv2(file.path(controls$data_path,"NCCSimpactsStakehold_DATA_2025-06-10_1142_SUPP.csv"))) 
  
  # Return
  dat_out = dplyr::left_join(dat0,sup,by="record_id") %>% 
    dplyr::relocate(type,canton,.after=`function`)
  
  return(dat_out)
  }


