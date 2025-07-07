#' ---
#' title: VBD surveys 
#' subtitle: Load data
#' author: jriou
#' date: 2025-06-10
#' ---

br_001_load = function(){

  # Load BeReady
  dat_out = read.csv(file.path(controls$data_path,"1951BEreadyHauptstud-VectorborneDiseases_DATA_2025-06-12_1515.csv"))
  
  return(dat_out)
}


