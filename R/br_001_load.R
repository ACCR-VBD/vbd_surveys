#' ---
#' title: VBD surveys 
#' subtitle: Load data
#' author: jriou
#' date: 2025-06-10
#' ---

br_001_load = function(){

  # Load BeReady
  dat_out = read.csv(file.path(controls$data_path,"1951BEreadyHauptstud-VectorborneDiseases_DATA_2025-08-12_1144.csv"),sep=";")
  
  return(dat_out)
}


