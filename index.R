#' ---
#' title: VBD surveys 
#' subtitle: index file
#' author: jriou
#' date: 2025-06-10
#' ---

#' # Block 0 - set-up --------------------------------------------------------

controls = list(
  analysis_date = "2025-06-26",
  data_path = file.path("data/"),
  run_dm_shs = FALSE,
  run_analysis_shs = FALSE,
  run_dm_brs = TRUE,
  run_analysis_brs = TRUE
)
source("R/setup.R")

#' # Block 1 - stakeholder survey -------------------------------------------

#' ## 1.1 - data management

if(controls$run_dm_shs) {
  shs_0 = sh_001_load()
  shs_1 = sh_003_filters(shs_0)
  shs_2 = sh_002_labels(shs_1, lang="EN")
  shs_3 = sh_004_new_variables(shs_2)
  
  save(controls,shs_3,file=file.path(controls$savepoint,"shs_3.Rdata"))
}

#' ## 1.2 - data description

if(controls$run_analysis_shs){
  
  rmarkdown::render("1_stakeholder_survey_description.R", output_dir=controls$savepoint)
  rmarkdown::render("2_stakeholder_survey_description_supp.R", output_dir=controls$savepoint)
  
  browseURL(paste0(controls$savepoint,"/1_stakeholder_survey_description.html"))
  browseURL(paste0(controls$savepoint,"/2_stakeholder_survey_description_supp.html"))
  
}

#' # Block 2 - BEready ----------------------------------------------------

if(controls$run_dm_brs) {
  brs_0 = br_001_load()
  brs_1 = br_003_filters(brs_0)
  brs_2 = br_002_labels(brs_1, lang="EN")
  brs_3 = br_004_new_variables(brs_2)
  
  save(controls,brs_3,file=file.path(controls$savepoint,"brs_3.Rdata"))
}


#' # Block 3 - Reports ----------------------------------------------------

if(controls$run_analysis_brs){
rmarkdown::render("1_BeReady_survey_description.R", output_dir=controls$savepoint)
rmarkdown::render("2_BeReady_survey_description_supp.R", output_dir=controls$savepoint)
browseURL(paste0(controls$savepoint,"/1_BeReady_survey_description.html"))
browseURL(paste0(controls$savepoint,"/2_BeReady_survey_description_supp.html"))
}
# rmd
