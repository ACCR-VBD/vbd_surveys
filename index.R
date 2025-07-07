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
  run_dm = TRUE,
  run_analysis = TRUE
)
source("R/setup.R")

#' # Block 1 - stakeholder survey -------------------------------------------

#' ## 1.1 - data management

if(controls$run_dm) {
  shs_0 = sh_001_load()
  shs_1 = sh_003_filters(shs_0)
  shs_2 = sh_002_labels(shs_1, lang="EN")
  shs_3 = sh_004_new_variables(shs_2)
  
  save(controls,shs_3,file=file.path(controls$savepoint,"shs_3.Rdata"))
}

#' ## 1.2 - data description

rmarkdown::render("1_stakeholder_survey_description.R", output_dir=controls$savepoint)
rmarkdown::render("2_stakeholder_survey_description_supp.R", output_dir=controls$savepoint)

browseURL(paste0(controls$savepoint,"/1_stakeholder_survey_description.html"))
browseURL(paste0(controls$savepoint,"/2_stakeholder_survey_description_supp.html"))

#' # Block 2 - BEready ----------------------------------------------------



#' # Block 3 - Reports ----------------------------------------------------

# rmd
