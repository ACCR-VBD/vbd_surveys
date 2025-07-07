#' ---
#' title: VBD surveys 
#' subtitle: New variables
#' author: jriou
#' date: 2025-06-10
#' ---

br_004_new_variables = function(dat_in){
  
  # Dates
  dat1 = dat_in %>% 
    dplyr::mutate(form_timestamp = lubridate::ymd_hms(form_timestamp,quiet = TRUE),
                  form_date =  as_date(form_timestamp),.after=form_timestamp)
  
  # Save and return
  dat_out = dat1
  return(dat_out)
}