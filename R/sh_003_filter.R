#' ---
#' title: VBD surveys 
#' subtitle: Filter out incomplete answers
#' author: jriou
#' date: 2025-06-10
#' ---

sh_003_filters = function(dat_in){
  
  # complete answers
  dat1 = dat_in %>% 
    dplyr::filter(form_complete==2) %>% 
    dplyr::filter(form_timestamp!="[not completed]")
  
  # dependencies between questions
  dat2 = dat1 %>% 
    dplyr::mutate(dplyr::across(dplyr::starts_with("b_1_1_"),~ if_else(b_1 != 2, 
                                                                       NA_real_, 
                                                                       .))) %>% 
    dplyr::mutate(dplyr::across(dplyr::starts_with("c_3___"),~ if_else(c_1___8 != 1 & c_1___10 != 1 & c_2___9 != 1 & c_2___11 != 1,
                                                                       .x,
                                                                       NA_real_)))
  
  # Return
  dat_out = dat2
  return(dat_out)
}