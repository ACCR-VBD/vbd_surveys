#' ---
#' title: VBD surveys
#' subtitle: Open text answers
#' author: jriou
#' date: 2025-06-16
#' ---

br_101_open_text <- function(dat, var_, lab_) {
  
  tbl = dat %>% 
    dplyr::select(var_) %>% 
    gtsummary::tbl_summary(missing="ifany") %>% 
    gtsummary::modify_header(label ~ "**Question**")  %>% 
    gtsummary::modify_table_body(
      ~ dplyr::mutate(.x, label = dplyr::if_else(dplyr::row_number() == 1, lab_, label))
    )
  
  return(tbl)
  
}

