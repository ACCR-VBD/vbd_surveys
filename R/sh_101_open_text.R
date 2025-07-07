#' ---
#' title: VBD surveys
#' subtitle: Open text answers
#' author: jriou
#' date: 2025-06-16
#' ---

sh_101_open_text <- function(dat, var_, lab_) {
  
  tbl = dat %>% 
    dplyr::select(var_) %>% 
    gtsummary::tbl_summary(missing="ifany", 
                           missing_text = "(Missing)") %>% 
    gtsummary::modify_header(label ~ "**Question**")  %>% 
    gtsummary::modify_table_body(
      ~ dplyr::mutate(.x, label = dplyr::if_else(dplyr::row_number() == 1, lab_, label))
    ) %>% bold_labels() %>%
    modify_column_indent(columns="label",rows=row_type=="level",indent=0) %>% 
    gtsummary::as_gt() %>%
    tab_style(
      style = "padding-left:20px;",
      locations = cells_body(
        columns = "label",
        rows    = row_type == "level"
      )
    ) %>% 
    gt::cols_width(
      label ~ gt::px(600),      # first column (variable label)
      stat_0 ~ gt::px(100)         # second column (e.g., "N = ...")
    )
  
  return(tbl)
  
}

