#' ---
#' title: VBD surveys
#' subtitle: Table formatting
#' author: jriou
#' date: 2025-06-13
#' ---

sh_100_add_header <- function(tbl, text, rownumber, sty = NULL) {
  updated_tbl <- gtsummary::modify_table_body(
    tbl,
    ~ dplyr::add_row(
      .x,
      variable = NA,
      var_type = NA,
      label = text,
      row_type = "label",
      .after = rownumber
    )
  ) %>%
    gtsummary::modify_header(label ~ "**Question**")
  
  return(updated_tbl)
}


sh_100_add_caption <- function(tbl, caption, ntable) {
  gt_tbl = tbl %>%
    gtsummary::as_gt() %>%
    gt::tab_caption(paste0("Table ", ntable, ". ", caption, "."))  %>% 
    gt::cols_width(
      label ~ gt::px(600),      # first column (variable label)
      stat_0 ~ gt::px(100)         # second column (e.g., "N = ...")
    )
  
  return(gt_tbl)
}

