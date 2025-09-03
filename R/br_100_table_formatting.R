#' ---
#' title: VBD surveys
#' subtitle: Table formatting
#' author: jriou
#' date: 2025-06-13
#' ---

br_100_add_header <- function(tbl, text, rownumber, sty = NULL) {
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


br_100_add_caption <- function(tbl, caption, ntable) {
  gt_tbl = tbl %>%
    gtsummary::as_gt() %>%
    gt::tab_caption(paste0("Table ", ntable, ". ", caption, "."))  %>% 
    gt::cols_width(
      label ~ gt::px(600),      # first column (variable label)
      stat_0 ~ gt::px(100)         # second column (e.g., "N = ...")
    )
  
  return(gt_tbl)
}

br_100_add_question_header=function(tbl,orig_df,before_var){
  question_var=str_split_1(before_var,"___")[1]
  text=attr(orig_df[[question_var]],"label")
  updated_tbl <- gtsummary::modify_table_body(
    tbl,
    ~ dplyr::add_row(.x,label = text, 
                     row_type = "label",.before = min(which(.x$variable == before_var)),
                     variable="buffer")
  )   %>% 
    modify_bold(rows=(variable=="buffer"),columns=label)
  return(updated_tbl)
}

br_100_add_missing=function(tbl, grep_blocks,pos=NULL,orig_table=NULL){
  if(is.null(orig_table)){
    blocks = lapply(grep_blocks,function(grep_exp)grep(grep_exp,tbl$table_body$variable,value=T))
  } else{blocks = lapply(grep_blocks,function(grep_exp)grep(grep_exp,colnames(orig_table),value=T))}
  updated_tbl=tbl
  for (i in 1:length(blocks)){
    updated_tbl=updated_tbl %>% 
      modify_table_body(~ {
        if(is.null(orig_table)){
          miss_n <- sum(rowSums(is.na(tbl$inputs$data[blocks[[i]]])) == length(blocks[[i]]))
        }else{miss_n=sum(rowSums(is.na(orig_table[blocks[[i]]])) == length(blocks[[i]]))}
        if(is.null(pos)){
          pos    <- max(which(.x$variable %in% blocks[[i]]))
        }
        
        dplyr::add_row(
          .x,
          variable  = NA_character_,
          var_type  = NA_character_,
          label     = "(Missing)",
          row_type  = "missing",
          stat_0    = as.character(miss_n),
          .after    = pos
        )})
  }
  return(updated_tbl)
}
