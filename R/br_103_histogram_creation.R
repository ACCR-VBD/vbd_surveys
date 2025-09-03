library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

to01 <- function(x) {
  if (is.numeric(x)) return(x)
  dplyr::case_when(
    as.character(x) == "Correct"   ~ 1,
    as.character(x) == "Incorrect" ~ 0,
    TRUE                           ~ NA_real_
  )
}

create_hist = function(dat_in){
  # -----------------------------
  # 0) Parameters and binning
  # -----------------------------
  bw <- 0.1
  breaks <- seq(0, 1, by = bw)
  
  dat_out = brs_3 %>% filter(filtering_var$filtering_var,!is.na(kn_total_prop))
  
  dat_binned <- dat_out %>%
    mutate(
      score_bin = cut(kn_total_prop, breaks = breaks, include.lowest = TRUE, right = T)
    )
  
  # Plain frequency histogram
  p_freq <- ggplot(dat_binned, aes(x = kn_total_prop)) +
    geom_histogram(binwidth = bw, color = "black", fill = "grey85", linewidth = 0.4) +
    labs(
      x = "Overall knowledge score (proportion correct)",
      y = "Number of respondents",
      title = "Distribution of knowledge scores"
    ) +
    theme_minimal(base_size = 12)
  
  # -------------------------------------------------------------
  # 1) Stacked contributions per bin (mosquito / tick / distractor)
  #     - For each respondent: count correct by domain (0/1 items).
  #     - Convert to contributions to overall proportion by dividing by 19.
  #     - Average these contributions within each bin.
  #     - Stack the three averages; their sum = mean(kn_total_prop) in the bin.
  # -------------------------------------------------------------
  
  # Per-respondent correct counts by domain (using available 0/1 items)
  domain_counts <- dat_out %>%
    transmute(
      kn_total_prop,
      score_bin = dat_binned$score_bin,
      
      mosq_correct = rowSums(cbind(
        to01(kn_cls_westnile), to01(kn_cls_dengue), to01(kn_cls_zika), to01(kn_cls_chikun),
        to01(kn_org_mosq),
        to01(kn_mat_wnf), to01(kn_mat_dengue), to01(kn_mat_zika), to01(kn_mat_chikun)
      ), na.rm = TRUE),
      
      tick_correct = rowSums(cbind(
        to01(kn_cls_lyme), to01(kn_org_ticks),
        to01(kn_mat_tbe),  to01(kn_mat_lyme)
      ), na.rm = TRUE),
      
      false_correct = rowSums(cbind(
        to01(kn_cls_influ), to01(kn_cls_measles),
        to01(kn_org_wasp),  to01(kn_org_bed),
        to01(kn_mat_influ), to01(kn_mat_measles)
      ), na.rm = TRUE)
    ) %>%
    mutate(
      contr_mosq  = mosq_correct  / 19,
      contr_tick  = tick_correct  / 19,
      contr_false = false_correct / 19
    )
  
  # Bin-level averages of contributions (these sum to mean overall proportion in each bin)
  bin_summary <- domain_counts %>%
    group_by(score_bin) %>%
    summarise(
      mean_contr_mosq  = mean(contr_mosq,  na.rm = TRUE),
      mean_contr_tick  = mean(contr_tick,  na.rm = TRUE),
      mean_contr_false = mean(contr_false, na.rm = TRUE),
      mean_total_prop  = mean(kn_total_prop, na.rm = TRUE),  # for reference/diagnostic
      n_bin            = dplyr::n(),
      .groups = "drop"
    ) %>%
    pivot_longer(c(mean_contr_mosq, mean_contr_tick, mean_contr_false),
                 names_to = "domain", values_to = "mean_contribution") %>%
    mutate(
      domain = recode(domain,
                      mean_contr_mosq  = "Mosquito-related",
                      mean_contr_tick  = "Tick-related",
                      mean_contr_false = "Distractors (wasp/bed bugs)"),
      score_bin = fct_inorder(score_bin)
    )
  
  # Stacked contributions plot (heights sum to bin mean of kn_total_prop)
  p_stack_contrib <- ggplot(bin_summary,
                            aes(x = score_bin, y = mean_contribution, fill = domain)) +
    geom_col(color = "white") +
    labs(
      x = "Overall knowledge score bins",
      y = "Average knowledge score stacked by vectors",
      fill = "Domain",
      title = "Decomposition of knowledge score by vectors within overall score bins"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.major.x = element_blank())
  
  # -------------------------------------------------------------
  # 2) Stacked contributions per test item (Disease/Vector/Disease-vector matching)
  #     - For each respondent: count correct by domain (0/1 items).
  #     - Convert to contributions to overall proportion by dividing by 19.
  #     - Average these contributions within each bin.
  #     - Stack the three averages; their sum = mean(kn_total_prop) in the bin.
  # -------------------------------------------------------------
  
  cat_contrib <- dat_out %>%
    transmute(
      kn_total_prop,
      score_bin = dat_binned$score_bin,
      contr_cls = kn_cls_n / 19,  # 7-item classification block
      contr_org = kn_org_n / 19,  # 4-item organism block
      contr_mat = kn_mat_n / 19   # 8-item matching block
    )
  
  # Bin-level averages of category contributions (sum to mean overall proportion in each bin)
  cat_summary <- cat_contrib %>%
    group_by(score_bin) %>%
    summarise(
      mean_contr_cls = mean(contr_cls, na.rm = TRUE),
      mean_contr_org = mean(contr_org, na.rm = TRUE),
      mean_contr_mat = mean(contr_mat, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(
      c(mean_contr_cls, mean_contr_org, mean_contr_mat),
      names_to = "category", values_to = "mean_contribution"
    ) %>%
    mutate(
      category = dplyr::recode(category,
                               mean_contr_cls = "Disease classification",
                               mean_contr_org = "Vector identification",
                               mean_contr_mat = "Disease–vector matching"),
      score_bin = forcats::fct_inorder(score_bin)
    )
  
  # Stacked contributions plot by knowledge item categories
  p_stack_categories <- ggplot(cat_summary,
                               aes(x = score_bin, y = mean_contribution, fill = category)) +
    geom_col(color = "white") +
    scale_fill_brewer(palette = "Set2") +
    labs(
      x = "Overall knowledge score bins",
      y = "Average knowledge score stacked by test items",
      fill = "Category",
      title = "Decomposition of knowledge score by test items within overall score bins"
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.major.x = element_blank())
  
  return(list(freq_histogram=p_freq,vect_domain=p_stack_contrib,item_domain=p_stack_categories))
  
}



