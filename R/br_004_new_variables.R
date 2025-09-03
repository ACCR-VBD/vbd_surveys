#'---
#' title: VBD surveys 
#' subtitle: New variables
#' author: jriou
#' date: 2025-06-10
#'---

br_004_new_variables = function(dat_in){
  
  dat_out <- dat_in %>% 
    mutate(
      ##------------------------------------------------------------------
      ## 1.  “Is this disease vector-borne?”  (7 items)
      ##     Correct = “Yes” for true VBD, “No” for non-VBD
      ##------------------------------------------------------------------
      kn_cls_westnile = if_else(bl_vbd_vector_dis___1 == "Yes", 1, 0, missing = NA_real_),
      kn_cls_dengue   = if_else(bl_vbd_vector_dis___2 == "Yes", 1, 0, missing = NA_real_),
      kn_cls_zika     = if_else(bl_vbd_vector_dis___3 == "Yes", 1, 0, missing = NA_real_),
      kn_cls_chikun   = if_else(bl_vbd_vector_dis___4 == "Yes", 1, 0, missing = NA_real_),
      kn_cls_lyme     = if_else(bl_vbd_vector_dis___5 == "Yes", 1, 0, missing = NA_real_),
      kn_cls_influ    = if_else(bl_vbd_vector_dis___6 == "No",  1, 0, missing = NA_real_),
      kn_cls_measles  = if_else(bl_vbd_vector_dis___7 == "No",  1, 0, missing = NA_real_),
      kn_cls_n        = kn_cls_westnile + kn_cls_dengue + kn_cls_zika + kn_cls_chikun +
        kn_cls_lyme     + kn_cls_influ  + kn_cls_measles,
      kn_cls_prop     = kn_cls_n / 7,
      
      ##------------------------------------------------------------------
      ## 2.  “Does this organism transmit diseases?” (4 items)
      ##     Correct = “Yes” for ticks & mosquitoes, “No” for wasps & bed bugs
      ##------------------------------------------------------------------
      kn_org_ticks = if_else(bl_vbd_transmitter___1 == "Yes", 1, 0, missing = NA_real_),
      kn_org_mosq  = if_else(bl_vbd_transmitter___2 == "Yes", 1, 0, missing = NA_real_),
      kn_org_wasp  = if_else(bl_vbd_transmitter___3 == "No",  1, 0, missing = NA_real_),
      kn_org_bed   = if_else(bl_vbd_transmitter___4 == "No",  1, 0, missing = NA_real_),
      kn_org_n     = kn_org_ticks + kn_org_mosq + kn_org_wasp + kn_org_bed,
      kn_org_prop  = kn_org_n / 4,
      
      ##------------------------------------------------------------------
      ## 3.  Disease → vector matching (8 items)
      ##     Correct answer shown in comments
      ##------------------------------------------------------------------
      kn_mat_tbe     = if_else(bl_vbd_tick_enc  == "Ticks",        1, 0, missing = NA_real_),  # TBE
      kn_mat_wnf     = if_else(bl_vbd_wnf       == "Mosquitoes",   1, 0, missing = NA_real_),  # WN fever
      kn_mat_dengue  = if_else(bl_vbd_dengue    == "Mosquitoes",   1, 0, missing = NA_real_),
      kn_mat_zika    = if_else(bl_vbd_zika      == "Mosquitoes",   1, 0, missing = NA_real_),
      kn_mat_chikun  = if_else(bl_vbd_chikun    == "Mosquitoes",   1, 0, missing = NA_real_),
      kn_mat_lyme    = if_else(bl_vbd_lyme      == "Ticks",        1, 0, missing = NA_real_),
      kn_mat_influ   = if_else(bl_vbd_influenza == "None of these",1, 0, missing = NA_real_),
      kn_mat_measles = if_else(bl_vbd_measle    == "None of these",1, 0, missing = NA_real_),
      kn_mat_n       = kn_mat_tbe + kn_mat_wnf + kn_mat_dengue + kn_mat_zika +
        kn_mat_chikun + kn_mat_lyme + kn_mat_influ + kn_mat_measles,
      kn_mat_prop    = kn_mat_n / 8,
      
      ##------------------------------------------------------------------
      ## 4.  Overall knowledge score (max = 19)
      ##------------------------------------------------------------------
      kn_total_n    = kn_cls_n + kn_org_n + kn_mat_n,
      kn_total_prop = kn_total_n / 19,
      
      ##------------------------------------------------------------------
      ## 5.  Internal consistency checks (“contradictions”)
      ##------------------------------------------------------------------
      ##  • Said “Ticks do NOT transmit diseases” AND used “Ticks” for Lyme or TBE
      contrad_ticks = if_else(
        bl_vbd_transmitter___1 == "No" &
          (bl_vbd_tick_enc == "Ticks" | bl_vbd_lyme == "Ticks"),
        1, 0, missing = NA_real_
      ),
      
      ##  • Said “Mosquitoes do NOT transmit diseases” AND used “Mosquitoes” for any arbovirus
      contrad_mosq = if_else(
        bl_vbd_transmitter___2 == "No" &
          (bl_vbd_wnf == "Mosquitoes" | bl_vbd_dengue == "Mosquitoes" |
             bl_vbd_zika == "Mosquitoes" | bl_vbd_chikun == "Mosquitoes"),
        1, 0, missing = NA_real_
      ),
      
      ##  • At least one contradiction?
      contrad_any = case_when(
        is.na(contrad_ticks) | is.na(contrad_mosq)                 ~ NA_real_,
        (contrad_ticks + contrad_mosq) > 0                         ~ 1,
        TRUE                                                       ~ 0
      )
    )
  
  ## addition of elevation and zone variables :
  elevation=read.csv("data/BeReady_extracted_alt.csv")
  elevation=elevation %>% select(record_id,mean_elev,zone)
  
  dat_out <- left_join(dat_out, elevation, by = "record_id") %>% 
    
    ## ───────────────────────────────────────────────────────────────
    ##  Variable- and value-labels for the new derived variables
    ## ───────────────────────────────────────────────────────────────
    labelled::set_variable_labels(
      ## 1. classification (7)
      kn_cls_westnile = "West Nile correctly classified as VBD",
      kn_cls_dengue   = "Dengue correctly classified as VBD",
      kn_cls_zika     = "Zika correctly classified as VBD",
      kn_cls_chikun   = "Chikungunya correctly classified as VBD",
      kn_cls_lyme     = "Lyme correctly classified as VBD",
      kn_cls_influ    = "Influenza correctly classified as not a VBD",
      kn_cls_measles  = "Measles correctly classified as not a VBD",
      kn_cls_n        = "number of correct VBD",
      kn_cls_prop     = "proportion of correct VBD",
      
      ## 2. organisms (4)
      kn_org_ticks    = "Ticks correctly identified as transmitting disease",
      kn_org_mosq     = "Mosquitoes correctly identified as transmitting disease",
      kn_org_wasp     = "Wasps correctly identified as not transmitting disease",
      kn_org_bed      = "Bed bugs identified as not transmitting disease",
      kn_org_n        = "number of correct transmitter",
      kn_org_prop     = "proportion of correct transmitter",
      
      ## 3. matches (8)
      kn_mat_tbe      = "TBE correctly matched to ticks",
      kn_mat_wnf      = "West Nile correctly matched to mosquitoes",
      kn_mat_dengue   = "Dengue correctly matched to mosquitoes",
      kn_mat_zika     = "Zika correctly matched to mosquitoes",
      kn_mat_chikun   = "Chikungunya correctly matched to mosquitoes",
      kn_mat_lyme     = "Lyme correctly matched to ticks",
      kn_mat_influ    = "Influenza correctly matched to none",
      kn_mat_measles  = "Measles correctly matched to none",
      kn_mat_n        = "number of correct match",
      kn_mat_prop     = "proportion of correct match",
      
      ## 4. overall
      kn_total_n      = "Number of correct response",
      kn_total_prop   = "Proportion of correct response",
      
      ## 5. internal consistency
      contrad_ticks   = "Denied tick as transmitter and identified ticks as transmiting a disease",
      contrad_mosq    = "Denied mosquitoes transmitter and identified mosquitoes as transmiting a disease",
      contrad_any     = "Any of the two contradiction",
      
      ## 6. added context variables
      mean_elev       = "Mean elevation of participant municipality (m)",
      zone            = "Mean elevation > 800m"
    ) %>% 
    labelled::set_value_labels(
      ## binary knowledge items
      kn_cls_westnile = c("Incorrect" = 0, "Correct" = 1),
      kn_cls_dengue   = c("Incorrect" = 0, "Correct" = 1),
      kn_cls_zika     = c("Incorrect" = 0, "Correct" = 1),
      kn_cls_chikun   = c("Incorrect" = 0, "Correct" = 1),
      kn_cls_lyme     = c("Incorrect" = 0, "Correct" = 1),
      kn_cls_influ    = c("Incorrect" = 0, "Correct" = 1),
      kn_cls_measles  = c("Incorrect" = 0, "Correct" = 1),
      
      kn_org_ticks    = c("Incorrect" = 0, "Correct" = 1),
      kn_org_mosq     = c("Incorrect" = 0, "Correct" = 1),
      kn_org_wasp     = c("Incorrect" = 0, "Correct" = 1),
      kn_org_bed      = c("Incorrect" = 0, "Correct" = 1),
      
      kn_mat_tbe      = c("Incorrect" = 0, "Correct" = 1),
      kn_mat_wnf      = c("Incorrect" = 0, "Correct" = 1),
      kn_mat_dengue   = c("Incorrect" = 0, "Correct" = 1),
      kn_mat_zika     = c("Incorrect" = 0, "Correct" = 1),
      kn_mat_chikun   = c("Incorrect" = 0, "Correct" = 1),
      kn_mat_lyme     = c("Incorrect" = 0, "Correct" = 1),
      kn_mat_influ    = c("Incorrect" = 0, "Correct" = 1),
      kn_mat_measles  = c("Incorrect" = 0, "Correct" = 1),
      
      contrad_ticks   = c("Consistent" = 0, "Contradiction" = 1),
      contrad_mosq    = c("Consistent" = 0, "Contradiction" = 1),
      contrad_any     = c("Consistent" = 0, "Contradiction" = 1)
    ) %>% mutate(
      ## convert every 0/1 knowledge or contradiction flag to labelled factors
      across(c(starts_with("kn_cls_"), starts_with("kn_org_"),
               starts_with("kn_mat_"), starts_with("contrad_"),zone,-ends_with("_n"),-ends_with("_prop")),
             labelled::to_factor)
    )

  return(dat_out)
}
