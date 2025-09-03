#' ---
#' title:  Knowledge, Attitudes and Practices on Vector-Borne Diseases in Switzerland – 2025
#' subtitle: Supplement
#' author: Lilian Goepp, Nina Huber, Arlette Szelecsenyi, Julien Riou
#' date: "`r Sys.Date()`"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#'     number_sections: true
#'     code_folding: hide
#'     theme: cosmo
#'     highlight: pygments
#'     fig_width: 10
#'     fig_height: 8
#'     fig_caption: true
#' ---

#+ results="hide", warnings="false", echo="false"
analysis_date = "2025-09-03"
load(paste0("savepoints/savepoint_",analysis_date,"/brs_3.Rdata"))
source("R/setup.R")
knitr::opts_chunk$set(echo = FALSE)
set_gtsummary_theme(list(
  "tbl_summary-fn:percent_fun" = function(x) style_percent(x, digits = 1)
))

#' # Sociodemography and exposure

#' ## Sociodemography

brs_3 %>%
  select(bl_sex, bl_gender_def, bl_education, bl_live_ch_since,bl_scg_marital_status,isco_lvl1) %>%
  tbl_summary(missing = "ifany",
              missing_text = "(Missing)") %>%
  modify_bold(rows=(variable=="bl_sex"&row_type=="label"),columns=label) %>% 
  modify_bold(rows=(variable=="bl_gender_def"&row_type=="label"),columns=label) %>% 
  modify_bold(rows=(variable=="bl_education"&row_type=="label"),columns=label) %>% 
  modify_bold(rows=(variable=="bl_live_ch_since"&row_type=="label"),columns=label) %>% 
  modify_bold(rows=(variable=="bl_scg_marital_status"&row_type=="label"),columns=label) %>% 
  modify_bold(rows=(variable=="isco_lvl1"&row_type=="label"),columns=label) %>% 
  br_100_add_caption("Socio-demographic characteristics of respondents", 1)

hist(brs_3$age_incl, main="Figure 1: Age at inclusion",xlab="Age (years)",breaks = 30)

#' ## Exposures from inclusion questionnaire

#' Almost all participants (98.2%) reported having received vaccines at some point, 
#' and most indicated full mobility, with 87.1% having no problems walking and fewer than 
#' 4% reporting moderate to severe difficulties. A large majority (99.8%) had traveled abroad 
#' for more than two days, with Europe being the most frequently visited continent (94%), 
#' followed by North America (60.4%), Asia (54.1%), and Africa (46.5%). When asked about 
#' future pandemic preparedness, respondents highlighted the importance of combating 
#' misinformation (69.7%) and maintaining public awareness (68.2%) and government commitment (55.8%). 
#' Other challenges mentioned included strengthening pathogen surveillance (35.4%) and addressing 
#' regulatory barriers in vaccine production (28.6%), while climate change mitigation (18.2%) and 
#' ventilation improvements (12.8%) were less commonly emphasized.

brs_3 %>%
  select(bl_vacc_yn,eq5d_mb_5l_swi_ger,bl_abroad_yn, starts_with("bl_continent___"), starts_with("bl_ppre_challenge___")) %>%
  tbl_summary(missing = "ifany",
              missing_text = "(Missing)",
              type         = list(bl_vacc_yn ~ "categorical",
                                  bl_abroad_yn ~ "categorical")) %>%
  modify_bold(rows=(variable=="bl_vacc_yn"&row_type=="label"),columns=label) %>% 
  modify_bold(rows=(variable=="bl_abroad_yn"&row_type=="label"),columns=label) %>% 
  modify_bold(rows=(variable=="eq5d_mb_5l_swi_ger"&row_type=="label"),columns=label) %>% 
  br_100_add_question_header(brs_3,"bl_continent___1") %>% 
  br_100_add_question_header(brs_3,"bl_ppre_challenge___1") %>% 
  br_100_add_missing(c("bl_ppre_challenge___","bl_continent___")) %>% 
  modify_table_body(
    ~ dplyr::filter(.x, !(grepl("___",variable) & row_type == "missing"))) %>%
  br_100_add_caption("Exposures from inclusion questionnaire", 2) %>% 
  tab_style(
    style = "padding-left:20px;",
    locations = cells_body(
      columns = "label",
      rows    = row_type == "label" & grepl("___",variable)
    )
  )

#' The EQ VAS records the patient’s self-rated health on a vertical visual analogue scale where the endpoints 
#' are labelled ‘The best health you can imagine’ and ‘The worst health you can imagine’. 
#' The VAS can be used as a quantitative measure of health outcome that reflects the patient’s own judgement.
#' We observe an average of 83 (SD 12.8), consistent with a representative survey in 
#' French-speaking Switzerland reporting a mean EQ-VAS of 81.7 (SD 15.5). (cite https://doi.org/10.1111/j.1524-4733.2010.00727.x)

hist(brs_3$eq5d5l_vas2_swi_ger, main = "EQ-VAS scale",breaks = 30)

#' ## Municipality of residence
#' 
#' For each municipality in the canton of Bern, we mapped the number of survey respondents.
#' The map illustrates the geographic distribution of the surveyed population, with a markedly 
#' higher concentration of respondents in the city of Bern than in the rest of the canton.

suppressWarnings(create_respondent_map())

#' # General knowledge
#' 

#' ## Baseline control variables
#' 

brs_3 %>% 
  select(bl_vbd_tick_yn) %>% 
  tbl_summary(
    missing      = "ifany",
    missing_text = "(Missing)"
  ) %>% 
  modify_bold(rows=(variable=="bl_vbd_tick_yn"&row_type=="label"),columns=label) %>%
  modify_header(label ~ "**Introductory text**") %>% 
  br_100_add_caption("Understanding check up", 3)

brs_3 %>% 
  select(starts_with("bl_vbd_diseases___"),starts_with("bl_vbd_organism___")) %>% 
  tbl_summary(
    missing      = "ifany",
    missing_text = "(Missing)"
  ) %>% 
  br_100_add_question_header(
    brs_3,
    "bl_vbd_diseases___1") %>%
  br_100_add_question_header(
    brs_3,
    "bl_vbd_organism___1") %>%
  br_100_add_missing(c("bl_vbd_diseases___","bl_vbd_organism___")) %>% 
  modify_table_body(
    ~ dplyr::filter(.x, !(grepl("___",variable) & row_type == "missing"))) %>% 
  modify_bold(rows=(variable=="bl_vbd_tick_yn"&row_type=="label"),columns=label) %>%
  modify_header(label ~ "**Question**") %>% 
  br_100_add_caption("Preliminary knowledge check-up", 4) %>% 
  tab_style(
    style = "padding-left:20px;",
    locations = cells_body(
      columns = "label",
      rows    = row_type == "label" & grepl("___",variable)
    )
  )

#' A majority of participants answered consistently when linking vectors to disease transmission, 
#' but a minority displayed contradictions. About 6% denied ticks or mosquitoes as transmitters 
#' while simultaneously identifying them as responsible for disease transmission. 
#' In total, 10.7% of respondents contradicted themselves in at least one case. 
#' This suggests that while knowledge was generally coherent, a small but notable share 
#' of respondents held conflicting beliefs or had difficulties applying knowledge consistently.

#+ contradictions-table
brs_3 %>%
  select(contrad_ticks, contrad_mosq, contrad_any) %>%
  tbl_summary(missing = "ifany",
              missing_text = "(Missing)") %>%
  modify_bold(rows=(variable=="contrad_ticks"&row_type=="label"),columns=label) %>%
  modify_bold(rows=(variable=="contrad_mosq"&row_type=="label"),columns=label) %>%
  modify_bold(rows=(variable=="contrad_any"&row_type=="label"),columns=label) %>%
  modify_header(label ~ "**Question**") %>%
  br_100_add_caption("Logical contradictions in respondents’ answers", 5) %>%
  tab_style(
    style = "padding-left:20px;",
    locations = cells_body(
      columns = "label",
      rows    = row_type == "label" & grepl("___",variable)
    )
  )

#' ## Item-level accuracy
#' 

#+ knowledge-item-table
brs_3 %>%
  filter(filtering_var$filtering_var) %>% 
  select(starts_with("kn_cls_"),-kn_cls_n,-kn_cls_prop) %>%
  tbl_summary(missing = "ifany",
              missing_text = "(Missing)") %>%
  modify_table_body(~ {
    dplyr::add_row(
      .x,
      variable  = NA_character_,
      var_type  = NA_character_,
      label     = "Question : Which of these diseases do you think are vector-borne diseases?",
      row_type  = "test_question",
      stat_0    = "",
      .before    = 1
    )}) %>% 
  modify_bold(rows=(row_type=="test_question"),columns=label) %>%
  br_100_add_missing("bl_vbd_vector_dis___",pos=1,orig_table = brs_3) %>%
  modify_bold(rows=(variable=="kn_cls_chikun"&row_type=="label"),columns=label) %>%
  modify_bold(rows=(variable=="kn_cls_westnile"&row_type=="label"),columns=label) %>%
  modify_bold(rows=(variable=="kn_cls_influ"&row_type=="label"),columns=label) %>%
  modify_bold(rows=(variable=="kn_cls_measles"&row_type=="label"),columns=label) %>%
  modify_bold(rows=(variable=="kn_cls_lyme"&row_type=="label"),columns=label) %>%
  modify_bold(rows=(variable=="kn_cls_dengue"&row_type=="label"),columns=label) %>%
  modify_bold(rows=(variable=="kn_cls_zika"&row_type=="label"),columns=label) %>%
  modify_table_body(
    ~ dplyr::filter(.x, !(grepl("_",variable) & row_type == "missing"))) %>%
  modify_header(label ~ "**Test item**") %>% 
  br_100_add_caption("Vector-borne disease test item", 6)%>% 
  tab_style(
    style = "padding-left:20px;",
    locations = cells_body(
      columns = "label",
      rows    = row_type == "label" & grepl("___",variable)
    )
  )

brs_3 %>%
  filter(filtering_var$filtering_var) %>% 
  select(starts_with("kn_org_"),-kn_org_n,-kn_org_prop) %>%
  tbl_summary(missing = "ifany",
              missing_text = "(Missing)") %>%
  modify_table_body(~ {
    dplyr::add_row(
      .x,
      variable  = NA_character_,
      var_type  = NA_character_,
      label     = "Question : Which of these organisms transmit diseases to humans?",
      row_type  = "test_question",
      stat_0    = "",
      .before    = 1
    )}) %>% 
  modify_bold(rows=(row_type=="test_question"),columns=label) %>%
  
  br_100_add_missing("bl_vbd_transmitter___",pos=1,orig_table = brs_3) %>% 
  
  modify_bold(rows=(variable=="kn_org_ticks"&row_type=="label"),columns=label) %>%
  modify_bold(rows=(variable=="kn_org_mosq"&row_type=="label"),columns=label) %>%
  modify_bold(rows=(variable=="kn_org_wasp"&row_type=="label"),columns=label) %>%
  modify_bold(rows=(variable=="kn_org_bed"&row_type=="label"),columns=label) %>%
  
  modify_table_body(
    ~ dplyr::filter(.x, !(grepl("_",variable) & row_type == "missing"))) %>%
  modify_header(label ~ "**Test item**") %>% 
  br_100_add_caption("Vector test item", 7)%>% 
  tab_style(
    style = "padding-left:20px;",
    locations = cells_body(
      columns = "label",
      rows    = row_type == "label" & grepl("___",variable)
    )
  )

brs_3 %>% 
  filter(filtering_var$filtering_var) %>% 
  select(starts_with("kn_mat_"), -kn_mat_n, -kn_mat_prop) %>% 
  tbl_summary(
    missing      = "ifany",
    missing_text = "(Missing)"
  ) %>% 
  modify_table_body(~ dplyr::add_row(
    .x,
    variable  = NA_character_,
    var_type  = NA_character_,
    label     = "Question : Match each disease on the left with its vector on the right (one per row). E.g. ’tick-borne encephalitis’ -> ’ticks’.",
    row_type  = "test_question",
    stat_0    = "",
    .before   = 1
  )) %>% 
  modify_bold(rows = row_type == "test_question", columns = label) %>% 
  modify_bold(rows=(variable=="kn_mat_tbe"&row_type=="label"),columns=label) %>%
  modify_bold(rows=(variable=="kn_mat_wnf"&row_type=="label"),columns=label) %>%
  modify_bold(rows=(variable=="kn_mat_dengue"&row_type=="label"),columns=label) %>%
  modify_bold(rows=(variable=="kn_mat_zika"&row_type=="label"),columns=label) %>%
  modify_bold(rows=(variable=="kn_mat_chikun"&row_type=="label"),columns=label) %>%
  modify_bold(rows=(variable=="kn_mat_lyme"&row_type=="label"),columns=label) %>%
  modify_bold(rows=(variable=="kn_mat_influ"&row_type=="label"),columns=label) %>%
  modify_bold(rows=(variable=="kn_mat_measles"&row_type=="label"),columns=label) %>%
  modify_table_body(
    ~ dplyr::filter(.x, !(grepl("_",variable) & row_type == "missing"))) %>%
  modify_header(label ~ "**Test item**") %>% 
  br_100_add_caption("Disease–vector matching test item", 8)

#' ## Overall knowledge score
#' 
#' We hereby display a more detailed visualization of the repartition of the overall knowledge score by highlighting 
#' the relative contribution of knowledge test items (recognizing organisms as vectors, recognizing diseases as VBDs, 
#' matching diseases with their corresponding vector) or associated vectors (Wasp/bed bugs distractors, Ticks, Mosquitoes).
#' The ability to identify vector-borne diseases and knowledge about mosquitoes and mosquito-borne diseases appear 
#' as important drivers of the overall knowledge score.
#' 
hist_results=create_hist(brs_3)

hist_results[[2]]
hist_results[[3]]

#' # Individual exposure : open-ended answers
#' 
#' Most respondents reported vaccination against tick-borne encephalitis (FSME/TBE) as 
#' their main preventive measure, often mentioning it explicitly for themselves or their children. 
#' A smaller number referred to protecting pets (e.g., with tick collars or treatments) as an 
#' indirect preventive strategy. Few participants mentioned other measures such as avoiding tall 
#' grass or forests, wearing protective clothing or hats, using repellents (sprays, oils, patches), 
#' or employing unconventional approaches such as vinegar, vitamin B, or ceramic bracelets. 
#' Overall, vaccination was by far the most commonly cited individual preventive action, 
#' with other measures appearing rarely and in isolated cases.
#' 
#' Respondents described a wide range of individual strategies to prevent mosquito bites. 
#' Common measures included the use of repellents such as insect sprays, mosquito-repellent candles, 
#' incense sticks, essential oils (e.g., lavender, clove, oregano), and specialized diffusers or bracelets. 
#' Several participants reported installing or using physical barriers such as mosquito nets, insect screens, 
#' or curtains, and others emphasized behavioral measures, including keeping lights off in the evening, 
#' ventilating rooms during the day, and avoiding open windows at night. Some mentioned directly eliminating 
#' mosquitoes indoors through catching, swatting, or using electric devices. Additional approaches included 
#' reducing breeding sites by covering or regularly changing standing water, maintaining bat-friendly gardens, 
#' or applying larvicides. A few respondents also referred to vaccination or the use of vitamin supplements as 
#' protective measures.
#' 
#' 
#' 
#' 
