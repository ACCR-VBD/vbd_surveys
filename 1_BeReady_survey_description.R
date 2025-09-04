#' ---
#' title:  Knowledge, Attitudes and Practices on Vector-Borne Diseases in Switzerland – 2025
#' author: Lilian Goepp, Nina Huber, Arlette Szelecsenyi, Julien Riou
#' date: "`r Sys.Date()`"
#' bibliography: data/Bibliography/Surveys_biblio.bib
#' link-citations: true
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
analysis_date = "2025-09-04"
load(paste0("savepoints/savepoint_",analysis_date,"/brs_3.Rdata"))
source("R/setup.R")
knitr::opts_chunk$set(echo = FALSE)
set_gtsummary_theme(list(
  "tbl_summary-fn:percent_fun" = function(x) style_percent(x, digits = 1)
))

#' # Introduction
#' 
#' This report presents findings from a survey conducted within the national research program NCCS-Impacts 
#' (“Decision Support for Dealing with Climate Change in Switzerland: a cross-sectoral approach”), 
#' which develops climate-related services for the environment, the economy, and society. Within this 
#' framework, the health project jointly led by the Federal Office of Public Health (FOPH) and the 
#' Federal Food Safety and Veterinary Office (FSVO) addresses heat, mycotoxins, and vector-borne diseases in humans and animals. 
#' (VBDs).
#' 
#' A comprehensive risk assessment of current and future VBD threats to humans and animals in Switzerland is 
#' being carried out as part of this effort. It includes documenting both the implementation of measures by 
#' cantonal authorities and the knowledge, attitude and practice (KAP) concerning vector-borne diseases among the Bernese population. 
#' The general population survey was embedded in the BEready cohort study at the University of Bern. 
#' BEready is a long-term study started in 2021 involving households across the canton of Bern, designed to generate data on 
#' infectious diseases and improve preparedness for future health threats. In this context, the survey 
#' investigates how Bernese residents perceive VBD risks and protective measures, while complementing 
#' the national survey of cantonal health, veterinary, and environmental departments on institutional 
#' engagement and prevention strategies.
#'
#' VBDs are a growing public-health concern in Switzerland. Tick-borne infections, most 
#' notably Lyme borreliosis and tick-borne encephalitis (TBE), remain the dominant domestic threat, 
#' while mosquito-borne diseases such as dengue, West Nile fever, and chikungunya are emerging in 
#' neighboring countries. Warmer conditions linked to climate change may lengthen vector activity seasons and expand suitable habitats, 
#' increasing opportunities for human–vector contacts and thus epidemic risk.
#'
#' Individual preventive measures are essential to limit the transmission of these diseases. 
#' Public awareness and knowledge are therefore important factors in planning and implementing 
#' public health actions. By collecting data on how Bernese residents perceive these risks and protect 
#' themselves, this survey assesses the population’s level of preparedness in the face of this emerging 
#' threat. Understanding current behaviors and perceptions will support the development of effective communication 
#' and prevention strategies, both to address today’s challenges and to respond to future health impacts.
#'
#' 
#' # Methods
#'
#' We conducted a cross-sectional, web-based survey of adults (≥18 years) residing in Switzerland in 2025. 
#' The survey was implemented within the BEready platform (University of Bern/MCID), which 
#' is registered on ClinicalTrials.gov (ID: NCT06739499) and operates as a general population population prospective 
#' cohort in the Canton of Bern under a One-Health framework. BEready plans to enroll approximately 1,500 households, 
#' including adults, children and pets and follow them for several decades. 
#' The study population comes a random selection of households in Bern using the cantonal residents' register and
#' from volunteer households. The study started with a pilot phase of 100 households in May 2023. Recruitment for the main 
#' phase started in April 2024. An additional module on VBD was added on our request to the 1-year questionnaire of participants
#' to the pilot phase, and to the baseline questionnaire of participants to the main phase. We obtained data on all participants
#' until May 2025.
#' 
#' Eligible participants were adults (≥18 years) who provided informed consent and who resided, 
#' on average, more than three days per week in a private household in the canton of Bern. Access 
#' to the internet (Wi-Fi, fixed, or mobile) and a personal email address was required. Sufficient language 
#' proficiency to understand, speak, read, and write in German, French, and/or English was required. 
#' Individuals were excluded if they had a planned move outside the canton of Bern during the study period. 
#' As the VBD module of the questionnaire was only provided to adults, 
#' children were not included in this report. 
#' 
#' At inclusion, participants have a brief physical checkup and give a blood sample. 
#' Immediately after the initial visit and annually thereafter, participants complete an online questionnaire 
#' about their current state of health and factors that may make them more susceptible to infections 
#' or protect against them, including their knowledge, attitude and practice concerning vector-borne diseases. 
#' Participation in BEready is always voluntary. Participants may leave the study at any time, and BEready 
#' complies with Swiss legislation and applicable international guidelines. The study was reviewed and approved 
#' by the responsible Ethics Committee. All collected data are processed under strict data-protection standards.
#' 
#' The questionnaire design followed a structured, multi-stage approach to assess public knowledge of 
#' vector-borne diseases and their transmission, incorporating both awareness and comprehension measures. 
#' In the first stage, respondents were presented with three items to determine whether they had 
#' previously encountered the relevant concepts or understood the underlying concepts as follows.
#' 1) An introductory text provided respondents with a definition of vector-borne diseases and the fact 
#' that tick-borne encephalitis is a disease transmitted by ticks. Respondents were subsequently asked to 
#' classify tick-borne encephalitis as vector-borne or not, serving as an initial measure of their 
#' conceptual understanding. 
#' 2) A disease awareness component, capturing whether respondents reported prior exposure to the names 
#' of particular diseases.
#' 3) A vector awareness component, capturing whether they reported prior exposure to the names of specified organisms. 
#' In the second stage, participants completed classification exercises in which they were required to identify, 
#' from structured lists, which diseases they believed were vector-borne and which organisms 
#' functioned as vectors. 
#'
#' Establishing the first stage baseline was essential to distinguish between lack of knowledge 
#' and misunderstanding in subsequent tasks. To evaluate the robustness of conceptual understanding and to detect 
#' systematic misconceptions, distractor items were intentionally included: measles 
#' and influenza in the disease set, and wasps and bed bugs in the organism set. These items, 
#' while familiar to most respondents, do not fall within the epidemiological definition of 
#' vector-borne diseases or recognized disease vectors, and thus provided a validity check. 
#' Results from the first stage as well as logical contradiction in the second stage were used to filter out
#' unreliable answers.
#' 
#' The online survey was implemented using REDCap and description of the data was performed in R (version `r getRversion()`).
#' 
#' # Results
#'
#' ## Sociodemographic variables
#' 
#' The survey sample included 991 participants, with a sex distribution biased toward females (56% female and 44% male, p-val <0.001). 
#' Gender identification largely mirrored sex assigned at birth, with only 0.4% reporting another gender. 
#' Educational attainment was diverse, though skewed toward higher education: 23% held a master’s degree, 
#' 15% a bachelor’s degree, and 9% a doctorate. Apprenticeship or vocational training was also common (19%). 
#' Most respondents (75%) had lived in Switzerland since birth, while 19% were born abroad. 
#' The majority were married and living with a spouse (56%), but single, divorced, or widowed individuals 
#' collectively made up nearly 43%. Occupations were varied, with the largest group working in intermediate 
#' occupations (39%) or administrative support (32%), while only 0.5% reported being in senior executive roles. 
#' The complete table as well as further details on exposure-related items from the enrollment questionnaire and the 
#' geographic distribution of respondents are provided in the Supplementary Material.  
#' 
#' ## General knowledge
#'
#' When asked to classify TBE, which is transmitted through tick bites, 
#' 78% of respondents correctly recognized it as a vector-borne disease, 11% incorrectly applied the classification, and 11% stated they did not know. This indicates a generally strong but not 
#' universal grasp of the basic definition of vector-borne diseases among the surveyed population. 
#' Awareness of diseases varied considerably. Measles (95%), influenza (94%), and dengue (88%) 
#' were the most widely recognized. Zika (62%) and Lyme disease (76%) were moderately well-known while West 
#' Nile fever (29%) and chikungunya (20%) were far less familiar. Awareness of potential vectors was 
#' consistently high, with ticks (96%), mosquitoes (96%), wasps (93%), and bed bugs (92%) all classified correctly 
#' by most respondents. The corresponding table is provided in the Supplementary Materials.
#'

filtering_var = brs_3 %>% mutate(filtering_var=bl_vbd_tick_yn=="Yes"&contrad_any=="Consistent") %>% select(filtering_var)
filtering_var[is.na(filtering_var)]=F

#'
#'
#' To further explore disease classification, analyses were restricted to participants who demonstrated both a basic understanding of VBD concepts and provided logically consistent responses (n=709, Supplementary Material 2.1.1). Within this group, correct classification was highest for dengue (86%), Lyme disease (83%), measles (83%, correctly identified as non–vector-borne), and influenza (80%, non–vector-borne). Recognition was lower for Zika (69%) and especially for West Nile fever (41%) and chikungunya (32%).
#'
#' These participants also showed strong accuracy in identifying disease-transmitting species. Ticks were universally identified as vectors, and nearly all recognized mosquitoes (99%). The most common misconception involved bed bugs, with 23% mistakenly believing they transmit diseases. Wasps were misclassified less frequently, with 7% incorrectly identified as vectors. Overall, knowledge about major vectors was strong, though confusion persisted about insects with nuisance potential but no role in pathogen transmission.
#' 
#' Accuracy in matching diseases with their correct vectors was more variable. 
#' TBE was almost universally matched to ticks (99.6%), and dengue (89%), Zika (72%), 
#' and Lyme disease (88%) were also correctly identified by most respondents. However, 
#' only 57% correctly matched West Nile fever to mosquitoes, and fewer than half 
#' recognized chikungunya as mosquito-borne (42%). Influenza and measles were mostly correctly 
#' classified as having no vector (93% and 92%, respectively). These findings suggest that while some pairings are familiar, 
#' knowledge of mosquito-borne diseases remains incomplete, particularly for those less prominent in Swiss 
#' public health discourse. The corresponding tables are reported in Supplementary Material.
#' 
#' The knowledge score, defined as the proportion of correct answers across three test items
#' assessing recognition of vector-borne diseases, identification of vectors, and disease–vector matching,
#' showed clear variation within the sample. Among the 556 respondents with complete answers to all items, 39 individuals (7%) scored below 50%,
#' indicating low overall knowledge. At the other end of the spectrum, 80 respondents (14%) achieved a
#' perfect score of 100%, demonstrating complete accuracy across all items. The majority of participants fell
#' between these two extremes: 210 (38%) had scores between 50 and 75%, and 227 (41%) between 75 and 100%,
#' reflecting moderate to high knowledge with occasional gaps. 
#' The distribution of the score suggests that most respondents possess a solid baseline of knowledge concerning ticks, while
#' the biggest knowledge gap concerns mosquito-related items (Supplementary material).

hist_results=create_hist(brs_3)

hist_results[[1]]

#' ## Individual risk exposure 
#' 
#' Tick exposure was less frequent than mosquito exposure. Among 991 participants, 61% reported never receiving tick bites in a typical year, 
#' 33% experienced 1–3 bites, and fewer than 5% reported ≥4 bites (4–6: 3%; 7–9: 1%; ≥10: 2%). 
#' Mosquito bites during summer were a widespread and routine occurrence in the population, with 9% bitten daily, 43% at least weekly, and 30% 
#' at least monthly. Only 1% claimed never to be bitten, while 6% were uncertain. 

brs_3 %>%
  select(bl_vbd_tickbite_nr,
         bl_vbd_mosqbite_nr) %>%
  tbl_summary(missing = "always",
              missing_text = "(Missing)") %>%
  modify_bold(rows=(variable=="bl_vbd_tickbite_nr"&row_type=="label"),columns=label) %>% 
  modify_bold(rows=(variable=="bl_vbd_mosqbite_nr"&row_type=="label"),columns=label) %>% 
  modify_header(label ~ "**Question**") %>% 
  br_100_add_caption(
    "Self-reported individual exposure", 9) %>% 
  tab_style(
    style = "padding-left:20px;",
    locations = cells_body(
      columns = "label",
      rows    = row_type == "label" & grepl("___",variable)
    )
  )

#' ## Individual prevention 
#' 
#' Most respondents reported adopting at least one protective measure against tick bites, particularly checking for and 
#' removing ticks after outdoor activities (74%) and wearing protective clothing (67%). The use of insect repellent was also commonly reported (48%).
#' Against mosquito bites, the most common strategies 
#' were using repellents (65%), wearing protective clothing (44%), and installing window screens (42%). 
#' Only 28% of respondents reported removing containers of standing water as a personal preventive measure.
#' Around 11% reported taking no measures against mosquitoes, compared to 8% for ticks.
#' 

brs_3 %>%
  select(starts_with("bl_vbd_meas_tick___"),
         starts_with("bl_vbd_meas_mosq___"),
  ) %>%
  tbl_summary(missing = "always",
              missing_text = "(Missing)") %>%
  br_100_add_question_header(
    brs_3,
    "bl_vbd_meas_tick___1") %>%
  br_100_add_question_header(
    brs_3,
    "bl_vbd_meas_mosq___1") %>%
  br_100_add_missing(c("bl_vbd_meas_tick___","bl_vbd_meas_mosq___")) %>% 
  modify_table_body(
    ~ dplyr::filter(.x, !(grepl("___",variable) & row_type == "missing"))) %>%
  modify_header(label ~ "**Question**") %>% 
  br_100_add_caption(
    "Self-reported preventive practices", 10)%>% 
  tab_style(
    style = "padding-left:20px;",
    locations = cells_body(
      columns = "label",
      rows    = row_type == "label" & grepl("___",variable)
    )
  )

#' ## Mitigation strategies and vector control 
#'
#' When asked about effective public-level interventions, respondents placed greater confidence 
#' in personal protective measures than in environmental or ecological strategies. For ticks, 
#' 79% endorsed promoting personal protection, while biological control (38%) was the next most supported. 
#' Measures targeting wildlife or habitat management received little support (<6%). For mosquitoes, 
#' personal protection (76%) and elimination of standing water (59%) were most often cited, followed 
#' by biological control (41%) and traps (36%). Genetically modified mosquitoes gained support from 18%. 
#' Overall, confidence leaned strongly toward individual-level protection and simple 
#' environmental measures over systemic ecological or technological interventions.

#+ attitudes-table
brs_3 %>%
  select(starts_with("bl_vbd_meas_tick_eff___"),
         starts_with("bl_vbd_meas_mosq_eff___")) %>%
  tbl_summary(missing = "always",
              missing_text = "(Missing)") %>%
  br_100_add_question_header(
    brs_3,
    "bl_vbd_meas_tick_eff___1") %>%
  br_100_add_question_header(
    brs_3,
    "bl_vbd_meas_mosq_eff___1") %>%
  br_100_add_missing(c("bl_vbd_meas_tick_eff___","bl_vbd_meas_mosq_eff___")) %>% 
  modify_table_body(
    ~ dplyr::filter(.x, !(grepl("___",variable) & row_type == "missing"))) %>%
  modify_header(label ~ "**Question**") %>% 
  br_100_add_caption(
    "Beliefs regarding effectiveness of public control measures", 11)%>% 
  tab_style(
    style = "padding-left:20px;",
    locations = cells_body(
      columns = "label",
      rows    = row_type == "label" & grepl("___",variable)
    )
  )

#'
#' ## Perception of risk
#' 
#' Most respondents considered tick-borne encephalitis (72%) and Lyme disease (61%) 
#' to be current health problems in Switzerland, whereas few perceived West Nile fever (2%), 
#' chikungunya (2%), Zika (6%), or dengue (10%) as current problems. Regarding the need 
#' for vector control measures in Switzerland, 48% answered “yes” 20% “no” and 31% “don’t know” 
#' indicating substantial perceived relevance but also considerable uncertainty.

brs_3 %>% select(starts_with("bl_vbd_dis_problem___"),bl_vbd_meth_yn) %>%
  tbl_summary(missing = "always",
              missing_text = "(Missing)") %>%
  br_100_add_question_header(
    brs_3,
    "bl_vbd_dis_problem___1") %>%
  modify_bold(rows=(variable=="bl_vbd_meth_yn"&row_type=="label"),columns=label) %>% 
  br_100_add_missing(c("bl_vbd_dis_problem___")) %>% 
  modify_table_body(
    ~ dplyr::filter(.x, !(grepl("___",variable) & row_type == "missing"))) %>%
  modify_header(label ~ "**Question**") %>% 
  br_100_add_caption(
    "Perception on vector-borne disease risk and control measures", 12)%>% 
  tab_style(
    style = "padding-left:20px;",
    locations = cells_body(
      columns = "label",
      rows    = row_type == "label" & grepl("___",variable)
    )
  )

#' ## Preparedness
#' 
#' From a preparedness perspective, respondents identified combating misinformation and maintaining public 
#' awareness as the top challenges for the next pandemic, followed by sustaining political commitment. 
#' 

#' # Discussion
#'
#' This population-based survey of adults in the canton of Bern provides a snapshot of knowledge, attitudes, 
#' and practices related to VBDs in 2025. Overall, the public demonstrated 
#' good understanding of vector concepts and strong recognition of ticks and mosquitoes as 
#' disease transmitters. However, knowledge was uneven across diseases: items involving 
#' TBE and Lyme disease were answered correctly by a large majority, while several mosquito-borne diseases, 
#' notably West Nile fever and chikungunya, were frequently misclassified or mismatched to their vectors. 
#' These gaps persisted even among respondents who otherwise scored highly on vector identification and 
#' on classifying non-vector diseases (influenza and measles) correctly, suggesting disease-specific blind 
#' spots rather than general confusion about the vector concept.
#' 
#' Perceived risk aligned with this pattern. Most respondents viewed TBE and Lyme disease as current 
#' health problems in Switzerland, while only small minorities considered West Nile fever, chikungunya, 
#' Zika, or dengue to be present concerns. Self-reported exposure also reflected local exposure: 
#' tick bites were uncommon for most, whereas mosquito bites in summer were routine, with a majority 
#' reporting at least weekly bites. This combination of frequent nuisance exposure to mosquitoes but limited 
#' awareness of potential mosquito-borne disease risks could help explain the selective knowledge gaps observed 
#' in classification and matching tasks. 
#' 
#' Individual preventive behaviors were widely reported and, for ticks, appeared well-aligned with best practices: 
#' post-activity tick checks and protective clothing were common, and nearly half used repellents. 
#' For mosquitoes, repellents, protective clothing, and window screens were the dominant strategies. 
#' Elimination of standing water was less prevalent despite being a core community-level measure. 
#' Respondents clearly prioritized public control measures focused on individual protection and simple environmental actions, 
#' such as eliminating standing water, over more complex interventions. 
#' Measures involving wildlife management for ticks or chemical approaches, such as pesticides, were rarely 
#' perceived as effective, indicating that the public largely considers these strategies inefficient or inappropriate. 
#' Similarly, advanced technological interventions for mosquito control, such as genetically modified mosquito release, 
#' received limited support. These perceptions suggest that future public health messaging and policy initiatives 
#' will need to carefully address the low public acceptance of wildlife-based and chemical control measures, 
#' while leveraging the strong support for personal protection and basic environmental management.
#' 
#' Strengths of this study include its integration within the BEready cohort, the use of structured 
#' multi-stage knowledge assessments with validity checks (distractors, contradiction screens), 
#' and the ability to compare tick- and mosquito-related domains within the same respondents. Several 
#' limitations warrant caution. Although the survey was designed to be representative of the Swiss adult population, 
#' the educational profile observed among respondents deviated from national benchmarks. Specifically, 
#' the sample includes a higher proportion of individuals with tertiary education (notably Master's and Doctorate degrees) 
#' and a lower proportion of respondents with apprenticeship or upper-secondary education, 
#' compared to the nationwide distribution from the Swiss Federal Statistical Office [@HighestCompletedEducation2025]. 
#' This form of participation bias where individuals with higher education are more likely to engage in voluntary-based studies
#' is a well-documented phenomenon in population-based surveys. Higher female participation has also been reported in such study designs which might explained the observed sex bias.
#' [@cheungImpactNonresponseBias2017, @reinikainenParticipationRatesEducational2018]. The web-based design and the observed sociodemographic 
#' profile may limit generalizability to the general population. Some items had missing 
#' data, and a subset of analyses relied on reduced denominators (e.g., N=709 for test items, N=556 for 
#' the composite score), which could introduce selection effects if completion correlated with knowledge. 
#' Self-reported exposures and behaviors are also subject to recall biases. Finally, the restriction to the Bernese 
#' cantonal population prevents from exploring regional variations in the Swiss population with regard to their
#' knowledge, attitude and practice concerning vector-borne disease.
#' 
#' This study is globally consistent with previous results. A study conducted in 2015 in Neuchâtel [@aenishaenslinFactorsAssociatedPreventive2015a] 
#' investigating the adoption of preventive behavior regarding ticks reported high uptake of personal protection: 57% of the 
#' total population reported performing tick checks after outdoor activities, 53% reported wearing protective clothing, 
#' and 29% reported using tick repellents. These levels are lower than those observed in our survey of the Bernese 
#' population, where 74% reported tick checks, 67% protective clothing, and 48% repellent use. 
#' The discrepancy might reflect regional discrepancy or an evolution in the interest in tick preventive 
#' measures over the last ten years. Interestingly, when authors restricted their analysis to individuals aware of Lyme disease,
#' reported adoption of tick checks and protective clothing became higher than in the present survey. 
#' For mosquitoes, a French survey [@ocampo-alvaradoFactorsAssociatedMosquitoborne2024] reported frequent use of repellents (36%), 
#' mosquito candles/coils (35%), and window nets (25%) as well as community-level practices such as removing standing 
#' water (37%). In comparison, our results show higher uptake of individual preventive behaviors, 
#' with 65% of Bernese respondents using repellents and 42% installing window screens, but lower uptake of 
#' community-level measures with 28% eliminating standing water. This suggests that in Switzerland, 
#' individual-level prevention is more widely adopted, while actions requiring sustained community engagement 
#' remain comparatively limited.
#' 
#' From a preparedness perspective, respondents identified combating misinformation and maintaining public 
#' awareness as the top challenges for the next pandemic, followed by sustaining political commitment. 
#' These views, combined with the VBD findings, point to several actionable implications. First, targeted 
#' education should prioritize mosquito-borne disease literacy using 
#' concrete, locally-relevant narratives and emphasizing practical household actions. 
#' Second, communications should continue reinforcing effective tick prevention behaviors. Third, 
#' public-level strategies could be framed to match existing preferences: scale up promotion of personal protection 
#' and low-complexity environmental measures, while involving communities early in dialogues 
#' about more complex or novel interventions (e.g., larvicide programs, genetic methods) to foster shared understanding
#' and sustained community engagement. 
#' 
#' # Conclusion
#' 
#' In summary, the Bernese public shows strong baseline knowledge on ticks and wide adoption of effective tick-related 
#' practices but exhibits disease-specific knowledge gaps for mosquito-borne infections despite frequent 
#' mosquito exposure. Public health actions should leverage existing support for personal protection and 
#' simple environmental measures, close specific literacy gaps on mosquito-borne diseases, and engage communities 
#' around the rationale, benefits, and trade-offs of more complex control strategies. These steps can 
#' strengthen readiness for both current tick-borne risks and evolving mosquito-borne threats in a warming climate.
#' 
#' 
#' # References
#' 



