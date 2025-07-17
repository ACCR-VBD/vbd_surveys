#' ---
#' title: Cantonal Engagement in the Prevention and Control of Vector-Borne Diseases in Switzerland and Liechtenstein
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
analysis_date = "2025-06-26"
load(paste0("savepoints/savepoint_",analysis_date,"/shs_3.Rdata"))
source("R/setup.R")
knitr::opts_chunk$set(echo = FALSE)

#' # Introduction
#' 
#' This survey is part of the health project within the research program 
#' "Decision Support for Dealing with Climate Change in Switzerland: a 
#' cross-sectoral approach" (NCCS-Impacts). The goal of NCCS-Impacts 
#' is to develop practical climate-related services for the environment, 
#' the economy, and society. The associated health project, jointly led 
#' by the Federal Office of Public Health (FOPH) and the Federal Food 
#' Safety and Veterinary Office (FSVO), focuses on the themes of heat, 
#' mycotoxins, and vector-borne diseases.
#' 
#' Regarding vector-borne diseases, a comprehensive risk assessment of 
#' current and future threats to humans and animals in Switzerland is 
#' being carried out. As part of this risk assessment, the project documents 
#' (1) the state of knowledge about vector-borne diseases among the Bernese 
#' population and (2) the implementation of measures by cantonal authorities. 
#' The national survey of cantonal departments of health, veterinary services, 
#' and the environment aims to gather information on cantonal engagement, 
#' specific measures, and collaboration with other stakeholders.
#' 
#' # Methods
#' 
#' To assess the engagement of cantonal authorities with regard to vector-borne 
#' diseases, a cross-sectional survey was conducted among selected stakeholders 
#' in cantonal departments responsible for health, veterinary services, and 
#' environmental protection. 
#' 
#' The survey was implemented using the REDCap electronic data capture platform 
#' (Research Electronic Data Capture, hosted by Unisanté) and administered via 
#' an online questionnaire. The questionnaire was distributed by email to targeted 
#' respondents within relevant cantonal services, based on their role and 
#' responsibilities in managing public health or environmental risks. Participation was voluntary.
#' Participants were able to save their answers and come back to it later.
#' 
#' The online survey was open from April 25 to June 6, 2025. 
#' A reminder was sent on May 16. The questionnaire included structured 
#' multiple-choice and checkbox items, as well as a small number of open-ended questions, covering 
#' topics such as awareness of vector-borne diseases, existing preventive measures, interdepartmental 
#' collaboration, and perceived needs or challenges.
#' 
#' The full questionnaire is available online: https://redcap.unisante.ch/surveys/?s=YKERXDNR4KAMWYP9.
#' Data were anonymised, exported securely from REDCap and described using R (version 4.4.2).
#' Open-text answers in multiple languages were summarized using GPT-4-turbo (June 2025 version). 
#' More details are available in the supplementary file. 
#' 
#' # Results
#' 
#' ## Participants

n_responses = nrow(shs_3)
n_canton = length(unique(shs_3$canton))
n_per_canton = shs_3 %>% 
  group_by(canton) %>% 
  count() %>% 
  arrange(-n)
n_dep = shs_3 %>% 
  group_by(type) %>% 
  count() %>% 
  arrange(-n)

#' Requests for participation were sent to X persons. A total of `r n_responses` complete responses were collected during the study period, covering all `r n_canton-1` Swiss cantons and Liechtenstein. The number of answers per canton varied between `r min(n_per_canton$n)` and `r max(n_per_canton$n)`. Participants primarily belonged to human health cantonal departments (n= `r n_dep$n[1]`), followed by the animal health  departments (n= `r n_dep$n[2]`) and the environment departments (n= `r n_dep$n[3]`). 
#' 
#' ## Involvement and resources
#' 
#' Most participants (77%) reported that their authority is currently involved in the implementation of measures or activities related to diseases transmitted by ticks and mosquitoes, such as public relations, monitoring, or control (Table 1). Among the 12 respondents who indicated no current involvement, the most frequently cited reasons included a lack of financial or human resources (50%) and a perception that such diseases are outside the authority's responsibilities (42%). Regarding the existence of cantonal strategies or action plans, 49% of respondents were aware of a plan for mosquito-borne diseases and 11% for tick-borne diseases, while 33% reported that no strategy exists in their canton. Half of the respondents (49%) indicated that their authority lacks human resources to develop or implement relevant activities, and 55% stated they have no dedicated budget. 
#' 

shs_3 %>% select(starts_with("b_1"), -b_1_2,starts_with("b_2___"),
                 starts_with("b_3___"),
                 starts_with("b_4___")) %>% 
  
  tbl_summary(missing = "always", 
              missing_text = "(Missing)") %>% 
  
  modify_bold(rows=(variable=="b_1"&row_type=="label"),columns=label) %>% 
  
  sh_100_add_question_header("If you answered no, what are the reasons?","b_1_1___1") %>% 
  sh_100_add_question_header("Do you know of any strategy or action plan in your canton regarding
                      diseases transmitted by ticks and mosquitoes?","b_2___1") %>% 
  sh_100_add_question_header("Does your authority have human resources to develop and implement 
                    activities related to diseases transmitted by ticks and mosquitoes?","b_3___1") %>% 
  sh_100_add_question_header("Does your authority have a budget for the development and implementation 
                    of activities in the field of diseases transmitted by ticks and mosquitoes?","b_4___1") %>% 
  sh_100_add_missing(c("b_2___","b_3___","b_4___")) %>% 
  
  
  modify_table_body(
    ~ dplyr::filter(.x, !(grepl("___",variable) & row_type == "missing"))) %>%
    modify_header(label ~ "**Question**") %>% 
  
  sh_100_add_caption("Involvement and resources in the field of mosquito- and tick-borne diseases",1) %>% 
  tab_style(
    style = "padding-left:20px;",
    locations = cells_body(
      columns = "label",
      rows    = row_type == "label" & grepl("___",variable)
    )
  )

# knitr::include_graphics(paste0("savepoints/savepoint_",analysis_date,"/Pie_plot.png"),dpi=10)

#' 
#' ## Concrete measures and activities
#' 
#' Reported concrete measures and activities in the field of vector-borne diseases varied markedly between tick- and mosquito-borne diseases (Table 2). The most common actions cited for ticks included public information campaigns (22%) and targeted awareness of professionals (13%), with rare reports of monitoring (4% passive; 0% active) and vaccination advice (6%). In contrast, involvement in mosquito-related activities was broader and more diverse: 40% of respondents reported engaging in public relations and professional awareness efforts, while active and passive mosquito monitoring were cited by 36% and 33% of respondents, respectively. Nearly one-third (29%) mentioned verifying sightings at new locations, and around 15–18% reported involvement in mosquito control on public or private land. Information and training activities were mentioned by 20% of respondents. Topics covered in awareness materials included vaccination (65%), protection against bites (53%), and elimination of breeding sites (47%). Open-text responses further highlighted involvement in national vaccination campaigns (e.g., against tick-borne encephalitis or bluetongue disease), interdepartmental coordination, media engagement, and surveillance of animal diseases transmitted by ticks and mosquitoes as defined in the Animal Disease Ordinance. Participants reported using a wide range of channels to disseminate information, most commonly official websites, emails, flyers, and direct communication with veterinarians or animal owners. Additional methods included newsletters, social media, articles in agricultural media, information sessions, and training courses, often in collaboration with municipalities or other local actors.
#' 

shs_3 %>% select(starts_with("c_1___"),starts_with("c_2___"),starts_with("c_3___")) %>% 
  
  tbl_summary(missing = "always", 
              missing_text = "(Missing)") %>% 
  
  sh_100_add_question_header("What activities is your authority currently 
                    (2024/2025) implementing in the field of tick-borne diseases?","c_1___1") %>% 
  sh_100_add_question_header("What activities is your authority currently 
                    (2024/2025) implementing in the field of mosquito-borne diseases?","c_2___1") %>% 
  sh_100_add_question_header("If your authority develops information, what subjects are covered?","c_3___1") %>%
  
  sh_100_add_missing(c("c_1___","c_2___","c_3___")) %>% 
  
  modify_table_body(
    ~ dplyr::filter(.x, !(grepl("___",variable) & row_type == "missing"))) %>%
  modify_header(label ~ "**Question**") %>% 
  
  sh_100_add_caption("Concrete measures and activities in the field of mosquito- and tick-borne diseases (part 1)",2) %>% 
  tab_style(
    style = "padding-left:20px;",
    locations = cells_body(
      columns = "label",
      rows    = row_type == "label" & grepl("___",variable)
    )
  )

if(FALSE){
  shs_3 %>% filter(!is.na(c_1_1)) %>% pull(c_1_1)
  shs_3 %>% filter(!is.na(c_2_1)) %>% pull(c_2_1)
  shs_3 %>% filter(!is.na(c_3_1)) %>% pull(c_3_1)
  shs_3 %>% filter(!is.na(c_4)) %>% pull(c_4)
}

if(FALSE) {
  shs_3 %>% filter(!is.na(c_5)) %>% pull(c_5)
}

#' Respondents reported using a wide variety of guidelines, support materials, and reference documents to inform the development and implementation of measures against vector-borne diseases. The most frequently cited sources included technical instructions and documents from the Federal Food Safety and Veterinary Office (OSAV/BLV), the Federal Office for the Environment (OFE/BAFU), and the Federal Office of Public Health (OFSP/BAG). Several participants also referred to cantonal materials (particularly from Ticino) as well as international resources such as those from the WHO, WOAH, or cross-border exchanges with France and Germany. Additionally, documents from academic or technical partners like SUPSI and Swiss TPH were mentioned. A few respondents reported using self-developed materials or relying on exchanges of expert knowledge, while others indicated they did not use any specific documents.
#' 
#' Respondents identified a range of knowledge gaps within their authorities concerning both tick- and mosquito-borne diseases (Table 3). For tick-borne diseases, the most commonly cited areas of limited knowledge included disease management (20%), followed by detection and prevention (both 18%). Open-text responses often emphasized limited relevance or jurisdictional responsibility,  while some highlighted a lack of resources or the need for clearer coordination and federally supported monitoring. For mosquito-borne diseases, the main knowledge gaps were in disease and vector management and control (36%), as well as prevention (22%) and detection (20%). Several respondents pointed to uncertainties around the appropriate use of adulticides, sharing of responsibilities among stakeholders, and long-term control strategies. Regarding actual control measures against mosquitoes, over half of participants (53%) reported that no active mosquito control measures are in place. Among those that did report activities, biological larvicides (25%) and elimination of breeding sites (20%) were the most common, while use of adulticides (11%) and non-chemical larvicides (7%) was less frequent. Open-text answers revealed a mix of pilot studies, public awareness campaigns, and experimental approaches like sterile insect techniques (SIT), often in collaboration with academic or federal partners. Some respondents indicated that control responsibilities lie with other agencies, such as municipalities or environmental offices. 
#' 

shs_3 %>% select(starts_with("c_6___"),starts_with("c_7___"),starts_with("c_8___")) %>% 
  
  tbl_summary(missing = "always", 
              missing_text = "(Missing)") %>% 
  
  sh_100_add_question_header("Among the following fields of action, where is the greatest need for information 
                    or the greatest lack of knowledge about tick-borne diseases in the authority you work for?","c_6___1") %>% 
  sh_100_add_question_header("Among the following fields of action, where is the greatest need for information 
                    or the greatest lack of knowledge about mosquito-borne diseases in the authority you work for?","c_7___1") %>% 
  sh_100_add_question_header("What measures is your authority implementing to control or combat invasive mosquitoes?","c_8___1") %>%
  
  sh_100_add_missing(c("c_6___","c_7___","c_8___")) %>% 
  
  modify_table_body(
    ~ dplyr::filter(.x, !(grepl("___",variable) & row_type == "missing"))) %>%
  modify_header(label ~ "**Question**") %>% 
  
  sh_100_add_caption("Concrete measures and activities in the field of mosquito- and tick-borne diseases (part 2)",3) %>% 
  tab_style(
    style = "padding-left:20px;",
    locations = cells_body(
      columns = "label",
      rows    = row_type == "label" & grepl("___",variable)
    )
  )

if(FALSE){
  shs_3 %>% filter(!is.na(c_6_1)) %>% pull(c_6_1)
  shs_3 %>% filter(!is.na(c_7_1)) %>% pull(c_7_1)
  shs_3 %>% filter(!is.na(c_8_1)) %>% pull(c_8_1)
}

#' 
#' ## Collaboration and coordination
#' 
#' Collaboration and coordination emerged as important themes in participants’ responses regarding vector-borne disease control (Table 4). While 60% of respondents reported active networks or exchanges with researchers and specialists for mosquito-borne diseases, only 20% indicated such collaborations for tick-borne diseases, and over a quarter (27%) reported no active exchange at all. Open-text responses highlighted needs for stronger inter-cantonal coordination, improved knowledge transfer, and harmonized federal guidelines, particularly for outbreak preparedness, surveillance, and the responsible use of control tools like biocides or adulticides. Several participants emphasized the importance of financial and human resource support, centralized information platforms, and a coordinated national monitoring strategy. Others cited challenges related to fragmented responsibilities across levels of government or lack of expertise and capacity. When asked about the integration of tick- and mosquito-borne disease issues into national and cantonal climate adaptation strategies, most respondents indicated limited awareness or engagement: 65% were unsure about national-level integration, and 53% did not know whether these issues were addressed in their own canton’s strategy. 
#' 

shs_3 %>% select(starts_with("d_1___"),starts_with("d_3___"),starts_with("d_4___")) %>% 
  
  tbl_summary(missing = "always", 
              missing_text = "(Missing)") %>% 
  
  sh_100_add_question_header("Does your authority maintain an active network or exchanges with researchers and specialists in the field?","d_1___1") %>% 
  sh_100_add_question_header("Is the issue of tick- and mosquito-borne diseases sufficiently or inadequately addressed in the implementation of the national climate change adaptation strategy?","d_3___1") %>% 
  sh_100_add_question_header("Is the issue of diseases transmitted by ticks and mosquitoes sufficiently or insufficiently taken into account in the implementation of your canton's climate change adaptation strategy (if any)?","d_4___1") %>%
  
  sh_100_add_missing(c("d_1___","d_3___","d_4___")) %>% 
  
  modify_table_body(
    ~ dplyr::filter(.x, !(grepl("___",variable) & row_type == "missing"))) %>%
  modify_header(label ~ "**Question**") %>% 
  
  sh_100_add_caption("Collaboration and coordination in the field of mosquito- and tick-borne diseases",4) %>% 
  tab_style(
    style = "padding-left:20px;",
    locations = cells_body(
      columns = "label",
      rows    = row_type == "label" & grepl("___",variable)
    )
  )

if(FALSE){
  shs_3 %>% filter(!is.na(d_2)) %>% pull(d_2)
}

#' 
#' # Conclusion
#' 
#' This national survey provides a first detailed overview of how cantonal authorities in Switzerland and Liechtenstein are engaged in the monitoring, prevention, and control of diseases transmitted by ticks and mosquitoes. While most respondents (77%) reported some level of involvement in related activities, findings reveal notable disparities in resources, actions, and strategic alignment across cantons and sectors.
#' 
#' Efforts appear more advanced for mosquito-borne diseases, with a broader range of implemented activities such as public awareness campaigns, professional training, and both active and passive monitoring. In contrast, tick-related activities were reported far less frequently, with more than half of respondents stating that their authority does not currently engage in any tick-related measures. Access to dedicated human or financial resources remains limited overall, particularly for tick-borne disease interventions.
#' 
#' The survey highlights important knowledge gaps reported by cantonal authorities, particularly in the areas of disease management and pathogen detection. These gaps were reported more frequently for mosquito-borne than tick-borne diseases, reflecting the growing public health importance of invasive mosquito species. Despite these challenges, several authorities reported innovative or pilot measures, such as the use of biological larvicides and participation in sterile insect technique studies. 
#' 
#' Collaborative networks are more commonly established around mosquito-related issues, with 60% of respondents indicating active exchanges with researchers and specialists in that domain, compared to only 20% for tick-related issues. Many participants called for stronger coordination across cantons, improved knowledge transfer, and clearer federal leadership, especially regarding outbreak preparedness, standardized intervention strategies, and resource sharing.
#' 
#' Finally, the survey underscores a general lack of integration of vector-borne diseases into climate adaptation strategies, both at the national and cantonal levels. Most respondents were unaware of whether these health issues are considered in existing climate strategies, suggesting a need to strengthen the connection between climate and vector-borne disease planning. Together, these findings point to the importance of reinforcing institutional capacity, inter-cantonal collaboration, and federal support in order to build a more coherent and effective response to the rising threat of vector-borne diseases under changing climatic conditions.