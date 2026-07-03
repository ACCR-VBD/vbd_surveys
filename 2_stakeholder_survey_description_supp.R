#' ---
#' title: Cantonal Engagement in the Prevention and Control of Vector-Borne Diseases in Switzerland and Liechtenstein
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
analysis_date = "2026-07-03"
load(paste0("savepoints/savepoint_",analysis_date,"/shs_3.Rdata"))
source("R/setup.R")
knitr::opts_chunk$set(echo = FALSE)

#' 
#' # Involvement and resources
#' 

shs_3 %>% select(starts_with("b_1"), -b_1_2,starts_with("b_2___"),
                 starts_with("b_3___"),
                 starts_with("b_4___"),type) %>% 
  
  tbl_summary(missing = "always",
              by=type,
              missing_text = "(Missing)",
              digits=everything()~0) %>%
  
  add_overall(last=FALSE) %>%
  
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

# # Concrete measures and activities

shs_3 %>% select(starts_with("c_1___"),starts_with("c_2___"),starts_with("c_3___"),type) %>% 
  
  tbl_summary(missing = "always",
              by =type,
              missing_text = "(Missing)",
              digits=everything()~0) %>% 
  add_overall(last=FALSE) %>% 
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

shs_3 %>% select(starts_with("c_6___"),starts_with("c_7___"),starts_with("c_8___")) %>% 
  
  tbl_summary(missing = "always", 
              missing_text = "(Missing)",
              digits=everything()~0) %>% 
  
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

# # Collaboration and coordination

shs_3 %>% select(starts_with("d_1___"),starts_with("d_3___"),starts_with("d_4___")) %>% 
  
  tbl_summary(missing = "always", 
              missing_text = "(Missing)",
              digits=everything()~0) %>% 
  
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

#' 
#' # Open text answers
#' 
#' The following tables show all open-text answers collected in the survey. 
#' Below each table is shown the result from a call to a locally run large language model
#' (qwen3.6:35b, june 2026) used to summarise all open-text answers, based on the following prompt:
#' 
#' *"I will provide you with a table of descriptive survey results from a 2025 national survey of cantonal authorities in Switzerland and Liechtenstein. The survey assessed awareness, measures, and needs related to tick- and mosquito-borne diseases. It was conducted via REDCap between April and June 2025 and included structured and open-text questions. The table includes open-text answers to the specified question in French or German. write a concise and consistent paragraph in English summarizing these open-text answers, in clear and neutral language suitable for a scientific report."*
#' 
#' The model received no information beyond this prompt and the table for each question.
#' Data processing was performed locally.
#'  
#' ## What other activities is your authority currently (2024/2025) implementing in the field of tick-borne diseases?
#'  
sh_101_open_text(shs_3,"c_1_1", "What other activities is your authority currently (2024/2025) implementing in the field of tick-borne diseases?")
#'  
#'  **qwen3.6:35b output: ** *Based on the provided 2025 national survey dataset, the open-text responses from 10 responding cantonal authorities (out of 55 total) revealed that additional tick-borne disease activities in 2024/2025 were largely routine and evenly distributed across several public health domains. Respondents primarily reported maintaining mandatory disease surveillance and reporting protocols coordinated with veterinary offices, particularly regarding tularemia and animal vector-borne pathogens. Public education efforts dominated the qualitative responses, including updates to official prevention websites and brochures, targeted outreach via agricultural publications, and professional information dissemination to veterinarians. Media relations and routine stakeholder communications were also frequently noted. A minority of jurisdictions highlighted specific occupational health measures, such as proposing free tick-borne encephalitis (TBE) vaccinations for state employees with professional exposure, while others aligned their activities with federal preventive guidelines. Overall, the responses reflected a standardized public health approach centered on intersectoral coordination, established surveillance frameworks, digital information dissemination, and targeted vaccination provisions rather than novel or outbreak-specific interventions.*
#'  
#' ## What other activities is your authority currently (2024/2025) implementing in the field of mosquitzo-borne diseases?
#'  
sh_101_open_text(shs_3,"c_2_1", "What other activities is your authority currently (2024/2025) implementing in the field of mosquito-borne diseases?")
#'  
#'  **qwen3.6:35b output: ** *Based on open-text responses from the 2025 national survey of cantonal authorities in Switzerland and Liechtenstein, respondents (N = 17 providing substantive answers out of 55 total) described a diverse portfolio of ongoing mosquito-borne disease activities. Primary interventions included passive and active vector surveillance, with several cantons having discontinued monitoring due to the established presence of Aedes albopictus (Asian tiger mosquito). Regulatory and operational measures centered on the authorization and oversight of control biocides, implementation of animal epidemic legislation (notably for bluetongue virus [BTV] and epizootic hemorrhagic disease), and coordination with national vaccination campaigns. Stakeholder communication was primarily conducted through seasonal press releases, downloadable informational materials, and routine media liaison. Authorities also emphasized intersectoral coordination among cantonal health agencies, veterinary services, municipal administrations, and specialized neobiota or wildlife offices. One respondent referred to external coordination bodies rather than listing direct jurisdictional activities, indicating variable implementation across regions.*
#'  
#' ## If your authority develops information, what other subjects are covered?
#' 
sh_101_open_text(shs_3,"c_3_1", "If your authority develops information, what other subjects are covered?")
#'  
#'  **qwen3.6:35b output:** *Among cantonal and Liechtenstein authorities that provided open-text responses regarding additional informational subjects, only two distinct topics were identified (n=2). One authority stated that its informational materials are exclusively focused on tick-borne diseases, with no supplementary content. The other indicated that animal protection is covered alongside human health messaging. The majority of respondents (53) did not provide an answer to this question. Given the limited number of completed responses, available data do not suggest systematic development of broader informational campaigns on related veterinary or ecological subjects by these authorities.*
#'  
#' ## How do you disseminate this information (e.g. websites, leaflets, information sessions, training courses)?
#' 
sh_101_open_text(shs_3,"c_4", "How do you disseminate this information (e.g. websites, leaflets, information sessions, training courses)?")
#'  
#'  **qwen3.6:35b output:** *Cantonal authorities primarily rely on official websites or state digital portals as their core dissemination channel, cited by twelve of the thirteen responding authorities. This online infrastructure is commonly supplemented by printed communications—including flyers, informational leaflets, and regular newsletters—alongside direct outreach via email, postal mail, and telephone contact. Several authorities also employ broader media strategies such as press releases, agricultural journals, social media platforms, and public presentations to maximize reach. Dissemination practices are typically stakeholder-specific: professional training sessions, municipal workshops, and targeted briefings are directed toward veterinarians, farmers, healthcare providers, and local administrators, while general public awareness efforts emphasize digital portals and mass-media channels. Qualitative answers were provided by only 13 authorities, indicating that detailed, practice-level insights on information dissemination remain limited across the national sample.*
#'  
#' ## What guidelines, support materials or reference documents (e.g. provided by the Confederation, other cantons or national or international organisations) do you (or the authority you work for) use to develop and implement measures?
#' 
sh_101_open_text(shs_3,"c_5", "What guidelines, support materials or reference documents (e.g. provided by the Confederation, other cantons or national or international organisations) do you (or the authority you work for) use to develop and implement measures?")
#'  
#'  **qwen3.6:35b output:** *Open-text responses show that cantonal authorities draw on a wide range of materials to support the development and implementation of measures related to tick- and mosquito-borne diseases. Frequently cited sources include technical guidelines and reference documents from federal bodies such as the Federal Office of Public Health (BAG/OFSP), the Federal Food Safety and Veterinary Office (BLV/OSAV), and the Federal Office for the Environment (BAFU/OFEV). Respondents also mentioned materials from specific cantons—particularly Ticino—as well as resources from academic institutions such as SUPSI and Swiss TPH. Several noted international exchanges with France and Germany, and the use of WHO or WOAH materials. In addition to official documents, some cantons developed their own materials or relied on scientific literature, peer-reviewed journals, and expert networks. However, a few respondents reported not using any specific documents or indicated a lack of engagement in developing measures.*
#'  
#' ## In addition to the mentioned fields of action, where is the greatest need for information or the greatest lack of knowledge about tick-borne diseases in the authority you work for?
#' 
sh_101_open_text(shs_3,"c_6_1", "In addition to the mentioned fields of action, where is the greatest need for information 
                    or the greatest lack of knowledge about tick-borne diseases in the authority you work for?")
#'  
#'  **qwen3.6:35b output: ** *Analysis of the open-text responses regarding information needs and knowledge gaps on tick-borne diseases reveals three dominant patterns: jurisdictional boundaries, absence of perceived need, and resource limitations. A substantial number of respondents indicated that tick-related surveillance or management falls outside their institutional mandate, with several authorities explicitly distinguishing between human health and veterinary domains and noting that current animal health priorities do not include tick-borne pathogens. Others reported no specific information needs or knowledge gaps, suggesting that existing frameworks sufficiently address the issue within their scope. Operational constraints were also cited, particularly limited personnel or financial resources, which hinder targeted knowledge acquisition. Only one response emphasized a systemic need for federally coordinated and funded monitoring of arthropod vectors and domestic/wildlife hosts to enable early detection of emerging or seasonal threats. The high proportion of non-responses (40 entries) further suggests that tick-borne disease surveillance is either not a focal concern for many cantonal departments or is centralized at the federal level. These qualitative insights are derived exclusively from the provided 2025 survey dataset, which reflects localized institutional perceptions rather than a uniform nationwide knowledge deficit.*
#'  
#' ## In addition to the mentioned fields of action, where is the greatest need for information or the greatest lack of knowledge about mosquito-borne diseases in the authority you work for?
#' 
sh_101_open_text(shs_3,"c_7_1", "In addition to the mentioned fields of action, where is the greatest need for information 
                    or the greatest lack of knowledge about mosquito-borne diseases in the authority you work for?")
#'  
#'  **qwen3.6:35b output: ** *Among respondents who provided qualitative input on mosquito-borne disease knowledge gaps (n = 15), open-text answers consistently highlighted three domains of operational uncertainty: institutional coordination and competence delineation across federal, cantonal, intercantonal, and municipal levels; technical guidance regarding evidence-based criteria for adulticide deployment, standardized vector density monitoring, and long-term non-chemical control alternatives; and public health communication strategies aimed at preventing the establishment of invasive species such as Aedes albopictus. Several authorities noted that existing collaborative frameworks (e.g., intercantonal working groups, One Health initiatives, and monitoring partnerships with institutions such as SUPSI) already address their needs, while others clarified that clinical diagnosis and therapeutic management fall outside their administrative mandate. Conversely, a subset of respondents indicated no current information gaps or referred to previously submitted responses. Overall, the qualitative data indicate that priority knowledge and capacity needs lie primarily in vector control governance, methodological standardization, and preventive sensitization rather than in clinical or individual medical guidance, reflecting the public health orientation of cantonal vector management programs (data sourced from the provided 2025 survey results).*
#'  
#' ## What other measures is your authority implementing to control or combat invasive mosquitoes?
#' 
sh_101_open_text(shs_3,"c_8_1", "What other measures is your authority implementing to control or combat invasive mosquitoes?")
#'  
#'  **qwen3.6:35b output: ** *Survey responses indicate a predominantly precautionary and coordinated approach to invasive mosquito management among the surveyed cantonal authorities. Chemical interventions, including adulticides and non-chemical larvicides such as Aquatain, are deployed selectively and only when necessary, with biological control agents like VectoBac (Bacillus thuringiensis subsp. israelensis) preferentially supplied by cantons to municipalities for targeted application. Control strategies emphasize integrated surveillance, sterile insect technique (SIT) pilot trials supported by SUPSI, surface water management to limit oviposition sites, and multi-level collaboration through cantonal working groups and federal platforms such as OneHealth and the GL-Vectors Working Group on Vectors UZ6. Several authorities noted that existing animal health legislation does not differentiate between invasive and native mosquitoes, prompting a focus on advisory measures for livestock owners, including vaccination guidelines and preventive husbandry practices to mitigate vector-borne virus transmission (e.g., bluetongue and EHD viruses). Additionally, multiple respondents reported no established populations of invasive species such as the Asian tiger mosquito in their jurisdictions, resulting in either limited direct intervention or a purely informational role. Overall, the reported measures reflect a shift toward evidence-based, integrated management that prioritizes monitoring, biological control, and veterinary public health coordination over broad-spectrum chemical treatment.*
#'  
#' ## In what areas of tick- and mosquito-borne diseases do you think the cantons need support from the Confederation?
#' 
sh_101_open_text(shs_3,"d_2", "In what areas of tick- and mosquito-borne diseases do you think the cantons need support from the Confederation?")
#'  
#'  **qwen3.6:35b output: ** *Analysis of the provided open-text responses indicates that cantonal and Liechtenstein authorities prioritize federal support across four interrelated domains: regulatory harmonization, enhanced surveillance infrastructure, dedicated resource allocation, and structured inter-cantonal coordination. Respondents consistently emphasized the need for standardized, nationwide guidelines and binding operational protocols to ensure policy coherence, particularly for outbreak management and response to detected invasive mosquitoes (e.g., Aedes albopictus). There was broad consensus on establishing a nationally coordinated pathogen and vector monitoring system, including technical standardization (such as viromia duration parameters), geospatial risk mapping, and systematic data collection on tick prevalence and adult TBE vaccination coverage. Financial and human resource support was frequently cited as essential for sustaining vector surveillance, controlling established non-native mosquito populations, funding epidemiological studies, and prioritizing interventions under constrained budgets. Respondents also highlighted the necessity of cross-sectoral coordination mechanisms, centralized federal information platforms, and formalized knowledge-transfer pathways between cantonal authorities, the Swiss Federal Office for the Environment (FOEN), and research institutions (e.g., IVI). Public communication and prevention emerged as another key priority, with requests for standardized preventive guidelines, biocide homologation, improved vaccine accessibility, targeted public awareness campaigns regarding emerging arboviral threats (e.g., West Nile virus, bluetongue disease), and civil protection guidance on breeding-site reduction on private land. A small proportion of respondents indicated no immediate need for federal support, preferred to await conclusions from ongoing federal working groups, or noted that existing veterinary regulatory frameworks already address their operational requirements. These qualitative findings align with documented Swiss public health priorities regarding climate-driven vector expansion and the need for harmonized arboviral risk management (Swiss Federal Office of Public Health [FOPH], Tick-borne Encephalitis in Switzerland; FOPH & Federal Council, National Strategy for Climate Change Adaptation).*

