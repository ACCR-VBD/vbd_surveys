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
analysis_date = "2025-06-26"
load(paste0("savepoints/savepoint_",analysis_date,"/shs_3.Rdata"))
source("R/setup.R")
knitr::opts_chunk$set(echo = FALSE)

#' 
#' # Open text answers
#' 
#' The following tables show all open-text answers collected in the survey. 
#' Below each table is shown the result from a call to GPT-4 Turbo (june 2025)
#' used to summarise all open-text answers, based on the following prompt:
#' 
#' *"I will provide you with a table of descriptive survey results from a 2025 national survey of cantonal authorities in Switzerland and Liechtenstein. The survey assessed awareness, measures, and needs related to tick- and mosquito-borne diseases. It was conducted via REDCap between April and June 2025 and included structured and open-text questions. The table includes open-text answers to the specified question in French or German. write a concise and consistent paragraph in English summarizing these open-text answers, in clear and neutral language suitable for a scientific report."*
#' 
#' These summaries were used in the main text to complement the results. No other information was given to GPT-4 Turbo.
#'  
#' ## What other activities is your authority currently (2024/2025) implementing in the field of tick-borne diseases?
#'  
sh_101_open_text(shs_3,"c_1_1", "What other activities is your authority currently (2024/2025) implementing in the field of tick-borne diseases?")
#'  
#'  **GPT-4 Turbo output: ** *In response to the question on additional activities currently implemented in the field of tick-borne diseases, a small number of respondents (n = 9) provided open-text answers indicating targeted and context-specific measures. These included public awareness campaigns via professional agricultural media, responses to media inquiries, and routine dissemination of information to veterinarians through websites, conferences, or circulars. Some authorities described activities aligned with the Animal Disease Ordinance, such as mandatory reporting and coordination on diseases like tularemia and Q fever. Vaccination-related measures were also mentioned, including proposals for free vaccination against tick-borne encephalitis for at-risk state employees. Other responses referenced federal-level initiatives and disease surveillance obligations. Overall, the responses reflect limited but diverse engagements, with most authorities (n = 45) not reporting any additional tick-related activities.*
#'  
#' ## What other activities is your authority currently (2024/2025) implementing in the field of mosquitzo-borne diseases?
#'  
sh_101_open_text(shs_3,"c_2_1", "What other activities is your authority currently (2024/2025) implementing in the field of mosquito-borne diseases?")
#'  
#'  **GPT-4 Turbo output: ** *Open-text responses describing additional activities implemented by cantonal authorities in the field of mosquito-borne diseases reveal a range of targeted measures, though reported by a minority of respondents (n = 16). Several authorities referenced activities linked to the control of animal diseases such as bluetongue and West Nile virus under the Animal Disease Ordinance, including vaccination campaigns and vector control. Others noted their role in media communication, chemical use oversight, and coordination with other cantonal services or the federal government. Examples of local engagement included public meetings on the tiger mosquito, support for municipalities in vector control, and surveillance of mosquito populations, although one respondent reported the discontinuation of passive monitoring due to widespread mosquito presence. A few responses indicated that specific responsibilities lay with other agencies, such as environmental or coordination offices. Overall, these activities illustrate a mix of animal health, public communication, and administrative coordination efforts, with most authorities (n = 38) not reporting additional mosquito-related measures.*
#'  
#' ## If your authority develops information, what other subjects are covered?
#' 
sh_101_open_text(shs_3,"c_3_1", "If your authority develops information, what other subjects are covered?")
#'  
#'  **GPT-4 Turbo output:** *Only two respondents provided open-text answers regarding additional subjects covered by their authority in information materials. One referred to existing content on tick-borne diseases available on their official website, while the other mentioned animal protection as a covered topic. The vast majority of respondents (n = 52) did not specify any additional subjects, suggesting limited extension beyond the predefined categories or lack of relevant materials.*
#'  
#' ## How do you disseminate this information (e.g. websites, leaflets, information sessions, training courses)?
#' 
sh_101_open_text(shs_3,"c_4", "How do you disseminate this information (e.g. websites, leaflets, information sessions, training courses)?")
#'  
#'  **GPT-4 Turbo output:** *Open-text responses regarding dissemination methods indicate that cantonal authorities use a variety of communication channels to share information on tick- and mosquito-borne diseases. Frequently mentioned approaches included official websites, email, and printed materials such as flyers and leaflets. Several respondents reported direct communication with professionals (e.g., veterinarians or farmers) via telephone or targeted mailings. Other dissemination formats included public presentations, newsletters, municipal-level engagement, professional training, and use of media outlets such as newspapers or social media. Despite this range of methods, only 12 respondents provided a detailed answer, suggesting that formal communication strategies may not be uniformly developed or consistently applied across cantons.*
#'  
#' ## What guidelines, support materials or reference documents (e.g. provided by the Confederation, other cantons or national or international organisations) do you (or the authority you work for) use to develop and implement measures?
#' 
sh_101_open_text(shs_3,"c_5", "What guidelines, support materials or reference documents (e.g. provided by the Confederation, other cantons or national or international organisations) do you (or the authority you work for) use to develop and implement measures?")
#'  
#'  **GPT-4 Turbo output:** *Open-text responses show that cantonal authorities draw on a wide range of materials to support the development and implementation of measures related to tick- and mosquito-borne diseases. Frequently cited sources include technical guidelines and reference documents from federal bodies such as the Federal Office of Public Health (BAG/OFSP), the Federal Food Safety and Veterinary Office (BLV/OSAV), and the Federal Office for the Environment (BAFU/OFEV). Respondents also mentioned materials from specific cantons—particularly Ticino—as well as resources from academic institutions such as SUPSI and Swiss TPH. Several noted international exchanges with France and Germany, and the use of WHO or WOAH materials. In addition to official documents, some cantons developed their own materials or relied on scientific literature, peer-reviewed journals, and expert networks. However, a few respondents reported not using any specific documents or indicated a lack of engagement in developing measures.*
#'  
#' ## In addition to the mentioned fields of action, where is the greatest need for information or the greatest lack of knowledge about tick-borne diseases in the authority you work for?
#' 
sh_101_open_text(shs_3,"c_6_1", "In addition to the mentioned fields of action, where is the greatest need for information 
                    or the greatest lack of knowledge about tick-borne diseases in the authority you work for?")
#'  
#'  **GPT-4 Turbo output: ** *Open-text responses regarding additional information needs related to tick-borne diseases revealed a range of perspectives. Several respondents stated that their authority is not responsible for tick-related issues or currently sees no need for additional information, particularly in the context of animal health. Others emphasized a general lack of resources or indicated that tick-borne diseases are a low priority. Some participants highlighted the challenge of identifying precise knowledge gaps, pointing instead to the need for improved coordination and federally supported monitoring systems—especially for early detection of emerging diseases. A few responses noted that while current needs are limited, continued access to up-to-date information remains important.*
#'  
#' ## In addition to the mentioned fields of action, where is the greatest need for information or the greatest lack of knowledge about mosquito-borne diseases in the authority you work for?
#' 
sh_101_open_text(shs_3,"c_7_1", "In addition to the mentioned fields of action, where is the greatest need for information 
                    or the greatest lack of knowledge about mosquito-borne diseases in the authority you work for?")
#'  
#'  **GPT-4 Turbo output: ** *Open-text responses identified several areas where additional information or clarification is needed regarding mosquito-borne diseases. Key themes included unclear roles and responsibilities between federal, cantonal, and municipal authorities, particularly in relation to coordination and implementation of control measures. Respondents highlighted the need for clearer guidance on the use of adulticides, including criteria for deployment, responsibilities, and alternatives. Some noted a lack of immediate information needs but anticipated future demand should outbreaks occur. Others expressed interest in improved methods for mosquito density assessment, prevention strategies to avoid tiger mosquito establishment, and ongoing collaboration with research institutions. A few respondents indicated no specific needs or stated that such issues were outside their department’s scope.*
#'  
#' ## What other measures is your authority implementing to control or combat invasive mosquitoes?
#' 
sh_101_open_text(shs_3,"c_8_1", "What other measures is your authority implementing to control or combat invasive mosquitoes?")
#'  
#'  **GPT-4 Turbo output: ** *Open-text responses revealed a variety of additional measures implemented by cantonal authorities to control or combat invasive mosquitoes. Several respondents reported coordination with specialized cantonal units or municipalities, which are responsible for operational activities such as larvicide application (e.g., Bti or Aquatain). Some authorities follow national guidelines, using adulticides or non-chemical larvicides only when necessary. Preventive recommendations for animal holders were also mentioned, particularly concerning vector-borne animal diseases like bluetongue. Pilot studies exploring sterile insect techniques (SIT) and the release of sterilized male mosquitoes were noted in collaboration with research institutions such as SUPSI. Some cantons reported no established populations of invasive mosquitoes and thus no active control measures, while others indicated their role was limited to oversight, awareness, or financial support. A few respondents also referenced interdisciplinary collaborations through working groups or national platforms.*
#'  
#' ## In what areas of tick- and mosquito-borne diseases do you think the cantons need support from the Confederation?
#' 
sh_101_open_text(shs_3,"d_2", "In what areas of tick- and mosquito-borne diseases do you think the cantons need support from the Confederation?")
#'  
#'  **GPT-4 Turbo output: ** *Cantonal authorities identified a broad range of areas where support from the Confederation would be beneficial for managing tick- and mosquito-borne diseases. Commonly mentioned needs included harmonized national guidelines, standardized action plans, and legal frameworks to ensure consistent responses across cantons. Respondents emphasized the importance of coordination—both inter-cantonal and with federal agencies—and requested dedicated platforms for information exchange and communication. Many highlighted the need for financial and human resource support, particularly for surveillance, monitoring, and control of invasive mosquito species. Additional areas of support included improved public communication, access to vaccines, authorization of effective biocides, and enhanced knowledge transfer. Some participants called for clearer delineation of roles and responsibilities, especially concerning outbreak preparedness, while others stressed the need for technical guidance on specific topics such as vector monitoring, disease risk assessment, and prevention strategies. A few respondents indicated no current need or were awaiting the results of ongoing federal working groups to assess future requirements.*

