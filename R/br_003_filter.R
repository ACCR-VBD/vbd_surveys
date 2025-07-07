#' ---
#' title: VBD surveys 
#' subtitle: Filter out incomplete answers
#' author: lgoepp
#' date: 2025-06-10
#' ---

br_003_filters = function(dat_in){
  
  ##### DATA MANAGEMENT - removal of NA variables
  dat_out=dat_in[,!apply(dat_in,2,function(x)all(is.na(x)))]
  
  # test that there is only NA values - yes, 93 subject have only year of birth and age
  # impossible to use, i remove it
  apply(is.na(dat_out),1,sum)==22
  test=dat_out[apply(is.na(dat_out),1,sum)==22,]
  test=test[,-c(1,2,3,4)]
  all(is.na(test)|test==0|test=="") 
  dat_out=dat_out %>% filter(apply(is.na(dat_out),1,sum)!=22)
  
  ##### MANAGEMENT OF PROFESSION
  #### note : bl_prof_now_sql variables are exclusive : table(apply(brs_0[,grepl("bl_prof_now_sql",colnames(brs_0))]=="",1,6-sum))
  
  varnames_prof=colnames(dat_out)[grepl("bl_prof_now_sql",colnames(dat_out))]
  bl_prof_now=apply(dat_out[,varnames_prof],1,paste,collapse="")
  
  dat_out[varnames_prof]=NULL #removal of profession
  
  #extraction of first digit
  ch_isco_code_levl1=str_extract(unlist(lapply(str_split(bl_prof_now," "),function(x)x[length(x)])), "\\d")
  
  #loading the name of categories : gpt translation of the result of https://www.i14y.admin.ch/api/Nomenclatures/HCL_CH_ISCO_19_PROF/levelexport/XLSX?language=fr&level=1&annotations=true
  isco_code_name=read_excel(file.path(controls$data_path,"HCL_CH_ISCO_19_PROF_level_1.xlsx"))
  dat_out$isco_lvl1=isco_code_name$Name_en[match(ch_isco_code_levl1,isco_code_name$Code)]
  
  #addition of global question when several answers are possible
  name_svrl_ans=unique(unlist(lapply(str_split(colnames(dat_out)[grepl("___",colnames(dat_out))],"___"),function(x)x[1])))
  dat_out[,name_svrl_ans]=NA
  
  dat_out$bl_vbd=NA
  #Return
  return(dat_out)
}