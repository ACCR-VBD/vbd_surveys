#' ---
#' title: VBD surveys 
#' subtitle: setup
#' author: jriou
#' date: 2025-06-10
#' ---


# libs --------------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse,
               gtsummary,
               labelled,
               readxl,
               gt)

# set paths ---------------------------------------------------------------

path_script = 
  file.path("R/")

# source functions --------------------------------------------------------

all_fns = 
  dir("R/", pattern="sh|be", full.names = TRUE) %>% 
  file.path()
lapply(all_fns, function(x) source(x, echo=FALSE))


# create savepoint --------------------------------------------------------

controls$savepoint = 
  paste0("savepoints/savepoint_",controls$analysis_date) %>% 
  file.path()
dir.create(controls$savepoint, showWarnings = FALSE)

# esthetics ---------------------------------------------------------------

theme_set(theme_bw())
controls$cols = c("firebrick","dodgerblue","chartreuse","goldenrod")

# short custom functions --------------------------------------------------

qsum = function(a,b,c,digits) {
  out = 
    paste0(
      formatC(a,format="f",big.mark=",",digits=digits),
      " (",
      formatC(b,format="f",big.mark=",",digits=digits),
      " to ",
      formatC(c,format="f",big.mark=",",digits=digits),
      ")")
  return(out)
}


