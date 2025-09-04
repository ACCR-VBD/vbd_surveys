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
               gt,
               gridExtra)

# set paths ---------------------------------------------------------------

path_script = 
  file.path("R/")

# source functions --------------------------------------------------------

all_fns = 
  dir("R/", pattern="sh|br", full.names = TRUE) %>% 
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

# html-to-pdf function (using microsoft edge)

# html_to_pdf = function(report_files, controls,survey_name="shs"){
#   
#   name_pdf_boolean = paste0("pdf_",survey_name)
#   report_files_pdf = report_files[controls[[name_pdf_boolean]]]
#   
#   edge <- if (nzchar(Sys.which("msedge"))) Sys.which("msedge")else "C:/Program Files/Microsoft/Edge/Application/msedge.exe"
#   
#   for (rfile in report_files_pdf) {
#     base <- tools::file_path_sans_ext(basename(rfile))
#     
#     html <- normalizePath(file.path(controls$savepoint, paste0(base, ".html")), winslash = "/")
#     pdf  <- normalizePath(file.path(controls$savepoint, paste0(base, ".pdf")),  winslash = "\\", mustWork = FALSE)
#     url  <- paste0("file:///", utils::URLencode(html, reserved = TRUE))
#     
#     system2(edge, c(
#       "--headless", "--disable-gpu",
#       paste0("--print-to-pdf=", shQuote(pdf, type = "cmd")),
#       shQuote(url, type = "cmd")
#     ), wait = TRUE)
#   }
# }

html_to_pdf <- function(report_files, controls, survey_name = "shs") {
  # report_files should be a *named* character vector: c(core="...html", supp="...html")
  want <- controls[[paste0("pdf_", survey_name)]]
  
  # align by names (core/supp), then keep only those flagged TRUE
  to_convert <- report_files[names(want)][as.logical(want)]
  to_convert <- to_convert[!is.na(to_convert) & file.exists(to_convert)]
  if (!length(to_convert)) return(invisible(NULL))
  
  # find Edge (64-bit or x86)
  edge <- c(
    Sys.which("msedge"),
    "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
  )
  edge <- edge[edge != "" & file.exists(edge)][1]
  if (is.na(edge)) stop("Microsoft Edge not found.")
  
  for (html in to_convert) {
    html <- normalizePath(html, winslash = "/", mustWork = TRUE)
    pdf  <- sub("\\.html?$", ".pdf", normalizePath(html, winslash = "\\"))
    
    ## --- injected: print-only CSS to remove horizontal scrollbars
    ##               and prevent right-side clipping of gt/gtsummary tables
    print_css <- paste(
      "@media print {",
      ".main-container,.toc-content{width:100%!important;max-width:none!important}",
      ".table-responsive,div[style*=\"overflow\"],.gt_table_container{overflow:visible!important}",
      "table{width:100%!important;table-layout:auto!important}",
      "th,td{white-space:normal!important;word-break:break-word}",
      ".gt_table{width:100%!important;overflow:visible!important}",
      "}",
      sep = "\n"
    )
    lines <- readLines(html, warn = FALSE, encoding = "UTF-8")
    insert <- c("<!-- injected print fixes -->", "<style>", print_css, "</style>")
    end_head <- grep("(?i)</head>", lines, perl = TRUE)
    if (length(end_head)) {
      lines <- append(lines, insert, after = end_head[1] - 1)
    } else {
      lines <- c("<!doctype html><html><head>", insert, "</head><body>", lines, "</body></html>")
    }
    patched <- tempfile(fileext = ".html")
    writeLines(lines, patched, useBytes = TRUE)
    ## --- end injected
    
    url  <- paste0("file:///", normalizePath(patched, winslash = "/"))  # print the patched copy
    
    system2(edge, c(
      "--headless", "--disable-gpu",
      paste0("--print-to-pdf=", shQuote(pdf, type = "cmd")),
      shQuote(url, type = "cmd")
    ), wait = TRUE)
    
    if (!file.exists(pdf) || is.na(file.info(pdf)$size) || file.info(pdf)$size < 1024)
      stop("Edge didn't produce a valid PDF for: ", html)
  }
  invisible(NULL)
}




