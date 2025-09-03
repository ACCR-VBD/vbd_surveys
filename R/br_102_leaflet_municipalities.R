library(sf)
library(dplyr)
library(leaflet)
library(rmapshaper)
library(htmlwidgets)

create_respondent_map <- function() {
  
  # load survey data
  dat_in=read.csv("data/1951BEreadyHauptstud-VectorborneDiseases_DATA_2025-06-12_1515.csv")
  
  # load municipal boundaries
  g <- read_sf("data/swissboundaries3d_2025-04_2056_5728.shp/swissBOUNDARIES3D_1_5_TLM_HOHEITSGEBIET.shp")
  
  # count respondents by municipality code
  counts <- dat_in %>%
    count(bl_commune) %>%
    rename(BFS_NUMMER = bl_commune, respondents = n)
  
  # join, fill missing with zero, drop Z, reproject, simplify shapes
  g2_wgs84 <- g %>%
    left_join(counts, by = "BFS_NUMMER") %>%
    mutate(respondents = coalesce(respondents, 0)) %>%
    st_zm(drop = TRUE) %>%
    st_transform(4326) %>% 
    ms_simplify(keep = 0.05, keep_shapes = TRUE)
  
  # color bins over positives
  pal <- colorBin(
    "YlOrRd",
    domain = g2_wgs84$respondents[g2_wgs84$respondents > 0],
    bins   = c(1, 5, 10, 20, 50, 100, 300)
  )
  
  # return leaflet map
  leaf_map=leaflet(g2_wgs84) %>%
    addTiles() %>%
    addPolygons(
      fillColor   = ~pal(respondents),
      
      #setting the opacity level : first number for empty municipalities, second for municipalities with respondents
      fillOpacity = ~ifelse(respondents == 0 | is.na(respondents), 0.1, 0.65), 
      
      color       = "grey", #color for empty municipalities
      weight      = 1,
      label       = ~paste(NAME, respondents, sep = ": ")
    ) %>%
    addLegend(
      "topright",
      pal    = pal,
      values = ~respondents,
      title  = "Respondents"
    )
  return(leaf_map)
}

# create_respondent_map()
# 
# htmlwidgets::saveWidget(create_respondent_map(), "data/map_BeReady.html")