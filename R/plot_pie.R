library(dplyr)
library(ggplot2)
library(patchwork)

# 1) b_1 data frame with labels
df1 <- shs_3 %>%
  count(b_1) %>%
  mutate(
    pct   = n / sum(n) * 100,
    label = paste0(n, "\n", sprintf("%.1f%%", pct))
  )

# 2) Helper that returns label_text for b_3 or b_4
make_pie_df <- function(df, prefix) {
  tick    <- df[[paste0(prefix, "___1")]] == "Yes"
  mosq    <- df[[paste0(prefix, "___2")]] == "Yes"
  neither <- df[[paste0(prefix, "___3")]] == "Yes"
  dk      <- df[[paste0(prefix, "___4")]] == "Yes"
  
  counts <- c(
    "Tick-borne diseases"           = sum(tick    & !mosq, na.rm = TRUE),
    "Mosquito-borne diseases"       = sum(mosq    & !tick, na.rm = TRUE),
    "Neither"              = sum(neither,            na.rm = TRUE),
    "Don't know"           = sum(dk,                  na.rm = TRUE),
    "Tick- & mosquito-borne disease" = sum(tick    & mosq,     na.rm = TRUE)
  )
  
  tibble(
    category   = names(counts),
    count      = counts
  ) %>%
    filter(count > 0) %>%
    mutate(
      pct         = count / sum(count) * 100,
      label_text  = paste0(count, "\n", sprintf("%.1f%%", pct))
    )
}



# 3) Data frames for b_3 and b_4
df3 <- make_pie_df(shs_3, "b_3")
df4 <- make_pie_df(shs_3, "b_4")

# 1) define your master palette & factor‐levels
pal <- c(
  "Tick-borne diseases"             = "#66c2a5",
  "Mosquito-borne diseases"         = "#fc8d62",
  "Tick- & mosquito-borne disease"  = "#8da0cb",
  "Neither"                         = "#e78ac3",
  "Don't know"                      = "#a6d854"
)
all_lvls <- names(pal)

# 2) force both data frames to use that same factor
df3 <- df3 %>% mutate(category = factor(category, levels = all_lvls))
df4 <- df4 %>% mutate(category = factor(category, levels = all_lvls))

# 3) define a shared fill scale
fill_scale <- scale_fill_manual(
  name   = NULL,      # no legend title
  values = pal,
  drop   = T     # show even zero‐count levels if you want
)

# 4) rebuild your pies (or just add the scale to the existing p2/p3)
p2 <- ggplot(df3, aes(x = 1, y = pct, fill = category)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(x = 1.7, label = label_text),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Does your authority have human resources to develop and implement 
activities related to diseases transmitted by ticks and mosquitoes?") +
  theme_void() +
  fill_scale

p3 <- ggplot(df4, aes(x = 1, y = pct, fill = category)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y", start = 0) +
  geom_text(aes(x = 1.7, label = label_text),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Does your authority have a budget for the development and implementation of 
activities in the field of diseases transmitted by ticks and mosquitoes?") +
  theme_void() +
  fill_scale

library(gridExtra)
grid.arrange(p2, p3, nrow = 1, widths = c(1, 1))

