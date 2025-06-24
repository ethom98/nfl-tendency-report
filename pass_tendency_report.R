#Install and Load Packages
install.packages(c("nflfastR", "tidyverse", "gt", "janitor"))
library(nflfastR)
library(tidyverse)
library(gt)
library(janitor)

#Load 2024 NFL Play-by-Play Data) 
pbp_2024 <- load_pbp(2024) 

#Filter for Regular Season + BAL & BUF + Only Valid Plays
pbp_filtered <- pbp_2024 %>%
  filter(season_type == "REG") %>%
  filter(posteam %in% c("BAL", "BUF")) %>%
  filter(play_type %in% c("run", "pass")) %>%
  mutate(
    pass = if_else(qb_scramble == 1, 1, if_else(play_type == "pass", 1, 0)),
    yardline_100 = ifelse(is.na(yardline_100), 99, yardline_100)
  )

#CREATE BUCKETS
#Down & Distance Buckets
pbp_filtered <- pbp_filtered %>%
  mutate(
    distance_bucket = case_when(
      down == 1 & ydstogo >= 10 ~ "1st & 10+",
      down == 1 & ydstogo < 10 ~ "1st & <10",
      down == 2 & ydstogo <= 3 ~ "2nd & <4",
      down == 2 & ydstogo <= 6 ~ "2nd & 4–7",
      down == 2 & ydstogo > 6 ~ "2nd & 7+",
      down == 3 & ydstogo <= 3 ~ "3rd & <4",
      down == 3 & ydstogo <= 6 ~ "3rd & 4–7",
      down == 3 & ydstogo > 6 ~ "3rd & 7+"
    )
  )
# Field Position Buckets
pbp_filtered <- pbp_filtered %>%
  mutate(
    field_position = case_when(
      yardline_100 >= 91 ~ "Goal Line",
      yardline_100 >= 81 ~ "Lower Red Zone",
      yardline_100 >= 71 ~ "Upper Red Zone",
      yardline_100 >= 21 ~ "In the Field",
      yardline_100 < 21  ~ "Backed Up"
    )
  )

# CREATE THE PASS RATE TABLE
#Group by Team + Down/Distance + Field Position
summary_table <- pbp_filtered %>%
  filter(!is.na(distance_bucket), !is.na(field_position)) %>%
  group_by(posteam, distance_bucket, field_position) %>%
  summarise(
    pass_plays = sum(pass),
    total_plays = n(),
    pass_pct = round(mean(pass) * 100, 1),
    display = paste0(pass_pct, "% (", pass_plays, "/", total_plays, ")"),
    .groups = "drop"
  )

# SPLIT INTO BAL AND BUF
summary_BAL <- summary_table %>%
  filter(posteam == "BAL") %>%
  select(distance_bucket, field_position, pass_pct)

summary_BUF <- summary_table %>%
  filter(posteam == "BUF") %>%
  select(distance_bucket, field_position, pass_pct)

# PIVOT TO WIDE FORMAT
summary_BAL_wide <- summary_BAL %>%
  pivot_wider(
    names_from = field_position,
    values_from = pass_pct
  ) %>%
  arrange(factor(distance_bucket, levels = c(
    "1st & 10+", "1st & <10", "2nd & 7+", "2nd & 4–7", "2nd & <4",
    "3rd & 7+", "3rd & 4–7", "3rd & <4"
  ))) %>%
  clean_names()

summary_BUF_wide <- summary_BUF %>%
  pivot_wider(
    names_from = field_position,
    values_from = pass_pct
  ) %>%
  arrange(factor(distance_bucket, levels = c(
    "1st & 10+", "1st & <10", "2nd & 7+", "2nd & 4–7", "2nd & <4",
    "3rd & 7+", "3rd & 4–7", "3rd & <4"
  ))) %>%
  clean_names()

# COLOR CODED GT TABLES
# Baltimore Table
summary_BAL_wide %>%
  gt() %>%
  tab_header(title = "Baltimore Ravens – Pass Rate % by Situation") %>%
  data_color(
    columns = where(is.numeric),
    colors = scales::col_numeric(
      palette = c("lightblue", "white", "tomato"),
      domain = c(0, 100)
    )
  )

# Buffalo Table
summary_BUF_wide %>%
  gt() %>%
  tab_header(title = "Buffalo Bills – Pass Rate % by Situation") %>%
  data_color(
    columns = where(is.numeric),
    colors = scales::col_numeric(
      palette = c("lightblue", "white", "tomato"),
      domain = c(0, 100)
    )
  )

# VIEW OR EXPORT THE TABLE
#View It in RStudio
summary_table %>%
  gt() %>%
  tab_header(title = md("**Pass Rate Breakdown: BAL vs BUF (2024)**"))

#Export to CSV
write_csv(summary_table, "pass_rate_summary_BAL_BUF.csv")






