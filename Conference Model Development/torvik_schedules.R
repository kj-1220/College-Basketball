library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(fuzzyjoin)

# -----------------------------
# 1. Read files
# -----------------------------
schedule <- read_csv("mbb_conference_schedules.csv", show_col_types = FALSE)
torvik   <- read_csv("torvik_daily_2025_2026.csv", show_col_types = FALSE)

schedule <- schedule %>%
  mutate(game_date = as.Date(game_date))

torvik <- torvik %>%
  mutate(date = as.Date(date))

# -----------------------------
# 2. Explicit mapping table
#    based on the actual files
# -----------------------------
team_map <- tribble(
  ~schedule_team,                  ~torvik_team,
  "Arizona State Sun Devils",      "Arizona St.",
  "Arizona Wildcats",              "Arizona",
  "BYU Cougars",                   "BYU",
  "Baylor Bears",                  "Baylor",
  "Boston College Eagles",         "Boston College",
  "Butler Bulldogs",               "Butler",
  "California Golden Bears",       "California",
  "Cincinnati Bearcats",           "Cincinnati",
  "Clemson Tigers",                "Clemson",
  "Colorado Buffaloes",            "Colorado",
  "Creighton Bluejays",            "Creighton",
  "DePaul Blue Demons",            "DePaul",
  "Duke Blue Devils",              "Duke",
  "Eastern Washington Eagles",     "Eastern Washington",
  "Florida State Seminoles",       "Florida St.",
  "Georgetown Hoyas",              "Georgetown",
  "Georgia Tech Yellow Jackets",   "Georgia Tech",
  "Houston Cougars",               "Houston",
  "Idaho State Bengals",           "Idaho St.",
  "Idaho Vandals",                 "Idaho",
  "Illinois Fighting Illini",      "Illinois",
  "Indiana Hoosiers",              "Indiana",
  "Iowa Hawkeyes",                 "Iowa",
  "Iowa State Cyclones",           "Iowa St.",
  "Kansas Jayhawks",               "Kansas",
  "Kansas State Wildcats",         "Kansas St.",
  "Louisville Cardinals",          "Louisville",
  "Marquette Golden Eagles",       "Marquette",
  "Maryland Terrapins",            "Maryland",
  "Miami Hurricanes",              "Miami FL",
  "Michigan State Spartans",       "Michigan St.",
  "Michigan Wolverines",           "Michigan",
  "Minnesota Golden Gophers",      "Minnesota",
  "Montana Grizzlies",             "Montana",
  "Montana State Bobcats",         "Montana St.",
  "NC State Wolfpack",             "N.C. State",
  "Nebraska Cornhuskers",          "Nebraska",
  "North Carolina Tar Heels",      "North Carolina",
  "Northern Arizona Lumberjacks",  "Northern Arizona",
  "Northern Colorado Bears",       "Northern Colorado",
  "Northwestern Wildcats",         "Northwestern",
  "Notre Dame Fighting Irish",     "Notre Dame",
  "Ohio State Buckeyes",           "Ohio St.",
  "Oklahoma State Cowboys",        "Oklahoma St.",
  "Oregon Ducks",                  "Oregon",
  "Penn State Nittany Lions",      "Penn St.",
  "Pittsburgh Panthers",           "Pittsburgh",
  "Portland State Vikings",        "Portland St.",
  "Providence Friars",             "Providence",
  "Purdue Boilermakers",           "Purdue",
  "Rutgers Scarlet Knights",       "Rutgers",
  "SMU Mustangs",                  "SMU",
  "Sacramento State Hornets",      "Sacramento St.",
  "Seton Hall Pirates",            "Seton Hall",
  "St. John's Red Storm",          "St. John's",
  "Stanford Cardinal",             "Stanford",
  "Syracuse Orange",               "Syracuse",
  "TCU Horned Frogs",              "TCU",
  "Texas Tech Red Raiders",        "Texas Tech",
  "UCF Knights",                   "UCF",
  "UCLA Bruins",                   "UCLA",
  "UConn Huskies",                 "Connecticut",
  "USC Trojans",                   "USC",
  "Utah Utes",                     "Utah",
  "Villanova Wildcats",            "Villanova",
  "Virginia Cavaliers",            "Virginia",
  "Virginia Tech Hokies",          "Virginia Tech",
  "Wake Forest Demon Deacons",     "Wake Forest",
  "Washington Huskies",            "Washington",
  "Weber State Wildcats",          "Weber St.",
  "West Virginia Mountaineers",    "West Virginia",
  "Wisconsin Badgers",             "Wisconsin",
  "Xavier Musketeers",             "Xavier"
)

# -----------------------------
# 3. Validate mapping coverage
# -----------------------------
all_schedule_teams <- sort(unique(c(schedule$home_team, schedule$away_team)))

missing_from_map <- setdiff(all_schedule_teams, team_map$schedule_team)
if (length(missing_from_map) > 0) {
  cat("Schedule teams missing from team_map:\n")
  print(missing_from_map)
}

missing_in_torvik <- setdiff(unique(team_map$torvik_team), unique(torvik$team))
if (length(missing_in_torvik) > 0) {
  cat("Mapped Torvik team names not found in Torvik file:\n")
  print(missing_in_torvik)
}

# -----------------------------
# 4. Add mapped team names
# -----------------------------
schedule_mapped <- schedule %>%
  left_join(team_map %>% rename(home_team = schedule_team,
                                home_torvik_team = torvik_team),
            by = "home_team") %>%
  left_join(team_map %>% rename(away_team = schedule_team,
                                away_torvik_team = torvik_team),
            by = "away_team")

# hard stop if mapping failed
if (any(is.na(schedule_mapped$home_torvik_team)) || any(is.na(schedule_mapped$away_torvik_team))) {
  stop("Some schedule teams were not mapped to Torvik names.")
}

# -----------------------------
# 5. Keep Torvik columns you want
# -----------------------------
torvik_keep <- torvik %>%
  select(team, date, rank, record, barthag, adj_o, adj_d, adj_tempo, wab)

# -----------------------------
# 6. Join latest PRIOR Torvik row
#    date < game_date
# -----------------------------

# Home join
home_joined <- schedule_mapped %>%
  left_join(
    torvik_keep,
    by = c("home_torvik_team" = "team")
  ) %>%
  filter(date < game_date) %>%
  group_by(game_date, season, conference, home_team, away_team, home_score, away_score, neutral_site_flag,
           home_torvik_team, away_torvik_team) %>%
  slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(
    home_torvik_date  = date,
    home_rank         = rank,
    home_record       = record,
    home_barthag      = barthag,
    home_adj_o        = adj_o,
    home_adj_d        = adj_d,
    home_adj_tempo    = adj_tempo,
    home_wab          = wab
  )

# Away join
master <- home_joined %>%
  left_join(
    torvik_keep,
    by = c("away_torvik_team" = "team")
  ) %>%
  filter(date < game_date) %>%
  group_by(game_date, season, conference, home_team, away_team, home_score, away_score, neutral_site_flag,
           home_torvik_team, away_torvik_team,
           home_torvik_date, home_rank, home_record, home_barthag, home_adj_o, home_adj_d, home_adj_tempo, home_wab) %>%
  slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(
    away_torvik_date  = date,
    away_rank         = rank,
    away_record       = record,
    away_barthag      = barthag,
    away_adj_o        = adj_o,
    away_adj_d        = adj_d,
    away_adj_tempo    = adj_tempo,
    away_wab          = wab
  ) %>%
  arrange(game_date, conference, home_team)

# -----------------------------
# 7. Validation checks
# -----------------------------
cat("Rows in schedule: ", nrow(schedule), "\n")
cat("Rows in master:   ", nrow(master), "\n")
cat("Missing home_adj_o: ", sum(is.na(master$home_adj_o)), "\n")
cat("Missing away_adj_o: ", sum(is.na(master$away_adj_o)), "\n")

# -----------------------------
# 8. Save outputs
# -----------------------------
write_csv(team_map, "team_name_mapping.csv")
write_csv(master, "master_conference_games_with_prior_torvik.csv")