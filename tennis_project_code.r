library(tidyverse)
library(tidymodels)
library(e1071)
library(corrplot)
library(slider)
library(elo)
library(yardstick)
library(corrplot)
library(broom)
library(vip)
library(ranger)


# Read data(2000-2018 for traning/2019 for testing
tennis <- bind_rows(
  read_csv("atp_matches_2000.csv"),
  read_csv("atp_matches_2001.csv"),
  read_csv("atp_matches_2002.csv"),
  read_csv("atp_matches_2003.csv"),
  read_csv("atp_matches_2004.csv"),
  read_csv("atp_matches_2005.csv"),
  read_csv("atp_matches_2006.csv"),
  read_csv("atp_matches_2007.csv"),
  read_csv("atp_matches_2008.csv"),
  read_csv("atp_matches_2009.csv"),
  read_csv("atp_matches_2010.csv"),
  read_csv("atp_matches_2011.csv"),
  read_csv("atp_matches_2012.csv"),
  read_csv("atp_matches_2013.csv"),
  read_csv("atp_matches_2014.csv"),
  read_csv("atp_matches_2015.csv"),
  read_csv("atp_matches_2016.csv"),
  read_csv("atp_matches_2017.csv"),
  read_csv("atp_matches_2018.csv"),
  read_csv("atp_matches_2019.csv"))

# Overview
skimr::skim_without_charts(tennis)

# Quick checks
# ————————————————————————Distribution checking————————————————————————————————
# --Check the distribution of court surfaces (Hard, Clay, Grass, Carpet)
# count(tennis, surface)

# --Check the distribution of tournament levels
# --"G" = Grand Slam, "M" = Masters 1000, "A" = ATP 250/500, "F" = Finals, "D" = Davis Cup
# count(tennis, tourney_level)

# --Check the distribution of tournament rounds
# --"ER", "R128", "R64", "R32", "R16", "QF", "SF", "RR", "F", "BR"
# table(tennis$round)

# --Check how players entered the tournaments (wildcard, qualifier, lucky loser, etc.)
# table(tennis$winner_entry)
# table(tennis$loser_entry)

# --Check distribution of players' dominant hand
# --"R" = right-handed, "L" = left-handed, "A"/"U" = unknown/ambidextrous
# table(tennis$winner_hand)
# table(tennis$loser_hand)

# ————————————————————————Outlier checking————————————————————————————————
# --Inspect the match score strings 
# table(tennis$score)

# --Identify unusual score entries that contain letters (RET, W/O, ABD, etc.)
# score_letter_patterns <- tennis %>%
#   filter(str_detect(score, "[A-Za-z]")) %>%           # keep rows with letters in score
#   mutate(letters_pattern = str_trim(str_extract(score, "[A-Za-z/\\s]+$"))) %>%
#   distinct(letters_pattern) %>%                       # keep unique combinations
#   arrange(letters_pattern)
# score_letter_patterns

# --Review the distribution of players’ heights to spot impossible or missing values
# summary(tennis$winner_ht)

# --Cross-check player IDs and names to detect duplicates or inconsistencies
# --Combine winner and loser records into one unified player table
# player_map <- bind_rows(
#   tennis %>% select(player_id = winner_id, player_name = winner_name),
#   tennis %>% select(player_id = loser_id,  player_name = loser_name)) %>%
#   distinct()   # remove duplicate pairs
# --Count how many distinct names appear per player_id
# name_check <- player_map %>% count(player_id) %>% filter(n > 1)
# player_map %>% semi_join(name_check, by = "player_id") %>% arrange(player_id)
# name_to_multi_id <- player_map %>%
#   count(player_name) %>% filter(n > 1)
# name_to_multi_id

# ————————————————————————Data clean————————————————————————————————
# Data clean
tennis_clean <- tennis %>%
  mutate(tourney_date = ymd(as.character(tourney_date)),
         winner_id   = as.character(winner_id),
         loser_id    = as.character(loser_id), 
         winner_name = as.character(winner_name),
         loser_name  = as.character(loser_name),
         surface = factor(surface, levels = c("Hard", "Clay", "Carpet", "Grass")),
         draw_size    = as.factor(draw_size),
         match_num    = as.integer(match_num),
         tourney_level = factor(tourney_level, levels = c("D", "A", "M", "F", "G"),ordered = TRUE),  # ascending orederd: # "D": Davis Cup; "A": ATP Tour 250 / 500; "M": Masters 1000; "F": Tour Finals; G": Grand Slam
         winner_seed = as.integer(winner_seed),
         loser_seed  = as.integer(loser_seed),
         winner_entry = factor(winner_entry, levels = c("ALT", "LL", "PR", "Q", "SE", "WC")),
         loser_entry = factor(na_if(loser_entry, "S"),levels = c("ALT", "LL", "PR", "Q", "SE", "WC")),
         winner_hand = ifelse(winner_hand == "A", "U", winner_hand),
         loser_hand  = ifelse(loser_hand == "A", "U", loser_hand),
         loser_hand  = replace_na(loser_hand, "U"),
         winner_hand = factor(winner_hand, levels = c("R", "L", "U")),
         loser_hand  = factor(loser_hand, levels = c("R", "L", "U")),
         winner_ht = as.integer(ifelse(winner_ht == 3, NA, winner_ht)),
         loser_ht  = as.integer(loser_ht),
         winner_ioc = factor(winner_ioc),
         loser_ioc = factor(loser_ioc),
         winner_age = as.numeric(winner_age),
         loser_age  = as.numeric(loser_age),
         score = as.character(score),
         best_of = as.factor(best_of),
         round = factor(round,
                        levels = c("ER", "R128", "R64", "R32", "R16", "QF", "SF", "RR", "F", "BR"),
                        ordered = TRUE),
         minutes = as.integer(minutes),
         winner_rank  = as.integer(winner_rank),
         loser_rank   = as.integer(loser_rank),
         winner_rank_points = as.numeric(winner_rank_points),
         loser_rank_points  = as.numeric(loser_rank_points)) %>%
  mutate(across(matches("^(w|l)_(ace|df|svpt|1stIn|1stWon|2ndWon|SvGms|bpSaved|bpFaced)$"),
           ~ as.integer(.x))) %>%
  mutate(
    winner_name = ifelse(
      winner_id == 104273 & winner_name == "Edouard Roger-Vasselin",
      "Edouard Roger Vasselin", winner_name),
    loser_name = ifelse(
      loser_id == 104273 & loser_name == "Edouard Roger-Vasselin",
      "Edouard Roger Vasselin", loser_name)) %>%
  mutate(result_type = case_when(
    # the match ended due to a player retirement
    #    (one player withdrew after starting the match, often due to injury)
    str_detect(score, "RET") ~ "retired",
    # one player was defaulted
    #    (disqualified for rule violations, misconduct, or penalties)
    str_detect(score, "DEF") | str_detect(score, "Default") | str_detect(score, "Def.") ~ "defaulted",
    # the match started but was not completed — usually due to weather or external interruption
    str_detect(score, "Played and abandoned") |
      str_detect(score, "ABD") |
      str_detect(score, "Played and unfinished") ~ "abandoned",
    # one player withdrew before the match started,
    #    and the opponent automatically advanced without play
    str_detect(score, "W/O") | str_detect(score, "Walkover") ~ "walkover",
    # All other cases are treated as normal completed matches
    TRUE ~ "completed"),
    result_type = factor(result_type,
                         levels = c("completed","retired","defaulted","abandoned","walkover"))) %>%
    # Keep only completed matches for modeling
    filter(result_type == "completed") %>%
    select(-result_type) %>%
    # Davis Cup is different from ATP Ranking. These events follow different ranking and scoring structures,
    # which could distort model calibration and generalization.
    filter(tourney_level != "D") 
dim(tennis)
dim(tennis_clean)

# Detect if score strings match the pattern
# - One or more normal sets written as: digit-digit  (e.g., "6-4")
# - tiebreak inside parentheses: (digit) or (digits), e.g. "7-6(5)"
# - Sets separated by a single space
# - A final match tie-break written in square brackets: [10-7]
pattern <- "^(?:\\d+-\\d+(?:\\(\\d+\\))?)(?: \\d+-\\d+(?:\\(\\d+\\))?)*(?: \\[\\d+-\\d+\\])?$"
# Check which score strings match the pattern
valid_flag <- str_detect(tennis_clean$score, pattern)
# Extract score entries that do NOT match the valid pattern
bad_scores <- tennis_clean$score[!(valid_flag | is.na(tennis_clean$score))]
unique(bad_scores)

# Parse score and construct new feature
# sets_won: Number of sets a player won in the match. This reflects performance at the set level and indicates how competitive the match was overall.
# tiebreaks_won: Number of tiebreaks a player won, including both standard tiebreaks (e.g., 7–6) and super tiebreaks. This measures performance in high-pressure situations.
# total_games: Total number of games a player won across all sets. This captures overall game-level performance and provides a continuous measure of match dominance or competitiveness.
# total_tb: Total number of tiebreaks played in the match. A higher value generally indicates a closer, more competitive match.
# game_diff_per_set: Average net game margin per set, calculated as the difference in total games won divided by the number of sets. It measures set-level dominance.
parse_score_to_wl <- function(score) {
  # extract sets: normal, tiebreak sets, super tiebreak
  sets <- unlist(str_extract_all(score, "\\d+-\\d+(?:\\(\\d+\\))?|\\[\\d+-\\d+\\]"))
  # initialize
  w_set_wins <- 0
  l_set_wins <- 0
  w_tb <- 0
  l_tb <- 0
  w_total_games <- 0
  l_total_games <- 0
  total_tb <- 0
  
  for (s in sets) {
    # Super tiebreak e.g. [10-7]
    if (str_detect(s, "^\\[")) {
      nums <- as.numeric(str_extract_all(s, "\\d+")[[1]])
      w <- nums[1]; l <- nums[2]
      # treat as a set
      if (w > l) {
        w_set_wins <- w_set_wins + 1
        w_tb <- w_tb + 1
      } else {
        l_set_wins <- l_set_wins + 1
        l_tb <- l_tb + 1
      }
      total_tb <- total_tb + 1  # count as a tiebreak
      # add to total games
      w_total_games <- w_total_games + w
      l_total_games <- l_total_games + l
      next
    }
    
    # Tiebreak set e.g. 7-6(5)
    if (str_detect(s, "\\(")) {
      nums <- as.numeric(str_extract_all(s, "\\d+")[[1]])
      # nums = c(setWinnerGames, setLoserGames, tbPoints?)
      w <- nums[1]; l <- nums[2]
      total_tb <- total_tb + 1
      if (w > l) {
        w_set_wins <- w_set_wins + 1
        w_tb <- w_tb + 1
      } else {
        l_set_wins <- l_set_wins + 1
        l_tb <- l_tb + 1
      }
      w_total_games <- w_total_games + w
      l_total_games <- l_total_games + l
      next
    }
  
    # Normal set e.g. 6-4
    nums <- as.numeric(str_extract_all(s, "\\d+")[[1]])
    w <- nums[1]; l <- nums[2]
    if (w > l) w_set_wins <- w_set_wins + 1 else l_set_wins <- l_set_wins + 1
    w_total_games <- w_total_games + w
    l_total_games <- l_total_games + l
  }
  num_sets <- length(sets)
  tibble(
    w_sets_won = w_set_wins,
    l_sets_won = l_set_wins,
    w_tiebreaks_won = w_tb,
    l_tiebreaks_won = l_tb,
    w_games = w_total_games,
    l_games = l_total_games,
    total_tb = total_tb,
    w_game_diff_per_set = (w_total_games - l_total_games) / num_sets,
    l_game_diff_per_set = (l_total_games - w_total_games) / num_sets
  )
}

tennis_clean <- tennis_clean %>%
  mutate(score_features = map(score, parse_score_to_wl)) %>%
  unnest(score_features)
tennis_clean <- tennis_clean %>% select(-score)



# ————————————————————————————————EDA——————————————————————————————————————————
# (1) Matches background
# ———————————————————————— Surface ———————————————————————— 
tennis_clean %>%
  ggplot(aes(x = surface, fill = surface)) +
  geom_bar() +
  labs(title = "Surface Distribution", x = "Surface", y = "Count")
# Hard > Clay > Grass > Carpet

# The number of carpet-court matches sharply disapeared after 2009 as the ATP removed this surface,
# Hard, clay and grass courts maintained relatively stable proportions.
tennis_clean %>%
  mutate(year = year(tourney_date)) %>%
  group_by(year, surface) %>%
  summarise(match_count = n(), .groups = "drop") %>%
  ggplot(aes(x = year, y = match_count, color = surface)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Trend of Matches by Surface (2000–2019)",
    x = "Year",
    y = "Number of Matches",
    color = "Surface")

# ———————————————————————— tourney_level ———————————————————————— 
tennis_clean %>%
  ggplot(aes(x = tourney_level, fill = tourney_level)) +
  geom_bar() +
  labs(title = "Tourney Level Distribution", x = "Tourney Level", y = "Count")
# A > M > G > F

tennis_clean %>%
  mutate(year = year(tourney_date)) %>%
  group_by(year, tourney_level) %>%
  summarise(match_count = n(), .groups = "drop") %>%
  ggplot(aes(x = year, y = match_count, color = tourney_level)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Trend of Matches by Tourney Level (2000–2019)",
    x = "Year",
    y = "Number of Matches",
    color = "Tourney Level")
# The number of ATP 250/500 matches shows a gradual decline from about 1,900 in 2000 to around 1,500 in later years.
# Other Tourney level have remained remarkably stable over two decades. 
# It highlights a clear imbalance in modern tennis with ATP being the dominant competition.

ggplot(tennis_clean, aes(x = tourney_level, fill = surface)) +
  geom_bar(position = "dodge") +
  labs(title = "Match Counts by Level and Surface",
       x = "Tournament Level", y = "Count")
# A: Mostly played on hard courts, followed by clay, with few grass or carpet events.→ Lower-tier events are mainly concentrated on hard and clay surfaces.
# M: Dominated by hard and clay surfaces. Grass events are almost nonexistent.
# G: Mostly played on hard courts, followed by clay and grass(almost even).
# F: Very limited number of matches, mostly held on hard courts with few carpet courts.

# ———————————————————————— round ———————————————————————— 
# The distribution of tournament rounds is heavily skewed toward early stages (R32, R16, R64),
# , reflecting the large number of players who exit in the initial rounds.
tennis_clean %>%
  ggplot(aes(x = round, fill = round)) +
  geom_bar() +
  labs(title = "Round Distribution", x = "Round", y = "Count") 

# ———————————————————————— draw size ———————————————————————— 
ggplot(tennis_clean, aes(x = draw_size)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Draw Sizes (2000–2019)",
    x = "Draw Size",
    y = "Number of Matches") 
# Most tournaments have a 32-player draw — this includes the majority of all mathes.
# Another clear peak at 128
# 28, 48 ,56 , 64, 96 in the middel
# Small group around 8 and 16
# The distribution of draw sizes between 2000 and 2019 shows distinct peaks at 32 and 128

ggplot(tennis_clean, aes(x = draw_size, fill = tourney_level)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Draw Size Distribution by Tournament Level",
    x = "Draw Size",
    y = "Number of Matches",
    fill = "Level") 
# The tallest bar at draw size 32 shows that most ATP-level tournaments are small to mid-scale events with 28-64 player draws.
# A smaller number of 48–64 draw tournaments also exist within ATP-level tournaments.
# The Masters Concentrated between 48 and 128 draws, with a peak on 64 drws.
# Grand Slams represented by a single tall yellow bar at 128 draw.
# The small green bar at 8 and 16 draw corresponds to the ATP Finals, which feature only the top eight players of the season.
# The draw-size distribution across tournament levels shows a clear hierarchy: ATP 250/500 events dominate with 32-player fields, Masters 1000 tournaments typically have 56–64 players, and Grand Slams are fixed at 128. The ATP Finals stand apart with eight players.


ggplot(tennis_clean, aes(x = draw_size, fill = round)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Draw Size Distribution by Round",
    x = "Draw Size",
    y = "Number of Matches",
    fill = "Round") 
# The distribution of draw sizes by round reveals the typical structure of professional tennis tournaments. Most matches occur in early rounds (R32, R16, R64) within 32- and 128-player draws. 

tennis_clean %>%
  group_by(year = year(tourney_date)) %>%
  summarise(mean_draw = mean(draw_size)) %>%
  ggplot(aes(year, mean_draw)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Draw Size per Year (2000–2019)",
    x = "Year",
    y = "Average Draw Size"
  ) +
  theme_minimal(base_size = 13)
# From 2000 to around 2015, the average draw size remained relatively stable, fluctuating between 57 and 60 players.
# After 2016, there was a sharp increase to above 64, indicating a structural change in the tournament dataset.

# ———————————————————————— Best of ———————————————————————— 
ggplot(tennis_clean, aes(x = best_of)) +
  geom_bar() +
  labs(
    title = "Distribution of Match Formats (Best-of-3 vs Best-of-5)",
    x = "Match Format (Best of Sets)",
    y = "Number of Matches") 
# Most matches are played in the Best-of-3 format, while only a small proportion are Best-of-5.
# Best-of-3 dominates regular ATP events, while Best-of-5 is exclusive to Grand Slams.

ggplot(tennis_clean, aes(x = best_of, y = minutes, fill = best_of)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#4E79A7", "#F28E2B")) +
  labs(
    title = "Match Duration by Format (Best-of-3 vs Best-of-5)",
    x = "Match Format (Best of Sets)",
    y = "Match Duration (minutes)") 
# Best-of-3 matches have a median around 90 minutes, with most matches lasting 60–120 minutes.
# Best-of-5 matches are significantly longer, with a median around 160–180 minutes, and a wider spread due to more possible sets.
# A few extreme outliers (above 300 minutes, some reaching over 1000 minutes)

# (2) players characteristic
# ———————————————————————— Hand ———————————————————————— 
tennis_clean %>%
  select(winner_hand, loser_hand) %>%
  pivot_longer(cols = everything(), names_to = "role", values_to = "hand") %>%
  ggplot(aes(x = hand, fill = role)) +
  geom_bar(position = "dodge") +
  labs(title = "Winner vs Loser Hand Distribution",
       x = "Hand", y = "Count") 
# Right-handed players dominate the dataset, while left-handed players form a small minority. Unknown-handed very rare.
# Left-handed (L) players make up a small but notable minority.
# The counts for winners and losers are roughly similar, indicating no strong bias in win rate by handedness.

# ———————————————————————— Height ———————————————————————— 
tennis_clean %>%
  mutate(height_diff = winner_ht - loser_ht) %>%
  ggplot(aes(x = height_diff)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(title = "Height Difference (Winner - Loser)",
       x = "Height Difference (cm)",
       y = "Count") 
# Negative values indicate that the winner is shorter than the loser, while positive values indicate that the winner is taller.
# The distribution of height differences between winners and losers is approximately normal and centered around zero, 
# indicating that most matches occur between players of similar height. 
# The slight right skew suggests that taller players have a marginal advantage, possibly due to greater serve speed and reach. 
# Very large height differences (greater than ±25 cm) are rare, showing that extreme mismatches in height are uncommon.
# However, the symmetry of the distribution implies that height alone is not a decisive factor in determining match outcomes.

tennis %>%
  summarise(
    mean_winner_ht = mean(winner_ht, na.rm = TRUE),
    mean_loser_ht  = mean(loser_ht, na.rm = TRUE),
    diff_mean_ht   = mean(winner_ht - loser_ht, na.rm = TRUE)
  )
# The average height of winners is 186.0 cm, while the average height of losers is 185.4 cm, 
# yielding an average difference of only 0.56 cm.
# This difference is negligible, indicating that player height has minimal influence on match outcomes.

ht_diff <- tennis_clean$winner_ht - tennis_clean$loser_ht
mean_diff <- mean(ht_diff, na.rm = TRUE)
sd_diff   <- sd(ht_diff, na.rm = TRUE)
mean_diff
sd_diff
skewness(tennis_clean$winner_ht - tennis_clean$loser_ht, na.rm = TRUE)

# ———————————————————————— Age ———————————————————————— 
# Negative values mean the winner is younger; positive values mean the winner is older.
# The distribution of age differences between winners and losers is approximately normal and centered around zero, 
# suggesting that most matches occur between players of similar age. 
# The majority of values lie between -10 and +10, meaning age differences larger than 10 years are rare.
# There is a slight skew toward positive values, which could mean that older players sometimes defeat slightly younger opponents, possibly due to more experienced.
# However, the symmetric shape also implies that age alone does not strongly determine match outcomes in professional tennis.

tennis_clean %>%
  mutate(age_diff = winner_age - loser_age) %>%
  ggplot(aes(x = age_diff)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(title = "Age Difference (Winner - Loser)",
       x = "Age Difference", y = "Count")

mean_diff <- mean(tennis_clean$winner_age - tennis_clean$loser_age, na.rm = TRUE)
sd_diff <- sd(tennis_clean$winner_age - tennis_clean$loser_age, na.rm = TRUE)
mean_diff
sd_diff
skewness(tennis_clean$winner_age - tennis_clean$loser_age, na.rm = TRUE)

# On average, winners are 0.09 years younger than losers, with a standard deviation of 5.23 years.
# This difference is extremely small, meaning there is no meaningful age advantage in match outcomes.
# Combined with the skewness value (~0.03), the age difference distribution is nearly perfectly symmetric, confirming that age does not systematically favor either side in most ATP matches from 2000–2019.


# ———————————————————————— Rank and rand points distribution ———————————————————————— 
tennis_clean %>%
  mutate(rank_diff = loser_rank - winner_rank) %>%
  filter(is.finite(rank_diff)) %>% 
  ggplot(aes(x = rank_diff)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 80) +
  labs(title = "Rank Difference (Loser - Winner)",
       x = "Rank Difference",
       y = "Count")

rank_diff <- tennis_clean$loser_rank - tennis_clean$winner_rank
summary(rank_diff)
# Negative values mean the winner is lower-ranked players; positive values mean the winner is higher-ranked players
# The rank difference distribution is heavily right-skewed, confirming that higher-ranked players win the majority of matches. 
# Most matches have the loser ranked worse (higher number) than the winner (since positive values dominate).
# The median rank difference is +21, meaning in half of all matches, the winner’s ranking is at least 21 spots better than the loser’s.
# A small portion of matches have negative values (min = -1711) — these are upsets, where a lower-ranked player defeated a higher-ranked opponent.
# The large range (-1711 to +2125) shows a few extreme mismatches, but the histogram indicates most rank differences are concentrated near 0–100.

tennis_clean %>%
  mutate(points_diff = winner_rank_points - loser_rank_points) %>%
  filter(is.finite(points_diff)) %>% 
  ggplot(aes(x = points_diff)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 80) +
  labs(title = "Rank Points Difference (Winner - Loser)",
       x = "Points Difference",
       y = "Count")

rankpoints_diff <- tennis_clean$winner_rank_points - tennis_clean$loser_rank_points
summary(rankpoints_diff)
# Positive values means winners have more points (higher-ranked) and negative values means winners have fewer points (lower-ranked).
# Most values cluster tightly around 0–1000, showing that most matches occur between players with relatively close points.
# However, the tails are long — from –15,875 to +16,641, meaning there are some matches between players with huge point gaps (e.g., top players vs qualifiers).
# In half of all matches, the winner had at least 274 more ATP ranking points than the loser.
# The small number of negative values represents upset matches.
# Ranking points provide a more granular and realistic measurement of skill gap than raw ranking numbers.

# Convert match data into a long format (one row per player per match)
points_long <- tennis_clean %>%
  transmute(tourney_date = as.Date(tourney_date),
    # winner 
    player_id_w   = as.character(winner_id),
    player_name_w = winner_name,
    pts_w         = winner_rank_points,
    # loser 
    player_id_l   = as.character(loser_id),
    player_name_l = loser_name,
    pts_l         = loser_rank_points) %>%
  pivot_longer(
    cols = c(player_id_w, player_name_w, pts_w, player_id_l, player_name_l, pts_l),
    names_to = c(".value", "side"),
    names_pattern = "(player_id|player_name|pts)_(w|l)") %>%
  rename(player_id = player_id, player_name = player_name, pts = pts) %>%
  filter(!is.na(pts)) %>%               
  mutate(year = year(tourney_date))

# For each player, find the last match of each year (their year-end points)
year_end_points <- points_long %>%
  group_by(year, player_id) %>%
  slice_max(order_by = tourney_date, n = 1, with_ties = FALSE) %>%
  ungroup()

# Within each year, rank players by their final points and keep only Top 10
top10_year_end <- year_end_points %>%
  group_by(year) %>%
  mutate(rank_year_end = dense_rank(desc(pts))) %>%
  filter(rank_year_end <= 10) %>%
  ungroup()

# yearly Top 10 points
ggplot(top10_year_end,
       aes(x = year, y = pts, color = factor(rank_year_end))) +
  geom_line() +
  geom_point() +
  scale_color_viridis_d(name = "Year-end Rank (1–10)") +
  scale_x_continuous(breaks = sort(unique(top10_year_end$year))) +
  labs(
    title = "Year-end Top 10 ATP Ranking Points (Approx. from Match Data)",
    subtitle = "For each year, points = the player's last-seen points in that year; then take Top 10.",
    x = "Year",
    y = "Ranking Points"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Ranking-point values for the Top 10 increased steadily from 2000 to 2012, 
# indicating a period of rising performance consistency and dominance among elite players.
# During 2008-2016, rank 1 often exceeded 12,000 points—nearly triple the level in 2000—while rank 10 also reached historical highs (~4,000 points).
# The vertical distance between rank 1 and rank 10 expanded notably, showing how a few players separated themselves from the rest of the field.
# After 2016, both rank-1 and lower-rank curves declined slightly, suggesting a more balanced competitive field.


# (3) Skill performance
# ———————————————————————— Minutes ———————————————————————— 
tennis_clean %>%
  filter(is.finite(minutes)) %>% 
  ggplot(aes(y = minutes)) + 
  geom_boxplot() +
  labs(
    title = "Match Duration Distribution (Minutes)",
    y = "Match Duration (minutes)")
summary(tennis_clean$minutes)
# The median duration is 98 minutes, and the mean is slightly higher at 106.2 minutes, 
# suggesting a right-skewed distribution — a few unusually long matches pull the mean upward.
# The middle 50% of matches (from the 1st to 3rd quartile) range from 77 to 128 minutes, 
# indicating that most matches last around 1.5 to 2 hours.
# Extremely long matches are kept because they are real events, not data errors. 
# Such outliers reflect real variations in endurance in tennis and therefore provide meaningful information.

tennis_clean %>%
  filter(!is.na(minutes), minutes <= 300) %>%
  ggplot(aes(x = round, y = minutes)) +
  geom_boxplot() +
  labs(title = "Match Duration by Round",
       x = "Tournament Round", y = "Match Duration (minutes)")
# Clay-court matches tend to have slightly longer durations on average, reflecting slower playing conditions and longer rallies.
# In contrast, matches on Carpet and Hard courts are generally faster, while Grass courts show wider variation.

tennis_clean %>%
  filter(!is.na(minutes), minutes <= 300) %>%
  ggplot(aes(x = surface, y = minutes, fill = surface)) +
  geom_boxplot() +
  labs(
    title = "Match Duration by Surface (≤ 300 minutes)",
    x = "Court Surface",
    y = "Match Duration (minutes)") +
  scale_fill_manual(values = c("Hard" = "#E69F9F", "Clay" = "#A3C585",
                               "Carpet" = "#56B4E9", "Grass" = "#CBA3E0")) 

# Early rounds (e.g., R128 and R64) tend to have higher median durations and more variation, possibly due to a wider range of player abilities.
# In later rounds (QF, SF, F), match durations remain relatively stable, with finals (F) and bronze rounds (BR) showing slightly longer average times, reflecting the increased competitiveness of top players.

# ——————————————————————— Score related performance ———————————————————————— 
score_vars <- tennis_clean %>%
  select(
    w_sets_won, l_sets_won,
    w_tiebreaks_won, l_tiebreaks_won,
    w_games, l_games,
    total_tb,
    w_game_diff_per_set, l_game_diff_per_set
  )
summary(score_vars)

score_long <- tennis_clean %>%
  select(tourney_id, tourney_name, total_tb, starts_with("w_"), starts_with("l_")) %>%
  # pivot score features
  pivot_longer(
    cols = matches("^(w|l)_.+$"),
    names_to = c("role", ".value"),
    names_pattern = "^(w|l)_(.*)$"
  ) %>%
  mutate(role = recode(role, "w" = "Winner", "l" = "Loser"),
         is_win = if_else(role == "Winner", 1L, 0L))

# Winner vs Loser game_diff_per_set
ggplot(score_long, aes(x = role, y = game_diff_per_set, fill = role)) +
  geom_boxplot() +
  labs(title = "Game Diff Per Set by Winner vs Loser", x = NULL, y = "Game diff per set")
# The descriptive statistics show that winners, on average, win about 2 more games per set than their opponents (median = 2, mean ≈ 2.13).
# This indicates that most matches are not blowouts, and winners typically secure each set by a moderate margin of 1–3 games.

# Tiebreak frequency
tennis_clean %>%
  count(total_tb) %>%
  mutate(prop = n / sum(n), percent = round(prop * 100, 1)) %>%
  ggplot(aes(x = factor(total_tb), y = prop)) +
  geom_col() +
  geom_text(aes(label = paste0(percent, "%")), 
            vjust = -0.5, size = 5) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Distribution of Tiebreak Counts in Matches",
       x = "Number of Tiebreaks in Match",
       y = "Proportion")
# 62.9% of matches have no tiebreaks. Most matches do not reach 6–6 in any set, indicating clear differences in performance or momentum.
# 30.0% of matches feature exactly 1 tiebreak. This is the most common sign of a “tight set.” At least one set is highly competitive.
# Only 6.36% of matches have 2 tiebreaks. Matches with two tiebreaks are usually close from start to finish, often involving players with similar skill levels.
# Just 0.665% of matches have 3 tiebreaks. Represents extremely competitive contests.
# 0.024% of matches have 4 tiebreaks. These are extremely rare and represent some of the most dramatic, high-pressure matches in the record.

# Total Games Played
ggplot(score_long, aes(x = games, fill = role)) +
  geom_density(alpha = 0.3)
# Losers have a peak around 6–10 games, consistent with losing 2–0 or 3–0.
# Winners have a peak around 12–20 games, reflecting that they must win more games to close the match.

ggplot(score_long, aes(x = factor(total_tb), y = game_diff_per_set, fill = factor(total_tb))) +
  geom_boxplot() +
  labs(title = "Game Diff Per Set vs Total Tiebreaks",
       x = "Number of tiebreaks in match",
       y = "Game diff per set")
# More tiebreaks indicate closer and more competitive matches, reflected in smaller per-set game differences.

tennis_clean %>%
  mutate(close_match = abs(w_game_diff_per_set) < 1) %>%
  count(close_match) %>%
  mutate(prop = n / sum(n))
# identifies extremely close matches, where the average game difference per set is less than one game, indicating nearly even performance between players across all sets
# Only about 16% of all matches are close, highlighting that competitive, high-pressure matches are relatively rare but crucial for understanding model failures.

# ——————————————————————— Serve performance ———————————————————————— 
# Aces per Match (Winner vs Loser)
tennis_clean %>%
  select(w_ace, l_ace) %>%
  filter(!is.na(w_ace)) %>%
  filter(!is.na(l_ace)) %>%
  pivot_longer(everything(), names_to = "role", values_to = "aces") %>%
  mutate(role = ifelse(role == "w_ace", "Winner", "Loser")) %>%
  ggplot(aes(x = role, y = aces, fill = role)) +
  geom_boxplot() +
  labs(title = "Aces per Match (Winner vs Loser)",
       x = "", y = "Number of Aces") 
# Winners generally record more aces, with a slightly higher median and a wider spread, 
# suggesting that stronger serving performance often correlates with match success.
# However, both groups show long tails, 
# indicating that extreme ace counts occasionally occur for both winners and losers.

# Average Aces per Match Over Time
tennis_clean %>%
  mutate(year = year(tourney_date)) %>%
  group_by(year) %>%
  summarise(mean_ace = mean(w_ace + l_ace, na.rm = TRUE)) %>%
  ggplot(aes(x = year, y = mean_ace)) +
  geom_line() + geom_point() +
  labs(title = "Average Aces per Match Over Time",
       x = "Year", y = "Average Aces")
# Overall, there is a clear upward trend, indicating that players are hitting more aces over time.
# This may reflect advancements in serve techniques, racket technology, and player fitness, 
# leading to more dominant serving performances in modern tennis.

# Service Points
tennis_clean %>%
  select(w_svpt, l_svpt) %>%
  filter(!is.na(w_svpt)) %>%
  filter(!is.na(l_svpt)) %>%
  pivot_longer(everything(), names_to = "role", values_to = "svpt") %>%
  mutate(role = ifelse(role == "w_svpt", "Winner", "Loser")) %>%
  ggplot(aes(x = role, y = svpt, fill = role)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Service Points per Match (Winner vs Loser)",
       x = "", y = "Number of Service Points") +
  theme_minimal(base_size = 14)
# Both distributions are quite similar, suggesting that players, regardless of outcome, tend to have comparable serving opportunities.
# However, winners show a slightly lower median, implying that they often finish their service games more efficiently
# for example, winning points more quickly and facing fewer extended rallies or deuces.

# Double Faults per Match (Winner vs Loser)
tennis_clean %>%
  select(w_df, l_df) %>%
  filter(!is.na(w_df)) %>%
  filter(!is.na(l_df)) %>%
  pivot_longer(everything(), names_to = "role", values_to = "df") %>%
  mutate(role = ifelse(role == "w_df", "Winner", "Loser")) %>%
  ggplot(aes(x = role, y = df, fill = role)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Double Faults per Match (Winner vs Loser)",
       x = "", y = "Number of Double Faults")
# Losers generally have a slightly higher median and a wider spread, 
# suggesting that greater serving inconsistency is associated with losing outcomes.

# 1st Serve Win Rate (Winner vs Loser)
tennis_clean %>%
  filter(!is.na(w_1stWon)) %>%
  filter(!is.na(l_1stWon)) %>%
  filter(!is.na(w_1stIn)) %>%
  filter(!is.na(l_1stIn)) %>%
  mutate(w_1stWinRate = w_1stWon / w_1stIn,
         l_1stWinRate = l_1stWon / l_1stIn) %>%
  select(w_1stWinRate, l_1stWinRate) %>%
  pivot_longer(everything(), names_to = "role", values_to = "win_rate") %>%
  mutate(role = ifelse(role == "w_1stWinRate", "Winner", "Loser")) %>%
  ggplot(aes(x = role, y = win_rate, fill = role)) +
  geom_boxplot() +
  labs(title = "1st Serve Win Rate (Winner vs Loser)",
       x = "", y = "Win Rate")
# Winners show a notably higher median and tighter distribution, indicating more consistent and effective first serves.
# Losers exhibit a wider spread and lower median, suggesting that inefficient first serves often contribute to match losses.
# This pattern highlights the crucial role of the first serve in controlling points and maintaining dominance during rallies.

# Second Serve Win Rate (Winner vs Loser) 
tennis_clean %>%
  filter(!is.na(w_2ndWon)) %>%
  filter(!is.na(l_2ndWon)) %>%
  filter(!is.na(w_1stIn)) %>%
  filter(!is.na(l_1stIn)) %>%
  filter(!is.na(w_svpt)) %>%
  filter(!is.na(l_svpt)) %>%
  mutate(
    w_2ndWinRate = w_2ndWon / (w_svpt - w_1stIn),
    l_2ndWinRate = l_2ndWon / (l_svpt - l_1stIn)
  ) %>%
  select(w_2ndWinRate, l_2ndWinRate) %>%
  pivot_longer(everything(), names_to = "role", values_to = "win_rate") %>%
  mutate(role = ifelse(role == "w_2ndWinRate", "Winner", "Loser")) %>%
  ggplot(aes(x = role, y = win_rate, fill = role)) +
  geom_boxplot() +
  labs(title = "2nd Serve Win Rate (Winner vs Loser)",
       x = "", y = "Win Rate") 
# Winners clearly maintain a higher median and tighter distribution, suggesting stronger performance under pressure when the first serve fails.
# Losers show lower averages and greater variability, indicating less consistency and vulnerability on second serves.
# This highlights how reliable second serves can be a decisive factor in maintaining control throughout a match.

# Break Point Save Ratio (Winner vs Loser) 
tennis_clean %>%
  filter(!is.na(w_bpSaved)) %>%
  filter(!is.na(l_bpSaved)) %>%
  filter(!is.na(w_bpFaced)) %>%
  filter(!is.na(l_bpFaced)) %>%
  mutate(
    w_bpSaveRatio = w_bpSaved / w_bpFaced,
    l_bpSaveRatio = l_bpSaved / l_bpFaced
  ) %>%
  select(w_bpSaveRatio, l_bpSaveRatio) %>%
  pivot_longer(everything(), names_to = "role", values_to = "ratio") %>%
  mutate(role = ifelse(role == "w_bpSaveRatio", "Winner", "Loser")) %>%
  ggplot(aes(x = role, y = ratio, fill = role)) +
  geom_boxplot() +
  labs(title = "Break Point Save Ratio (Winner vs Loser)",
       x = "", y = "Saved / Faced Ratio") 
# Winners demonstrate higher break point save ratios, reflecting stronger mental resilience and performance under pressure.


# Across all serve-related metrics, winners consistently outperform losers.
# These patterns collectively confirm that serve effectiveness and stability are critical determinants of match success.

# 
player_freq <- tennis_clean %>%
  transmute(player_id = winner_id) %>%
  bind_rows(
    tennis_clean %>% transmute(player_id = loser_id)
  ) %>%
  count(player_id, name = "matches_played") %>%
  arrange(desc(matches_played))
summary(player_freq$matches_played)

ggplot(player_freq, aes(x = matches_played)) +
  geom_histogram(binwidth = 0.15, fill = "#59A14F", color = "white") +
  scale_x_log10() +
  labs(title = "Player Match Frequency (Log Scale)",
       x = "Matches played (log scale)",
       y = "Count of players")
# Most players have played very few matches, resulting in a highly long-tailed distribution.
# The median player has only 8 matches, and many players have only 1–3 matches in the entire dataset.
# Using a time-based rolling window (e.g., last 30 days) would produce mostly empty or meaningless rolling values.
# Given the extremely uneven match frequency and the long-tailed distribution, rolling windows should be constructed by match count rather than by days.



player_match <- tennis_clean %>%
  transmute(
    player_id = winner_id,
    year = year(tourney_date)
  ) %>%
  bind_rows(
    tennis_clean %>%
      transmute(
        player_id = loser_id,
        year = year(tourney_date)))

player_yearly_counts <- player_match %>%
  group_by(player_id, year) %>%
  summarise(matches = n(), .groups = "drop")

player_avg_per_year <- player_yearly_counts %>%
  group_by(player_id) %>%
  summarise(
    total_matches = sum(matches),
    active_years    = n_distinct(year), 
    career_start = min(year),
    career_end   = max(year),
    career_span_years = career_end - career_start + 1,
    avg_matches_per_year = total_matches / career_span_years,
    avg_matches_per_active_year = total_matches / active_years)
summary(player_avg_per_year)
# The distribution of each player’s active-year match intensity (avg_matches_per_active_year) shows a clear long-tail pattern.
# a very small number of players participate heavily each year, while the majority appear only occasionally on the tour.
player_avg_per_year %>%
  mutate(intensity_group = cut(
    avg_matches_per_active_year,
    breaks = c(0, 5, 15, 30, 60, Inf),
    labels = c("≤5", "5–15", "15–30", "30–60", ">60"),
    right = TRUE)) %>%
  count(intensity_group)%>%
  mutate(proportion = n / sum(n))


names(tennis_clean)

# Feature selection
tennis_feature_pre <- tennis_clean %>%
  select(
    # Pre-match
    # Tournament-level information 
    tourney_id,          # Tournament unique identifier
    surface,             # Court surface (Hard, Clay, Grass)
    draw_size,           # Tournament draw size (number of players)
    tourney_level,       # Tournament level (e.g., G, M, A, C)
    tourney_date,        # Match date
    best_of,             # Best of 3 or 5 sets
    round,               # Round of the tournament (R32, QF, SF, F)
    match_num,
    # Player characteristics 
    winner_id, loser_id,     # Player IDs for winner and loser
    winner_name, loser_name,
    winner_ht, loser_ht,     # Height in cm
    winner_age, loser_age,   # Age at the time of the match
    # Seed and entry information 
    winner_seed, loser_seed, # Seed number (can be NA if unseeded)
    # Player ranking information 
    winner_rank, loser_rank,                   # ATP ranking
    winner_rank_points, loser_rank_points,    # ATP ranking points
    
    # In-match performance
    # Winner serve statistics
    w_ace, w_df, w_svpt, w_1stIn, w_1stWon, w_2ndWon, w_SvGms, w_bpSaved, w_bpFaced,
    # Loser serve statistics
    l_ace, l_df, l_svpt, l_1stIn, l_1stWon, l_2ndWon, l_SvGms, l_bpSaved, l_bpFaced,
    
    # Match result
    w_sets_won,l_sets_won,w_tiebreaks_won,l_tiebreaks_won,w_games,l_games,w_game_diff_per_set,l_game_diff_per_set,
    total_tb, minutes) 


# construct player short term status
player_timeline <- tennis_feature_pre %>%
  transmute(tourney_date, tourney_id, match_num, player_id = winner_id) %>%
  bind_rows(
    tennis_feature_pre %>%
      transmute(tourney_date, tourney_id, match_num, player_id = loser_id)) %>%
  arrange(player_id, tourney_date, match_num) %>%
  group_by(player_id) %>%
  mutate(last_match_date = lag(tourney_date),
         days_since_prev = as.numeric(tourney_date - last_match_date),
         matches_last_30d = slide_int(
           .x = tourney_date,
           .f = ~ {
             current_date <- .x[length(.x)]     
             sum(current_date - .x > 0 & current_date - .x <= 30)
             },
           .before = Inf,
           .complete = FALSE)
         ) %>% ungroup()

tennis_feature_pre <- tennis_feature_pre %>%
  left_join(player_timeline %>%
      select(tourney_id, tourney_date, match_num, player_id, days_since_prev, matches_last_30d) %>%
      rename(winner_id = player_id,
             w_days_since_prev  = days_since_prev,
             w_matches_last_30d = matches_last_30d),
      by = c("tourney_id", "tourney_date", "match_num", "winner_id")) %>%
  left_join(player_timeline %>%
      select(tourney_id, tourney_date, match_num, player_id, days_since_prev, matches_last_30d) %>%
      rename(loser_id = player_id,
             l_days_since_prev = days_since_prev,
             l_matches_last_30d = matches_last_30d),
      by = c("tourney_id", "tourney_date", "match_num", "loser_id"))

tennis_feature_pre <- tennis_feature_pre %>%
  mutate(w_days_since_prev  = replace_na(w_days_since_prev, 0),
         l_days_since_prev  = replace_na(l_days_since_prev, 0))

skimr::skim_without_charts(tennis_feature_pre)

# Rolling previous 10 times
train_pre <- tennis_feature_pre %>% filter(year(tourney_date) >= 2000, year(tourney_date) <= 2018)
test_pre  <- tennis_feature_pre %>% filter(year(tourney_date) == 2019)
skimr::skim_without_charts(train_pre)
dim(tennis_feature_pre)
# Imputate NA
# winner + loser
long_train <- bind_rows(
  train_pre %>%
    transmute(
      ace = w_ace,
      df = w_df,
      svpt = w_svpt,
      `1stIn` = w_1stIn,
      `1stWon` = w_1stWon,
      `2ndWon` = w_2ndWon,
      SvGms = w_SvGms,
      bpSaved = w_bpSaved,
      bpFaced = w_bpFaced,
      ht = winner_ht),
  train_pre %>%
    transmute(
      ace = l_ace,
      df = l_df,
      svpt = l_svpt,
      `1stIn` = l_1stIn,
      `1stWon` = l_1stWon,
      `2ndWon` = l_2ndWon,
      SvGms = l_SvGms,
      bpSaved = l_bpSaved,
      bpFaced = l_bpFaced,
      ht = loser_ht))

stats_means <- long_train %>% summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

ace_mean      <- stats_means$ace
df_mean       <- stats_means$df
svpt_mean     <- stats_means$svpt
firstIn_mean  <- stats_means$`1stIn`
firstWon_mean <- stats_means$`1stWon`
secondWon_mean<- stats_means$`2ndWon`
SvGms_mean    <- stats_means$SvGms
bpSaved_mean  <- stats_means$bpSaved
bpFaced_mean  <- stats_means$bpFaced
ht_mean       <- stats_means$ht
overall_means <- train_pre %>% summarise(minutes_mean = mean(minutes,  na.rm = TRUE))
minutes_mean  <- overall_means$minutes_mean

# Recipe: Imputation
rec_prematch <- recipe(~ ., data = train_pre) %>%
  update_role(tourney_id, winner_id, loser_id, winner_name, loser_name,
    tourney_date, match_num, new_role = "id" ) %>% 
  step_mutate(
  w_ace      = if_else(is.na(w_ace), ace_mean, w_ace),
  l_ace      = if_else(is.na(l_ace), ace_mean, l_ace),
  w_df       = if_else(is.na(w_df),  df_mean,  w_df),
  l_df       = if_else(is.na(l_df),  df_mean,  l_df),
  w_svpt     = if_else(is.na(w_svpt), svpt_mean, w_svpt),
  l_svpt     = if_else(is.na(l_svpt), svpt_mean, l_svpt),
  w_1stIn    = if_else(is.na(w_1stIn), firstIn_mean, w_1stIn),
  l_1stIn    = if_else(is.na(l_1stIn), firstIn_mean, l_1stIn),
  w_1stWon   = if_else(is.na(w_1stWon), firstWon_mean, w_1stWon),
  l_1stWon   = if_else(is.na(l_1stWon), firstWon_mean, l_1stWon),
  w_2ndWon   = if_else(is.na(w_2ndWon), secondWon_mean, w_2ndWon),
  l_2ndWon   = if_else(is.na(l_2ndWon), secondWon_mean, l_2ndWon),
  w_SvGms    = if_else(is.na(w_SvGms), SvGms_mean, w_SvGms),
  l_SvGms    = if_else(is.na(l_SvGms), SvGms_mean, l_SvGms),
  w_bpSaved  = if_else(is.na(w_bpSaved), bpSaved_mean, w_bpSaved),
  l_bpSaved  = if_else(is.na(l_bpSaved), bpSaved_mean, l_bpSaved),
  w_bpFaced  = if_else(is.na(w_bpFaced), bpFaced_mean, w_bpFaced),
  l_bpFaced  = if_else(is.na(l_bpFaced), bpFaced_mean, l_bpFaced),
  winner_ht  = if_else(is.na(winner_ht), ht_mean, winner_ht),
  loser_ht  = if_else(is.na(loser_ht), ht_mean, loser_ht),
  minutes    = if_else(is.na(minutes), minutes_mean, minutes)) 
  
rec_prep <- rec_prematch %>% prep(training = train_pre)
rec_prep
train_ready <- bake(rec_prep, new_data = train_pre)
test_ready  <- bake(rec_prep, new_data = test_pre)


tennis_ready <- bind_rows(train_ready, test_ready)
dim(tennis_ready)
skimr::skim_without_charts(tennis_ready)

roll_long <- tennis_ready %>%
  transmute(
    tourney_id, tourney_date, match_num,
    # winner 
    player_id = winner_id,
    ace = w_ace,
    df = w_df,
    svpt = w_svpt,
    `1stIn` = w_1stIn,
    `1stWon` = w_1stWon,
    `2ndWon` = w_2ndWon,
    SvGms = w_SvGms,
    bpSaved = w_bpSaved,
    bpFaced = w_bpFaced,
    games = w_games,
    sets_won = w_sets_won,
    tiebreaks_won = w_tiebreaks_won,
    game_diff_per_set = w_game_diff_per_set,
    total_tb = total_tb,
    minutes = minutes
  ) %>%
  bind_rows(
    tennis_ready %>%
      transmute(
        tourney_id, tourney_date, match_num,
        # loser 
        player_id = loser_id,
        ace = l_ace,
        df = l_df,
        svpt = l_svpt,
        `1stIn` = l_1stIn,
        `1stWon` = l_1stWon,
        `2ndWon` = l_2ndWon,
        SvGms = l_SvGms,
        bpSaved = l_bpSaved,
        bpFaced = l_bpFaced,
        games = l_games,
        sets_won = l_sets_won,
        tiebreaks_won = l_tiebreaks_won,
        game_diff_per_set = l_game_diff_per_set,
        total_tb = total_tb,
        minutes = minutes
      )
  ) %>%
  arrange(player_id, tourney_date, match_num)

roll_vars <- c(
  "ace", "df", "svpt", "1stIn", "1stWon", "2ndWon",
  "SvGms", "bpSaved", "bpFaced",
  "games", "sets_won", "tiebreaks_won",
  "game_diff_per_set", "total_tb", "minutes"
)

roll_long <- roll_long %>%
  group_by(player_id) %>%
  mutate(
    across(
      all_of(roll_vars),
      ~ slide_dbl(
        .x,
        .f = ~ {
          # remove current
          prev_vals <- head(.x, -1)
          if (length(prev_vals) == 0) {
            NA_real_   
          } else {
            mean(prev_vals, na.rm = TRUE)  # at most 10 matches
          }
        },
        .before   = 10,      
        .complete = FALSE    
      ),
      .names = "avg_{.col}_10"
    )) %>%
  ungroup()

# winner rolling
roll_winner <- roll_long %>%
  select(player_id, tourney_id, tourney_date, match_num,
         ends_with("10")) %>%
  rename(winner_id = player_id) %>%
  rename_with(~ paste0("w_", .x), ends_with("10"))

tennis_ready <- tennis_ready %>%
  left_join(roll_winner, by = c("winner_id", "tourney_id", "tourney_date", "match_num"))


# loser rolling
roll_loser <- roll_long %>%
  select(player_id, tourney_id, tourney_date, match_num,
         ends_with("10")) %>%
  rename(loser_id = player_id) %>%
  rename_with(~ paste0("l_", .x), ends_with("10")) %>%
  distinct(loser_id, tourney_id, tourney_date, match_num, .keep_all = TRUE)

tennis_ready <- tennis_ready %>%
  left_join(
    roll_loser,
    by = c("loser_id", "tourney_id", "tourney_date", "match_num")
  )


dim(tennis_ready)

# Imputation for all
train <- tennis_ready %>% filter(year(tourney_date) >= 2000, year(tourney_date) <= 2018)
test  <- tennis_ready %>% filter(year(tourney_date) == 2019)

# Recipe: Imputation
rec_tennis <- recipe(~ ., data = train) %>%
  update_role(tourney_id, winner_id, loser_id, winner_name, loser_name,
              tourney_date, match_num, new_role = "id") %>%
  step_mutate(
    winner_rank = ifelse(is.na(winner_rank), 2200, winner_rank),
    loser_rank  = ifelse(is.na(loser_rank), 2200, loser_rank),
    winner_rank_points = ifelse(is.na(winner_rank_points), 0, winner_rank_points),
    loser_rank_points  = ifelse(is.na(loser_rank_points), 0, loser_rank_points)) %>%
  # Rule imputations (constant fill; fitted on train only)
  step_mutate(
    winner_seed = ifelse(is.na(winner_seed), 36L, winner_seed),
    loser_seed  = ifelse(is.na(loser_seed),  36L, loser_seed)) %>%
  step_mutate_at(all_numeric_predictors(), fn = ~ if_else(is.na(.), 0, .)) %>%
  step_rm(
    w_ace, w_df, w_svpt, w_1stIn, w_1stWon, w_2ndWon, w_SvGms, 
    w_bpSaved, w_bpFaced, l_ace, l_df, l_svpt, l_1stIn, l_1stWon, 
    l_2ndWon, l_SvGms, l_bpSaved, l_bpFaced, w_sets_won, l_sets_won,
    w_tiebreaks_won, l_tiebreaks_won, w_games, l_games, 
    w_game_diff_per_set, l_game_diff_per_set, total_tb, minutes) %>%
  step_mutate(
    # Define whether Player 1 (P1) is the higher-ranked player before the match
    # Use numeric encoding:
    #   1 = winner is higher-ranked (stronger player)
    #   0 = loser is higher-ranked
    p1_win = as.integer((winner_rank < loser_rank) |
                          (winner_rank == loser_rank & winner_rank_points > loser_rank_points))) %>%
  # Build P1/P2 views for paired columns
  step_mutate(
    # id
    p1_id = if_else(p1_win == 1L, winner_id, loser_id),
    p2_id = if_else(p1_win == 1L, loser_id, winner_id),
    # Ranking & points
    P1_rank        = if_else(p1_win == 1L, winner_rank,        loser_rank),
    P2_rank        = if_else(p1_win == 1L, loser_rank,         winner_rank),
    P1_rank_points = if_else(p1_win == 1L, winner_rank_points, loser_rank_points),
    P2_rank_points = if_else(p1_win == 1L, loser_rank_points,  winner_rank_points),
    # Demographics
    P1_age = if_else(p1_win == 1L, winner_age, loser_age),
    P2_age = if_else(p1_win == 1L, loser_age, winner_age),
    P1_ht  = if_else(p1_win == 1L, winner_ht,  loser_ht),
    P2_ht  = if_else(p1_win == 1L, loser_ht,   winner_ht),
    P1_seed = if_else(p1_win == 1L, winner_seed, loser_seed),
    P2_seed = if_else(p1_win == 1L, loser_seed,  winner_seed),
    # Short-term activity
    P1_days_since_prev  = if_else(p1_win == 1L, w_days_since_prev,  l_days_since_prev),
    P2_days_since_prev  = if_else(p1_win == 1L, l_days_since_prev,  w_days_since_prev),
    P1_matches_last_30d = if_else(p1_win == 1L, w_matches_last_30d, l_matches_last_30d),
    P2_matches_last_30d = if_else(p1_win == 1L, l_matches_last_30d, w_matches_last_30d),
    # Rolling serve form
    P1_avg_ace_10   = if_else(p1_win == 1L, w_avg_ace_10,   l_avg_ace_10),
    P2_avg_ace_10   = if_else(p1_win == 1L, l_avg_ace_10,   w_avg_ace_10),
    
    P1_avg_df_10    = if_else(p1_win == 1L, w_avg_df_10,    l_avg_df_10),
    P2_avg_df_10    = if_else(p1_win == 1L, l_avg_df_10,    w_avg_df_10),
    
    P1_avg_svpt_10  = if_else(p1_win == 1L, w_avg_svpt_10,  l_avg_svpt_10),
    P2_avg_svpt_10  = if_else(p1_win == 1L, l_avg_svpt_10,  w_avg_svpt_10),
    
    P1_avg_1stIn_10 = if_else(p1_win == 1L, w_avg_1stIn_10, l_avg_1stIn_10),
    P2_avg_1stIn_10 = if_else(p1_win == 1L, l_avg_1stIn_10, w_avg_1stIn_10),
    
    P1_avg_1stWon_10 = if_else(p1_win == 1L, w_avg_1stWon_10, l_avg_1stWon_10),
    P2_avg_1stWon_10 = if_else(p1_win == 1L, l_avg_1stWon_10, w_avg_1stWon_10),
    
    P1_avg_2ndWon_10 = if_else(p1_win == 1L, w_avg_2ndWon_10, l_avg_2ndWon_10),
    P2_avg_2ndWon_10 = if_else(p1_win == 1L, l_avg_2ndWon_10, w_avg_2ndWon_10),
    
    P1_avg_SvGms_10 = if_else(p1_win == 1L, w_avg_SvGms_10, l_avg_SvGms_10),
    P2_avg_SvGms_10 = if_else(p1_win == 1L, l_avg_SvGms_10, w_avg_SvGms_10),
    
    P1_avg_bpSaved_10 = if_else(p1_win == 1L, w_avg_bpSaved_10, l_avg_bpSaved_10),
    P2_avg_bpSaved_10 = if_else(p1_win == 1L, l_avg_bpSaved_10, w_avg_bpSaved_10),
    
    P1_avg_bpFaced_10 = if_else(p1_win == 1L, w_avg_bpFaced_10, l_avg_bpFaced_10),
    P2_avg_bpFaced_10 = if_else(p1_win == 1L, l_avg_bpFaced_10, w_avg_bpFaced_10),
    # Rolling score form
    P1_avg_games_10 = if_else(p1_win == 1L, w_avg_games_10, l_avg_games_10),
    P2_avg_games_10 = if_else(p1_win == 1L, l_avg_games_10, w_avg_games_10),
    
    P1_avg_sets_won_10 = if_else(p1_win == 1L, w_avg_sets_won_10, l_avg_sets_won_10),
    P2_avg_sets_won_10 = if_else(p1_win == 1L, l_avg_sets_won_10, w_avg_sets_won_10),
    
    P1_avg_tiebreaks_won_10 = if_else(p1_win == 1L, w_avg_tiebreaks_won_10, l_avg_tiebreaks_won_10),
    P2_avg_tiebreaks_won_10 = if_else(p1_win == 1L, l_avg_tiebreaks_won_10, w_avg_tiebreaks_won_10),
    
    P1_avg_game_diff_per_set_10 = if_else(p1_win == 1L, w_avg_game_diff_per_set_10, l_avg_game_diff_per_set_10),
    P2_avg_game_diff_per_set_10 = if_else(p1_win == 1L, l_avg_game_diff_per_set_10, w_avg_game_diff_per_set_10),
    
    P1_avg_total_tb_10 = if_else(p1_win == 1L, w_avg_total_tb_10, l_avg_total_tb_10),
    P2_avg_total_tb_10 = if_else(p1_win == 1L, l_avg_total_tb_10, w_avg_total_tb_10),
    
    P1_avg_minutes_10 = if_else(p1_win == 1L, w_avg_minutes_10, l_avg_minutes_10),
    P2_avg_minutes_10 = if_else(p1_win == 1L, l_avg_minutes_10, w_avg_minutes_10)) %>%
  # pre-match diffs（P1 − P2）
  step_mutate(
    diff_rank_points = P1_rank_points - P2_rank_points,
    diff_age         = P1_age         - P2_age,
    diff_ht          = P1_ht          - P2_ht,
    diff_seed        = P1_seed        - P2_seed,
    diff_days_since_prev  = P1_days_since_prev  - P2_days_since_prev,
    diff_matches_last_30d = P1_matches_last_30d - P2_matches_last_30d,
    diff_avg_ace_10   = P1_avg_ace_10   - P2_avg_ace_10,
    diff_avg_df_10    = P1_avg_df_10    - P2_avg_df_10,
    diff_avg_svpt_10  = P1_avg_svpt_10  - P2_avg_svpt_10,
    diff_avg_1stIn_10 = P1_avg_1stIn_10 - P2_avg_1stIn_10,
    diff_avg_1stWon_10 = P1_avg_1stWon_10 - P2_avg_1stWon_10,
    diff_avg_2ndWon_10 = P1_avg_2ndWon_10 - P2_avg_2ndWon_10,
    diff_avg_SvGms_10 = P1_avg_SvGms_10 - P2_avg_SvGms_10,
    diff_avg_bpSaved_10 = P1_avg_bpSaved_10 - P2_avg_bpSaved_10,
    diff_avg_bpFaced_10 = P1_avg_bpFaced_10 - P2_avg_bpFaced_10,
    diff_avg_games_10 = P1_avg_games_10 - P2_avg_games_10,
    diff_avg_sets_won_10 = P1_avg_sets_won_10 - P2_avg_sets_won_10,
    diff_avg_tiebreaks_won_10 = P1_avg_tiebreaks_won_10 - P2_avg_tiebreaks_won_10,
    diff_avg_game_diff_per_set_10 = P1_avg_game_diff_per_set_10 - P2_avg_game_diff_per_set_10,
    diff_avg_total_tb_10 = P1_avg_total_tb_10 - P2_avg_total_tb_10,
    diff_avg_minutes_10 = P1_avg_minutes_10 - P2_avg_minutes_10)
rec_full <- rec_tennis %>% prep(training = train)
rec_full
train_full <- bake(rec_full, new_data = train)
test_full  <- bake(rec_full, new_data = test)
full_pre <- bind_rows(train_full, test_full)
dim(full_pre)


# Compute yearly point quantiles and statistics
# Used to assign player tiers based on yearly ranking points
year_pts_cuts <- full_pre %>%
  transmute(
    year = year(tourney_date),
    pts_w = winner_rank_points,
    pts_l = loser_rank_points
  ) %>%
  pivot_longer(c(pts_w, pts_l),
               names_to = "role", values_to = "pts") %>%
  group_by(year) %>%
  summarise(
    q40 = quantile(pts, 0.40, na.rm = TRUE),   # lower-mid percentile
    q70 = quantile(pts, 0.70, na.rm = TRUE),   # high boundary
    q90 = quantile(pts, 0.90, na.rm = TRUE),   # top tier
    mean_pts = mean(pts, na.rm = TRUE),        # yearly mean
    sd_pts   = sd(pts,   na.rm = TRUE),        # yearly standard deviation
    .groups = "drop"
  )

#  Add player tiers, High/Low groups, and continuous z-gap
add_tiers_and_gap <- function(full_pre) {
  full_pre %>%
    mutate(year = year(tourney_date)) %>%
    left_join(year_pts_cuts, by = c("year")) %>%
    mutate(
      # -----------------------------------------
      # Assign player tiers based on yearly percentiles
      # Top10% / 10–30% / 30–60% / Bottom40%
      # -----------------------------------------
      P1_tier = case_when(
        P1_rank_points >= q90 ~ "Top10%",
        P1_rank_points >= q70 ~ "10–30%",
        P1_rank_points >= q40 ~ "30–60%",
        TRUE          ~ "Bottom40%"
      ),
      P2_tier = case_when(
        P2_rank_points >= q90 ~ "Top10%",
        P2_rank_points >= q70 ~ "10–30%",
        P2_rank_points >= q40 ~ "30–60%",
        TRUE          ~ "Bottom40%"
      ),
      P1_tier = factor(P1_tier,
                       levels = c("Top10%", "10–30%", "30–60%", "Bottom40%")),
      P2_tier = factor(P2_tier,
                       levels = c("Top10%", "10–30%", "30–60%", "Bottom40%")),

      # High vs Low grouping
      # High = yearly top 30% (>= q70)
      P1_HL = ifelse(P1_rank_points >= q70, "High", "Low"),
      P2_HL = ifelse(P2_rank_points >= q70, "High", "Low"),
      
      rank_group = factor(
        paste(P1_HL, "vs", P2_HL),
        levels = c("High vs Low", "Low vs High", "High vs High", "Low vs Low")
      ),
      
      # Continuous standardized strength difference (z-gap)
      # Measures matchup difficulty: smaller |z_gap| = more evenly matched (if z-gap close to 0，the prediction will be harder)
      z_gap = ifelse(sd_pts > 0,
                     (P1_rank_points - P2_rank_points) / sd_pts,
                     NA_real_)
    ) %>%
    # remove temporary join fields
    select(-q40, -q70, -q90, -mean_pts, -sd_pts)
}


train_full <- bake(rec_full, new_data = train) %>%
  add_tiers_and_gap()
test_full  <- bake(rec_full, new_data = test) %>%
  add_tiers_and_gap()

dim(train_full)
dim(test_full)

# ————————————————————————logistic regression————————————————————————
logit_fit <- glm(
  p1_win ~ 0 + diff_rank_points,
  data = train_full,
  family = binomial(link = "logit")
)
coef(logit_fit) 
summary(logit_fit)
# Predict probability for P1 (higher-ranked) win on 2019
test_full <- test_full %>%
  mutate(p_logit = predict(logit_fit, newdata = test_full, type = "response"))

# ————————————————————————Elo (K = 32)————————————————————————
K_ELO    <- 32
INIT_ELO <- 1500

# Combine TRAIN + TEST in strict time order (data from 2000-2018 & 2019)
stream_all <- bind_rows(
  train_full %>%
    arrange(tourney_date, match_num) %>%
    transmute(part = "train",tourney_id = tourney_id, date = tourney_date, match = match_num,
              y = p1_win, A = as.character(p1_id), B = as.character(p2_id)),
  test_full %>%
    arrange(tourney_date, match_num) %>%
    transmute(part = "test",tourney_id = tourney_id, date = tourney_date, match = match_num,
              y = p1_win, A = as.character(p1_id), B = as.character(p2_id))
) %>% arrange(date, match)

elo_online <- elo.run(
  y ~ A + B + group(date),
  data         = stream_all,
  k            = K_ELO,
  initial.elos = INIT_ELO
)

p_all <- as.data.frame(elo_online)$p.A
p_elo_online <- p_all[stream_all$part == "test"]

test_full <- test_full %>%
  arrange(tourney_date, match_num) %>%
  mutate(p_elo_online = p_elo_online)

elo_df <- stream_all %>%
  mutate(
    P1_elo_online = as.data.frame(elo_online)$elo.A,
    P2_elo_online = as.data.frame(elo_online)$elo.B
  )

elo_train <- elo_df %>%
  filter(part == "train") %>%
  transmute(
    tourney_id,
    tourney_date = date,
    match_num    = match,
    P1_elo_online,
    P2_elo_online
  )

elo_test <- elo_df %>%
  filter(part == "test") %>%
  transmute(
    tourney_id,
    tourney_date = date,
    match_num    = match,
    P1_elo_online,
    P2_elo_online
  )

train_full <- train_full %>%
  left_join(elo_train,
            by = c("tourney_id", "tourney_date", "match_num"))

test_full <- test_full %>%
  left_join(elo_test,
            by = c("tourney_id", "tourney_date", "match_num"))
dim(train_full)
dim(test_full)

skimr::skim_without_charts(train_full)
skimr::skim_without_charts(test_full)

# Only train Elo on 2000–2018
stream_train <- train_full %>%
  arrange(tourney_date, match_num) %>%
  transmute(
    date  = tourney_date,
    match = match_num,
    y     = p1_win,
    A     = as.character(p1_id),
    B     = as.character(p2_id)
  )

elo_f_train <- elo.run(
  y ~ A + B + group(date),
  data         = stream_train,
  k            = K_ELO,
  initial.elos = INIT_ELO
)

elo_final <- elo_f_train$elos
elo_table <- setNames(as.numeric(elo_final), names(elo_final))

elo_prob <- function(e_i, e_j) {
  1 / (1 + 10 ^ ((e_j - e_i) / 400))
}

get_elo <- function(id_vec, elo_tab, init = 1500) {
  r <- elo_tab[as.character(id_vec)]
  r[is.na(r)] <- init
  as.numeric(r)
}

# Predict Frozen Elo on 2019
test_full <- test_full %>%
  arrange(tourney_date, match_num) %>%
  mutate(
    P1_elo_frozen = get_elo(p1_id, elo_table),
    P2_elo_frozen = get_elo(p2_id, elo_table),
    p_elo_frozen  = elo_prob(P1_elo_frozen, P2_elo_frozen)
  )
skimr::skim_without_charts(test_full)

# ————————————————————————Regression & Elo Evaluation————————————————————————
base_test_eval <- test_full %>%
  transmute(
    y_true     = factor(p1_win, levels = c(0, 1)),
    y_true_num = as.integer(as.character(p1_win)),
    
    p_logit      = p_logit,
    p_elo_online = p_elo_online,   
    p_elo_frozen = p_elo_frozen,  
    
    pred_logit_cls       = factor(if_else(p_logit      >= 0.5, 1, 0), levels = c(0, 1)),
    pred_elo_online_cls  = factor(if_else(p_elo_online >= 0.5, 1, 0), levels = c(0, 1)),
    pred_elo_frozen_cls  = factor(if_else(p_elo_frozen >= 0.5, 1, 0), levels = c(0, 1))
  )

test_full <- test_full %>%
  select(-p_logit, -p_elo_online, -p_elo_frozen, -P1_elo_frozen, -P2_elo_frozen)

calibration <- function(prob, y_true) {sum(prob) / sum(y_true)}

ovell_metrics_logit_elo <- tibble(
  model = c("Logit", "Elo (2000-2019 Train)", "Elo (2000-2018 Train)"),
  accuracy = c(
    accuracy(base_test_eval, y_true, pred_logit_cls)$.estimate,
    accuracy(base_test_eval, y_true, pred_elo_online_cls)$.estimate,
    accuracy(base_test_eval, y_true, pred_elo_frozen_cls)$.estimate
  ),
  log_loss = c(
    mn_log_loss(base_test_eval, y_true, p_logit, event_level="second")$.estimate,
    mn_log_loss(base_test_eval, y_true, p_elo_online, event_level="second")$.estimate,
    mn_log_loss(base_test_eval, y_true, p_elo_frozen, event_level="second")$.estimate
  ),
  precision = c(
    precision(base_test_eval, y_true, pred_logit_cls,      event_level="second")$.estimate,
    precision(base_test_eval, y_true, pred_elo_online_cls, event_level="second")$.estimate,
    precision(base_test_eval, y_true, pred_elo_frozen_cls, event_level="second")$.estimate
  ),
  recall = c(
    recall(base_test_eval, y_true, pred_logit_cls,      event_level="second")$.estimate,
    recall(base_test_eval, y_true, pred_elo_online_cls, event_level="second")$.estimate,
    recall(base_test_eval, y_true, pred_elo_frozen_cls, event_level="second")$.estimate
  ),
  f1 = c(
    f_meas(base_test_eval, y_true, pred_logit_cls,      event_level="second")$.estimate,
    f_meas(base_test_eval, y_true, pred_elo_online_cls, event_level="second")$.estimate,
    f_meas(base_test_eval, y_true, pred_elo_frozen_cls, event_level="second")$.estimate
  ),
  calibration = c(
    calibration(base_test_eval$p_logit,      base_test_eval$y_true_num),
    calibration(base_test_eval$p_elo_online, base_test_eval$y_true_num),
    calibration(base_test_eval$p_elo_frozen, base_test_eval$y_true_num)
  )
)

ovell_metrics_logit_elo

# Probability distributions for y = 0 vs y = 1
baseline_prob_long <- base_test_eval %>%
  transmute(
    y_true = y_true_num,
    Logit = p_logit,
    Elo_online = p_elo_online,
    Elo_frozen = p_elo_frozen
  ) %>%
  pivot_longer(cols = c(Logit, Elo_online, Elo_frozen),
               names_to = "model",
               values_to = "prob")

ggplot(baseline_prob_long, aes(x = prob, fill = factor(y_true))) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ model, ncol = 1) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"),
                    name = "Actual Outcome",
                    labels = c("P1 lost (0)", "P1 won (1)")) +
  labs(
    title = "Predicted Probability Distribution by Model",
    x = "Predicted Probability (P1 win)",
    y = "Density") 
# The predicted probability for the winning side (y=1) is concentrated in high probabilities, 
# while the predicted probability for the losing side (y=0) is concentrated in low probabilities.

# Online Elo Updates Per Match: Winning against a strong opponent → Elo increases
# Losing streak → Elo decreases
# This reflects momentum, recent state, fatigue recovery, sudden state changes
# Therefore, Online Elo will exhibit: 
  # y=1 (P1 wins) → Probability distribution skewed to the right (higher)
  # y=0 (P1 loses) → Probability distribution skewed to the left (lower)
# Online Elo = Strongest discriminative power. Visually, it is represented by the most obvious separation between the two density plots.

# logistic regression is: p = logistic(β × diff_rank_points)
  # diff_rank_points > 0: P1 has a very high probability of winning → probability close to 1
  # diff_rank_points ≈ 0: the predicted probability is close to 0.5
  # diff_rank_points is very small (negative) in a minority of cases → probability close to 0
# Due to most ATP matches are: Higher ranking vs. lower ranking → larger diff_rank_points
   # → higher logistic output (0.6~0.85)
# A small number of matches: Closer rankings → logistic output close to 0.5
# Therefore, the logit plot will show a bimodal distribution (one around 0.55, and the other around 0.7).
# The ranking point difference is relatively static, and the model lacks dynamic information.

# Frozen Elo: The 2019 tournament used the Elo from late 2018.
# It didn't update short-term states at all. Has worst predictive performance.
# The predicted probabilities are concentrated in the middle (0.4–0.6).
# It has very poor discrimination against real matches.
#  The two distributions (y=0 vs y=1) almost overlap.

calib_data <- baseline_prob_long %>%
  mutate(bin = ntile(prob, 10)) %>%   # Using binning (10 bins), calculate for each bin
  group_by(model, bin) %>%
  summarise(
    avg_pred = mean(prob), # Average prediction probability
    avg_true = mean(y_true), # Actual win rate
    .groups = "drop")

ggplot(calib_data, aes(x = avg_pred, y = avg_true, color = model)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +  # perfect calibration
  scale_color_manual(values = c("Logit" = "#1B9E77",
                                "Elo_online" = "#D95F02",
                                "Elo_frozen" = "#7570B3")) +
  labs(title = "Calibration Curves (10-bin)",
       x = "Predicted Probability (bin average)",
       y = "Actual Win Rate",
       color = "Model")

# The logit value is close to 1 (1.02) in calibration, and it shows that it has a certain ability to distinguish between y=1 and y=0.


# Baseline model Upset prediction evaluation
base_upset_eval <- base_test_eval %>% 
  filter(y_true_num == 0) %>% 
  select(p_logit, p_elo_online, p_elo_frozen)

base_upset_long <- base_upset_eval %>%
  pivot_longer(everything(), names_to="model", values_to="prob")

# Upset probability distribution (only considering matches where y=0)
ggplot(base_upset_long, aes(prob, fill=model)) +
  geom_density(alpha=0.35) +
  scale_fill_manual(values=c(
    "p_logit"="orange", 
    "p_elo_online"="skyblue",
    "p_elo_frozen"="darkgreen"
  )) +
  labs(
    title="Predicted Probability for TRUE Upsets (p1_win=0)",
    x="Predicted Probability of P1 Win",
    y="Density") 

# Logit's predictions based on ranking difference:
  # If the ranking difference is small → prediction probability ~0.5 (prone to upsets)
  # If the ranking difference is large → Logit gives a high probability (0.7–0.9)
  # → Even if an upset occurs, it won't be seen in advance.
# It can only look at "structural upsets" (situations where rankings are close).
# Logit may still predict higher (the stronger should win), so even in a true upset, a misprediction of 0.8 may occur.

# Elo_online is more left-skewed (predicts lower odds) than Logit, has a wider distribution, a long tail, and probabilities as low as ~0.10 or 0.2.
# Online Elo updates player strength based on recent form/win/loss streaks.
# When a true upset occurs, the typical scenario is:
# P1 (the stronger player) is in poor form recently → Elo lowers its confidence in P1.
# P2 (the weaker player) is in good form → Elo raises its confidence.
# In matches with true upsets, Online Elo will give a lower predicted probability for P1 win in advance, meaning it "senses danger."
# Elo_online is available for upset detection.

# Elo_frozen (Worst-case scenario, almost no chance of an upset).Probability range: 0.45–0.55
# There are absolutely no early warning signs of an upset (weakest team wins).
# It's further left than Online Elo, but has a wider peak and weaker information, making it less able to distinguish upsets.
# It cannot reflect recent changes in form, it cannot predict upsets.



# Upset Calibration Curve
make_calibration_df <- function(pred, truth, nbins=10){
  tibble(pred=pred, truth=truth) %>%
    mutate(bin = ntile(pred, nbins)) %>%
    group_by(bin) %>%
    summarise(
      avg_pred = mean(1 - pred),      # upset pre pro
      avg_true = mean(truth == 0),    # accutual upset pro
      n = n()
    )
}

cal_logit  <- make_calibration_df(base_test_eval$p_logit,      base_test_eval$y_true_num)
cal_online <- make_calibration_df(base_test_eval$p_elo_online, base_test_eval$y_true_num)
cal_frozen <- make_calibration_df(base_test_eval$p_elo_frozen, base_test_eval$y_true_num)

cal_all <- bind_rows(
  mutate(cal_logit,  model="Logit"),
  mutate(cal_online, model="Elo_online"),
  mutate(cal_frozen, model="Elo_frozen"))

ggplot(cal_all, aes(avg_pred, avg_true, color=model)) +
  geom_line() +
  geom_point() +
  geom_abline(linetype=2) +
  scale_color_manual(values=c("orange", "skyblue", "darkgreen")) +
  labs(
    title="Calibration Curve for Upsets",
    x="Predicted upset probability",
    y="Actual upset rate") 

# Elo_online is closest to diagonal and best captures upsets.
# The closest point to the 45º perfect calibration line (black) is slightly to the right and higher, meaning that when the model predicts an upset probability of 0.4, the actual upset rate is approximately 0.45–0.5. 
# This indicates that the model underestimates the potential for upsets.
# It can give a higher upset probability before an upset occurs,
# but the prediction is conservative (underconfident), 
# and actual upsets are more frequent than predicted.


# Logit performs slightly worse than Elo online, with its curve generally positioned below the black line. 
# The Logit model often overestimates the chances of a strong upset.
# Even when projecting a high P1 win, it fails to recognize that the actual upset rate is much higher.
# The Logit model cannot identify unstructured upsets (e.g., state/fatigue/minor injury).
# It only predicts based on the ranking difference, therefore:
  # When the ranking difference is large → it predicts the strong player will win
# But in reality, many upsets occur.
# Therefore, it significantly underestimates the upset probability in the "invisible upset" scenario.

# Frozen Elo performed the worst, with all predictions concentrated in a single x-range (approximately 0.46–0.56), 
# while the actual upset rate fluctuated between 0.30 and 0.45.
# It was completely uncalibrated and lacked any discriminative ability.
# Frozen Elo didn't update at all, missing all short-term status information.
# Therefore, it couldn't distinguish which matches were more likely to result in upsets.
# All predictions fell around the same 0.5 (= "don't know").

train_upset_by_group <- train_full %>%
  group_by(rank_group) %>%
  summarise(
    n = n(),
    upset_rate = mean(p1_win == 0),
    .groups = "drop"
  )

test_upset_by_group <- test_full %>%
  group_by(rank_group) %>%
  summarise(
    n = n(),
    upset_rate = mean(p1_win == 0),
    .groups = "drop"
  )

train_upset_by_group
test_upset_by_group
# Prediction difficulty: High vs Low < High vs High < Low vs Low

# Why is the upset rate highest in Low vs Low matches? (44% is perfectly reasonable)
# Low-ranked players = inconsistent + newcomers + low playing frequency + high volatility
# Therefore, matches between them are unpredictable, but a lower ranking doesn't necessarily mean weaker, just fewer points.
# Many upsets come from lower-ranked players in good form (returning from injury, rising young stars).
# This naturally leads to the most chaotic Low vs Low matches, with a higher upset rate (~40–45%). 
# This group has the most unpredictable upsets.

# Strong vs. Weak → Higher ranking generally indicates a stronger team, which will win.
# However, tennis still has many structural upsets (physical condition, injuries, back-to-back play, court differences).
# The test is slightly higher than the train.
# 2019 ATP saw a large number of high-ranked injury return cases (Murray, Raonic, etc.)
# Rising stars (Sinner, Berrettini, Tsitsipas) experienced more upsets
# Inconsistency (physical fitness, schedule density)

# High vs High = 35% upset?
# This is the most typical scenario where high vs high = evenly matched.
# The concept of upset is more nuanced, so the probability of an upset is naturally higher in high vs low scenarios.

# An upset is not random, but structural.
# Upsets are not uniformly distributed in tennis—they sharply increase as the ranking gap shrinks.
# “High vs Low” matches are the most predictable (low upsets), 
# “High vs High” are more volatile
# “Low vs Low” matches are the least predictable due to the inherent instability of lower-ranked players.

base_test_eval <- base_test_eval %>%
  bind_cols(test_full %>% select(rank_group))


base_eval_long <- bind_rows(
  base_test_eval %>%
    transmute(
      rank_group,
      model   = "Logit",
      y_true,
      y_true_num,
      p_hat   = p_logit,
      pred_cls = pred_logit_cls
    ),
  base_test_eval %>%
    transmute(
      rank_group,
      model   = "Elo_online",
      y_true,
      y_true_num,
      p_hat   = p_elo_online,
      pred_cls = pred_elo_online_cls
    ),
  base_test_eval %>%
    transmute(
      rank_group,
      model   = "Elo_frozen",
      y_true,
      y_true_num,
      p_hat   = p_elo_frozen,
      pred_cls = pred_elo_frozen_cls
    )
)

# P1_win = 1 is positive

base_metrics_by_group <- base_eval_long %>%
  group_by(rank_group, model) %>%
  summarise(
    n          = n(),
    accuracy   = accuracy_vec(y_true, pred_cls),
    log_loss   = mn_log_loss_vec(y_true, p_hat, event_level = "second"),
    roc_auc    = roc_auc_vec(y_true, p_hat, event_level = "second"),
    precision  = precision_vec(y_true, pred_cls, event_level = "second"),
    recall     = recall_vec(y_true, pred_cls, event_level = "second"),
    .groups = "drop"
  )

base_metrics_by_group

base_metrics_by_group <- base_metrics_by_group %>%
  mutate(rank_group = factor(rank_group, 
                             levels = c("High vs Low", "High vs High", "Low vs Low")))

# ---- Plot Accuracy ----
p_acc <- ggplot(base_metrics_by_group,
                aes(x = rank_group, y = accuracy, color = model, group = model)) +
  geom_line() +
  geom_point() +
  labs(title = "Accuracy by Rank Group",
       x = "Rank Group",
       y = "Accuracy",
       color = "Model")

# ---- Plot AUC ----
p_auc <- ggplot(base_metrics_by_group,
                aes(x = rank_group, y = roc_auc, color = model, group = model)) +
  geom_line() +
  geom_point() +
  labs(title = "ROC AUC by Rank Group",
       x = "Rank Group",
       y = "AUC",
       color = "Model") 

# ---- Plot Log-loss ----
p_logloss <- ggplot(base_metrics_by_group,
                    aes(x = rank_group, y = log_loss, color = model, group = model)) +
  geom_line() +
  geom_point() +
  labs(title = "Log-loss by Rank Group",
       x = "Rank Group",
       y = "Log-loss (lower is better)",
       color = "Model") 

p_acc
p_auc
p_logloss

# P1_loss = 1 is positive
upset_base_metrics_by_group <- base_eval_long %>%
  group_by(rank_group, model) %>%
  summarise(
    n = n(),
    roc_auc_upset = roc_auc_vec(y_true, 1 - p_hat, event_level = "first"),
    precision_upset = precision_vec(y_true, pred_cls, event_level = "first"),
    recall_upset    = recall_vec(y_true, pred_cls, event_level = "first"),
    .groups = "drop")
upset_base_metrics_by_group

# Logit is essentially a linear rank-points model: Always tends to believe high-scoring players
# Very reluctant to predict upsets. Therefore, recall for upsets = 0.

# Frozen Elo never updates to 2019. Ratings remain at the 2018 level
# High-ranking players always maintain high ratings
# It never believes in upsets. Recall is always 0.

# Elo_online allows ratings to change dynamically. New input can correct expectations
# Therefore, it is the only model that can predict some upsets. Upset precision ≈ 0.5
# With a very low recall (0.129), it almost never actually predicts upsets (high threshold), 
# and only dares to predict upsets in a very few matches with a very low probability (<0.3).
# Elo_online is also affected by the Elo rating gap.
# The difference between high and low Elo ratings is huge, with a very high baseline P1_win.
# Although it's a dynamic Elo, it's still difficult to predict upsets.
# For High-vs-Low matches, Elo_online identifies only the most extreme upset-risk cases, achieving high precision but low recall.

# High vs. High recall shows a significant improvement (0.214 > 0.129). When two strong competitors play against each other, 
# the rating difference is small, leading to greater model uncertainty and a greater willingness to predict upsets.
# This indicates that Elo_online is more flexible and adept at capturing state fluctuations in high vs. high games.
# Elo_online is better at detecting upsets in closely matched (High-vs-High) games, showing both higher recall and still-solid precision.

# Low vs Low
#  Upset is inherently ambiguous in the definition of Low vs Low: 
# Two weaker players battling each other → unstable ratings → high rating noise 
# → highest difficulty in Elo prediction
# Recall = 0.339: highest among the three groups
# → Elo_online is more likely to predict upset
# → because the dynamic update range of ratings is the largest (weaker players' ratings naturally fluctuate more)
# AUC = 0.596: close to random → weak model ranking ability
# → difficult to truly distinguish "this weaker player is more likely to lose"
# Low vs Low = Noise vs Noise
# Dynamic Elo struggles to find good signals in such unstable data.
# However, because the prediction baseline is more unstable, the model is more likely to push some probabilities to <0.5, resulting in high recall.

names(test_full)

rec_model <- recipe(p1_win ~ ., data = train_full) %>%
  update_role(tourney_id, tourney_date, match_num, p1_id, p2_id, new_role = "id") %>%
  update_role(P1_rank, P2_rank, P1_HL, P2_HL, new_role = "unused") %>%
  step_mutate(
    P1_elo = P1_elo_online,
    P2_elo = P2_elo_online,
    diff_elo = P1_elo_online - P2_elo_online) %>%
  step_mutate(p1_win = factor(p1_win, levels = c(0, 1))) %>%
  step_rm(
    starts_with("winner_"),
    starts_with("loser_"),
    starts_with("w_"),
    starts_with("l_"),
    starts_with("P1_"),
    starts_with("P2_"),
    P1_elo_online, P2_elo_online, year, -p1_win, -P1_tier, -P2_tier,
    -P1_rank, -P2_rank, -P1_HL, -P2_HL, -p1_id, -p2_id) 

rec_model_prep <- rec_model %>% prep(training = train_full)
rec_model_prep
train_model_ready <- bake(rec_model_prep, new_data = train_full)
test_model_ready  <- bake(rec_model_prep,  new_data = test_full)
names(train_model_ready)
dim(train_model_ready)
dim(test_model_ready)

# ———————————————————————— Logistic: recipe ————————————————————————
rec_logit <- recipe(p1_win ~ ., data = train_model_ready) %>%
  update_role(tourney_id, tourney_date, match_num, p1_id, p2_id,
              new_role = "id") %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors())

# ——————————————— Logistic model: L1 selected features ____________________
logit_l1 <- logistic_reg(
  mode = "classification",
  penalty = tune(),    # λ
  mixture = 1          # LASSO (L1)
) %>% 
  set_engine("glmnet")

set.seed(123)

wf_logit_l1 <- workflow() %>%
  add_model(logit_l1) %>%
  add_recipe(rec_logit)

folds <- vfold_cv(train_model_ready, v = 5)

grid_l1 <- grid_regular(
  penalty(range = c(-4, 0)),   # λ = 10^-4 ~ 10^0
  levels = 20
)

tuned_l1 <- tune_grid(
  wf_logit_l1,
  resamples = folds,
  grid = grid_l1,
  metrics = metric_set(roc_auc, mn_log_loss, accuracy)
)

best_l1 <- select_best(tuned_l1, metric = "mn_log_loss")
best_l1

fit_l1_final <- finalize_workflow(wf_logit_l1, best_l1) %>%
  fit(data = train_model_ready)

test_l1_pred <- predict(fit_l1_final, new_data = test_model_ready, type = "prob") %>%
  bind_cols(predict(fit_l1_final, new_data = test_model_ready, type = "class")) %>%
  bind_cols(test_model_ready %>% select(p1_win))

l1_test_eval <- test_l1_pred %>%
  transmute(
    y_true        = p1_win,
    y_true_num    = as.integer(as.character(p1_win)),
    p_l1          = .pred_1,
    pred_l1_class = .pred_class)

l1_metrics <- tibble(
  model = "Logit_L1",
  accuracy = accuracy(l1_test_eval, y_true, pred_l1_class)$.estimate,
  log_loss = mn_log_loss(l1_test_eval, y_true, p_l1, event_level = "second")$.estimate,
  precision  = precision(l1_test_eval, y_true, pred_l1_class, event_level = "second")$.estimate,
  recall     = recall(l1_test_eval, y_true, pred_l1_class, event_level = "second")$.estimate,
  f1         = f_meas(l1_test_eval, y_true, pred_l1_class, event_level = "second")$.estimate,
  calibration = calibration(l1_test_eval$p_l1, l1_test_eval$y_true_num))


compare_metrics <- bind_rows(ovell_metrics_logit_elo,l1_metrics)
compare_metrics

# ——————————————— Upset Logistic model: L1 selected features ____________________
upset_train_model_ready <- train_model_ready %>%
  mutate(upset = if_else(p1_win == 0, 1, 0)) %>%
  mutate(upset = factor(upset, levels = c(0, 1)))

upset_test_model_ready <- test_model_ready %>%
  mutate(upset = if_else(p1_win == 0, 1, 0)) %>%
  mutate(upset = factor(upset, levels = c(0, 1)))

rec_upset <- recipe(upset ~ ., data = upset_train_model_ready) %>%
  update_role(tourney_id, tourney_date, match_num, p1_id, p2_id,
              new_role = "id") %>%
  step_rm(p1_win) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) 

logit_l1_upset <- logistic_reg(
  mode = "classification",
  penalty = tune(), 
  mixture = 1        # L1 penalization
) %>%
  set_engine("glmnet")

wf_upset_l1 <- workflow() %>%
  add_model(logit_l1_upset) %>%
  add_recipe(rec_upset)

folds_upset <- vfold_cv(upset_train_model_ready, v = 5)

grid_l1_upset <- grid_regular(
  penalty(range = c(-4, 0)),
  levels = 30
)

tuned_upset <- tune_grid(
  wf_upset_l1,
  resamples = folds_upset,
  grid = grid_l1_upset,
  metrics = metric_set(roc_auc, mn_log_loss)
)

best_upset_l1 <- select_best(tuned_upset, metric = "mn_log_loss")

fit_l1_upset_final <- finalize_workflow(wf_upset_l1, best_upset_l1) %>%
  fit(upset_train_model_ready)

test_l1_upset_pred <- predict(fit_l1_upset_final, new_data = upset_test_model_ready, type = "prob") %>%
  bind_cols(predict(fit_l1_upset_final, new_data = upset_test_model_ready, type = "class")) %>%
  bind_cols(upset_test_model_ready %>% select(upset))

upset_l1_test_eval <- test_l1_upset_pred %>%
  transmute(
    y_true        = upset,
    y_true_num    = as.integer(as.character(upset)),
    p_l1          = .pred_1,
    pred_l1_class = .pred_class)

l1_upset_metrics <- tibble(
  model = "Logit_L1_upset",
  accuracy = accuracy(upset_l1_test_eval, y_true, pred_l1_class)$.estimate,
  log_loss = mn_log_loss(upset_l1_test_eval, y_true, p_l1, event_level = "second")$.estimate,
  precision  = precision(upset_l1_test_eval, y_true, pred_l1_class, event_level = "second")$.estimate,
  recall     = recall(upset_l1_test_eval, y_true, pred_l1_class, event_level = "second")$.estimate,
  f1         = f_meas(upset_l1_test_eval, y_true, pred_l1_class, event_level = "second")$.estimate,
  calibration = calibration(upset_l1_test_eval$p_l1, upset_l1_test_eval$y_true_num))


compare_metrics <- bind_rows(ovell_metrics_logit_elo,l1_metrics,l1_upset_metrics)
compare_metrics

# ——————————————————————————— Random Forest Model ____________________
rec_rf <- recipe(p1_win ~ ., data = train_model_ready) %>%
  update_role(tourney_id, tourney_date, match_num, p1_id, p2_id,
              new_role = "id") %>%
  # step_interact(terms = ~ starts_with("diff_") : starts_with("diff_")) %>% the performance no change
  step_zv(all_predictors())

# rf_spec <- rand_forest(
#   mode  = "classification",
#   trees = 300,
#   mtry  = tune(),
#   min_n = tune()) %>%
#   set_engine("ranger", probability = TRUE, importance = "impurity")

rf_spec <- rand_forest(
  mode  = "classification",
  trees = 200  
  # mtry / min_n use ranger default
) %>%
  set_engine("ranger", probability = TRUE, importance = "impurity")

wf_rf <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(rec_rf)

# grid_rf <- grid_regular(
#   mtry(range = c(3, 20)),
#   min_n(range = c(5, 50)),
#   levels = 5)
# doParallel::registerDoParallel()
# tuned_rf <- tune_grid(
#   wf_rf,
#   resamples = folds,
#   grid      = grid_rf,
#   metrics   = metric_set(roc_auc, mn_log_loss, accuracy))
# best_rf <- select_best(tuned_rf, metric = "mn_log_loss")
# best_rf
# fit_rf_final <- finalize_workflow(wf_rf, best_rf) %>%
#   fit(data = train_model_ready)

set.seed(123)
fit_rf_final <- wf_rf %>% fit(data = train_model_ready)


test_rf_pred <- predict(fit_rf_final, new_data = test_model_ready, type = "prob") %>%
  bind_cols(predict(fit_rf_final, new_data = test_model_ready, type = "class")) %>%
  bind_cols(test_model_ready %>% select(p1_win))

rf_test_eval <- test_rf_pred %>%
  transmute(
    y_true        = p1_win,
    y_true_num    = as.integer(as.character(p1_win)),
    p_rf          = .pred_1,
    pred_rf_class = .pred_class
  )

rf_metrics <- tibble(
  model      = "RF_win",
  accuracy   = accuracy(rf_test_eval, y_true, pred_rf_class)$.estimate,
  log_loss   = mn_log_loss(rf_test_eval, y_true, p_rf, event_level = "second")$.estimate,
  precision  = precision(rf_test_eval, y_true, pred_rf_class, event_level = "second")$.estimate,
  recall     = recall(rf_test_eval, y_true, pred_rf_class, event_level = "second")$.estimate,
  f1         = f_meas(rf_test_eval, y_true, pred_rf_class, event_level = "second")$.estimate,
  calibration = calibration(rf_test_eval$p_rf, rf_test_eval$y_true_num)
)


compare_metrics <- bind_rows(compare_metrics, rf_metrics)
compare_metrics
# ——————————————— Upset Random Forest Model ____________________
rec_upset_rf <- recipe(upset ~ ., data = upset_train_model_ready) %>%
  update_role(tourney_id, tourney_date, match_num, p1_id, p2_id,
              new_role = "id") %>%
  step_rm(p1_win) %>%
  step_zv(all_predictors()) 

rf_upset_spec <- rand_forest(
  mode  = "classification",
  trees = 200   
) %>% set_engine("ranger", probability = TRUE, importance = "impurity")

wf_rf_upset <- workflow() %>%
  add_model(rf_upset_spec) %>%
  add_recipe(rec_upset_rf)

set.seed(123)
fit_rf_upset_final <- wf_rf_upset %>%
  fit(data = upset_train_model_ready)

test_rf_upset_pred <- predict(fit_rf_upset_final,
                              new_data = upset_test_model_ready,
                              type = "prob") %>%
  bind_cols(predict(fit_rf_upset_final,
            new_data = upset_test_model_ready,
            type = "class")) %>%
  bind_cols(upset_test_model_ready %>% select(upset))

upset_rf_test_eval <- test_rf_upset_pred %>%
  transmute(
    y_true        = upset,
    y_true_num    = as.integer(as.character(upset)),
    p_rf          = .pred_1,        
    pred_rf_class = .pred_class)


rf_upset_metrics <- tibble(
  model      = "RF_upset",
  accuracy   = accuracy(upset_rf_test_eval, y_true, pred_rf_class)$.estimate,
  log_loss   = mn_log_loss(upset_rf_test_eval, y_true, p_rf, event_level = "second")$.estimate,
  precision  = precision(upset_rf_test_eval, y_true, pred_rf_class, event_level = "second")$.estimate,
  recall     = recall(upset_rf_test_eval, y_true, pred_rf_class, event_level = "second")$.estimate,
  f1         = f_meas(upset_rf_test_eval, y_true, pred_rf_class, event_level = "second")$.estimate,
  calibration = calibration(upset_rf_test_eval$p_rf, upset_rf_test_eval$y_true_num)
)

compare_metrics <- bind_rows(compare_metrics, rf_upset_metrics)
compare_metrics

# Random Forest does not outperform the L1-regularized logistic model.
# RF also fails to learn any *new* nonlinear patterns from the features 
# because the feature engineering has already captured almost all of the structural information. 
# The diff-based features follow a simple rule: the larger the difference, the higher the win probability. 
# These are monotonic and almost linearly separable. As a result, a nonlinear model like RF has no extra patterns to discover, 
# and its complexity is essentially wasted.

# RF even performs slightly worse than Logistic L1 because Random Forests are not strong under extreme class imbalance. 
# Upsets are rare (around 20–30%), and RF is very sensitive to the minority-class recall.

# The L1 logistic model can reliably learn weak but stable linear signals such as *diff_avg_sets_won_10* or *diff_elo*. 
# RF, however, struggles in weak-signal settings:
# it is easily overwhelmed by noise, makes too many unnecessary splits, and ends up overfitting with poor calibration. 
# Therefore, RF ends up worse than logistic L1 across multiple metrics.

# RF_upset also brings no benefit. Upsets are generated by linear patterns plus noise 
# factors like fatigue, form, and recent performance change gradually and are naturally suited to linear models 
# rather than nonlinear tree structures.

# Overall, once the data is transformed into diff features, the problem becomes almost linearly separable. 
# Rolling-10 features further smooth the data and weaken nonlinear structure. 
# This is why Logistic L1 remains the best model. 

# More complex models cannot magically detect upsets — only richer *state* information can.

# ————————————————Random Forest Model(use P1/P2 feature, remove diff feature)—————————————————————————————
rec_model_player <- recipe(p1_win ~ ., data = train_full) %>%
  update_role(tourney_id, tourney_date, match_num, p1_id, p2_id, new_role = "id") %>%
  step_mutate(
    P1_elo = P1_elo_online,
    P2_elo = P2_elo_online) %>%
  step_mutate(p1_win = factor(p1_win, levels = c(0, 1))) %>%
  step_rm(
    starts_with("winner_"),
    starts_with("loser_"),
    starts_with("w_"),
    starts_with("l_"),
    starts_with("diff_"),
    P1_elo_online, P2_elo_online, year) 

rec_model_player_prep <- rec_model_player %>% prep(training = train_full)
rec_model_player_prep
train_model_pready <- bake(rec_model_player_prep, new_data = train_full)
test_model_pready  <- bake(rec_model_player_prep,  new_data = test_full)

rec_rf_p <- recipe(p1_win ~ ., data = train_model_pready) %>%
  update_role(tourney_id, tourney_date, match_num, p1_id, p2_id,
              new_role = "id") %>%
  step_zv(all_predictors())


rf_p_spec <- rand_forest(
  mode  = "classification",
  trees = 200   
) %>%
  set_engine("ranger", probability = TRUE, importance = "impurity")

wf_rf_p <- workflow() %>%
  add_model(rf_p_spec) %>%
  add_recipe(rec_rf_p)

set.seed(123)
fit_rf_p_final <- wf_rf_p %>%
  fit(data = train_model_pready)

test_rf_p_pred <- predict(fit_rf_p_final,
                          new_data = test_model_pready,
                          type = "prob") %>%
  bind_cols(
    predict(fit_rf_p_final,
            new_data = test_model_pready,
            type = "class")
  ) %>%
  bind_cols(test_model_pready %>% select(p1_win))

rf_p_test_eval <- test_rf_p_pred %>%
  transmute(
    y_true        = p1_win,
    y_true_num    = as.integer(as.character(p1_win)),
    p_rf_p        = .pred_1,       
    pred_rf_p_cls = .pred_class   
  )

rf_p_metrics <- tibble(
  model      = "RF_win_P1P2",
  accuracy   = accuracy(rf_p_test_eval, y_true, pred_rf_p_cls)$.estimate,
  log_loss   = mn_log_loss(rf_p_test_eval, y_true, p_rf_p, event_level = "second")$.estimate,
  precision  = precision(rf_p_test_eval, y_true, pred_rf_p_cls, event_level = "second")$.estimate,
  recall     = recall(rf_p_test_eval, y_true, pred_rf_p_cls, event_level = "second")$.estimate,
  f1         = f_meas(rf_p_test_eval, y_true, pred_rf_p_cls, event_level = "second")$.estimate,
  calibration = calibration(rf_p_test_eval$p_rf_p, rf_p_test_eval$y_true_num)
)

compare_metrics <- bind_rows(compare_metrics, rf_p_metrics)
compare_metrics

# ————————————————Random Forest Model(interaction term)—————————————————————————————
rec_rf_term <- recipe(p1_win ~ ., data = train_model_ready) %>%
  update_role(tourney_id, tourney_date, match_num, p1_id, p2_id,
              new_role = "id") %>%
  step_interact(terms = ~ starts_with("diff_") : starts_with("diff_")) %>% 
  step_zv(all_predictors())

rf_spec <- rand_forest(
  mode  = "classification",
  trees = 200  
) %>%
  set_engine("ranger", probability = TRUE, importance = "impurity")

wf_rf_term <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(rec_rf_term)

set.seed(123)

fit_rf_term_final <- wf_rf_term %>% fit(data = train_model_ready)

test_rf_term_pred <- predict(fit_rf_term_final, new_data = test_model_ready, type = "prob") %>%
  bind_cols(predict(fit_rf_term_final, new_data = test_model_ready, type = "class")) %>%
  bind_cols(test_model_ready %>% select(p1_win))

rf_term_test_eval <- test_rf_term_pred %>%
  transmute(
    y_true        = p1_win,
    y_true_num    = as.integer(as.character(p1_win)),
    p_rf          = .pred_1,
    pred_rf_class = .pred_class
  )

rf_term_metrics <- tibble(
  model      = "RF_win_interaction",
  accuracy   = accuracy(rf_term_test_eval, y_true, pred_rf_class)$.estimate,
  log_loss   = mn_log_loss(rf_term_test_eval, y_true, p_rf, event_level = "second")$.estimate,
  precision  = precision(rf_term_test_eval, y_true, pred_rf_class, event_level = "second")$.estimate,
  recall     = recall(rf_term_test_eval, y_true, pred_rf_class, event_level = "second")$.estimate,
  f1         = f_meas(rf_term_test_eval, y_true, pred_rf_class, event_level = "second")$.estimate,
  calibration = calibration(rf_term_test_eval$p_rf, rf_term_test_eval$y_true_num)
)


compare_metrics <- bind_rows(compare_metrics, rf_term_metrics)
compare_metrics

#——————————————————————— L1 Feature Importance Comparison ——————————————————————
important_l1 <- tidy(fit_l1_final) %>%
  filter(term != "(Intercept)") %>%
  filter(abs(estimate) > 0) %>%     
  arrange(desc(abs(estimate)))    
important_l1

vip(fit_l1_final$fit$fit, num_features = 20)

important_upset_l1 <- tidy(fit_l1_upset_final) %>%
  filter(term != "(Intercept)") %>%
  filter(abs(estimate) > 0) %>%     
  arrange(desc(abs(estimate)))    
important_upset_l1

vip(fit_l1_upset_final$fit$fit, num_features = 20)

# The L1‐regularized logistic regression models for predicting the higher-ranked player’s win 
# and predicting an upset select almost exactly the same set of important features.

# First, both models rely on the same underlying information to discriminate match outcomes. 
# Features such as diff_elo, diff_rank_points, diff_avg_sets_won_10, and serve-related rolling statistics represent skill gaps 
 #and short-term form differences between the two players. 
# These structural factors influence match outcomes regardless of whether the model is framed as predicting a win or predicting an upset. 
# Consequently, the L1 regularization process selects essentially the same variables in both cases, because both models are detecting the same sources of variation in competitive strength.

# But the exact magnitudes differ slightly.

extract_coef <- function(fit, penalty) {
  tidy(fit, penalty = penalty) %>%
    filter(term != "(Intercept)")
}

coef_l1_all <- extract_coef(fit_l1_final$fit$fit, best_l1$penalty)
coef_l1_up  <- extract_coef(fit_l1_upset_final$fit$fit, best_upset_l1$penalty)

compare_importance <- coef_l1_all %>%
  select(term, estimate_all = estimate) %>%
  full_join(
    coef_l1_up %>% select(term, estimate_up = estimate),
    by = "term"
  ) %>%
  replace_na(list(estimate_all = 0, estimate_up = 0)) %>%
  mutate(
    abs_all = abs(estimate_all),
    abs_up  = abs(estimate_up),
    diff = abs_up - abs_all
  ) %>%
  arrange(desc(abs(diff)))
compare_importance
# Which features become more important in the Upset task?
# Serve stability & volatility
# diff_avg_SvGms_10、diff_avg_svpt_10、diff_avg_bpFaced_10
# These variables capture recent serve performance: number of service games, total serve points, break points faced, and break points saved.
# Their coefficients are substantially larger in the upset model, indicating that:
# Upsets are highly sensitive to short-term volatility in serving performance.
# When the stronger player shows instability on serve or struggles under break-point pressure, the likelihood of an upset increases sharply.

# Set-level performance trends
# diff_avg_sets_won_10、diff_avg_game_diff_per_set_10
# These set-level rolling statistics quantify recent momentum in match dominance.
# Their increased weight in the upset model indicates that:
# Upsets depend more on short-term shifts in performance momentum.
# When the lower-ranked player improves recently or the favorite shows declining form, the probability of an upset rises.

# Sensitivity to ability gap
# diff_rank_points、z_gap、diff_elo
# Although skill-gap features matter in both models, their slightly larger coefficients in the upset model indicate:
# Upsets occur when long-term ability gaps appear large but short-term performance narrows unexpectedly.
# The upset model becomes more sensitive to this contradiction between structural ability and recent form.

# Player tier / ranking group
# P1_HL_Low、P2_HL_Low
# These dummy variables have higher influence in the upset model, indicating:
# Upsets are more likely in matches involving lower-tier players,
# especially when they show recent improvement or the favorite displays instability.

# Compared to the win-prediction model, the upset model relies more heavily on:
# Short-term variability in serving performance
# Recent momentum and set-level dominance trends
# Situations where long-term ability gaps temporarily shrink
# The involvement of lower-tier players
# These patterns show that upsets are not simply the inverse of wins; they are driven by volatility, momentum shifts, and pressure-point dynamics.

compare_importance %>%
  slice_max(order_by = abs(diff), n = 20) %>%
  pivot_longer(cols = c(estimate_all, estimate_up),
               names_to = "model", values_to = "coef") %>%
  ggplot(aes(x = reorder(term, coef), y = coef, color = model)) +
  geom_point(size = 3) +
  coord_flip() +
  labs(title = "Feature Importance: Win Model (L1) vs Upset Model (L1)",
       y = "Coefficient (L1 regularized)",
       x = "Feature")

compare_importance %>%
  slice_max(order_by = abs(diff), n = 20) %>%
  ggplot(aes(x = reorder(term, diff), y = diff)) +
  geom_col(fill = "#2B7A77") +
  coord_flip() +
  labs(title = "Difference in Importance (Upset L1 − Win L1)",
       y = "Difference in |Coefficient|",
       x = "Feature")

# Top unset case analysis

names(upset_eval_all)

diff_vars <- names(test_model_ready)[grepl("^diff_", names(test_model_ready))]

upset_eval_all_full <- base_test_eval %>%
  bind_cols(
    test_model_ready %>% select(all_of(diff_vars)),
    tibble(
      p_upset_logit      = upset_eval_all$p_upset_logit,
      p_upset_elo_online = upset_eval_all$p_upset_elo_online,
      p_upset_elo_frozen = upset_eval_all$p_upset_elo_frozen,
      p_upset_l1         = upset_eval_all$p_upset_l1,
      pred_upset_l1      = upset_eval_all$pred_upset_l1,
      p_upset_rf         = upset_eval_all$p_upset_rf,
      pred_upset_rf      = upset_eval_all$pred_upset_rf,
      y_upset            = upset_eval_all$y_upset,
      rank_group         = upset_eval_all$rank_group
    )
  )

top_n <- 100  

top_upsets <- upset_eval_all_full %>%
  filter(y_upset == 1) %>%                      
  slice_max(order_by = p_upset_l1, n = top_n)  

avg_top <- top_upsets %>%
  summarise(across(all_of(diff_vars), mean)) %>%
  mutate(type = "top_upset")

avg_all <- train_model_ready %>%
  summarise(across(all_of(diff_vars), mean)) %>%
  mutate(type = "overall_train")

avg_compare <- bind_rows(avg_top, avg_all) %>%
  tidyr::pivot_longer(cols = all_of(diff_vars),
                      names_to = "feature",
                      values_to = "value")

ggplot(avg_compare, aes(x = feature, y = value, fill = type)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Top-upset (test, by p_upset_l1) vs Overall (train) — diff features",
    x = "Feature",
    y = "Mean value",
    fill = ""
  )

avg_compare

corr_df <- train_model_ready %>%
  mutate(upset = if_else(p1_win == 0, 1, 0)) %>%
  select(upset, all_of(diff_vars))

corr_vec <- cor(corr_df, method = "spearman")["upset", ]  
corr_vec <- sort(corr_vec, decreasing = TRUE)

head(corr_vec, 20)  

# Although the Spearman correlations are relatively small (all < 0.20), this pattern is fully consistent with the nature of upset prediction, which is a rare, multi-factor and highly nonlinear event.
# In such settings, no single feature is expected to show strong linear correlation.
# Instead, predictive power arises from the interaction of multiple diff-features, as confirmed by the much higher AUC of the L1 and Random Forest models.


#——————————————————————— Model eda ——————————————————————
base_test_eval_ext <- base_test_eval %>%
  bind_cols(
    l1_test_eval %>% select(p_l1, pred_l1_class),
    rf_test_eval %>% select(p_rf, pred_rf_class))

base_test_eval_ext <- base_test_eval %>%
  bind_cols(
    l1_test_eval %>% select(p_l1, pred_l1_class),
    rf_test_eval %>% select(p_rf, pred_rf_class)
  )

prob_long_ext <- base_test_eval_ext %>%
  transmute(
    y_true = y_true_num,
    Logit      = p_logit,
    Elo_online = p_elo_online,
    Elo_frozen = p_elo_frozen,
    Logit_L1   = p_l1,
    RF_win     = p_rf
  ) %>%
  pivot_longer(
    cols = -y_true,
    names_to  = "model",
    values_to = "prob"
  )

ggplot(prob_long_ext, aes(x = prob, fill = factor(y_true))) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ model, ncol = 1) +
  scale_fill_manual(
    values = c("#E69F00", "#56B4E9"),
    name   = "Actual Outcome",
    labels = c("P1 lost (0)", "P1 won (1)")
  ) +
  labs(
    title = "Predicted Probability Distribution by Model",
    x = "Predicted Probability (P1 win)",
    y = "Density"
  )

calib_ext <- prob_long_ext %>%
  mutate(bin = ntile(prob, 10)) %>%
  group_by(model, bin) %>%
  summarise(
    avg_pred = mean(prob),
    avg_true = mean(y_true),
    .groups = "drop"
  )

ggplot(calib_ext, aes(x = avg_pred, y = avg_true, color = model)) +
  geom_point(size = 2) +
  geom_line(size = 0.9) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Calibration Curves (Logit / Elo / Logit_L1 / RF_win)",
    x = "Predicted Probability (bin average)",
    y = "Actual Win Rate",
    color = "Model"
  )

base_eval_long_ext <- bind_rows(
  base_eval_long,  
  base_test_eval_ext %>%
    transmute(
      rank_group,
      model    = "Logit_L1",
      y_true,
      y_true_num,
      p_hat    = p_l1,
      pred_cls = pred_l1_class
    ),
  base_test_eval_ext %>%
    transmute(
      rank_group,
      model    = "RF_win",
      y_true,
      y_true_num,
      p_hat    = p_rf,
      pred_cls = pred_rf_class
    )
)

base_metrics_by_group_ext <- base_eval_long_ext %>%
  group_by(rank_group, model) %>%
  summarise(
    n          = n(),
    accuracy   = accuracy_vec(y_true, pred_cls),
    log_loss   = mn_log_loss_vec(y_true, p_hat, event_level = "second"),
    roc_auc    = roc_auc_vec(y_true, p_hat, event_level = "second"),
    precision  = precision_vec(y_true, pred_cls, event_level = "second"),
    recall     = recall_vec(y_true, pred_cls, event_level = "second"),
    .groups = "drop"
  ) %>%
  mutate(rank_group = factor(
    rank_group,
    levels = c("High vs Low", "High vs High", "Low vs Low")
  ))

base_metrics_by_group_ext

p_acc <- ggplot(base_metrics_by_group_ext,
                aes(x = rank_group, y = accuracy, color = model, group = model)) +
  geom_line() +
  geom_point() +
  labs(title = "Accuracy by Rank Group",
       x = "Rank Group", y = "Accuracy", color = "Model")

p_auc <- ggplot(base_metrics_by_group_ext,
                aes(x = rank_group, y = roc_auc, color = model, group = model)) +
  geom_line() +
  geom_point() +
  labs(title = "ROC AUC by Rank Group",
       x = "Rank Group", y = "AUC", color = "Model")

p_logloss <- ggplot(base_metrics_by_group_ext,
                    aes(x = rank_group, y = log_loss, color = model, group = model)) +
  geom_line() +
  geom_point() +
  labs(title = "Log-loss by Rank Group",
       x = "Rank Group", y = "Log-loss (lower is better)", color = "Model")

p_acc
p_auc
p_logloss

# ——————————————————————————————upset eda——————————————————————————————————
upset_eval_all <- base_test_eval %>%
  bind_cols(test_full %>% select(rank_group)) %>%
  mutate(
    y_upset = if_else(y_true_num == 0, 1L, 0L),
    p_upset_logit      = 1 - p_logit,
    p_upset_elo_online = 1 - p_elo_online,
    p_upset_elo_frozen = 1 - p_elo_frozen
  ) %>%
  bind_cols(
    upset_l1_test_eval %>% 
      transmute(
        p_upset_l1   = p_l1,
        pred_upset_l1 = pred_l1_class
      ),
    upset_rf_test_eval %>%
      transmute(
        p_upset_rf    = p_rf,
        pred_upset_rf = pred_rf_class
      )
  )

str(upset_eval_all)

upset_prob_long <- upset_eval_all %>%
  transmute(
    y_upset = y_upset,  
    Logit_upset       = p_upset_logit,
    Elo_online_upset  = p_upset_elo_online,
    Elo_frozen_upset  = p_upset_elo_frozen,
    Logit_L1_upset    = p_upset_l1,
    RF_upset          = p_upset_rf
  ) %>%
  pivot_longer(
    cols = -y_upset,
    names_to  = "model",
    values_to = "prob_upset"
  )

ggplot(upset_prob_long, aes(x = prob_upset, fill = factor(y_upset))) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ model, ncol = 1) +
  scale_fill_manual(
    values = c("#56B4E9", "#E69F00"),
    name   = "Actual",
    labels = c("No upset (0)", "Upset (1)")
  ) +
  labs(
    title = "Predicted Upset Probability Distribution by Model",
    x = "Predicted upset probability",
    y = "Density"
  )

upset_prob_true_only <- upset_eval_all %>%
  filter(y_upset == 1) %>%
  transmute(
    Logit_upset       = p_upset_logit,
    Elo_online_upset  = p_upset_elo_online,
    Elo_frozen_upset  = p_upset_elo_frozen,
    Logit_L1_upset    = p_upset_l1,
    RF_upset          = p_upset_rf
  ) %>%
  pivot_longer(
    everything(),
    names_to  = "model",
    values_to = "prob_upset"
  )

ggplot(upset_prob_true_only, aes(x = prob_upset, fill = model)) +
  geom_density(alpha = 0.35) +
  labs(
    title = "Predicted Upset Probabilities (True upsets only)",
    x = "Predicted upset probability",
    y = "Density"
  )

make_upset_calibration_df <- function(pred_upset, y_upset, nbins = 10) {
  tibble(pred_upset = pred_upset, y_upset = y_upset) %>%
    mutate(bin = ntile(pred_upset, nbins)) %>%
    group_by(bin) %>%
    summarise(
      avg_pred = mean(pred_upset),       
      avg_true = mean(y_upset == 1),    
      n        = n(),
      .groups  = "drop"
    )
}

cal_logit_upset   <- make_upset_calibration_df(
  upset_eval_all$p_upset_logit, upset_eval_all$y_upset)
cal_online_upset  <- make_upset_calibration_df(
  upset_eval_all$p_upset_elo_online, upset_eval_all$y_upset)
cal_frozen_upset  <- make_upset_calibration_df(
  upset_eval_all$p_upset_elo_frozen, upset_eval_all$y_upset)
cal_l1_upset      <- make_upset_calibration_df(
  upset_eval_all$p_upset_l1, upset_eval_all$y_upset)
cal_rf_upset      <- make_upset_calibration_df(
  upset_eval_all$p_upset_rf, upset_eval_all$y_upset)

cal_upset_all <- bind_rows(
  mutate(cal_logit_upset,   model = "Logit"),
  mutate(cal_online_upset,  model = "Elo_online"),
  mutate(cal_frozen_upset,  model = "Elo_frozen"),
  mutate(cal_l1_upset,      model = "Logit_L1_upset"),
  mutate(cal_rf_upset,      model = "RF_upset")
)

ggplot(cal_upset_all, aes(x = avg_pred, y = avg_true, color = model)) +
  geom_line() +
  geom_point() +
  geom_abline(linetype = 2) +
  labs(
    title = "Calibration Curve for Upsets (all models)",
    x = "Predicted upset probability",
    y = "Actual upset rate"
  )


# upset metrics by rank_group ----------------------
upset_metrics_win_by_group <- base_eval_long_ext %>%
  group_by(rank_group, model) %>%
  summarise(
    n = n(),
    roc_auc_upset = roc_auc_vec(
      y_true,
      1 - p_hat,              
      event_level = "first"  
    ),
    precision_upset = precision_vec(
      y_true, pred_cls,
      event_level = "first"
    ),
    recall_upset = recall_vec(
      y_true, pred_cls,
      event_level = "first"
    ),
    .groups = "drop"
  ) %>%
  mutate(rank_group = factor(
    rank_group,
    levels = c("High vs Low", "High vs High", "Low vs Low")
  ))

upset_metrics_win_by_group

# Upset=1 Model EDA
upset_eval_all <- upset_eval_all %>%
  rename(rank_group = rank_group...9) %>%  
  select(-rank_group...10)

upset_eval_only_long <- bind_rows(
  upset_eval_all %>%
    transmute(
      rank_group,
      model      = "Logit_L1_upset",
      y_upset    = factor(y_upset, levels = c(0, 1)),
      p_upset    = p_upset_l1,
      pred_upset = pred_upset_l1
    ),
  upset_eval_all %>%
    transmute(
      rank_group,
      model      = "RF_upset",
      y_upset    = factor(y_upset, levels = c(0, 1)),
      p_upset    = p_upset_rf,
      pred_upset = pred_upset_rf
    )
)

# 所有比赛
ggplot(upset_eval_only_long,
       aes(x = p_upset, fill = y_upset)) +
  geom_density(alpha = 0.4) +
  facet_wrap(~ model, ncol = 1) +
  scale_fill_manual(
    values = c("#56B4E9", "#E69F00"),
    name   = "Actual",
    labels = c("No upset (0)", "Upset (1)")
  ) +
  labs(
    title = "Upset-only models: probability distribution",
    x = "Predicted upset probability",
    y = "Density"
  )

# 只看真实爆冷
upset_only_true <- upset_eval_only_long %>%
  filter(y_upset == 1)

ggplot(upset_only_true,
       aes(x = p_upset, fill = model)) +
  geom_density(alpha = 0.35) +
  labs(
    title = "Upset-only models: probabilities for TRUE upsets",
    x = "Predicted upset probability",
    y = "Density"
  )

make_calib_upset_only <- function(df, nbins = 10) {
  df %>%
    mutate(bin = ntile(p_upset, nbins)) %>%
    group_by(model, bin) %>%
    summarise(
      avg_pred = mean(p_upset),
      avg_true = mean(y_upset == 1),
      n        = n(),
      .groups = "drop"
    )
}

cal_upset_only <- make_calib_upset_only(upset_eval_only_long)

ggplot(cal_upset_only,
       aes(x = avg_pred, y = avg_true, color = model)) +
  geom_line() +
  geom_point() +
  geom_abline(linetype = 2) +
  labs(
    title = "Calibration Curves (Upset-only models)",
    x = "Predicted upset probability",
    y = "Actual upset rate",
    color = "Model"
  )


upset_metrics_only_by_group <- upset_eval_only_long %>%
  group_by(rank_group, model) %>%
  summarise(
    n = n(),
    roc_auc_upset = roc_auc_vec(
      y_upset, p_upset,
      event_level = "second"   # class "1" = upset
    ),
    precision_upset = precision_vec(
      y_upset, pred_upset,
      event_level = "second"
    ),
    recall_upset = recall_vec(
      y_upset, pred_upset,
      event_level = "second"
    ),
    .groups = "drop"
  ) %>%
  mutate(rank_group = factor(
    rank_group,
    levels = c("High vs Low", "High vs High", "Low vs Low")
  ))

upset_metrics_only_by_group

# After adding the diff features and the rolling form features (Logit_L1 / RF), 
# the performance gap between the three rank groups becomes much smaller.
# This shows that the same set of features and one global model work well for all rank groups.

# Most of the difficulty in Low vs Low matches came from the baseline models not using any information about player form.
# Once rolling serve stats, activity levels, and diff_elo were added, 
# the predictability of low-ranked matches improved and became similar to the other groups.
# There is no sign that any rank group requires a completely different model.
# Instead, the ranking of model performance is consistent across all groups.

# The upset-only model and the approach of using 1 − p_win from the win model give almost the same performance.
# Their behaviour is also very similar across the three rank groups.
# This means the global win model already learns the structure needed to detect upsets.

# Low-ranked matches are harder to predict, but this difficulty can be reduced by good features and a strong global model.
# There is no need to train separate models for each rank group.
 








