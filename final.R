# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------- Set up Workspace -----------------------------------------------
# ---------------------------------------------------------------------------------------------------------

setwd("C:/Users/natha/OneDrive - Coventry University/University/University Year 3/331MP Project/Analysis")

#install.packages("kableExtra")
#install.packages("caret", dependencies = TRUE)

library(readr)
library(dplyr)
library(lubridate)
library(kableExtra)
library(ggplot2)
library(car)
library(caret)
library(ResourceSelection)

# ---------------------------------------------------------------------------------------------------------
# -------------------------------------------- Data Load --------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

# Results from 19/20 season

results1920 <- read_csv("results_19_20.csv") %>%
  # Add Covid flag
  mutate(date=dmy(Date)) %>% 
  mutate(COVID = ifelse(date > as.Date("2020-03-13"), "Post", "Pre")) %>%
  select(home=HomeTeam, away=AwayTeam, result=FTR, COVID) %>%
  # Remove newly Promoted Teams
  filter(!home %in% c("Aston Villa", "Norwich", "Sheffield United") &
           !away %in% c("Aston Villa", "Norwich", "Sheffield United"))

# Standings from 18/19 season

table1819 <- read_csv("table_18_19.csv") %>% 
  # Remove Relagated Teams
  filter(Position <= 17) %>% 
  select(team=Team, position=Position, points=Pts, gd=GD, goalsfor=F, goalsagainst=A)

# ---------------------------------------------------------------------------------------------------------
# --------------------------------------- Data Manipulation -----------------------------------------------
# ---------------------------------------------------------------------------------------------------------

analysis <- results1920 %>% 
  mutate(result = case_when(
    result == "H" ~ "Win",
    TRUE ~ "Loss/Draw"
  )) %>%
  mutate(result_bin = ifelse(result=="Win", 1, 0)) %>% 
  left_join(select(table1819, team, home_pos=position, home_pts=points, home_gd=gd,
                   home_goalsfor=goalsfor, home_goalsagainst=goalsagainst), 
            by=c("home"="team")) %>% 
  left_join(select(table1819, team, away_pos=position, away_pts=points, away_gd=gd,
                   away_goalsfor=goalsfor, away_goalsagainst=goalsagainst), 
            by=c("away"="team"))

analysis$result <- factor(analysis$result, levels = c("Loss/Draw", "Win"))
analysis$COVID <- factor(analysis$COVID, levels = c("Pre", "Post"))

# ---------------------------------------------------------------------------------------------------------
# ------------------------------------- Prelimineary Analysis ---------------------------------------------
# ---------------------------------------------------------------------------------------------------------

# ----------------------------------------- Results Breakdown ---------------------------------------------

# Frequencies of Result for home team - Win/Draw/Loss

t_wins <- results1920 %>%
  mutate(result = case_when(
    result == "H" ~ "Win",
    result == "D" ~ "Draw",
    result == "A" ~ "Loss",
    TRUE ~ ""
  )) %>%
  group_by(result) %>% 
  summarise(n = n(), .groups="drop") %>% 
  mutate(win_proportion = round(n / sum(n), 2)) %>% 
  arrange(desc(n)) %>% 
  kbl(caption = "Distrubution of home results (19/20)", 
      col.names=c("Result", "Count", "Proportion")) %>% 
  kable_classic(full_width = F)

t_wins

# Binary frequencies of result for home team - Win/Otherwise

t_wins_bin <- analysis %>%
  group_by(result) %>% 
  summarise(n = n(), .groups="drop") %>% 
  mutate(win_proportion = round(n / sum(n), 2)) %>% 
  arrange(desc(result)) %>% 
  kbl(caption = "Distrubution of binary home results (19/20)", col.names=c("Result", "Count", "Proportion")) %>% 
  kable_classic(full_width = F)

t_wins_bin

# ----------------------------------------- Home Position ---------------------------------------------

# Frequencies of Result - By Home Position

wins_home_pos <- analysis %>%
  group_by(home_pos, result) %>% 
  summarise(wins = n(), .groups="drop_last") %>% 
  mutate(games_played = sum(wins)) %>% 
  mutate(win_proportion = round(wins / sum(wins), 2)) %>%
  mutate(win_logit = round(log(wins / (games_played - wins)), 2)) %>%
  filter(result == "Win") %>%
  select(-result)

t_wins_home_pos <- wins_home_pos %>% 
  select(home_pos, wins, games_played, win_proportion, win_logit) %>% 
  kbl(caption = "Number of home wins (19/20) by home team's previous season position (18/19)", 
      col.names=c("Position", "Wins", "Played", "Proportion", "Logit")) %>% 
  kable_classic(full_width = F)

t_wins_home_pos

# Proportion (Wins) for home team against Home Position

plot(x=wins_home_pos$home_pos, y=wins_home_pos$win_proportion,
     xlab="Home Position (18/19)", ylab="Home Win Proportion (19/20)",
     main="Proportion of home wins (19/20) by home team's previous season position (18/19)")

abline(lm(wins_home_pos$win_proportion ~ wins_home_pos$home_pos))

# Logit (Wins) for home team against Home Position 

plot(x=wins_home_pos$home_pos, y=wins_home_pos$win_logit,
     xlab="Home Position (18/19)", ylab="Logit (Home Wins 19/20)",
     main="Logit home wins (19/20) by home team's previous season position (18/19)")

abline(lm(wins_home_pos$win_logit ~ wins_home_pos$home_pos))

# ----------------------------------------- Away Positon ---------------------------------------------

# Frequencies of Result - By away Position

wins_away_pos <- analysis %>%
  group_by(away_pos, result) %>% 
  summarise(wins = n(), .groups="drop_last") %>% 
  mutate(games_played = sum(wins)) %>% 
  mutate(win_proportion = round(wins / sum(wins), 2)) %>%
  mutate(win_logit = round(log(wins / (games_played - wins)), 2)) %>%
  filter(result == "Win") %>%
  select(-result)

t_wins_away_pos <- wins_away_pos %>% 
  select(away_pos, wins, games_played, win_proportion, win_logit) %>% 
  kbl(caption = "Number of home wins (19/20) by away team's previous season position (18/19)", 
      col.names=c("Position", "Wins", "Played", "Proportion", "Logit")) %>% 
  kable_classic(full_width = F)

t_wins_away_pos

# Proportion (Wins) for home team against away position

plot(x=wins_away_pos$away_pos, y=wins_away_pos$win_proportion,
     xlab="Away Position (18/19)", ylab="Home Win Proportion (19/20)",
     main="Proportion of home wins (19/20) by away team's previous season position (18/19)")

abline(lm(wins_away_pos$win_proportion ~ wins_away_pos$away_pos))

# Logit (Wins) for home team against away Position 

plot(x=wins_away_pos$away_pos, y=wins_away_pos$win_logit,
     xlab="Away Position (18/19)", ylab="Logit (Home Wins 19/20)",
     main="Logit home wins (19/20) by away team's previous season position (18/19)")

abline(lm(wins_away_pos$win_logit ~ wins_away_pos$away_pos))

# -------------------------------------- Home Points ---------------------------------------------

# Frequencies of Result - By Home Points

wins_home_pts <- analysis %>%
  group_by(home_pts, result) %>% 
  summarise(wins = n(), .groups="drop_last") %>% 
  mutate(games_played = sum(wins)) %>% 
  mutate(win_proportion = round(wins / sum(wins), 2)) %>%
  mutate(win_logit = round(log(wins / (games_played - wins)), 2)) %>%
  arrange(home_pts) %>% 
  filter(result == "Win") %>%
  select(-result)

t_wins_home_pts <- wins_home_pts %>% 
  select(home_pts, wins, games_played, win_proportion, win_logit) %>% 
  kbl(caption = "Number of home wins (19/20) by home team's previous season points (18/19)", 
      col.names=c("Points", "Wins", "Played", "Proportion", "Logit")) %>% 
  kable_classic(full_width = F)

t_wins_home_pts

# Proportion (Wins) against Home Points

plot(x=wins_home_pts$home_pts, y=wins_home_pts$win_proportion,
     xlab="Home Points (18/19)", ylab="Home win Proportion (19/20)",
     main="Proportion of home wins (19/20) by home team's previous season points (18/19)")

abline(lm(wins_home_pts$win_proportion ~ wins_home_pts$home_pts))

# Logit (Wins) against Home Points

plot(x=wins_home_pts$home_pts, y=wins_home_pts$win_logit,
     xlab="Home Points (18/19)", ylab="Logit (Home wins 19/20)",
     main="Logit home wins (19/20) by home team's previous season points (18/19)")

abline(lm(wins_home_pts$win_logit ~ wins_home_pts$home_pts))

# -------------------------------------- Away Points ---------------------------------------------

# Frequencies of Result - By Away Points

wins_away_pts <- analysis %>%
  group_by(away_pts, result) %>% 
  summarise(wins = n(), .groups="drop_last") %>% 
  mutate(games_played = sum(wins)) %>% 
  mutate(win_proportion = round(wins / sum(wins), 2)) %>%
  mutate(win_logit = round(log(wins / (games_played - wins)), 2)) %>%
  arrange(away_pts) %>% 
  filter(result == "Win") %>%
  select(-result)

t_wins_away_pts <- wins_away_pts %>% 
  select(away_pts, wins, games_played, win_proportion, win_logit) %>% 
  kbl(caption = "Number of home wins (19/20) by away team's previous season points (18/19)", 
      col.names=c("Points", "Wins", "Played", "Proportion", "Logit")) %>% 
  kable_classic(full_width = F)

t_wins_away_pts

# Proportion (Wins) against Away Points

plot(x=wins_away_pts$away_pts, y=wins_away_pts$win_proportion,
     xlab="Away Points (18/19)", ylab="Home win Proportion (19/20)",
     main="Proportion of home wins (19/20) by away team's previous season points (18/19)")

abline(lm(wins_away_pts$win_proportion ~ wins_away_pts$away_pts))

# Logit (Wins) against Home Points

plot(x=wins_away_pts$away_pts, y=wins_away_pts$win_logit,
     xlab="Away Points (18/19)", ylab="Logit (Home wins 19/20)",
     main="Logit home wins (19/20) by away team's previous season points (18/19)")

abline(lm(wins_away_pts$win_logit ~ wins_away_pts$away_pts))

# ------------------------------------- Home Goal Difference -----------------------------------------

# Frequencies of Result - By Home Goal Difference

wins_home_gd <- analysis %>%
  group_by(home_gd, result) %>% 
  summarise(wins = n(), .groups="drop_last") %>% 
  mutate(games_played = sum(wins)) %>% 
  mutate(win_proportion = round(wins / sum(wins), 2)) %>%
  mutate(win_logit = round(log(wins / (games_played - wins)), 2)) %>%
  arrange(home_gd) %>% 
  filter(result == "Win") %>%
  select(-result)

t_wins_home_gd <- wins_home_gd %>% 
  select(home_gd, wins, games_played, win_proportion, win_logit) %>% 
  kbl(caption = "Number of home wins (19/20) by home team's previous season goal difference (18/19)", 
      col.names=c("Goal Diff", "Wins", "Played", "Proportion", "Logit")) %>% 
  kable_classic(full_width = F)

t_wins_home_gd

# Proportion (Wins) against Home Goal Difference

plot(x=wins_home_gd$home_gd, y=wins_home_gd$win_proportion,
     xlab="Home Goal Diff (18/19)", ylab="Home win Proportion (19/20)",
     main="Proportion of home wins (19/20) by home team's previous season goal difference (18/19)")

abline(lm(wins_home_gd$win_proportion ~ wins_home_gd$home_gd))

# Logit (Wins) against Home Goal Difference

plot(x=wins_home_gd$home_gd, y=wins_home_gd$win_logit,
     xlab="Home Goal Diff (18/19)", ylab="Logit (Home wins 19/20)",
     main="Logit home wins (19/20) by home team's previous season goal difference (18/19)")

abline(lm(wins_home_gd$win_logit ~ wins_home_gd$home_gd))

# ------------------------------------- Away Goal Difference -----------------------------------------

# Frequencies of Result - By Away Goal Difference

wins_away_gd <- analysis %>%
  group_by(away_gd, result) %>% 
  summarise(wins = n(), .groups="drop_last") %>% 
  mutate(games_played = sum(wins)) %>% 
  mutate(win_proportion = round(wins / sum(wins), 2)) %>%
  mutate(win_logit = round(log(wins / (games_played - wins)), 2)) %>%
  arrange(away_gd) %>% 
  filter(result == "Win") %>%
  select(-result)

t_wins_away_gd <- wins_away_gd %>% 
  select(away_gd, wins, games_played, win_proportion, win_logit) %>% 
  kbl(caption = "Number of home wins (19/20) by away team's previous season goal difference (18/19)", 
      col.names=c("Goal Diff", "Wins", "Played", "Proportion", "Logit")) %>% 
  kable_classic(full_width = F)

t_wins_away_gd

# Proportion (Wins) against Away Goal Difference

plot(x=wins_away_gd$away_gd, y=wins_away_gd$win_proportion,
     xlab="Away Goal Diff (18/19)", ylab="Home win Proportion (19/20)",
     main="Proportion of home wins (19/20) by away team's previous season goal difference (18/19)")

abline(lm(wins_away_gd$win_proportion ~ wins_away_gd$away_gd))

# Logit (Wins) against Away Goal Difference

plot(x=wins_away_gd$away_gd, y=wins_away_gd$win_logit,
     xlab="Away Goal Diff (18/19)", ylab="Logit (Home wins 19/20)",
     main="Logit home wins (19/20) by away team's previous season goal difference (18/19)")

abline(lm(wins_away_gd$win_logit ~ wins_away_gd$away_gd))

# ------------------------------------- Home Goals For -----------------------------------------

# Frequencies of Result - By Home Goals For

wins_home_goalsfor <- analysis %>%
  group_by(home_goalsfor, result) %>% 
  summarise(wins = n(), .groups="drop_last") %>% 
  mutate(games_played = sum(wins)) %>% 
  mutate(win_proportion = round(wins / sum(wins), 2)) %>%
  mutate(win_logit = round(log(wins / (games_played - wins)), 2)) %>%
  arrange(home_goalsfor) %>% 
  filter(result == "Win") %>%
  select(-result)

t_wins_home_goalsfor <- wins_home_goalsfor %>% 
  select(home_goalsfor, wins, games_played, win_proportion, win_logit) %>% 
  kbl(caption = "Number of home wins (19/20) by home team's previous season goals for (18/19)", 
      col.names=c("Goals For", "Wins", "Played", "Proportion", "Logit")) %>% 
  kable_classic(full_width = F)

t_wins_home_goalsfor

# Proportion (Wins) against Home Goals For

plot(x=wins_home_goalsfor$home_goalsfor, y=wins_home_goalsfor$win_proportion,
     xlab="Home goals for (18/19)", ylab="Home win Proportion (19/20)",
     main="Proportion of home wins (19/20) by home team's previous season goals for (18/19)")

abline(lm(wins_home_goalsfor$win_proportion ~ wins_home_goalsfor$home_goalsfor))

# Logit (Wins) against Home Goals For

plot(x=wins_home_goalsfor$home_goalsfor, y=wins_home_goalsfor$win_logit,
     xlab="Home goals for (18/19)", ylab="Logit (Home wins 19/20)",
     main="Logit home wins (19/20) by home team's previous season goals for (18/19)")

abline(lm(wins_home_goalsfor$win_logit ~ wins_home_goalsfor$home_goalsfor))

# ------------------------------------- Away Goals For -----------------------------------------

# Frequencies of Result - By Home Goals For

wins_away_goalsfor <- analysis %>%
  group_by(away_goalsfor, result) %>% 
  summarise(wins = n(), .groups="drop_last") %>% 
  mutate(games_played = sum(wins)) %>% 
  mutate(win_proportion = round(wins / sum(wins), 2)) %>%
  mutate(win_logit = round(log(wins / (games_played - wins)), 2)) %>%
  arrange(away_goalsfor) %>% 
  filter(result == "Win") %>%
  select(-result)

t_wins_away_goalsfor <- wins_away_goalsfor %>% 
  select(away_goalsfor, wins, games_played, win_proportion, win_logit) %>% 
  kbl(caption = "Number of home wins (19/20) by away team's previous season goals for (18/19)", 
      col.names=c("Goals For", "Wins", "Played", "Proportion", "Logit")) %>% 
  kable_classic(full_width = F)

t_wins_away_goalsfor

# Proportion (Wins) against Home Goals For

plot(x=wins_away_goalsfor$away_goalsfor, y=wins_away_goalsfor$win_proportion,
     xlab="Away goals for (18/19)", ylab="Home win Proportion (19/20)",
     main="Proportion of home wins (19/20) by away team's previous season goals for (18/19)")

abline(lm(wins_away_goalsfor$win_proportion ~ wins_away_goalsfor$away_goalsfor))

# Logit (Wins) against Home Goals For

plot(x=wins_away_goalsfor$away_goalsfor, y=wins_away_goalsfor$win_logit,
     xlab="Away goals for (18/19)", ylab="Logit (Home wins 19/20)",
     main="Logit home wins (19/20) by away team's previous season goals for (18/19)")

abline(lm(wins_away_goalsfor$win_logit ~ wins_away_goalsfor$away_goalsfor))

# ------------------------------------- Home Goals Against -----------------------------------------

# Frequencies of Result - By Home Goals Against

wins_home_goalsagainst <- analysis %>%
  group_by(home_goalsagainst, result) %>% 
  summarise(wins = n(), .groups="drop_last") %>% 
  mutate(games_played = sum(wins)) %>% 
  mutate(win_proportion = round(wins / sum(wins), 2)) %>%
  mutate(win_logit = round(log(wins / (games_played - wins)), 2)) %>%
  arrange(home_goalsagainst) %>% 
  filter(result == "Win") %>%
  select(-result)

t_wins_home_goalsagainst <- wins_home_goalsagainst %>% 
  select(home_goalsagainst, wins, games_played, win_proportion, win_logit) %>% 
  kbl(caption = "Number of home wins (19/20) by home team's previous season goals against (18/19)", 
      col.names=c("Goals Against", "Wins", "Played", "Proportion", "Logit")) %>% 
  kable_classic(full_width = F)

t_wins_home_goalsagainst

# Proportion (Wins) against Home Goals Against

plot(x=wins_home_goalsagainst$home_goalsagainst, y=wins_home_goalsagainst$win_proportion,
     xlab="Home goals against (18/19)", ylab="Home win Proportion (19/20)",
     main="Proportion of home wins (19/20) by home team's previous season goals against (18/19)")

abline(lm(wins_home_goalsagainst$win_proportion ~ wins_home_goalsagainst$home_goalsagainst))

# Logit (Wins) against Home Goals against

plot(x=wins_home_goalsagainst$home_goalsagainst, y=wins_home_goalsagainst$win_logit,
     xlab="Home goals against (18/19)", ylab="Logit (Home wins 19/20)",
     main="Logit home wins (19/20) by home team's previous season goals against (18/19)")

abline(lm(wins_home_goalsagainst$win_logit ~ wins_home_goalsagainst$home_goalsagainst))

# ------------------------------------- Away Goals Against -----------------------------------------

# Frequencies of Result - By Home Goals against

wins_away_goalsagainst <- analysis %>%
  group_by(away_goalsagainst, result) %>% 
  summarise(wins = n(), .groups="drop_last") %>% 
  mutate(games_played = sum(wins)) %>% 
  mutate(win_proportion = round(wins / sum(wins), 2)) %>%
  mutate(win_logit = round(log(wins / (games_played - wins)), 2)) %>%
  arrange(away_goalsagainst) %>% 
  filter(result == "Win") %>%
  select(-result)

t_wins_away_goalsagainst <- wins_away_goalsagainst %>% 
  select(away_goalsagainst, wins, games_played, win_proportion, win_logit) %>% 
  kbl(caption = "Number of home wins (19/20) by away team's previous season goals against (18/19)", 
      col.names=c("Goals Against", "Wins", "Played", "Proportion", "Logit")) %>% 
  kable_classic(full_width = F)

t_wins_away_goalsagainst

# Proportion (Wins) against Home Goals against

plot(x=wins_away_goalsagainst$away_goalsagainst, y=wins_away_goalsagainst$win_proportion,
     xlab="Away goals against (18/19)", ylab="Home win Proportion (19/20)",
     main="Proportion of home wins (19/20) by away team's previous season goals against (18/19)")

abline(lm(wins_away_goalsagainst$win_proportion ~ wins_away_goalsagainst$away_goalsagainst))

# Logit (Wins) against Home Goals against

plot(x=wins_away_goalsagainst$away_goalsagainst, y=wins_away_goalsagainst$win_logit,
     xlab="Away goals against (18/19)", ylab="Logit (Home wins 19/20)",
     main="Logit home wins (19/20) by away team's previous season goals against (18/19)")

abline(lm(wins_away_goalsagainst$win_logit ~ wins_away_goalsagainst$away_goalsagainst))

# ---------------------------------------- COVID ----------------------------------------------------

# Frequencies of Result - By COVID ----------------------------------------------------------------

wins_covid <- analysis %>%
  group_by(COVID, result) %>% 
  summarise(wins = n(), .groups="drop_last") %>% 
  mutate(games_played = sum(wins)) %>% 
  mutate(win_proportion = round(wins / sum(wins), 2)) %>% 
  mutate(win_logit = round(log(wins / (games_played - wins)), 2)) %>%
  filter(result == "Win") %>%
  select(-result)

t_wins_covid <- wins_covid %>% 
  select(COVID, wins, games_played, win_proportion, win_logit) %>% 
  kbl(caption = "Number of home wins (19/20) pre/post COVID lockdown", 
      col.names=c("COVID", "Wins", "Played", "Proportion", "Logit")) %>% 
  kable_classic(full_width = F)

t_wins_covid

# Proportion (Wins) against COVID --------------------------------------------------------------------

barplot(win_proportion ~ COVID, data = wins_covid, main="Proportion of home Wins (19/20) by Pre/Post COVID lockdown",
        xlab="COVID Lockdown", ylab="Win Proportion (19/20)")

# Logit (Wins) against COVID --------------------------------------------------------------------

barplot(win_logit ~ COVID, data = wins_covid, main="Logit home Wins (19/20) by Pre/Post COVID lockdown",
        xlab="COVID Lockdown", ylab="Win Logit (19/20)")

# ---------------------------------------------------------------------------------------------------------
# ------------------------------------------- Measure of Strength -----------------------------------------
# ---------------------------------------------------------------------------------------------------------

# Testing for correaltions between measures of strength 
round(cor(select(analysis, home_pos, home_pts, home_gd, home_goalsfor, home_goalsagainst)), 2)
round(cor(select(analysis, away_pos, away_pts, away_gd, away_goalsfor, away_goalsagainst)), 2)

# Clearly, these measures of strength are hihgly correlated so we can not use all of them in our model. So, we must now decide
# which measure of strength is the most predctive; standings, points, goal difference, goals for, goals against, goals for + 
# goals against. We do this by fitting the models and choosing the model with the lowest AIC.

win.glm_1 <- glm(result ~ home_pos + away_pos, data=analysis, family = "binomial")
summary(win.glm_1)
# AIC = 343.97

win.glm_2 <- glm(result ~ home_pts + away_pts, data=analysis, family = "binomial")
summary(win.glm_2)
# AIC = 338.91

win.glm_3 <- glm(result ~ home_gd + away_gd, data=analysis, family = "binomial")
summary(win.glm_3)
# AIC = 339.1

win.glm_4 <- glm(result ~ home_goalsfor + home_goalsagainst + away_goalsfor + away_goalsagainst, data=analysis, family = "binomial")
summary(win.glm_4)
# AIC = 342.05

# Model 4 does not have all variables significant, so we exclude this model. From the remaining models, we choose model 2 as it has 
# the lowest AIC value. So, we use number of points last season as our measure of strength.

# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------- Model Selection ------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

# One Variable Models

win.glm_5 <- glm(result ~ home_pts, data=analysis, family = "binomial")
summary(win.glm_5)
# AIC = 342.36

win.glm_6 <- glm(result ~ away_pts, data=analysis, family = "binomial")
summary(win.glm_6)
# AIC = 371.19

win.glm_7 <- glm(result ~ COVID, data=analysis, family = "binomial")
summary(win.glm_7)
# AIC = 377.73
# Not all variables significant - exclude

# So, we choose model 5 as our best current model as it has lowest AIC and all variables significant.

# Two variable models

win.glm_8 <- glm(result ~ home_pts + away_pts, data=analysis, family = "binomial")
summary(win.glm_8)
# AIC = 338.91

win.glm_9 <- glm(result ~ home_pts + COVID, data=analysis, family = "binomial")
summary(win.glm_9)
# AIC = 344.33
# Not all variables significant - exclude

win.glm_10 <- glm(result ~ away_pts + COVID, data=analysis, family = "binomial")
summary(win.glm_10)
# AIC = 373.15
# Not all variables significant - exclude

# So, we choose model 8 because it has the lowest AIC and all variables significant.

# Three variable models

win.glm_11 <- glm(result ~ home_pts + away_pts + COVID, data=analysis, family = "binomial")
summary(win.glm_11)
# AIC = 340.87
# Not all variables significant - exclude

# Hence, model 8 is still our best model. Now, we try adding interaction to this model.

win.glm_12 <- glm(result ~ home_pts + away_pts + home_pts*away_pts, data=analysis, family = "binomial")
summary(win.glm_12)
# AIC = 340.69
anova(win.glm_8, win.glm_12, test="Chisq")
# P Value > 0.05 so conclude model 8 is better than model with interaction.

# Hence, model 8 is the best model

win.glm <- glm(result ~ home_pts + away_pts, data=analysis, family = "binomial")
summary(win.glm)

# ---------------------------------------------------------------------------------------------------------
# ----------------------------------- Interpretatation  --------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

# Log(Odds Of Home Win) = -1.767002 + 0.043804(home_pts) - 0.017441(away_pts)

# All P Values are less than 0.05 which tells us there is evidence that home points and away points
# are significant predicttors of the home team winnng.

exp(coef(win.glm))
exp(confint(win.glm))

# Odds Ratio for home points:
# For a match against the same away team (same away points), for every 1 point more
# the home team had in the previous season, the odds of the home team winning is 4.5% higher.
# We can see the 95% CI for odds ratio us (1.029, 1.062) which does not include 1 so we can be 95% sure that 
# this is a significant predictor of the reference team winning.

# Odds Ratio for away points:
# For a match against the same home team (same home points), for every 1 point 
# more the away team had in the previous season, the odds of the home team winning is 1.7% lower.
# We can see the 95% CI for odds ratio us (0.968, 0.997) which does not include 1 so we can be 95% sure that 
# this is a significant predictor of the reference team winning.

# ---------------------------------------------------------------------------------------------------------
# -------------------------------------- Predictions - ----------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

predict(win.glm, newdata=data.frame(home_pts=100, away_pts=50), type = "response")
predict(win.glm, newdata=data.frame(home_pts=50, away_pts=50), type = "response")
predict(win.glm, newdata=data.frame(home_pts=50, away_pts=100), type = "response")

# ---------------------------------------------------------------------------------------------------------
# -------------------------------------- Fitted Value Plots -----------------------------------------------
# ---------------------------------------------------------------------------------------------------------

# Win proportion against home points - playing against a team with average away points

# obs_data_home_pts <- analysis %>%
#   group_by(home_pts, result) %>% 
#   summarise(wins = n(), .groups="drop_last") %>% 
#   mutate(games_played = sum(wins)) %>% 
#   mutate(win_proportion = round(wins / sum(wins), 2)) %>%
#   mutate(win_logit = round(log(wins / (games_played - wins)), 2)) %>%
#   arrange(home_pts) %>% 
#   filter(result == "Win") %>%
#   select(-result)
# 
# homepts_val <- seq(from=min(obs_data_home_pts$home_pts), to=max(obs_data_home_pts$home_pts), by=.1)
# awaypts_val <- mean(analysis$away_pts)
# 
# home <- predict(win.glm, newdata=data.frame(reference_points=refpts_val, adverse_points=rep(advpts_val, length(refpts_val)),
#                                             location=rep("Home", length(refpts_val))), type="response")
# away <- predict(win.glm, newdata=data.frame(reference_points=refpts_val, adverse_points=rep(advpts_val, length(refpts_val)),
#                                             location=rep("Away", length(refpts_val))), type="response")
# 
# plot(x=obs_data_ref_pts$reference_points, y=obs_data_ref_pts$win_proportion,
#      xlab="Reference Points (18/19)", ylab="Win Proportion (19/20)",
#      main="Proportion of Wins (19/20) for the Reference Team by their Previous Season
#      Points (18/19), given that the Adverse Team had the Average Number of Previous
#      Season Points - 58.4",
#      col=c("red", "blue")[obs_data_ref_pts$location])
# 
# lines(refpts_val, home, col="blue", type="l", lwd=2, lty=2)
# lines(refpts_val, away, col="red", type="l", lwd=2, lty=2)
# 
# legend('topleft', legend=c("Observed - Home", "Observed - Away", "FItted - Home", "Fitted - Away"), col=c("blue", "red"),
#        lty=c(NA,NA,2,2), pch=c(1,1,NA,NA), cex=0.8)







