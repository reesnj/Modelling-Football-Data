# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------- Set up Workspace -----------------------------------------------
# ---------------------------------------------------------------------------------------------------------

setwd("./logistic_regression/data")

# install.packages("kableExtra")
# install.packages("caret", dependencies = TRUE)
# install.packages("OneR")
# install.packages("geosphere")
# install.packages("corrplot")

library(geosphere)
library(readr)
library(dplyr)
library(lubridate)
library(kableExtra)
library(corrplot)
library(ggplot2)
library(car)
library(caret)
library(OneR)
library(ResourceSelection)
library(pROC)

# ---------------------------------------------------------------------------------------------------------
# ------------------------------------ Data Load (19/20) - Training Data ----------------------------------
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

# Calculating distances between stadiums
teams1920 <- unique(results1920$home)

stadiums1920 <- read_csv("stadiums.csv") %>% 
  filter(Team %in% teams1920) %>% 
  select(team=Team, lat=Latitude, long=Longitude)

stadiums1920_long <- results1920 %>% 
  left_join(select(stadiums1920, team, home_lat=lat, home_long=long), 
            by=c("home"="team")) %>% 
  left_join(select(stadiums1920, team, away_lat=lat, away_long=long), 
            by=c("away"="team")) %>% 
  select(home, away, home_long, home_lat, away_long, away_lat)

stadiums1920_long$distance <- distHaversine(stadiums1920_long[,3:4], stadiums1920_long[,5:6])/1000

# ---------------------------------------------------------------------------------------------------------
# ------------------------------ Data Manipulation - Training Data ----------------------------------------
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
            by=c("away"="team")) %>% 
  left_join(select(stadiums1920_long, home, away, distance), 
            by=c("home"="home", "away"="away")) %>% 
  mutate(distance = case_when(
    distance <= 100 ~ "Short",
    distance > 100  ~ "Long",
    TRUE ~ "Missing"
  ))

analysis$result <- factor(analysis$result, levels = c("Loss/Draw", "Win"))
analysis$COVID <- factor(analysis$COVID, levels = c("Pre", "Post"))
analysis$distance <- factor(analysis$distance, levels = c("Short", "Long"))

# ---------------------------------------------------------------------------------------------------------
# ------------------------------------- Prelimineary Analysis ---------------------------------------------
# ---------------------------------------------------------------------------------------------------------

# ----------------------------------------- Results Breakdown ---------------------------------------------

#### Table 1 ###

# Binary frequencies of result for home team - Win/Otherwise
t_wins_bin <- analysis %>%
  group_by(result) %>% 
  summarise(n = n(), .groups="drop") %>% 
  mutate(win_proportion = round(n / sum(n), 2)) %>% 
  arrange(desc(result)) %>% 
  kbl(caption = "Distribution of results (binary) for the home team (19/20)", col.names=c("Result", "Count", "Proportion")) %>% 
  kable_classic(full_width = F)
t_wins_bin

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
     xlab="Home Team's Points (18/19)", ylab="Home win Proportion (19/20)",
     main="Proportion of home wins (19/20) by home team's previous season points (18/19)")

abline(lm(wins_home_pts$win_proportion ~ wins_home_pts$home_pts))

# Logit (Wins) against Home Points
plot(x=wins_home_pts$home_pts, y=wins_home_pts$win_logit,
     xlab="Home Team's Points (18/19)", ylab="Logit (Home wins 19/20)",
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
     xlab="Away Team's Points (18/19)", ylab="Home win Proportion (19/20)",
     main="Proportion of home wins (19/20) by away team's previous season points (18/19)")

abline(lm(wins_away_pts$win_proportion ~ wins_away_pts$away_pts))

# Logit (Wins) against Home Points
plot(x=wins_away_pts$away_pts, y=wins_away_pts$win_logit,
     xlab="Away Team's Points (18/19)", ylab="Logit (Home wins 19/20)",
     main="Logit home wins (19/20) by away team's previous season points (18/19)")

abline(lm(wins_away_pts$win_logit ~ wins_away_pts$away_pts))

# ---------------------------------------- Distance ----------------------------------------------------

#### Table 2 ###

# Fequencies
table(analysis$distance)
# Proportions
round(table(analysis$distance)/272*100, 1)

# Frequencies of Result - By Distance
wins_distance <- analysis %>%
  group_by(distance, result) %>% 
  summarise(wins = n(), .groups="drop_last") %>% 
  mutate(games_played = sum(wins)) %>% 
  mutate(win_proportion = round(wins / sum(wins), 2)) %>% 
  mutate(win_logit = round(log(wins / (games_played - wins)), 2)) %>%
  filter(result == "Win") %>%
  select(-result)

t_wins_distance <- wins_distance %>% 
  select(distance, wins, games_played, win_proportion, win_logit) %>% 
  kbl(caption = "Number of home wins (19/20) by distance travelled by away team", 
      col.names=c("Distance", "Wins", "Played", "Proportion", "Logit")) %>% 
  kable_classic(full_width = F)
t_wins_distance

# ---------------------------------------- COVID ----------------------------------------------------

#### Table 3 ###

# Fequencies
table(analysis$COVID)
# Proportions
round(table(analysis$COVID)/272*100, 1)

### Table 8 ###

# Frequencies of Result - By COVID
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
  kbl(caption = "Number of home wins (19/20) by COVID-19 stoppage in season", 
      col.names=c("COVID", "Wins", "Played", "Proportion", "Logit")) %>% 
  kable_classic(full_width = F)
t_wins_covid

# ---------------------------------------------------------------------------------------------------------
# ------------------------------------------- Measure of Strength -----------------------------------------
# ---------------------------------------------------------------------------------------------------------

# Testing for correaltions between measures of strength 
correlations <- round(cor(select(analysis, Pos=home_pos, Pts=home_pts, GD=home_gd, 
                                 GF=home_goalsfor, GA=home_goalsagainst)), 2)
correlations

# Fit a model with each different statistic as the measure of ability and choose
# the model with the lowest AIC.

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

# Null model

null <- glm(result~1, data = analysis, family = "binomial")
summary(null)
# AIC = 375.76

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
anova(null, win.glm_7, test="Chisq")
# Not all variables significant - exclude

win.glm_8 <- glm(result ~ distance, data=analysis, family = "binomial")
summary(win.glm_8)
anova(null, win.glm_8, test="Chisq")
# AIC = 374.94
# Not all variables significant - exclude

# So, we choose model 5 as our best current model as it has lowest AIC and all variables significant.

# Two variable models

win.glm_9 <- glm(result ~ home_pts + away_pts, data=analysis, family = "binomial")
summary(win.glm_9)
# AIC = 338.91

win.glm_10 <- glm(result ~ home_pts + COVID, data=analysis, family = "binomial")
summary(win.glm_10)
# AIC = 344.33
anova(win.glm_5, win.glm_10, test="Chisq")
# Not all variables significant - exclude

win.glm_11 <- glm(result ~ home_pts + distance, data=analysis, family = "binomial")
summary(win.glm_11)
anova(win.glm_5, win.glm_11, test="Chisq")
# AIC = 339

# So, we choose model 9 because it has the lowest AIC and all variables significant.

# Three variable models

win.glm_12 <- glm(result ~ home_pts + away_pts + COVID, data=analysis, family = "binomial")
summary(win.glm_12)
# AIC = 340.87
anova(win.glm_9, win.glm_12, test="Chisq")
# Not all variables significant - exclude

win.glm_13 <- glm(result ~ home_pts + away_pts + distance, data=analysis, family = "binomial")
summary(win.glm_13)
# AIC = 336.27
anova(win.glm_9, win.glm_13, test="Chisq")
# All variables significant

# Hence, model 13 is  our new best model.

# Four variable model - ### Table 6 ###

win.glm_14 <- glm(result ~ home_pts + away_pts + distance + COVID, data=analysis, family = "binomial")
summary(win.glm_14)
# AIC = 338.27
anova(win.glm_13, win.glm_14, test="Chisq")
# Not all variables significant - exclude

# Interaction models

win.glm_15 <- glm(result ~ home_pts + away_pts + distance + home_pts*away_pts, data=analysis, family = "binomial")
summary(win.glm_15)
# AIC = 337.93
anova(win.glm_13, win.glm_15, test="Chisq")
# No improvement

win.glm_16 <- glm(result ~ home_pts + away_pts + distance + home_pts*distance, data=analysis, family = "binomial")
summary(win.glm_16)
# AIC = 338.11
anova(win.glm_13, win.glm_16, test="Chisq")
# No improvement

win.glm_17 <- glm(result ~ home_pts + away_pts + distance + away_pts*distance, data=analysis, family = "binomial")
summary(win.glm_17)
# AIC = 336.61
anova(win.glm_13, win.glm_17, test="Chisq")
# No improvement

# Hence, model 13 is the best model - ### Table 7 ###

win.glm <- glm(result ~ home_pts + away_pts + distance, data=analysis, family = "binomial")
summary(win.glm)

# ---------------------------------------------------------------------------------------------------------
# ----------------------------------- Interpretatation  --------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

# Log(Odds Of Home Win) = -2.471221 + 0.046378(home_pts) - 0.016334(away_pts) + 0.663690(distanceLong)

exp(coef(win.glm))
exp(confint(win.glm))

# ---------------------------------------------------------------------------------------------------------
# -------------------------------------- Predictions - ----------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

predict(win.glm, newdata=data.frame(home_pts=100, away_pts=50, distance="Long"), type = "response")
predict(win.glm, newdata=data.frame(home_pts=100, away_pts=50, distance="Short"), type = "response")
predict(win.glm, newdata=data.frame(home_pts=50, away_pts=100, distance="Long"), type = "response")
predict(win.glm, newdata=data.frame(home_pts=50, away_pts=100, distance="Short"), type = "response")

# ---------------------------------------------------------------------------------------------------------
# -------------------------------------- Fitted Value Plots -----------------------------------------------
# ---------------------------------------------------------------------------------------------------------

# Win proportion against home points - playing against a team with average away points

obs_data_home_pts <- analysis %>%
  group_by(home_pts, distance, result) %>%
  summarise(wins = n(), .groups="drop_last") %>%
  mutate(games_played = sum(wins)) %>%
  mutate(win_proportion = round(wins / sum(wins), 2)) %>%
  mutate(win_logit = round(log(wins / (games_played - wins)), 2)) %>%
  arrange(home_pts) %>%
  filter(result == "Win") %>%
  select(-result)

homepts_val <- seq(from=min(obs_data_home_pts$home_pts), to=max(obs_data_home_pts$home_pts), by=.1)
awaypts_val <- mean(analysis$away_pts)

short <- predict(win.glm, newdata=data.frame(home_pts = homepts_val, 
                                             away_pts = rep(awaypts_val, length(homepts_val)),
                                             distance = rep("Short", length(homepts_val))), type="response")
long <- predict(win.glm, newdata=data.frame(home_pts = homepts_val, 
                                            away_pts = rep(awaypts_val, length(homepts_val)),
                                            distance = rep("Long", length(homepts_val))), type="response")

plot(x=obs_data_home_pts$home_pts, y=obs_data_home_pts$win_proportion,
     xlab="Home Points (18/19)", ylab="Home Win Proportion (19/20)",
     main="Proportion of Home Wins (19/20) by the Home Team's Previous Season Points (18/19),
     given that the Away Team had the Average Number of Points (18/19) - 58.4",
     col=c("blue", "red")[obs_data_home_pts$distance])

lines(homepts_val, short, col="blue", type="l", lwd=2, lty=2)
lines(homepts_val, long, col="red", type="l", lwd=2, lty=2)

legend('topleft', legend=c("Observed - Short Distance", "Observed - Long Distance", "FItted - Short Distance", "Fitted - Long Distance"), col=c("blue", "red"),
       lty=c(NA,NA,2,2), pch=c(1,1,NA,NA), cex=0.8)

# Win proportion against away points - playing against a team with average home points

obs_data_away_pts <- analysis %>%
  group_by(away_pts, distance, result) %>%
  summarise(wins = n(), .groups="drop_last") %>%
  mutate(games_played = sum(wins)) %>%
  mutate(win_proportion = round(wins / sum(wins), 2)) %>%
  mutate(win_logit = round(log(wins / (games_played - wins)), 2)) %>%
  arrange(away_pts) %>%
  filter(result == "Win") %>%
  select(-result)

homepts_val <- mean(analysis$home_pts)
awaypts_val <- seq(from=min(obs_data_away_pts$away_pts), to=max(obs_data_away_pts$away_pts), by=.1)

short <- predict(win.glm, newdata=data.frame(home_pts=rep(homepts_val, length(awaypts_val)),
                                            away_pts=awaypts_val,
                                            distance = rep("Short", length(homepts_val))), type="response")
long <- predict(win.glm, newdata=data.frame(home_pts=rep(homepts_val, length(awaypts_val)),
                                            away_pts=awaypts_val,
                                            distance = rep("Long", length(homepts_val))), type="response")

plot(x=obs_data_away_pts$away_pts, y=obs_data_away_pts$win_proportion,
     xlab="Away Points (18/19)", ylab="Home Win Proportion (19/20)",
     main="Proportion of Home Wins (19/20) by the Away Team's Previous Season Points (18/19),
     given that the Home Team had the Average Number of Points (18/19) - 58.4",
     col=c("blue", "red")[obs_data_home_pts$distance])

lines(awaypts_val, short, col="blue", type="l", lwd=2, lty=2)
lines(awaypts_val, long, col="red", type="l", lwd=2, lty=2)

legend('topright', legend=c("Observed - Short Distance", "Observed - Long Distance", "FItted - Short Distance", "Fitted - Long Distance"), col=c("blue", "red"),
       lty=c(NA,NA,2,2), pch=c(1,1,NA,NA), cex=0.8)

# ---------------------------------------------------------------------------------------------------------
# -------------------------------------- Residual Analysis ------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

### Figure 1 ###

par(mfrow=c(2,2))

plot(residuals(win.glm, type="pearson") ~ analysis$home_pts, 
     xlab ="Home Points (18/19)", ylab="Std. Pearson resid.",
     main="Pearson residuals against home points")
abline(h=0, lty=2)

plot(residuals(win.glm, type="pearson") ~ analysis$away_pts, 
     xlab ="Away Points (18/19)", ylab="Std. Pearson resid.",
     main="Pearson residuals against away points")
abline(h=0, lty=2)

plot.default(residuals(win.glm, type="pearson") ~ factor(analysis$distance, levels=c("Short", "Long")),
     main = "Pearson residuals against distance", xlab="Distance", ylab = "Std. Pearson resid.",
     xlim = c(0.5, 2.5), xaxt="n")
axis(1, at=1:2, labels=c("Short", "Long"))
abline(h=0, lty=2)

plot(win.glm, which=5, main="Pearson residuals against Leverage", caption="")

par(mfrow=c(1,1))

### Table 9 ###
# Hosmer Lemeshaw to test adequacy
hl <- hoslem.test(analysis$result_bin, fitted(win.glm))
hl

# ---------------------------------------------------------------------------------------------------------
# -------------------------------------- Model Calibration ------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

### Figure 2 ###

# Create Vector of Predicted Probabilities
p <- predict(win.glm, type="response")
#hist(p)

# Bin Predicted Probabilities
p.categories <- bin(p, nbins=10, method="content")
p.categories
table(p.categories)

# Calculate Observed Probabilities
observed.p <- tapply(analysis$result == "Win", p.categories, mean)
predicted.p <- tapply(p, p.categories, mean)
plot(observed.p, predicted.p, xlim = c(0, 1), ylim = c(0, 1),
     main="Model Calibration Plot", xlab="Observed Probabilities", ylab="Predicted Probabilities")
abline(0, 1, lty = 2)

# ---------------------------------------------------------------------------------------------------------
# -------------------------------------- Multicolinearity  ------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

vif(win.glm)
# The results suggest that there is no noticeable evidence of multicolinearity present.


# ---------------------------------------------------------------------------------------------------------
# ------------------------------------ Data Load (20/21) - Testing Data -----------------------------------
# ---------------------------------------------------------------------------------------------------------

# Results from 20/21 season
results2021 <- read_csv("results_20_21.csv") %>%
  # Add Covid flag
  mutate(date=dmy(Date)) %>% 
  mutate(COVID = ifelse(date > as.Date("2020-03-13"), "Post", "Pre")) %>%
  select(home=HomeTeam, away=AwayTeam, result=FTR, COVID) %>%
  # Remove newly Promoted Teams
  filter(!home %in% c("Leeds", "West Brom", "Fulham") &
           !away %in% c("Leeds", "West Brom", "Fulham"))

# Standings from 19/20 season
table1920 <- read_csv("table_19_20.csv") %>% 
  # Remove Relagated Teams
  filter(Position <= 17) %>% 
  select(team=Team, position=Position, points=Pts, gd=GD, goalsfor=F, goalsagainst=A)

# Calculating distances between stadiums
teams2021 <- unique(results2021$home)

stadiums2021 <- read_csv("stadiums.csv") %>% 
  filter(Team %in% teams2021) %>% 
  select(team=Team, lat=Latitude, long=Longitude)

stadiums2021_long <- results2021 %>% 
  left_join(select(stadiums2021, team, home_lat=lat, home_long=long), 
            by=c("home"="team")) %>% 
  left_join(select(stadiums2021, team, away_lat=lat, away_long=long), 
            by=c("away"="team")) %>% 
  select(home, away, home_long, home_lat, away_long, away_lat)

stadiums2021_long$distance <- distHaversine(stadiums2021_long[,3:4], stadiums2021_long[,5:6])/1000

# ---------------------------------------------------------------------------------------------------------
# ------------------------------ Data Manipulation - Testing Data ----------------------------------------
# ---------------------------------------------------------------------------------------------------------

testing <- results2021 %>% 
  mutate(result = case_when(
    result == "H" ~ "Win",
    TRUE ~ "Loss/Draw"
  )) %>%
  mutate(result_bin = ifelse(result=="Win", 1, 0)) %>% 
  left_join(select(table1920, team, home_pos=position, home_pts=points, home_gd=gd,
                   home_goalsfor=goalsfor, home_goalsagainst=goalsagainst), 
            by=c("home"="team")) %>% 
  left_join(select(table1920, team, away_pos=position, away_pts=points, away_gd=gd,
                   away_goalsfor=goalsfor, away_goalsagainst=goalsagainst), 
            by=c("away"="team")) %>% 
  left_join(select(stadiums2021_long, home, away, distance), 
            by=c("home"="home", "away"="away")) %>% 
  mutate(distance = case_when(
    distance <= 100 ~ "Short",
    distance > 100  ~ "Long",
    TRUE ~ "Missing"
  ))

testing$result <- factor(testing$result, levels = c("Loss/Draw", "Win"))
testing$COVID <- factor(testing$COVID, levels = c("Pre", "Post"))
testing$distance <- factor(testing$distance, levels = c("Short", "Long"))

# ---------------------------------------------------------------------------------------------------------
# ------------------------------------- ROC Plot - Testing Data   ----------------------------------------
# ---------------------------------------------------------------------------------------------------------

### Figure 3 ###

# ROC Creation
p = predict(win.glm, newdata = testing, type = "response")
roc = roc(testing$result ~ p)

# AUC value
as.numeric(roc$auc)

# Best threshold
best <- coords(roc, "best", "threshold", transpose = FALSE)
best

# ROC Plot
plot(1, type="n", xlab="Specificity", ylab="Sensitivity", xlim=c(1, 0), ylim=c(0, 1),
     main="Receiver Operating Characteristic - ROC", cex.main=0.95)
plot.roc(roc, add=TRUE, print.thres="best", print.thres.col="blue", print.auc=FALSE, identity=TRUE)

# TPR vs TNR
matplot(data.frame(roc$sensitivities, roc$specificities),
        x = roc$thresholds, type='l', xlab = 'Threshold', ylab='Rate',
        main="Distribution of TPR and TNR")
legend('bottomright', legend=c('TPR', 'TNR'), lty=1:2, col=1:2)

# ---------------------------------------------------------------------------------------------------------
# ------------------------------- Classification Table - Testing Data   ----------------------------------
# ---------------------------------------------------------------------------------------------------------

### Table 10 ###

p = predict(win.glm, newdata = testing, type = "response")
class_prediction <- factor(ifelse(p > best[[1]], "Win", "Loss/Draw"), levels=c("Loss/Draw", "Win"))

confusionMatrix(data=class_prediction, reference=testing$result, positive="Win")

