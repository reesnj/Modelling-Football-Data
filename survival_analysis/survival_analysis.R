# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------- Set up Workspace -----------------------------------------------
# ---------------------------------------------------------------------------------------------------------

setwd("./survival_analysis/data")

#install.packages("ggfortify")
#install.packages("SurvRegCensCov")

library(readr)
library(dplyr)
library(lubridate)
library(survival)
library(ggplot2)
library(ggfortify)
library(SurvRegCensCov)


# ---------------------------------------------------------------------------------------------------------
# ------------------------------------------- Data Load ---------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

# Goal times from 19/20 EPL season

goaltimes1920 <- read_csv("goaltimes_19_20.csv") %>%
  # Add Covid flag
  mutate(date=dmy(Date)) %>% 
  mutate(COVID = ifelse(date > as.Date("2020-03-13"), "Post", "Pre")) %>%
  select(home=HomeTeam, away=AwayTeam, firsthomegoal=FHG, firstawaygoal=FAG, COVID) %>%
  # Remove newly Promoted Teams
  filter(!home %in% c("Aston Villa", "Norwich", "Sheffield United") &
           !away %in% c("Aston Villa", "Norwich", "Sheffield United"))

# Standings from 18/19 season

table1819 <- read_csv("table_18_19.csv") %>% 
  # Remove Relagated Teams
  filter(Position <= 17) %>% 
  select(team=Team, position=Position, points=Pts, gd=GD, goalsfor=F, goalsagainst=A)

# Data Manipulation

analysis <- rbind(
  data.frame(ref = goaltimes1920$home,
             adv = goaltimes1920$away,
             loc = "Home",
             covid = goaltimes1920$COVID,
             goaltime = goaltimes1920$firsthomegoal
             ),
  data.frame(ref = goaltimes1920$away,
             adv = goaltimes1920$home,
             loc = "Away",
             covid = goaltimes1920$COVID,
             goaltime = goaltimes1920$firstawaygoal)
  ) %>%
  mutate(cens = ifelse(is.na(goaltime), 0, 1)) %>% 
  mutate(goaltime = ifelse(is.na(goaltime), 90, goaltime)) %>% 
  left_join(select(table1819, team, ref_pos=position, ref_pts=points, ref_gd=gd, ref_gf=goalsfor, ref_ga=goalsagainst), 
            by=c("ref"="team")) %>% 
  left_join(select(table1819, team, adv_pos=position, adv_pts=points, adv_gd=gd, adv_gf=goalsfor, adv_ga=goalsagainst), 
            by=c("adv"="team")) %>% 
  mutate(ref_ability = ifelse(ref_pos<=6, "Top 6", "Not Top 6")) %>% 
  mutate(adv_ability = ifelse(adv_pos<=6, "Top 6", "Not Top 6"))

analysis$covid <- factor(analysis$covid, levels = c("Pre", "Post"))
analysis$loc <- factor(analysis$loc, levels = c("Away", "Home"))
analysis$ref_ability <- factor(analysis$ref_ability, levels = c("Top 6", "Not Top 6"))
analysis$adv_ability <- factor(analysis$adv_ability, levels = c("Top 6", "Not Top 6"))

# -------------------------------------------------------------------------------------------------------
# ----------------------------------------- Exploration -------------------------------------------------
# -------------------------------------------------------------------------------------------------------

# Overall plot of first goal timings
barplot(table(analysis$goaltime))

# Plot of non-censored first goal timings
goals_not_cens <- analysis %>% 
  filter(cens==1)
barplot(table(goals_not_cens$goaltime))

# ------------------------------------------------------------------------------------------------------
# --------------------------------------- Kaplan Meier -------------------------------------------------
# ------------------------------------------------------------------------------------------------------

# Kaplan Meier Survival object -------------------------------------------------------------------------

km <- with(analysis, Surv(time=goaltime, event=cens))
head(km, 80)
# Note that a “+” after the time in the print out of km indicates censoring.

# General Kaplan Meier Model ---------------------------------------------------------------------------

km_fit <- survfit(Surv(goaltime, cens) ~ 1, data=analysis)
km_fit
summary(km_fit)
autoplot(km_fit)

# The median time in which a team scores their first goal in a match is the 51st minute.
# A team will score their first goal in a match within the first 24 minutes, 25% of the time. 
# A team will fail to score at all in the match, 26.5% of the time. 
# The largest observations (90 mins) are censored, s0 we cannot calculate the exact mean.

# Kaplan Meier Model - by location ----------------------------------------------------------------------

km_loc_fit <- survfit(Surv(goaltime, cens) ~ loc, data=analysis)
km_loc_fit
summary(km_loc_fit)

autoplot(km_loc_fit)

# Test proportional Hazards assumption
loc_haz <- data.frame(time=km_loc_fit$time, cumhaz=km_loc_fit$cumhaz, logcumhaz=log(km_loc_fit$cumhaz)) %>% 
  mutate(loc=case_when(
    row_number() < 76 ~ "Away",
    row_number() >= 76 ~ "Home",
    TRUE ~ "Missing"
  ))
ggplot(data=loc_haz, aes(x=time, y=logcumhaz, color=loc)) +
  geom_line()

# Log Rank Test
logrank_loc <- survdiff(Surv(goaltime, cens) ~ loc, data=analysis)
logrank_loc
# No signifciant difference between the survival times based on location (home/away)

# Wilcoxon Test
wilcoxon_loc <- survdiff(Surv(goaltime, cens) ~ loc, rho=1, data=analysis)
wilcoxon_loc
# No signifciant difference between the survival times based on location (home/away)


# Kaplan Meier Model - by covid -------------------------------------------------------------------------

km_covid_fit <- survfit(Surv(goaltime, cens) ~ covid, data=analysis)
km_covid_fit
summary(km_covid_fit)

autoplot(km_covid_fit)

# Test proportional Hazards assumption
covid_haz <- data.frame(time=km_covid_fit$time, cumhaz=km_covid_fit$cumhaz, logcumhaz=log(km_covid_fit$cumhaz)) %>% 
  mutate(covid=case_when(
    row_number() < 86 ~ "Pre",
    row_number() >= 86 ~ "Post",
    TRUE ~ "Missing"
  ))
ggplot(data=covid_haz, aes(x=time, y=logcumhaz, color=covid)) +
  geom_line()

# Log Rank Test
logrank_covid <- survdiff(Surv(goaltime, cens) ~ covid, data=analysis)
logrank_covid
# No signifciant difference between the survival times based on Covid (pre/post)

# Wilcoxon Test
wilcoxon_covid <- survdiff(Surv(goaltime, cens) ~ covid, rho=1, data=analysis)
wilcoxon_covid
# No signifciant difference between the survival times based on covid (pre/post)

# Kaplan Meier Model - by ref team ability --------------------------------------------------------------

km_ref_pos_fit <- survfit(Surv(goaltime, cens) ~ ref_ability, data=analysis)
km_ref_pos_fit
summary(km_ref_pos_fit)

autoplot(km_ref_pos_fit)

# Test proportional Hazards assumption
ref_pos_haz <- data.frame(time=km_ref_pos_fit$time, cumhaz=km_ref_pos_fit$cumhaz, logcumhaz=log(km_ref_pos_fit$cumhaz)) %>% 
  mutate(ref_pos=case_when(
    row_number() < 69 ~ "Top 6",
    row_number() >= 69 ~ "Not Top 6",
    TRUE ~ "Missing"
  ))
ggplot(data=ref_pos_haz, aes(x=time, y=logcumhaz, color=ref_pos)) +
  geom_line()

# Log Rank Test
logrank_ref_pos <- survdiff(Surv(goaltime, cens) ~ ref_ability, data=analysis)
logrank_ref_pos
# There is a significant difference between the time in which first goal is scored for reference teams in 
# top 6 compared to teams not in top 6.

# Wilcoxon Test
wilcoxon_ref_pos <- survdiff(Surv(goaltime, cens) ~ ref_ability, rho=1, data=analysis)
wilcoxon_ref_pos
# There is a significant difference between the time in which first goal is scored for reference teams in 
# top 6 compared to teams not in top 6.

# Kaplan Meier Model - by adv team ability --------------------------------------------------------------

km_adv_pos_fit <- survfit(Surv(goaltime, cens) ~ adv_ability, data=analysis)
km_adv_pos_fit
summary(km_adv_pos_fit)

autoplot(km_adv_pos_fit)

# Test proportional Hazards assumption
adv_pos_haz <- data.frame(time=km_adv_pos_fit$time, cumhaz=km_adv_pos_fit$cumhaz, logcumhaz=log(km_adv_pos_fit$cumhaz)) %>% 
  mutate(adv_pos=case_when(
    row_number() < 71 ~ "Top 6",
    row_number() >= 71 ~ "Not Top 6",
    TRUE ~ "Missing"
  ))
ggplot(data=adv_pos_haz, aes(x=time, y=logcumhaz, color=adv_pos)) +
  geom_line()

# Log Rank Test
logrank_adv_pos <- survdiff(Surv(goaltime, cens) ~ adv_ability, data=analysis)
logrank_adv_pos
# There is a significant difference between the time in which first goal is scored for adverse teams in 
# top 6 compared to teams not in top 6.

# Wilcoxon Test
wilcoxon_adv_pos <- survdiff(Surv(goaltime, cens) ~ adv_ability, rho=1, data=analysis)
wilcoxon_adv_pos
# There is a significant difference between the time in which first goal is scored for adverse teams in 
# top 6 compared to teams not in top 6.

# ------------------------------------------------------------------------------------------------------
# ------------------------------------- Cumulative Hazard ----------------------------------------------
# ------------------------------------------------------------------------------------------------------

plot(km_fit$time, km_fit$cumhaz)
lm1 <- lm(km_fit$cumhaz ~ km_fit$time)
summary(lm1)
abline(lm1)
1 / lm1$coefficients[2]
# H(t) appears to be linear, indicating that h(t) is constant. The plot appears to be a straight line through
# the origin, suggesting that the expoential distribution may be an appropiate model. If this is the case, the
# slope of the line gives an apporximate estimate of the hazard rate (1 / mean). 
# mean = 1 / slope = 68.1 minutes

# ------------------------------------------------------------------------------------------------------
# -------------------------------------- Weibull Model  ------------------------------------------------
# ------------------------------------------------------------------------------------------------------

# Plot log cumulative hazard against log time ----------------------------------------------------------

plot(log(km_fit$time), log(km_fit$cumhaz))
lm2 <- lm(log(km_fit$cumhaz) ~ log(km_fit$time))
summary(lm2)
abline(lm2)
# gamma = slope , mu = exp( -intercept / gamma)
lm2$coefficients[2] # gamma (shape)
exp(-lm2$coefficients[1] / lm2$coefficients[2]) # mu (scale)

# Model ------------------------------------------------------------------------------------------------

weibull <- survreg(Surv(goaltime, cens) ~ ref_ability + adv_ability + loc, data=analysis, dist="weibull")
summary(weibull)
# A log likelihood test shows that the model is significantly better than null model (P=2.4e–08).
ConvertWeibull(weibull, conf.level = 0.95)

# Adequacy --------------------------------------------------------------------------------------------

WeibullDiag(Surv(goaltime, cens) ~ ref_ability, data=analysis)
WeibullDiag(Surv(goaltime, cens) ~ adv_ability, data=analysis)
WeibullDiag(Surv(goaltime, cens) ~ loc, data=analysis)

# ------------------------------------------------------------------------------------------------------
# --------------------------------- Cox Proportional Hazards  ------------------------------------------
# ------------------------------------------------------------------------------------------------------

cox <- coxph(Surv(goaltime, cens) ~ ref_ability + adv_ability + loc, data = analysis)
summary(cox)
# HR = 0.59 implies that around 0.6 times as many females are dying as males, at any given time.
# A top six team scores around 40% more goals than a non-top six team, at any given time. 
# A team playing against a top six team scores around 23% less goals than a team playing against
# a non top six team, at any given time. 
# A team playing at home scores around 21% more goals than a team playing away, at any given time. 

cox_fit <- survfit(cox)
autoplot(cox_fit)
