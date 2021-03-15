# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------- Set up Workspace -----------------------------------------------
# ---------------------------------------------------------------------------------------------------------

setwd("./survival_analysis/data")

#install.packages("ggfortify")
#install.packages("SurvRegCensCov")
#install.packages("survminer")

library(readr)
library(dplyr)
library(lubridate)
library(survival)
library(ggplot2)
library(ggfortify)
library(SurvRegCensCov)
library(survminer)

# ---------------------------------------------------------------------------------------------------------
# ------------------------------------------- Data Load ---------------------------------------------------
# ---------------------------------------------------------------------------------------------------------

# Goal times from 19/20 EPL season
goaltimes1920 <- read_csv("goaltimes_19_20.csv") %>%
  # Add Covid flag
  mutate(date=dmy(Date)) %>% 
  mutate(COVID = ifelse(date > as.Date("2020-03-13"), "Post", "Pre")) %>%
  select(home=HomeTeam, away=AwayTeam, firsthomegoal=FHG, firstawaygoal=FAG, COVID, result=FTR) %>%
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
analysis$ref_ability <- factor(analysis$ref_ability, levels = c("Not Top 6", "Top 6"))
analysis$adv_ability <- factor(analysis$adv_ability, levels = c("Not Top 6", "Top 6"))

# -------------------------------------------------------------------------------------------------------
# ----------------------------------------- Exploration -------------------------------------------------
# -------------------------------------------------------------------------------------------------------

# Histogram of first goal timings
hist(analysis$goaltime, 
     main="Histogram of the Time to First Goal - Including Censored Data", 
     xlab="Time (Minutes)",
     xlim=c(0,90),
     breaks=15,
     xaxt="n")
axis(1, xaxp=c(0, 90, 6))

# Extract the goal times that were not cesnored
goals_not_cens <- analysis %>% 
  filter(cens==1)

# Histogram of non-censored first goal timings
hist(goals_not_cens$goaltime, 
     main="Histogram of the Time to First Goal - Excluding Censored Data", 
     xlab="Time (Minutes)",
     xlim=c(0,90),
     ylim=c(0,40),
     breaks=15,
     xaxt="n")
axis(1, xaxp=c(0, 90, 6))

# Number of censored observations (0-0 matches)
table(analysis$cens)

# Number of matches where first score wins
result_by_firstgoal <- goaltimes1920 %>% 
  # If no goals scored, set time to 999
  mutate(firsthomegoal = ifelse(is.na(firsthomegoal), 999, firsthomegoal)) %>% 
  mutate(firstawaygoal = ifelse(is.na(firstawaygoal), 999, firstawaygoal)) %>%
  # Remove 0-0 draws
  filter(firsthomegoal!=999 | firstawaygoal!=999) %>% 
  mutate(scoredfirst = case_when(
    firsthomegoal < firstawaygoal ~ "H",
    firsthomegoal > firstawaygoal ~ "A",
    TRUE ~ ""
  )) %>% 
  # Remove if first goal scored in same minute
  filter(scoredfirst!="") %>% 
  mutate(scoredfirstWin = ifelse(scoredfirst==result, 1, 0))

# Table of team scoring first winning / not winning match
tab1 <- table(result_by_firstgoal$scoredfirstWin)
tab1
prop.table(tab1)*100
# The team scoring first goes on to win 67.45% of their matches

# Table of Team scoring first against Match Outcome
tab2 <- table(factor(result_by_firstgoal$scoredfirst, levels=c("H", "A")),
      factor(result_by_firstgoal$result, levels=c("H", "D", "A")))
tab2

chisq <- chisq.test(tab2)
chisq
chisq$observed
round(chisq$expected,2)
round(chisq$residuals, 3)
# So the team scoring first is signifcantly associated with the match outcome.

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
summary(km_fit)$table
ggsurvplot(km_fit, data=analysis, title="Kaplan-Meier Plot of Time to First Goal", xlab="Time (Minutes)",
           break.time.by=15)

# The median time in which a team scores their first goal in a match is the 51st minute.
# A team will score their first goal in a match within the first 24 minutes, 25% of the time. 
# A team will fail to score at all in the match, 26.5% of the time. 
# The largest observations (90 mins) are censored, s0 we cannot calculate the exact mean.

# Kaplan Meier Model - by location ----------------------------------------------------------------------

km_loc_fit <- survfit(Surv(goaltime, cens) ~ loc, data=analysis)
km_loc_fit
summary(km_loc_fit)
summary(km_loc_fit)$table 
ggsurvplot(km_loc_fit, data=analysis, title="Kaplan-Meier Plot of Time to First Goal - Split by Location (Home/Away)", xlab="Time (Minutes)",
           break.time.by=15, conf.int = TRUE, legend.labs = c("Away","Home"))


# Test proportional Hazards assumption
loc_haz <- data.frame(time=km_loc_fit$time, logtime=log(km_loc_fit$time),
                      cumhaz=km_loc_fit$cumhaz, logcumhaz=log(km_loc_fit$cumhaz)) %>% 
  mutate(loc=case_when(
    row_number() < 76 ~ "Away",
    row_number() >= 76 ~ "Home",
    TRUE ~ "Missing"
  ))
ggplot(data=loc_haz, aes(x=logtime, y=logcumhaz, color=loc)) +
  ggtitle("Log(Cumulative Hazard) versus Log(Time)") +
  xlab("Log(Time)") + ylab("Log(Cumulative Hazard)") +
  labs(color = "Location") +
  geom_line()
# Lines do not appear parallel, cannot assume proportional hazards.

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
summary(km_covid_fit)$table
ggsurvplot(km_covid_fit, data=analysis, title="Kaplan-Meier Plot of Time to First Goal - Split by Covid (Pre/Post)", xlab="Time (Minutes)",
           break.time.by=15, conf.int = TRUE, legend.labs = c("Pre","Post"))

# Test proportional Hazards assumption - change this to log time!!!!!!!
covid_haz <- data.frame(time=km_covid_fit$time, logtime=log(km_covid_fit$time),
                        cumhaz=km_covid_fit$cumhaz, logcumhaz=log(km_covid_fit$cumhaz)) %>% 
  mutate(covid=case_when(
    row_number() < 86 ~ "Pre",
    row_number() >= 86 ~ "Post",
    TRUE ~ "Missing"
  ))
ggplot(data=covid_haz, aes(x=logtime, y=logcumhaz, color=covid)) +
  ggtitle("Log(Cumulative Hazard) versus Log(Time)") +
  xlab("Log(Time)") + ylab("Log(Cumulative Hazard)") +
  labs(color = "COVID") +
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
summary(km_ref_pos_fit)$table
ggsurvplot(km_ref_pos_fit, data=analysis, title="Kaplan-Meier Plot of Time to First Goal - Split by Reference Team Position", xlab="Time (Minutes)",
           break.time.by=15, conf.int = TRUE, legend.labs = c("Not Top 6","Top 6"))


# Test proportional Hazards assumption
ref_pos_haz <- data.frame(time=km_ref_pos_fit$time, logtime=log(km_ref_pos_fit$time),
                          cumhaz=km_ref_pos_fit$cumhaz, logcumhaz=log(km_ref_pos_fit$cumhaz)) %>% 
  mutate(ref_pos=case_when(
    row_number() < 87 ~ "Not Top 6",
    row_number() >= 87 ~ "Top 6",
    TRUE ~ "Missing"
  ))
ggplot(data=ref_pos_haz, aes(x=logtime, y=logcumhaz, color=ref_pos)) +
  ggtitle("Log(Cumulative Hazard) versus Log(Time)") +
  xlab("Log(Time)") + ylab("Log(Cumulative Hazard)") +
  labs(color = "Ref Position") +
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
summary(km_adv_pos_fit)$table
ggsurvplot(km_adv_pos_fit, data=analysis, title="Kaplan-Meier Plot of Time to First Goal - Split by Adverse Team Position", xlab="Time (Minutes)",
           break.time.by=15, conf.int = TRUE, legend.labs = c("Not Top 6","Top 6"))
surv_median(km_adv_pos_fit)

# Test proportional Hazards assumption
adv_pos_haz <- data.frame(time=km_adv_pos_fit$time, logtime=log(km_adv_pos_fit$time), 
                          cumhaz=km_adv_pos_fit$cumhaz, logcumhaz=log(km_adv_pos_fit$cumhaz)) %>% 
  mutate(adv_pos=case_when(
    row_number() < 82 ~ "Not Top 6",
    row_number() >= 82 ~ "Top 6",
    TRUE ~ "Missing"
  ))
ggplot(data=adv_pos_haz, aes(x=logtime, y=logcumhaz, color=adv_pos)) +
  ggtitle("Log(Cumulative Hazard) versus Log(Time)") +
  xlab("Log(Time)") + ylab("Log(Cumulative Hazard)") +
  labs(color = "Adv Position") +
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
# -------------------------------------- Weibull Model  ------------------------------------------------
# ------------------------------------------------------------------------------------------------------

weibull <- survreg(Surv(goaltime, cens) ~ ref_ability + adv_ability + loc, data=analysis, dist="weibull")
summary(weibull)
# A log likelihood test shows that the model is significantly better than null model (P=2.4e–08).
ConvertWeibull(weibull, conf.level = 0.95)

# Exponetiate coefficents in table 1 to get ratio of two medians or means
# Look at HR from table 2 to get interpration in terms of hazard ratios 

# shape = 1 / scale > 0 --- so hazard rate is increasing
# Ci does not contain one so rule out expoential distyribution


# Adequacy --------------------------------------------------------------------------------------------

WeibullDiag(Surv(goaltime, cens) ~ ref_ability, data=analysis)
WeibullDiag(Surv(goaltime, cens) ~ adv_ability, data=analysis)
WeibullDiag(Surv(goaltime, cens) ~ loc, data=analysis)

# ------------------------------------------------------------------------------------------------------
# --------------------------------- Cox Proportional Hazards  ------------------------------------------
# ------------------------------------------------------------------------------------------------------

cox <- coxph(Surv(goaltime, cens) ~ ref_ability + adv_ability + loc, data = analysis)
summary(cox)

# The bottom table provides Hazard ratios
# HR ref_ability = 0.59 ~ 0.6
# Non top six teams score approximately 0.6 times the rate per unit time as top six teams.
# HR_adv_ability = 1.23
# A team playing against a non top 6 team scores approximately 1.2 times the rate per unit time
# compared to a team playing against a top 6 team.
# HR_locHome = 1.21
# A team playing at home scores approximately 1.2 times the rate per unit time as a team playing away.

# Adjusted survial curves
ggadjustedcurves(cox, variable="ref_ability", title="Adjusted Survival Curves of Time to First Goal - Split by Reference team ability", 
                 data=analysis, xlab="Time (Minutes)")
ggadjustedcurves(cox, variable="adv_ability", title="Adjusted Survival Curves of Time to First Goal - Split by Adverse team ability", 
                 data=analysis, xlab="Time (Minutes)")
ggadjustedcurves(cox, variable="loc", title="Adjusted Survival Curves of Time to First Goal - Split by Location (Home/Away)", 
                 data=analysis, xlab="Time (Minutes)")

# Testing proportional hazards

cz <- cox.zph(cox)
print(cz)
# We do not reject the null. There is not enough evidence to suggest that propoirtonal hazard assumption
# has been violated.
plot(cz)
# Deviation from a zero-slope line is evidence that the proportional hazards assumption is violated


