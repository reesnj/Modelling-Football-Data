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
library(ggpubr)
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
  select(team=Team, position=Position, points=Pts)

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
  left_join(select(table1819, team, ref_pos=position, ref_pts=points), 
            by=c("ref"="team")) %>% 
  left_join(select(table1819, team, adv_pos=position, adv_pts=points), 
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

### Table 11/12 ####

# Illustrating data manipulation with one match
singlematch <- select(goaltimes1920, -COVID, -result)[8,]
singlematch_transformed <- rbind(
  data.frame(ref = singlematch$home,
             adv = singlematch$away,
             loc = "Home",
             goaltime = singlematch$firsthomegoal
  ),
  data.frame(ref = singlematch$away,
             adv = singlematch$home,
             loc = "Away",
             goaltime = singlematch$firstawaygoal))
singlematch
singlematch_transformed

### Figure 4 ###
structure <- select(analysis, Reference=ref, Adverse=adv, Location=loc, Covid=covid, Ref_Points=ref_pts, Ref_Ability=ref_ability,
                    Adv_Points=adv_pts, Adv_Ability=adv_ability, Censored=cens, Goal_Time=goaltime)[c(8, 310),]

### Table 13 ###

# Number of censored observations
table(analysis$cens)

### Figure 5 ###

par(mfrow=c(1,2))
# Histogram of first goal timings
hist(analysis$goaltime, 
     main="Histogram of the Time to First Goal\n(Including Censored Data)", 
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
     main="Histogram of the Time to First Goal\n(Excluding Censored Data)", 
     xlab="Time (Minutes)",
     xlim=c(0,90),
     ylim=c(0,40),
     breaks=15,
     xaxt="n")
axis(1, xaxp=c(0, 90, 6))
par(mfrow=c(1,1))

### Table 14 ###

# Number of matches where first score wins
result_by_firstgoal <- goaltimes1920 %>% 
  # If no goals scored, set time to 999
  mutate(firsthomegoal = ifelse(is.na(firsthomegoal), 999, firsthomegoal)) %>% 
  mutate(firstawaygoal = ifelse(is.na(firstawaygoal), 999, firstawaygoal)) %>%
  # Remove 0-0 draws
  filter(firsthomegoal!=999 | firstawaygoal!=999) %>% 
  # Team that scored first
  mutate(scoredfirst = case_when(
    firsthomegoal < firstawaygoal ~ "H",
    firsthomegoal > firstawaygoal ~ "A",
    TRUE ~ ""
  )) %>% 
  # Create frequencies
  mutate(freq = case_when(
    scoredfirst==result ~ "Win",
    result=="D" ~ "Draw",
    scoredfirst!=result & result!="D" ~ "Loss",
    TRUE ~ ""
  )) %>%
  # Remove if first goal scored in same minute
  filter(scoredfirst!="") %>% 
  mutate(scoredfirstWin = ifelse(scoredfirst==result, 1, 0))

table(result_by_firstgoal$freq)

# ------------------------------------------------------------------------------------------------------
# --------------------------------------- Kaplan Meier -------------------------------------------------
# ------------------------------------------------------------------------------------------------------

# Kaplan Meier Survival object -------------------------------------------------------------------------

km <- with(analysis, Surv(time=goaltime, event=cens))
head(km, 80)
# Note that a “+” after the time in the print out of km indicates censoring.

# General Kaplan Meier Model ---------------------------------------------------------------------------

### Table 15 ###
km_fit <- survfit(Surv(goaltime, cens) ~ 1, data=analysis)
km_fit
summary(km_fit)
summary(km_fit)$table

### Figure 6 ###
ggsurvplot(km_fit, data=analysis, title="Kaplan-Meier Plot of Time Taken for Reference Team to Score First Goal", xlab="Time (Minutes)",
           break.time.by=15)

# Kaplan Meier Model - by location ----------------------------------------------------------------------

### Table 16 ###
km_loc <- survfit(Surv(goaltime, cens) ~ loc, data=analysis)
km_loc
summary(km_loc)
summary(km_loc)$table

### Figure 7 ###
ggsurvplot(km_loc, data=analysis, title="Kaplan-Meier Plot of Time Taken for Reference Team to Score First Goal\nSplit by Location of Reference Team (Home/Away)", xlab="Time (Minutes)",
           break.time.by=15, conf.int = TRUE, legend.labs = c("Away","Home"))

# Test proportional Hazards assumption
loc_haz <- data.frame(time=km_loc$time, logtime=log(km_loc$time),
                      cumhaz=km_loc$cumhaz, logcumhaz=log(km_loc$cumhaz)) %>% 
  mutate(loc=case_when(
    row_number() < 76 ~ "Away",
    row_number() >= 76 ~ "Home",
    TRUE ~ "Missing"
  ))

### Figure 8 ###
ggplot(data=loc_haz, aes(x=logtime, y=logcumhaz, color=loc)) +
  ggtitle("Log(Cumulative Hazard) versus Log(Time) - Split by Location of Reference Team (Home/Away)") +
  xlab("Log(Time)") + ylab("Log(Cumulative Hazard)") +
  labs(color = "Location") +
  geom_line() + 
  theme_classic()
# Lines do not appear parallel, cannot assume proportional hazards.

### Table 17 ###
# Wilcoxon Test
wilcoxon_loc <- survdiff(Surv(goaltime, cens) ~ loc, rho=1, data=analysis)
wilcoxon_loc
# No signifciant difference between the survival times based on location (home/away)

# Kaplan Meier Model - by covid -------------------------------------------------------------------------

### Table 22 ###
km_covid <- survfit(Surv(goaltime, cens) ~ covid, data=analysis)
km_covid
summary(km_covid)
summary(km_covid)$table

### Figure 13 ###
ggsurvplot(km_covid, data=analysis, title="Kaplan-Meier Plot of Time Taken for Reference Team to Score First Goal\nSplit by (Pre/Post) Covid-19", xlab="Time (Minutes)",
           break.time.by=15, conf.int = TRUE, legend.labs = c("Pre","Post"))

### Figure 14 ###
# Test proportional Hazards assumption
covid_haz <- data.frame(time=km_covid$time, logtime=log(km_covid$time),
                        cumhaz=km_covid$cumhaz, logcumhaz=log(km_covid$cumhaz)) %>% 
  mutate(covid=case_when(
    row_number() < 86 ~ "Pre",
    row_number() >= 86 ~ "Post",
    TRUE ~ "Missing"
  ))
ggplot(data=covid_haz, aes(x=logtime, y=logcumhaz, color=covid)) +
  ggtitle("Log(Cumulative Hazard) versus Log(Time) - Split by (Pre/Post) Covid-19") +
  xlab("Log(Time)") + ylab("Log(Cumulative Hazard)") +
  labs(color = "COVID") +
  geom_line() + 
  theme_classic()

### Table 23 ###
# Wilcoxon Test
wilcoxon_covid <- survdiff(Surv(goaltime, cens) ~ covid, rho=1, data=analysis)
wilcoxon_covid
# No signifciant difference between the survival times based on covid (pre/post)


# Kaplan Meier Model - by ref team ability --------------------------------------------------------------

### Table 18 ###
km_ref_pos <- survfit(Surv(goaltime, cens) ~ ref_ability, data=analysis)
km_ref_pos
summary(km_ref_pos)
summary(km_ref_pos)$table

### Figure 9 ###
ggsurvplot(km_ref_pos, data=analysis, title="Kaplan-Meier Plot of Time Taken for Reference Team to Score First Goal\nSplit by Ability of Reference Team (Top 6/Not Top 6)", xlab="Time (Minutes)",
           break.time.by=15, conf.int = TRUE, legend.labs = c("Not Top 6","Top 6"))

### Figure 10 ###
# Test proportional Hazards assumption
ref_pos_haz <- data.frame(time=km_ref_pos$time, logtime=log(km_ref_pos$time),
                          cumhaz=km_ref_pos$cumhaz, logcumhaz=log(km_ref_pos$cumhaz)) %>% 
  mutate(ref_pos=case_when(
    row_number() < 87 ~ "Not Top 6",
    row_number() >= 87 ~ "Top 6",
    TRUE ~ "Missing"
  ))
ggplot(data=ref_pos_haz, aes(x=logtime, y=logcumhaz, color=ref_pos)) +
  ggtitle("Log(Cumulative Hazard) versus Log(Time) - Split by Ability of Reference Team (Top 6/Not Top 6)") +
  xlab("Log(Time)") + ylab("Log(Cumulative Hazard)") +
  labs(color = "Ref Position") +
  geom_line() + 
  theme_classic()

### Table 19 ###
# Log Rank Test
logrank_ref_pos <- survdiff(Surv(goaltime, cens) ~ ref_ability, data=analysis)
logrank_ref_pos

# Kaplan Meier Model - by adv team ability --------------------------------------------------------------

### Table 20 ###
km_adv_pos <- survfit(Surv(goaltime, cens) ~ adv_ability, data=analysis)
km_adv_pos
summary(km_adv_pos)
summary(km_adv_pos)$table

### Figure 11 ###
ggsurvplot(km_adv_pos, data=analysis, title="Kaplan-Meier Plot of Time Taken for Reference Team to Score First Goal\nSplit by Ability of Adverse Team (Top 6/Not Top 6)", xlab="Time (Minutes)",
           break.time.by=15, conf.int = TRUE, legend.labs = c("Not Top 6","Top 6"))
surv_median(km_adv_pos)

### Figure 12 ###
# Test proportional Hazards assumption
adv_pos_haz <- data.frame(time=km_adv_pos$time, logtime=log(km_adv_pos$time), 
                          cumhaz=km_adv_pos$cumhaz, logcumhaz=log(km_adv_pos$cumhaz)) %>% 
  mutate(adv_pos=case_when(
    row_number() < 82 ~ "Not Top 6",
    row_number() >= 82 ~ "Top 6",
    TRUE ~ "Missing"
  ))
ggplot(data=adv_pos_haz, aes(x=logtime, y=logcumhaz, color=adv_pos)) +
  ggtitle("Log(Cumulative Hazard) versus Log(Time) - Split by Ability of Adverse Team (Top 6/Not Top 6)") +
  xlab("Log(Time)") + ylab("Log(Cumulative Hazard)") +
  labs(color = "Adv Position") +
  geom_line() + 
  theme_classic()

### Table 21 ###
# Wilcoxon Test
wilcoxon_adv_pos <- survdiff(Surv(goaltime, cens) ~ adv_ability, rho=1, data=analysis)
wilcoxon_adv_pos
# There is a significant difference between the time in which first goal is scored for adverse teams in 
# top 6 compared to teams not in top 6.


# ------------------------------------------------------------------------------------------------------
# -------------------------------------- Weibull Model  ------------------------------------------------
# ------------------------------------------------------------------------------------------------------

### Table 24 ###
weibull_1 <- survreg(Surv(goaltime, cens) ~ ref_pts + adv_pts + loc + covid, data=analysis, dist="weibull")
summary(weibull_1)
# Covid is not signifcant, remove

weibull_2 <- survreg(Surv(goaltime, cens) ~ ref_pts + adv_pts + loc, data=analysis, dist="weibull")
summary(weibull_2)
# All significant

anova(weibull_2, weibull_1, test="Chisq")
# No imporvement when COVID is included in the model.

### Table 25 ###
# Final Model
weibull_final <- survreg(Surv(goaltime, cens) ~ ref_pts + adv_pts + loc, data=analysis, dist="weibull")
summary(weibull_final)
# Extract Hazard ratios
ConvertWeibull(weibull_final, conf.level = 0.95)

# Plots to check PH and Weibull assumption -------------------------------------------------------------------------------------

### FIgure 15 ###
# 1) All observations
haz <- data.frame(time=km_fit$time, logtime=log(km_fit$time),
                          cumhaz=km_fit$cumhaz, logcumhaz=log(km_fit$cumhaz))
plot1 <- ggplot(data=haz, aes(x=logtime, y=logcumhaz)) +
  ggtitle("Log(Cumulative Hazard) versus Log(Time)") +
  xlab("Log(Time)") + ylab("Log(Cumulative Hazard)") +
  geom_line() + 
  theme_classic()

# 2) Split by reference ability
plot2 <- ggplot(data=ref_pos_haz, aes(x=logtime, y=logcumhaz, color=ref_pos)) +
  ggtitle("Log(Cumulative Hazard) versus Log(Time)\nSplit by Ability of Reference Team (Top 6/Not Top 6)") +
  xlab("Log(Time)") + ylab("Log(Cumulative Hazard)") +
  labs(color = "Ref Position") +
  geom_line() + 
  theme_classic()

# 3) Split by adverse ability
plot3 <- ggplot(data=adv_pos_haz, aes(x=logtime, y=logcumhaz, color=adv_pos)) +
  ggtitle("Log(Cumulative Hazard) versus Log(Time)\nSplit by Ability of Adverse Team (Top 6/Not Top 6)") +
  xlab("Log(Time)") + ylab("Log(Cumulative Hazard)") +
  labs(color = "Adv Position") +
  geom_line() + 
  theme_classic()

# 4) Split by location
plot4 <- ggplot(data=loc_haz, aes(x=logtime, y=logcumhaz, color=loc)) +
  ggtitle("Log(Cumulative Hazard) versus Log(Time)\nSplit by Location of Reference Team (Home/Away)") +
  xlab("Log(Time)") + ylab("Log(Cumulative Hazard)") +
  labs(color = "Location") +
  geom_line() + 
  theme_classic()

ggarrange(plot1, plot2, plot3, plot4)

# ------------------------------------------------------------------------------------------------------
# --------------------------------- Cox Proportional Hazards  ------------------------------------------
# ------------------------------------------------------------------------------------------------------

### Table 26 ###
cox1 <- coxph(Surv(goaltime, cens) ~ ref_pts + adv_pts + loc + covid, data = analysis)
summary(cox1)
# Covid not signifcant- remove

cox2 <- coxph(Surv(goaltime, cens) ~ ref_pts + adv_pts + loc, data = analysis)
summary(cox2)
# All variables signifcant (or borderline)

### Table 27 ###
# Final Model
cox <- coxph(Surv(goaltime, cens) ~ ref_pts + adv_pts + loc, data = analysis)
summary(cox)

# Testing proportional hazards
cz <- cox.zph(cox)
print(cz)
# We do not reject the null. There is not enough evidence to suggest that propoirtonal hazard assumption
# has been violated.
plot(cz)
# Deviation from a zero-slope line is evidence that the proportional hazards assumption is violated