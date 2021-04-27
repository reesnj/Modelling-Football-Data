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
  mutate(adv_ability = ifelse(adv_pos<=6, "Top 6", "Not Top 6")) %>% 
  mutate(ref_attack = ifelse(ref_gf>52, "Strong", "Weak")) %>% 
  mutate(ref_defence = ifelse(ref_ga<51, "Strong", "Weak")) %>% 
  mutate(adv_attack = ifelse(adv_gf>52, "Strong", "Weak")) %>% 
  mutate(adv_defence = ifelse(adv_ga<51, "Strong", "Weak"))

analysis$covid <- factor(analysis$covid, levels = c("Pre", "Post"))
analysis$loc <- factor(analysis$loc, levels = c("Away", "Home"))
analysis$ref_ability <- factor(analysis$ref_ability, levels = c("Not Top 6", "Top 6"))
analysis$adv_ability <- factor(analysis$adv_ability, levels = c("Not Top 6", "Top 6"))
analysis$ref_attack <- factor(analysis$ref_attack, levels = c("Weak", "Strong"))
analysis$ref_defence <- factor(analysis$ref_defence, levels = c("Weak", "Strong"))
analysis$adv_attack <- factor(analysis$adv_attack, levels = c("Weak", "Strong"))
analysis$adv_defence <- factor(analysis$adv_defence, levels = c("Weak", "Strong"))

# -------------------------------------------------------------------------------------------------------
# ----------------------------------------- Exploration -------------------------------------------------
# -------------------------------------------------------------------------------------------------------

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

# Number of censored observations
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
ggsurvplot(km_fit, data=analysis, title="Kaplan-Meier Plot of Time Taken for Reference Team to Score First Goal", xlab="Time (Minutes)",
           break.time.by=15)

# The median time in which a team scores their first goal in a match is the 51st minute.
# A team will score their first goal in a match within the first 24 minutes, 25% of the time. 
# A team will fail to score at all in the match, 26.5% of the time. 
# The largest observations (90 mins) are censored, s0 we cannot calculate the exact mean.

# Kaplan Meier Model - by location ----------------------------------------------------------------------

km_loc <- survfit(Surv(goaltime, cens) ~ loc, data=analysis)
km_loc
summary(km_loc)
summary(km_loc)$table 
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
ggplot(data=loc_haz, aes(x=logtime, y=logcumhaz, color=loc)) +
  ggtitle("Log(Cumulative Hazard) versus Log(Time) - Split by Location of Reference Team (Home/Away)") +
  xlab("Log(Time)") + ylab("Log(Cumulative Hazard)") +
  labs(color = "Location") +
  geom_line() + 
  theme_classic()
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

km_covid <- survfit(Surv(goaltime, cens) ~ covid, data=analysis)
km_covid
summary(km_covid)
summary(km_covid)$table
ggsurvplot(km_covid, data=analysis, title="Kaplan-Meier Plot of Time Taken for Reference Team to Score First Goal\nSplit by (Pre/Post) Covid-19", xlab="Time (Minutes)",
           break.time.by=15, conf.int = TRUE, legend.labs = c("Pre","Post"))

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

# Log Rank Test
logrank_covid <- survdiff(Surv(goaltime, cens) ~ covid, data=analysis)
logrank_covid
# No signifciant difference between the survival times based on Covid (pre/post)

# Wilcoxon Test
wilcoxon_covid <- survdiff(Surv(goaltime, cens) ~ covid, rho=1, data=analysis)
wilcoxon_covid
# No signifciant difference between the survival times based on covid (pre/post)


# Kaplan Meier Model - by ref team ability --------------------------------------------------------------

km_ref_pos <- survfit(Surv(goaltime, cens) ~ ref_ability, data=analysis)
km_ref_pos
summary(km_ref_pos)
summary(km_ref_pos)$table
ggsurvplot(km_ref_pos, data=analysis, title="Kaplan-Meier Plot of Time Taken for Reference Team to Score First Goal\nSplit by Ability of Reference Team (Top 6/Not Top 6)", xlab="Time (Minutes)",
           break.time.by=15, conf.int = TRUE, legend.labs = c("Not Top 6","Top 6"))


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

km_adv_pos <- survfit(Surv(goaltime, cens) ~ adv_ability, data=analysis)
km_adv_pos
summary(km_adv_pos)
summary(km_adv_pos)$table
ggsurvplot(km_adv_pos, data=analysis, title="Kaplan-Meier Plot of Time Taken for Reference Team to Score First Goal\nSplit by Ability of Adverse Team (Top 6/Not Top 6)", xlab="Time (Minutes)",
           break.time.by=15, conf.int = TRUE, legend.labs = c("Not Top 6","Top 6"))
surv_median(km_adv_pos)

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


# Kaplan Meier Model - by ref team attack --------------------------------------------------------------

km_ref_attack <- survfit(Surv(goaltime, cens) ~ ref_attack, data=analysis)
km_ref_attack
summary(km_ref_attack)
summary(km_ref_attack)$table
ggsurvplot(km_ref_attack, data=analysis, title="Kaplan-Meier Plot of Time to First Goal - Split by Reference Team Attacking Strength", xlab="Time (Minutes)",
           break.time.by=15, conf.int = TRUE, legend.labs = c("Weak","Strong"))

# Test proportional Hazards assumption
ref_attack_haz <- data.frame(time=km_ref_attack$time, logtime=log(km_ref_attack$time),
                          cumhaz=km_ref_attack$cumhaz, logcumhaz=log(km_ref_attack$cumhaz)) %>% 
  mutate(ref_attack=case_when(
    row_number() < 83 ~ "Weak",
    row_number() >= 83 ~ "Strong",
    TRUE ~ "Missing"
  ))
ggplot(data=ref_attack_haz, aes(x=logtime, y=logcumhaz, color=ref_attack)) +
  ggtitle("Log(Cumulative Hazard) versus Log(Time)") +
  xlab("Log(Time)") + ylab("Log(Cumulative Hazard)") +
  labs(color = "Ref Attack") +
  geom_line()

# Log Rank Test
logrank_ref_attack <- survdiff(Surv(goaltime, cens) ~ ref_attack, data=analysis)
logrank_ref_attack
# There is a significant difference between the time in which first goal is scored for reference teams with
# a strong attack, compared to reference teams with a weak attack

# Wilcoxon Test
wilcoxon_ref_attack <- survdiff(Surv(goaltime, cens) ~ ref_attack, rho=1, data=analysis)
wilcoxon_ref_attack
# There is a significant difference between the time in which first goal is scored for reference teams with
# a strong attack, compared to reference teams with a weak attack


# Kaplan Meier Model - by ref team defence --------------------------------------------------------------

km_ref_defence <- survfit(Surv(goaltime, cens) ~ ref_defence, data=analysis)
km_ref_defence
summary(km_ref_defence)
summary(km_ref_defence)$table
ggsurvplot(km_ref_defence, data=analysis, title="Kaplan-Meier Plot of Time to First Goal - Split by Reference Team Defensive Strength", xlab="Time (Minutes)",
           break.time.by=15, conf.int = TRUE, legend.labs = c("Weak","Strong"))

# Test proportional Hazards assumption
ref_defence_haz <- data.frame(time=km_ref_defence$time, logtime=log(km_ref_defence$time),
                             cumhaz=km_ref_defence$cumhaz, logcumhaz=log(km_ref_defence$cumhaz)) %>% 
  mutate(ref_defence=case_when(
    row_number() < 86 ~ "Weak",
    row_number() >= 86 ~ "Strong",
    TRUE ~ "Missing"
  ))
ggplot(data=ref_defence_haz, aes(x=logtime, y=logcumhaz, color=ref_defence)) +
  ggtitle("Log(Cumulative Hazard) versus Log(Time)") +
  xlab("Log(Time)") + ylab("Log(Cumulative Hazard)") +
  labs(color = "Ref Defence") +
  geom_line()

# Log Rank Test
logrank_ref_defence <- survdiff(Surv(goaltime, cens) ~ ref_defence, data=analysis)
logrank_ref_defence
# There is a significant difference between the time in which first goal is scored for reference teams with
# a strong defence, compared to reference teams with a weak defence

# Wilcoxon Test
wilcoxon_ref_defence <- survdiff(Surv(goaltime, cens) ~ ref_defence, rho=1, data=analysis)
wilcoxon_ref_defence
# There is a significant difference between the time in which first goal is scored for reference teams with
# a strong defence, compared to reference teams with a weak defence


# Kaplan Meier Model - by adv team attack --------------------------------------------------------------

km_adv_attack <- survfit(Surv(goaltime, cens) ~ adv_attack, data=analysis)
km_adv_attack
summary(km_adv_attack)
summary(km_adv_attack)$table
ggsurvplot(km_adv_attack, data=analysis, title="Kaplan-Meier Plot of Time to First Goal - Split by Adverse Team Attacking Strength", xlab="Time (Minutes)",
           break.time.by=15, conf.int = TRUE, legend.labs = c("Weak","Strong"))

# Test proportional Hazards assumption
adv_attack_haz <- data.frame(time=km_adv_attack$time, logtime=log(km_adv_attack$time),
                             cumhaz=km_adv_attack$cumhaz, logcumhaz=log(km_adv_attack$cumhaz)) %>% 
  mutate(adv_attack=case_when(
    row_number() < 79 ~ "Weak",
    row_number() >= 79 ~ "Strong",
    TRUE ~ "Missing"
  ))
ggplot(data=adv_attack_haz, aes(x=logtime, y=logcumhaz, color=adv_attack)) +
  ggtitle("Log(Cumulative Hazard) versus Log(Time)") +
  xlab("Log(Time)") + ylab("Log(Cumulative Hazard)") +
  labs(color = "Adv Attack") +
  geom_line()

# Log Rank Test
logrank_adv_attack <- survdiff(Surv(goaltime, cens) ~ adv_attack, data=analysis)
logrank_adv_attack
# There is no significant difference between the time in which first goal is scored for adverse teams with
# a strong attack, compared to adverse teams with a weak attack

# Wilcoxon Test
wilcoxon_adv_attack <- survdiff(Surv(goaltime, cens) ~ adv_attack, rho=1, data=analysis)
wilcoxon_adv_attack
# There is no significant difference between the time in which first goal is scored for adverse teams with
# a strong attack, compared to adverse teams with a weak attack


# Kaplan Meier Model - by adv team defence --------------------------------------------------------------

km_adv_defence <- survfit(Surv(goaltime, cens) ~ adv_defence, data=analysis)
km_adv_defence
summary(km_adv_defence)
summary(km_adv_defence)$table
ggsurvplot(km_adv_defence, data=analysis, title="Kaplan-Meier Plot of Time to First Goal - Split by Adverse Team Attacking Strength", xlab="Time (Minutes)",
           break.time.by=15, conf.int = TRUE, legend.labs = c("Weak","Strong"))

# Test proportional Hazards assumption
adv_defence_haz <- data.frame(time=km_adv_defence$time, logtime=log(km_adv_defence$time),
                             cumhaz=km_adv_defence$cumhaz, logcumhaz=log(km_adv_defence$cumhaz)) %>% 
  mutate(adv_defence=case_when(
    row_number() < 80 ~ "Weak",
    row_number() >= 80 ~ "Strong",
    TRUE ~ "Missing"
  ))
ggplot(data=adv_defence_haz, aes(x=logtime, y=logcumhaz, color=adv_defence)) +
  ggtitle("Log(Cumulative Hazard) versus Log(Time)") +
  xlab("Log(Time)") + ylab("Log(Cumulative Hazard)") +
  labs(color = "Adv Defence") +
  geom_line()

# Log Rank Test
logrank_adv_defence <- survdiff(Surv(goaltime, cens) ~ adv_defence, data=analysis)
logrank_adv_defence
# There is no significant difference between the time in which first goal is scored for adverse teams with
# a strong attack, compared to adverse teams with a weak attack

# Wilcoxon Test
wilcoxon_adv_defence <- survdiff(Surv(goaltime, cens) ~ adv_defence, rho=1, data=analysis)
wilcoxon_adv_defence
# There is no significant difference between the time in which first goal is scored for adverse teams with
# a strong attack, compared to adverse teams with a weak attack


# ------------------------------------------------------------------------------------------------------
# -------------------------------------- Weibull Model  ------------------------------------------------
# ------------------------------------------------------------------------------------------------------

weibull_1 <- survreg(Surv(goaltime, cens) ~ ref_pts + adv_pts + loc + covid, data=analysis, dist="weibull")
summary(weibull_1)
# Covid is not signifcant, remove

weibull_2 <- survreg(Surv(goaltime, cens) ~ ref_pts + adv_pts + loc, data=analysis, dist="weibull")
summary(weibull_2)
# All significant

anova(weibull_2, weibull_1, test="Chisq")
# No imporvement when COVID is included in the model.

# Final Model
weibull_final <- survreg(Surv(goaltime, cens) ~ ref_pts + adv_pts + loc, data=analysis, dist="weibull")
summary(weibull_final)

# Extract Hazard ratios
ConvertWeibull(weibull_final, conf.level = 0.95)

# Plots to check PH and Weibull assumption -------------------------------------------------------------------------------------

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

cox1 <- coxph(Surv(goaltime, cens) ~ ref_pts + adv_pts + loc + covid, data = analysis)
summary(cox1)
# Covid not signifcant- remove

cox2 <- coxph(Surv(goaltime, cens) ~ ref_pts + adv_pts + loc, data = analysis)
summary(cox2)
# All variables signifcant (or borderline)

# Final Model
cox <- coxph(Surv(goaltime, cens) ~ ref_pts + adv_pts + loc, data = analysis)
summary(cox)

# Hazard ratios
# For every additional 10 points the reference team accumulated in the previous season,
#    the risk of the reference team scoring their first goal increases by 16.3%.
# For every additional 10 points the adverse team obtained in the previous season, 
#    the risk of the reference team scoring their first goal decreases by 6.4%
# The risk of the reference team scoring their first goal is 21.7% higher when they 
#     are playing at home rather than away 


# Testing proportional hazards
cz <- cox.zph(cox)
print(cz)
# We do not reject the null. There is not enough evidence to suggest that propoirtonal hazard assumption
# has been violated.
plot(cz)
# Deviation from a zero-slope line is evidence that the proportional hazards assumption is violated



# Illustrating data manipulation with one match --------------------------------------------------------------

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

# Illustrating struvture of data --------------------------------------------------------------

structure <- select(analysis, Reference=ref, Adverse=adv, Location=loc, Covid=covid, Ref_Points=ref_pts, Ref_Ability=ref_ability,
                    Adv_Points=adv_pts, Adv_Ability=adv_ability, Censored=cens, Goal_Time=goaltime)[c(8, 310),]
