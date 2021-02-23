# ---------------------------------------------------------------------------------------------------------
# ---------------------------------------- Set up Workspace -----------------------------------------------
# ---------------------------------------------------------------------------------------------------------

setwd("./survival_analysis/data")

#install.packages("ggfortify")

library(readr)
library(dplyr)
library(lubridate)
library(survival)
library(ggplot2)
library(ggfortify)

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
            by=c("adv"="team"))

analysis$covid <- factor(analysis$covid, levels = c("Pre", "Post"))

# -------------------------------------------------------------------------------------------------------
# ------------------------------------------- Testing ---------------------------------------------------
# -------------------------------------------------------------------------------------------------------

barplot(table(analysis$goaltime))

test <- analysis %>% 
  filter(cens==1)

barplot(table(test$goaltime))

# ------------------------------------------------------------------------------------------------------
# --------------------------------------- Kaplan Meier -------------------------------------------------
# ------------------------------------------------------------------------------------------------------

# Kaplan Meier Survival object

km <- with(analysis, Surv(goaltime, cens))
head(km, 80)
# Note that a “+” after the time in the print out of km indicates censoring.

# General Kaplan Meier Model 
km_fit <- survfit(Surv(goaltime, cens) ~ 1, data=analysis)
summary(km_fit)
autoplot(km_fit)

# Kaplan Meier Model - by location
km_loc_fit <- survfit(Surv(goaltime, cens) ~ loc, data=analysis)
summary(km_loc_fit)
autoplot(km_loc_fit)

# Kaplan Meier Model - by covid
km_covid_fit <- survfit(Surv(goaltime, cens) ~ covid, data=analysis)
summary(km_covid_fit)
autoplot(km_covid_fit)


# ------------------------------------------------------------------------------------------------------
# --------------------------------- Cox Proportional Hazards  ------------------------------------------
# ------------------------------------------------------------------------------------------------------

cox <- coxph(Surv(goaltime, cens) ~ loc, data = analysis)
summary(cox)

cox_fit <- survfit(cox)
autoplot(cox_fit)
