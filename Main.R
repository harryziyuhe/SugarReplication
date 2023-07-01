library(haven)
library(tidyverse)
library(lmtest)
library(clusterSEs)
SugarReplicate <- read_dta("OneDrive/Harry He/UCSD/Spring 2023/Replication Game/replication data file/SugarReplicate.dta")
data <- SugarReplicate
data$yesvote <- as.numeric(data$vote == "Aye")
data$repub <- as.numeric(data$vote == "Republican")
data$rsugcont <- data$totalcont
data$rsugcont[data$cong == 115] <- data$totalcont[data$cong == 115] / 1.103

## Fix Name Mismatch
data$name[data$name == "Cárdenas"] = "Cardenas"
data$name[data$name == "Sander Levin"] = "Levin"
data$name[data$name == " Levin"] = "Levin"
data$name[data$name == "Luján"] = "Lujan"
data$name[data$name == "Velázquez"] = "Velazquez"
data$name[data$name == "Sánchez"] = "Sanchez"
data$name[data$name == "Gutiérrez"] = "Gutierrez"
data$name[data$name == " Smith"] = "Smith"

model1 <- glm(yesvote ~ rsugcont + ncu + tenure + poverty + bach + 
      medincome + over65 + agcom + factor(id), family = binomial(link = "logit"),
      data = data)
model2 <- glm(yesvote ~ rsugcont + ncu + tenure + poverty + bach + 
                medincome + over65 + agcom, family = binomial(link = "logit"),
              data = data)
summary(model2)

data_wide <- data |> 
  dplyr::select(cong, name, yesvote, rsugcont, ncu, tenure, poverty, bach, medincome, over65, agcom, id, state) |> 
  pivot_wider(names_from = cong,
              values_from = c(name, yesvote, rsugcont, ncu, tenure, poverty, bach, medincome, over65, agcom))

data_wide$diff_name <- as.numeric(data_wide$name_113 != data_wide$name_115)
data_wide$vote_change <- data_wide$yesvote_115 - data_wide$yesvote_113
data_wide$sugdiff <- data_wide$rsugcont_115 - data_wide$rsugcont_113
data_wide$ncu_diff <- data_wide$ncu_115 - data_wide$ncu_113
data_wide$pov_avg <- (data_wide$poverty_113 + data_wide$poverty_115) / 2
data_wide$bach_avg <- (data_wide$bach_113 + data_wide$bach_115) / 2
data_wide$medincome_avg <- (data_wide$medincome_113 + data_wide$medincome_115) / 2
data_wide$over65_avg <- (data_wide$over65_113 + data_wide$over65_115) / 2
data_wide$agcom_diff <- data_wide$agcom_115 - data_wide$agcom_113
data_wide <- data_wide |> 
  dplyr::select(id, state, name_113, name_115, diff_name, yesvote_113, yesvote_115,
                vote_change, sugdiff, ncu_diff, tenure_113, tenure_115, pov_avg, 
                bach_avg, medincome_avg, over65_avg, agcom_diff)

samerep_ytn <- data_wide |> 
  filter(diff_name == 0 & vote_change <= 0 & yesvote_113 == 1)
samerep_ytn$vote_change <- -samerep_ytn$vote_change

samerep_nty <- data_wide |> 
  filter(diff_name == 0 & vote_change >= 0 & yesvote_113 == 0)

model3 <- glm(vote_change ~ sugdiff + ncu_diff + tenure_113 + pov_avg + bach_avg +
                medincome_avg + over65_avg + agcom_diff + factor(state), data = samerep_ytn)
coeftest(model3, vcov = vcovHC(model3, type = "HC0", cluster = "id"))

model4 <- glm(vote_change ~ sugdiff + ncu_diff + tenure_113 + pov_avg + bach_avg +
                medincome_avg + over65_avg + agcom_diff + factor(state), data = samerep_nty)
coeftest(model4, vcov = vcovHC(model4, type = "HC0", cluster = "id"))

all_ytn <- data_wide |> 
  filter(vote_change <= 0 & yesvote_113 == 1)
all_ytn$vote_change <- -all_ytn$vote_change

all_nty <- data_wide |> 
  filter(vote_change >= 0 & yesvote_113 == 0)

model5 <- glm(vote_change ~ sugdiff * diff_name + ncu_diff + tenure_113 + pov_avg + bach_avg +
                medincome_avg + over65_avg + agcom_diff + factor(state), data = all_ytn)
coeftest(model5, vcov = vcovHC(model5, type = "HC0", cluster = "id"))

model6 <- glm(vote_change ~ sugdiff * diff_name + ncu_diff + tenure_113 + pov_avg + bach_avg +
                medincome_avg + over65_avg + agcom_diff + factor(state), data = all_nty)
coeftest(model6, vcov = vcovHC(model6, type = "HC0", cluster = "id"))
