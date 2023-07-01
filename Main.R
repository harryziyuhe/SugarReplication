library(haven)
library(tidyverse)
library(lmtest)
library(clusterSEs)
library(marginaleffects)
library(margins)
library(foreign)
SugarReplicate <- read_dta("Documents/GitHub/SugarReplication/replication data file/SugarReplicate.dta")
SugarReplicate <- read_dta("/Users/teri/Documents/GitHub/SugarReplication/replication data file/SugarReplicate.dta")
sugdata <- SugarReplicate
sugdata$yesvote <- as.numeric(sugdata$vote == "Aye")
sugdata$repub <- as.numeric(sugdata$vote == "Republican")
sugdata$rsugcont <- sugdata$totalcont
sugdata$rsugcont[sugdata$cong == 115] <- sugdata$totalcont[sugdata$cong == 115] / 1.103

## Fix Name Mismatch
sugdata$name[sugdata$name == "Cárdenas"] = "Cardenas"
sugdata$name[sugdata$name == "Sander Levin"] = "Levin"
sugdata$name[sugdata$name == " Levin"] = "Levin"
sugdata$name[sugdata$name == "Luján"] = "Lujan"
sugdata$name[sugdata$name == "Velázquez"] = "Velazquez"
sugdata$name[sugdata$name == "Sánchez"] = "Sanchez"
sugdata$name[sugdata$name == "Gutiérrez"] = "Gutierrez"
sugdata$name[sugdata$name == " Smith"] = "Smith"
sugdata$id <- relevel(as.factor(sugdata$id), ref = "5506")

# models 1 and 2 are the author's model 
model1 <- glm(yesvote ~ rsugcont + ncu + tenure + poverty + bach + 
                medincome + over65 + agcom + id, family = binomial(link = "logit"),
              data = sugdata)
coeftest(model1, vcov = vcovHC(model1, type = "HC0"))

model2 <- glm(yesvote ~ rsugcont + ncu + tenure + poverty + bach + 
                medincome + over65 + agcom + id, family = binomial(link = "logit"),
              data = sugdata[sugdata$both == 1, ])
coeftest(model2, vcov = vcovHC(model2, type = "HC0"))

data_wide <- sugdata |> 
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

# Models 3 4 5 and 6 are our models based
model3 <- glm(vote_change ~ sugdiff * ncu_diff + tenure_113 + pov_avg + bach_avg +
                medincome_avg + over65_avg + agcom_diff + factor(state), 
              family = binomial(link = "logit"), data = samerep_ytn)
coeftest(model3, vcov = vcovHC(model3, type = "HC0", cluster = "id"))

model4 <- glm(vote_change ~ sugdiff + ncu_diff + tenure_113 + pov_avg + bach_avg +
                medincome_avg + over65_avg + agcom_diff + factor(state), 
              family = binomial(link = "logit"), data = samerep_nty)
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

plot_predictions(model3, condition = "sugdiff")
plot_predictions(model1, condition = "rsugcont")
x = cplot(model1, x = "rsugcont")
margins(model1, type = "response")
x = cplot(model3, "sugdiff", what = "prediction", main = "Predicted Fuel Economy, Given Weight")
cplot(randommod, "bach")
margins(model1, at = list(rsugcont = seq(0, 20000, 500)))

summary(model1)
