library(lme4)
library(dplyr)
library(tidyverse)
library(pwr)
library(simr)
library(sjPlot)

# sjPlot::tab_model(context_model)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1. Data ----
## 1.1 Exp 1 ----
context.data <- read.csv("../data/1_context_choice/1_context_choice_main-trials.csv", header=TRUE) %>% 
  filter(!workerid %in% c("319", "323", "360", "312", "321", "301", "298")) # exclusion based on language

# exclusion based on filler items: 1 participant
context_filler_data <- subset(context.data, condition=="filler")
context_filler_summary <- context_filler_data %>% 
  group_by(workerid) %>% 
  summarize(error_num = sum(response=="incorrect"))
context_eligible_subjects = context_filler_summary$workerid[context_filler_summary$error_num < 2]
length(context_eligible_subjects) # 42
context.data = subset(context.data, workerid %in% context_eligible_subjects)

# data cleaning
context_clean_data <- context.data %>% 
  # filter(item_id!=9) %>%
  filter(!grepl("practice", condition)) %>% 
  filter(condition!="filler") %>% 
  mutate(response_corr = case_when(condition !="filler" & verb==response ~ "correct",
                                   condition !="filler" & verb!=response ~ "incorrect",
                                   condition == "filler" ~ response),
         response_num = if_else(response_corr == "correct", 1, 0),
         discourse_type = sub(".*_", "",condition)) %>% 
  mutate(discourse_type = if_else(discourse_type == "contrastive", "supported", "unsupported"))

## 1.2 Exp 2 ----
no_context.data <- read.csv("../data/2_no_context_choice/2_no_context_choice_main-trials.csv", header=TRUE) %>% 
  filter(!workerid %in% c("379", "371", "383", "357", "287", "369")) # exclusion based on incomplete etc.

# exclusion based on filler items # 3 participants
no_context_filler_data <- subset(no_context.data, condition=="filler")
no_context_filler_summary <- no_context_filler_data %>% 
  group_by(workerid) %>% 
  summarize(error_num = sum(response=="incorrect"))
no_context_eligible_subjects = no_context_filler_summary$workerid[no_context_filler_summary$error_num < 2]
length(no_context_eligible_subjects) # 40
no_context.data = subset(no_context.data, workerid %in% no_context_eligible_subjects)

# data cleaning
no_context_clean_data <- no_context.data %>% 
  filter(!grepl("practice", condition)) %>% 
  filter(condition!="filler") %>% 
  mutate(response_corr = case_when(condition !="filler" & verb==response ~ "correct",
                                   condition !="filler" & verb!=response ~ "incorrect",
                                   condition == "filler" ~ response),
         response_num = if_else(response_corr == "correct", 1, 0),
         discourse_type = sub(".*_", "",condition)) %>% 
  mutate(discourse_type = if_else(discourse_type == "contrastive", "supported", "unsupported"))

## 1.3 Combined Exps ----
all_data <- bind_rows(lst(context_clean_data, no_context_clean_data), .id="context") %>% 
  mutate(context = if_else(context == "context_clean_data", "presence", "absence"),
         context = fct_relevel(context, "presence", "absence")) 

all_data$verb <- as.factor(all_data$verb)
all_data$discourse_type <- as.factor(all_data$discourse_type)
all_data$context <- as.factor(all_data$context)
all_data$context <- relevel(all_data$context, ref="absence")

context_clean_data$verb <- as.factor(context_clean_data$verb)
context_clean_data$discourse_type <- as.factor(context_clean_data$discourse_type)
no_context_clean_data$verb <- as.factor(no_context_clean_data$verb)
no_context_clean_data$discourse_type <- as.factor(no_context_clean_data$discourse_type)

# 2. Power analysis ----
context_model <- glmer(response_num ~ verb * discourse_type + (1|item_id) + (1|workerid),
                       context_clean_data,
                       family=binomial,
                       control = glmerControl(
                         optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5)
                       ))
fixef(context_model) 
summary(context_model)
VarCorr(context_model)

ps_interaction <- powerSim(context_model, 
                           test = fixed("verbyiwei:discourse_typeunsupported"), 
                           nsim = 1000,
                           fitOpts = list(control = glmerControl(
                             optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))))
ps_interaction

ps_discourse <- powerSim(context_model, 
                           test = fixed("discourse_typeunsupported"), 
                           nsim = 1000,
                           fitOpts = list(control = glmerControl(
                             optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))))
ps_discourse

# having more participants
model_extended <- extend(context_model, along = "workerid", n = 150)
context_pwrcurve <- powerCurve(model_extended,
                               test = fixed("verbyiwei:discourse_typeunsupported"),
                               along = "workerid",
                               breaks = c(42, 60, 80, 100, 120, 150),
                               nsim = 200,
                               fitOpts = list(control = glmerControl(
                                 optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))))
saveRDS(context_pwrcurve, file= "power_analysis/context_pwrcurve_participants.rds")
plot(context_pwrcurve)
pdf("power_analysis/context_pwrcurve_participants.pdf", width=8, height=6)
print(plot(context_pwrcurve))
dev.off()

# having more items
model_extended_items <- extend(context_model, along = "item_id", n = 80)
context_pwrcurve_items <- powerCurve(model_extended_items,
                       test = fixed("verbyiwei:discourse_typeunsupported"),
                       along = "item_id",
                       breaks = c(10, 20, 30, 40, 60, 80),
                       nsim = 500,
                       fitOpts = list(control = glmerControl(
                         optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))))
saveRDS(model_extended_items, file= "power_analysis/context_pwrcurve_items.rds")
plot(context_pwrcurve_items)
pdf("power_analysis/context_pwrcurve_items.pdf", width=8, height=6)
print(plot(context_pwrcurve_items))
dev.off()

# having both more participants and more items
model_extended_both <- extend(context_model, along = "workerid", n = 60)
model_extended_both <- extend(model_extended_both, along = "item_id", n = 120)
context_pwrcurve_both <- powerCurve(model_extended_both,
                                     test = fixed("verbyiwei:discourse_typeunsupported"),
                                     along = "item_id",
                                     breaks = c(10, 30, 60, 90, 120),
                                     nsim = 500,
                                     fitOpts = list(control = glmerControl(
                                       optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))))
# worker_id=60, incremental items
saveRDS(context_pwrcurve_both, file= "power_analysis/context_pwrcurve_both_2.rds")
plot(context_pwrcurve_both)
pdf("power_analysis/context_pwrcurve_both_2.pdf", width=8, height=6)
print(plot(context_pwrcurve_both))
dev.off()

# item=120, incremental worker_id
saveRDS(context_pwrcurve_both, file= "power_analysis/context_pwrcurve_both_1.rds")
plot(context_pwrcurve_both)
pdf("power_analysis/context_pwrcurve_both_1.pdf", width=8, height=6)
print(plot(context_pwrcurve_both))
dev.off()

