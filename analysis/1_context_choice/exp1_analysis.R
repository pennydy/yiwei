library(lme4)
library(dplyr)
library(emmeans)
library(tidyverse)
library(ggplot2)
library(ggsignif)
library(tidytext)
library(RColorBrewer)
library(stringr)

theme_set(theme_bw())
# color-blind-friendly palette
# cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
cbPalette <- c("#E69F00", "#009E73") 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("helpers.R")

# 0. schematic ----
hypothetical.data <- data.frame(
  verb = c("juede", "juede", "yiwei", "yiwei"),
  discourse_type = c("clear", "unclear", "clear", "unclear"),
  # accuracy = c(0.85, 0.5, 0.85, 0.2)
  # accuracy = c(0.85, 0.83, 0.85, 0.83)
  accuracy = c(0.85, 0.83, 0.23, 0.21)
) %>% 
  mutate(verb=fct_relevel(as.factor(verb), "yiwei", "juede"))

ggplot(data=hypothetical.data,
       aes(x=verb,
           y=accuracy,
           fill=verb,
           alpha=discourse_type)) +
  geom_hline(yintercept=0.5,linetype = "dashed", color="lightgrey")+
  geom_bar(stat="identity",
           position=position_dodge(width=0.8),
           width=0.8,
           aes(color=verb)) +
  theme_bw() +
  ylim(0,1)+
  scale_fill_manual(values=cbPalette, guide = NULL) +
  scale_color_manual(values=cbPalette, guide = NULL) +
  scale_alpha_discrete(range = c(0.4, 0.9), name="context cue")

# 1. Data ----
context.data <- read.csv("../../data/1_context_choice/1_context_choice-sandbox-trials.csv", header=TRUE) %>% 
  filter(!workerid %in% c("250")) # exclusion based on incomplete etc.

# exclusion based on filler items
context_filler_data <- subset(context.data, condition=="filler")
context_filler_summary <- context_filler_data %>% 
  group_by(workerid) %>% 
  summarize(error_num = sum(response=="incorrect"))
eligible_subjects = context_filler_summary$workerid[context_filler_summary$error_num < 2]

# data cleaning
context.data = subset(context.data, workerid %in% eligible_subjects)
context_clean_data <- context.data %>% 
  filter(!grepl("practice", condition)) %>% 
  filter(condition!="filler") %>% 
  mutate(response_corr = case_when(condition !="filler" & verb==response ~ "correct",
                                   condition !="filler" & verb!=response ~ "incorrect",
                                   condition == "filler" ~ response),
         response_num = if_else(response_corr == "correct", 1, 0),
         discourse_type = sub(".*_", "",condition)) %>% 
  mutate(discourse_type = if_else(discourse_type == "contrastive", "clear", "unclear"))
  
context_summary <- context_clean_data %>% 
  group_by(condition, verb, discourse_type) %>% 
  summarize(accuracy = mean(response_num),
            CILow = ci.low(response_num),
            CIHigh = ci.high(response_num)) %>% 
  ungroup() %>% 
  mutate(YMin = accuracy-CILow,
         YMax = accuracy+CIHigh)

# 2. Plot ----
ggplot(data=context_summary %>% 
         mutate(verb = fct_relevel(verb, "yiwei", "juede")),
       aes(x=verb,
           y=accuracy,
           fill=verb,
           alpha=discourse_type)) +
  geom_hline(yintercept=0.5,linetype = "dashed", color="lightgrey")+
  geom_bar(stat="identity",
           position=position_dodge(width=0.8),
           width=0.8,
           aes(color=verb)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax), 
                width=.2,
                position=position_dodge(width=0.8),
                show.legend = FALSE) +
  theme_bw() +
  scale_fill_manual(values=cbPalette, guide = NULL) +
  scale_color_manual(values=cbPalette, guide = NULL) +
  ylim(0,1)+
  scale_alpha_discrete(range = c(0.4, 0.9), name="context cue")

