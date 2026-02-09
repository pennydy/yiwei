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

# 0. schematic plots ----
hypothetical.data <- data.frame(
  verb = c("juede", "juede", "yiwei", "yiwei"),
  discourse_type = c("clear", "unclear", "clear", "unclear"),
  accuracy = c(0.43, 0.45, 0.62, 0.6)
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
  scale_fill_manual(values=cbPalette, guide = NULL) +
  scale_color_manual(values=cbPalette, guide = NULL) +
  ylim(0,1)+
  scale_alpha_discrete(range = c(0.4, 0.9), name="context cue")

# 1. Data ----
context_blank.data <- read.csv("../../data/3_context_blank/3_context_blank-trials.csv", header=TRUE) %>% 
  filter(!workerid %in% c("387")) # exclusion based on incomplete etc.

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

no_context_summary <- no_context_clean_data %>% 
  group_by(condition, verb, discourse_type) %>% 
  summarize(accuracy = mean(response_num),
            CILow = ci.low(response_num),
            CIHigh = ci.high(response_num)) %>% 
  ungroup() %>% 
  mutate(YMin = accuracy-CILow,
         YMax = accuracy+CIHigh)

no_context_participant_accuracy <- no_context_clean_data %>% 
  group_by(workerid, condition, verb, discourse_type) %>% 
  summarize(accuracy = mean(response_num),
            CILow = ci.low(response_num),
            CIHigh = ci.high(response_num)) %>% 
  ungroup() %>% 
  mutate(YMin = accuracy-CILow,
         YMax = accuracy+CIHigh)

no_context_item_accuracy <- no_context_clean_data %>% 
  group_by(item_id, condition, verb, discourse_type) %>% 
  summarize(accuracy = mean(response_num),
            CILow = ci.low(response_num),
            CIHigh = ci.high(response_num)) %>% 
  ungroup() %>% 
  mutate(YMin = accuracy-CILow,
         YMax = accuracy+CIHigh)

# 2. Plot ----
no_context_plot <- ggplot(data=no_context_summary,
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
  ylim(0,1)+
  scale_fill_manual(values=cbPalette, guide = NULL) +
  scale_color_manual(values=cbPalette, guide = NULL) +
  scale_alpha_discrete(range = c(0.4, 0.9), name="Discourse type") +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12))
no_context_plot
ggsave(no_context_plot, file="graphs/exp2_no_context.pdf", width=7, height=4)

# violin plots
no_context_plot_violin <- ggplot(data=no_context_item_accuracy %>% 
         mutate(verb = fct_relevel(verb, "yiwei", "juede")),
       aes(x=verb,
           y=accuracy,
           fill=verb,
           alpha=discourse_type)) +
  geom_hline(yintercept=0.5,linetype = "dashed", color="lightgrey")+
  geom_point(aes(shape=discourse_type),
             position=position_dodge2(width=.8,preserve = "single")) +
  geom_violin(data=no_context_participant_accuracy %>% 
                mutate(verb = fct_relevel(verb, "yiwei", "juede")),
              position=position_dodge(width=.8)) +
  geom_boxplot(data=no_context_participant_accuracy %>% 
                 mutate(verb = fct_relevel(verb, "yiwei", "juede")),
               width=0.1,
               position=position_dodge(width=.8),
               show.legend = FALSE) +
  theme_bw() +
  scale_fill_manual(values=cbPalette, guide = NULL) +
  ylim(0,1)+
  scale_alpha_discrete(range = c(0.4, 0.9), name="Discourse type") +
  scale_shape_manual(values=c("supported"=22, "unsupported"=24), name="Discourse type") +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12)) +
  guides(alpha = guide_legend(override.aes = list(fill = "grey40")))+
  labs(x="Verb",
       y="Accuracy")
no_context_plot_violin
ggsave(no_context_plot_violin, file="graphs/exp2_no_context-violin.pdf", width=6, height=4)

# lines connecting dots for individual means
ggplot(data=no_context_participant_accuracy %>% 
         mutate(condition = fct_relevel(condition, "yiwei_contrastive", 
                                        "yiwei_unclear",
                                        "juede_contrastive",
                                        "juede_unclear")),
       aes(x=condition,
           y=accuracy,
           color=discourse_type)) +
  geom_hline(yintercept=0.5,linetype = "dashed", color="lightgrey")+
  geom_line(aes(group=interaction(workerid,verb)),
            position=position_dodge2(width=.8,preserve="single"),
            color="black",
            alpha=0.5)+
  geom_point(position=position_dodge2(width=.8,preserve = "single"),
             alpha=0.6)+
  theme_bw() +
  scale_fill_manual(values=cbPalette, guide = NULL) +
  ylim(0,1)+
  scale_alpha_discrete(range = c(0.4, 0.9), name="Discourse type") +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12))

# lines connecting dots for individual means by item
ggplot(data=no_context_item_accuracy %>% 
         mutate(condition = fct_relevel(condition, "yiwei_contrastive", 
                                        "yiwei_unclear",
                                        "juede_contrastive",
                                        "juede_unclear")),
       aes(x=condition,
           y=accuracy,
           color=discourse_type)) +
  geom_hline(yintercept=0.5,linetype = "dashed", color="lightgrey")+
  geom_point(position=position_dodge2(width=.8,preserve = "single"),
             alpha=0.6)+
  theme_bw() +
  scale_fill_manual(values=cbPalette, guide = NULL) +
  ylim(0,1)+
  scale_alpha_discrete(range = c(0.4, 0.9), name="Discourse type") +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12))


# 3. Analysis ----
# sum coding
no_context_clean_data$verb <- as.factor(no_context_clean_data$verb)
contrasts(no_context_clean_data$verb) <- contr.sum(2)
contrasts(no_context_clean_data$verb)
no_context_clean_data$discourse_type <- as.factor(no_context_clean_data$discourse_type)
contrasts(no_context_clean_data$discourse_type) <- contr.sum(2)
contrasts(no_context_clean_data$discourse_type)

no_context_model <- glmer(response_num ~ verb * discourse_type + (1|item_id) + (1+verb*discourse_type|workerid),
      data=no_context_clean_data,
      family=binomial,
      control = glmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 2e5)
      ))
summary(no_context_model)
VarCorr(no_context_model)

simple_no_context_model <- glmer(response_num ~ verb * discourse_type + (1+verb*discourse_type|workerid),
                                 data=no_context_clean_data,
                                 family=binomial,
                                 control = glmerControl(
                                   optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)
                                 ))
summary(simple_no_context_model)

# 4. Combined with Exp1 ----
## 4.1 data ----
context.data <- read.csv("../../data/1_context_choice/1_context_choice_main-trials.csv", header=TRUE) %>% 
  filter(!workerid %in% c("319", "323", "360", "312", "321", "301", "298")) # exclusion based on language

# exclusion based on filler items: 1 participant
context_filler_data <- subset(context.data, condition=="filler")
context_filler_summary <- context_filler_data %>% 
  group_by(workerid) %>% 
  summarize(error_num = sum(response=="incorrect"))
context_eligible_subjects = context_filler_summary$workerid[context_filler_summary$error_num < 2]
length(context_eligible_subjects) # 42
context.data = subset(context.data, workerid %in% context_eligible_subjects)

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
all_data <- bind_rows(lst(context_clean_data, no_context_clean_data), .id="context") %>% 
  mutate(context = if_else(context == "context_clean_data", "presence", "absence"),
         context = fct_relevel(context, "presence", "absence")) 

all_summary <- all_data %>% 
  group_by(condition, verb, discourse_type, context) %>% 
  summarize(accuracy = mean(response_num),
            CILow = ci.low(response_num),
            CIHigh = ci.high(response_num)) %>% 
  ungroup() %>% 
  mutate(YMin = accuracy-CILow,
         YMax = accuracy+CIHigh,
         context = fct_relevel(context, "presence", "absence"))

all_participant_accuracy <- all_data %>% 
  group_by(workerid, condition, verb, discourse_type, context) %>% 
  summarize(accuracy = mean(response_num),
            CILow = ci.low(response_num),
            CIHigh = ci.high(response_num)) %>% 
  ungroup() %>% 
  mutate(YMin = accuracy-CILow,
         YMax = accuracy+CIHigh,
         context = fct_relevel(context, "presence", "absence"))

all_item_accuracy <- all_data %>% 
  group_by(item_id, condition, verb, discourse_type, context) %>% 
  summarize(accuracy = mean(response_num),
            CILow = ci.low(response_num),
            CIHigh = ci.high(response_num)) %>% 
  ungroup() %>% 
  mutate(YMin = accuracy-CILow,
         YMax = accuracy+CIHigh,
         context = fct_relevel(context, "presence", "absence"))

## 4.2 plot ----
context_type <- list("presence"="Context Presence",
                     "absence"="Context Absence")
context_labeller <- function(variable,value){
  return(context_type[value])
}

all_plot_violin <- ggplot(data=all_item_accuracy %>% 
                            mutate(verb = fct_relevel(verb, "yiwei", "juede")),
                                 aes(x=verb,
                                     y=accuracy,
                                     fill=verb,
                                     alpha=discourse_type)) +
  geom_hline(yintercept=0.5,linetype = "dashed", color="lightgrey")+
  geom_point(aes(shape=discourse_type),
             position=position_dodge2(width=.8,preserve = "single")) +
  geom_violin(data=all_participant_accuracy %>% 
                mutate(verb = fct_relevel(verb, "yiwei", "juede")),
              position=position_dodge(width=.8)) +
  geom_boxplot(data=all_participant_accuracy %>% 
                 mutate(verb = fct_relevel(verb, "yiwei", "juede")),
               width=0.1,
               position=position_dodge(width=.8),
               show.legend = FALSE) +
  theme_bw() +
  ylim(0,1) +
  facet_grid(.~context,labeller=context_labeller) +
  scale_fill_manual(values=cbPalette, guide = NULL) +
  scale_alpha_discrete(range = c(0.3, 0.9), name="Discourse type") +
  scale_shape_manual(values=c("supported"=22, "unsupported"=24), name="Discourse type") +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        strip.text.x = element_text(size = 10)) +
  guides(alpha = guide_legend(override.aes = list(fill = "grey40")))+
  labs(x="Verb",
       y="Accuracy")
all_plot_violin
ggsave(all_plot_violin, file="graphs/all_no_context-violin_1.pdf", width=8, height=4)

## 4.3 analysis ----
all_data$verb <- as.factor(all_data$verb)
contrasts(all_data$verb) <- contr.sum(2)
contrasts(all_data$verb)
all_data$discourse_type <- as.factor(all_data$discourse_type)
contrasts(all_data$discourse_type) <- contr.sum(2)
contrasts(all_data$discourse_type)
all_data$context <- as.factor(all_data$context)
all_data$context <- relevel(all_data$context, ref="absence")
contrasts(all_data$context) <- contr.sum(2)
contrasts(all_data$context)

all_model <- glmer(response_num ~ verb * discourse_type * context + (1 + context|item_id) + (1+verb+discourse_type+context|workerid),
                          data=all_data,
                          family=binomial,
                   control = glmerControl(
                     optimizer = "bobyqa",
                     optCtrl = list(maxfun = 2e5)
                   ))
summary(all_model)
VarCorr(all_model)

emmeans(all_model, ~ verb, type = "response")
pairs(emmeans(all_model, ~context|discourse_type))

simple_all_model <- glmer(response_num ~ verb * discourse_type * context + (1+verb+discourse_type+context|workerid),
                   data=all_data,
                   family=binomial,
                   control = glmerControl(
                     optimizer = "bobyqa",
                     optCtrl = list(maxfun = 2e5)
                   ))
summary(simple_all_model)
