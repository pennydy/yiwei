library(lme4)
library(dplyr)
library(emmeans)
library(tidyverse)
library(ggplot2)
library(ggsignif)
library(tidytext)
library(RColorBrewer)
library(stringr)
library(forcats)

theme_set(theme_bw())
# color-blind-friendly palette
# cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
cbPalette <- c("#E69F00", "#009E73") 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("helpers.R")

# 0. schematic plots ----
n  <- 40
sd <- 0.1
hypothetical.data <- expand_grid(
  verb = c("yiwei", "juede"),
  discourse_type = c("supported", "unsupported"),
  context = c("Context Presence", "Context Absence"),
  id = 1:n
) %>%
  mutate(
    mean_accuracy = case_when(
      verb == "yiwei" & discourse_type == "supported" & context == "Context Presence"  ~ 0.85,
      verb == "yiwei" & discourse_type == "unsupported" & context == "Context Presence" ~ 0.35,
      verb == "juede" & discourse_type == "supported" & context == "Context Presence"  ~ 0.85,
      verb == "juede" & discourse_type == "unsupported" & context == "Context Presence"~ 0.60,
      verb == "yiwei" & discourse_type == "supported" & context == "Context Absence"  ~ 0.35,
      verb == "yiwei" & discourse_type == "unsupported" & context == "Context Absence" ~ 0.35,
      verb == "juede" & discourse_type == "supported" & context == "Context Absence"  ~ 0.85,
      verb == "juede" & discourse_type == "unsupported" & context == "Context Absence" ~ 0.60,
    ),
    accuracy = rnorm(n(), mean_accuracy, sd),
    accuracy = pmin(pmax(accuracy, 0), 1),  # keep between 0 and 1
    verb = fct_relevel(as.factor(verb), "yiwei", "juede"),
    context = fct_relevel(as.factor(context), "Context Presence", "Context Absence")
  )

hypothetical.plot <- ggplot(data=hypothetical.data %>% 
         mutate(verb = fct_relevel(verb, "yiwei", "juede")),
       aes(x=verb,
           y=accuracy,
           fill=verb,
           alpha=discourse_type)) +
  geom_hline(yintercept=0.5,linetype = "dashed", color="lightgrey")+
  geom_boxplot(width=0.3,
               position=position_dodge(width=.8),
               color="white") +
  theme_bw() +
  facet_grid(.~context) + 
  scale_fill_manual(values=cbPalette, guide = NULL) +
  scale_color_manual(values=cbPalette, guide = NULL) +
  ylim(0,1)+
  scale_alpha_discrete(range = c(0.3, 0.9), name="Discourse type") +
  theme(legend.position = c(0.88, 0.15),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12)) +
  guides(alpha = guide_legend(override.aes = list(fill = "grey40")))+
  labs(x="Verb",
       y="Accuracy")
hypothetical.plot
ggsave(hypothetical.plot, file="graphs/hypothetical_plot_blank.pdf", width=7, height=4)

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
context.data <- read.csv("../../data/1_context_choice/1_context_choice_main-trials.csv", header=TRUE) %>% 
  filter(!workerid %in% c("319", "323", "360", "312", "321", "301", "298")) # exclusion based on language

# exclusion based on filler items: 1 participant
context_filler_data <- subset(context.data, condition=="filler")
context_filler_summary <- context_filler_data %>% 
  group_by(workerid) %>% 
  summarize(error_num = sum(response=="incorrect"))
eligible_subjects = context_filler_summary$workerid[context_filler_summary$error_num < 2]
length(eligible_subjects) # 42
context.data = subset(context.data, workerid %in% eligible_subjects)

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
  mutate(discourse_type = if_else(discourse_type == "contrastive", "supported", "unclear"))
  
context_summary <- context_clean_data %>% 
  group_by(condition, verb, discourse_type) %>% 
  summarize(accuracy = mean(response_num),
            CILow = ci.low(response_num),
            CIHigh = ci.high(response_num)) %>% 
  ungroup() %>% 
  mutate(YMin = accuracy-CILow,
         YMax = accuracy+CIHigh)

context_participant_accuracy <- context_clean_data %>% 
  group_by(workerid, condition, verb, discourse_type) %>% 
  summarize(accuracy = mean(response_num),
            CILow = ci.low(response_num),
            CIHigh = ci.high(response_num)) %>% 
  ungroup() %>% 
  mutate(YMin = accuracy-CILow,
         YMax = accuracy+CIHigh)

context_item_accuracy <- context_clean_data %>% 
  group_by(item_id, condition, verb, discourse_type) %>% 
  summarize(accuracy = mean(response_num),
            CILow = ci.low(response_num),
            CIHigh = ci.high(response_num)) %>% 
  ungroup() %>% 
  mutate(YMin = accuracy-CILow,
         YMax = accuracy+CIHigh)
  
# 2. Plot ----
context_plot <- ggplot(data=context_summary %>% 
         mutate(verb = fct_relevel(verb, "yiwei", "juede"),
                discourse_type = if_else(discourse_type == "clear", "supported", discourse_type)),
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
  scale_alpha_discrete(range = c(0.4, 0.9), name="Discourse type") +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12))
context_plot
ggsave(context_plot, file="graphs/exp1_with_context.pdf", width=7, height=4)

# violin plot
context_plot_violin <- ggplot(data=context_item_accuracy %>% 
         mutate(verb = fct_relevel(verb, "yiwei", "juede")),
       aes(x=verb,
           y=accuracy,
           fill=verb,
           alpha=discourse_type)) +
  geom_hline(yintercept=0.5,linetype = "dashed", color="lightgrey")+
  geom_point(aes(shape=discourse_type),
             position=position_dodge2(width=.8,preserve = "single")) +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax), 
  #               width=.2,
  #               position=position_dodge2(width=0.8, preserve = "single"),
  #               show.legend = FALSE) +
  geom_violin(data=context_participant_accuracy %>% 
                mutate(verb=fct_relevel(verb,"yiwei", "juede")),
              position=position_dodge(width=.8)) +
  geom_boxplot(data=context_participant_accuracy %>% 
                 mutate(verb=fct_relevel(verb,"yiwei", "juede")),
               width=0.1,
               position=position_dodge(width=.8),
               show.legend = FALSE) +
  theme_bw() +
  scale_fill_manual(values=cbPalette, guide = NULL) +
  scale_color_manual(values=cbPalette, guide = NULL) +
  scale_shape_manual(values=c("supported"=22, "unclear"=24), name="Discourse type") +
  ylim(0,1)+
  scale_alpha_discrete(range = c(0.3, 0.9), name="Discourse type") +
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
context_plot_violin
ggsave(context_plot_violin, file="graphs/exp1_with_context-violin.pdf", width=6, height=4)

# lines connecting dots for individual means by item
ggplot(data=context_item_accuracy %>% 
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
context_clean_data$verb <- as.factor(context_clean_data$verb)
contrasts(context_clean_data$verb) <- contr.sum(2)
contrasts(context_clean_data$verb)
context_clean_data$discourse_type <- as.factor(context_clean_data$discourse_type)
contrasts(context_clean_data$discourse_type) <- contr.sum(2)
contrasts(context_clean_data$discourse_type)

context_model <- glmer(response_num ~ verb * discourse_type + (1|item_id) + (1+verb*discourse_type|workerid),
                       data=context_clean_data,
                       family=binomial,
                       control = glmerControl(
                         optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5)
                         ))
summary(context_model)
VarCorr(context_model)

emmeans(context_model, ~verb*discourse_type, type="response")
plot(emmeans(context_model, ~verb*discourse_type, type="response"))
pairs(emmeans(context_model, ~verb|discourse_type))
pairs(emmeans(context_model, ~discourse_type|verb))

fixed_context_model <- glm(response_num ~ verb * discourse_type,
                           data=context_clean_data,
                           family=binomial)
summary(fixed_context_model)

simple_context_model <- glmer(response_num ~ verb * discourse_type + (1+verb+discourse_type|workerid),
                       data=context_clean_data,
                       family=binomial,
                       control = glmerControl(
                         optimizer = "bobyqa",
                         optCtrl = list(maxfun = 2e5)
                       ))
summary(simple_context_model)
AIC(context_model, simple_context_model)
