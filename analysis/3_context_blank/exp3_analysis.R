library(lme4)
library(dplyr)
library(emmeans)
library(tidyverse)
library(ggplot2)
library(ggsignif)
library(stringr)
library(tidytext)
library(RColorBrewer)
library(stringr)

theme_set(theme_bw())
# color-blind-friendly palette
# cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
cbPalette <- c("#E69F00", "#009E73") 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("helpers.R")

# 1. Data ----
context_blank.data <- read.csv("../../data/3_context_blank/3_context_blank_main-trials.csv", header=TRUE) %>% 
  filter(!workerid %in% c("455", "464", "428"), # exclusion based on language status.
         !workerid %in% c("477")) # responded all in english

filler_answer <- read.csv("../../data/3_context_blank/filler_answer.csv", header=TRUE) %>% 
  select(c("item_id", "answer"))

context_blank.data <- context_blank.data %>% 
  left_join(filler_answer, by="item_id") %>% 
  mutate(response_correct = 
    str_detect(response, str_replace_all(answer, "/","|"))
  )
  
# exclusion based on filler items 
context_blank_filler.data <- subset(context_blank.data, condition=="filler")
context_blank_filler_summary <- context_blank_filler.data %>% 
  group_by(workerid) %>% 
  summarize(error_num = sum(!response_correct)) # 510 and 480 excluded
context_blank_eligible_subjects = context_blank_filler_summary$workerid[context_blank_filler_summary$error_num < 2]
length(context_blank_eligible_subjects) # 44
context_blank.data = subset(context_blank.data, workerid %in% context_blank_eligible_subjects)

# data cleaning
context_blank.data <- context_blank.data
context_blank_clean_data <- context_blank.data %>% 
  filter(!grepl("practice", condition)) %>% 
  filter(condition!="filler") %>% 
  mutate(response_clean = case_when(
    str_detect(response, "以为|以為|以爲") ~ "yiwei",
    str_detect(response, "觉得|覺得") ~ "juede",
    TRUE ~ "other"
  ),
  response_type = case_when(
    str_detect(response, "以为|以為|以爲|当|當|假装|假裝") ~ "contra",
    str_detect(response, "觉得|覺得|认为|認為|認爲|感觉|感覺|相信|猜|赞成|贊成") ~ "non-belief", # 赞成(贊成) is not a belief verb but mental-state verb
    str_detect(response, "知道") ~ "factive-belief",
    str_detect(response, "看到|发现|發現|承认|确认") ~ "factive-other",
    str_detect(response, "怕|擔心|担心") ~ "non-emotion",
    str_detect(response, "说|説|說|講|讲|告訴|告诉|喊|叫|强调") ~ "non-say",
    str_detect(response, "要|想") ~ "ambiguous want",
    TRUE ~ "other"
  ),
  response_num = if_else(response_clean == verb, 1, 0),
         discourse_type = sub(".*_", "",condition)) %>% 
  mutate(discourse_type = if_else(discourse_type == "contrastive", "constrained", "unconstrained"))

# save the clean dataset
write.csv(context_blank_clean_data, "../../data/3_context_blank/3_context_blank_main-trials_clean.csv", row.names=FALSE)

context_blank_summary <- context_blank_clean_data %>% 
  group_by(condition, verb, discourse_type) %>% 
  summarize(accuracy = mean(response_num),
            CILow = ci.low(response_num),
            CIHigh = ci.high(response_num)) %>% 
  ungroup() %>% 
  mutate(YMin = accuracy-CILow,
         YMax = accuracy+CIHigh)

context_blank_yiwei_summary <- context_blank_clean_data %>% 
  mutate(response_yiwei = if_else(response_clean == "yiwei", 1, 0)) %>% 
  group_by(condition, verb, discourse_type) %>% 
  summarize(prop_yiwei = mean(response_yiwei),
            CILow = ci.low(response_yiwei),
            CIHigh = ci.high(response_yiwei)) %>% 
  ungroup() %>% 
  mutate(YMin = prop_yiwei-CILow,
         YMax = prop_yiwei+CIHigh)

context_blank_type_summary <- context_blank_clean_data %>% 
  group_by(condition, verb, discourse_type, response_type) %>% 
  summarize(count = n(), .groups="drop") %>% 
  group_by(condition, verb, discourse_type) %>% 
  mutate(percent = count/sum(count),
         verb = factor(verb, levels = c("yiwei", "juede")),
         discourse_type = factor(discourse_type, levels = c("constrained", "unconstrained")),
         verb_discourse = factor(
           paste(verb, discourse_type, sep = "."),
           levels = c(
             "yiwei.constrained",
             "yiwei.unconstrained",
             "juede.constrained",
             "juede.unconstrained"
           )
         )
  )


context_blank_participant_accuracy <- context_blank_clean_data %>% 
  group_by(workerid, condition, verb, discourse_type) %>% 
  summarize(accuracy = mean(response_num),
            CILow = ci.low(response_num),
            CIHigh = ci.high(response_num)) %>% 
  ungroup() %>% 
  mutate(YMin = accuracy-CILow,
         YMax = accuracy+CIHigh)

context_blank_item_accuracy <- context_blank_clean_data %>% 
  group_by(item_id, condition, verb, discourse_type) %>% 
  summarize(accuracy = mean(response_num),
            CILow = ci.low(response_num),
            CIHigh = ci.high(response_num)) %>% 
  ungroup() %>% 
  mutate(YMin = accuracy-CILow,
         YMax = accuracy+CIHigh)

context_blank_participant_prop_yiwei <- context_blank_clean_data %>% 
  mutate(response_yiwei = if_else(response_clean == "yiwei", 1, 0)) %>% 
  group_by(workerid, condition, verb, discourse_type) %>% 
  summarize(prop_yiwei = mean(response_yiwei),
            CILow = ci.low(response_yiwei),
            CIHigh = ci.high(response_yiwei)) %>% 
  ungroup() %>% 
  mutate(YMin = prop_yiwei-CILow,
         YMax = prop_yiwei+CIHigh)

context_blank_item_prop_yiwei <- context_blank_clean_data %>% 
  mutate(response_yiwei = if_else(response_clean == "yiwei", 1, 0)) %>% 
  group_by(item_id, condition, verb, discourse_type) %>% 
  summarize(prop_yiwei = mean(response_yiwei),
            CILow = ci.low(response_yiwei),
            CIHigh = ci.high(response_yiwei)) %>% 
  ungroup() %>% 
  mutate(YMin = prop_yiwei-CILow,
         YMax = prop_yiwei+CIHigh)

# 2. Plot ----
## accuracy bar graph ----
context_blank_plot <- ggplot(data=context_blank_summary %>% 
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
context_blank_plot
ggsave(context_blank_plot, file="graphs/exp3_context_bar.pdf", width=7, height=4)

## accuracy violin graph ----
context_blank_plot_violin <- ggplot(data=context_blank_item_accuracy %>% 
         mutate(verb = fct_relevel(verb, "yiwei", "juede")),
       aes(x=verb,
           y=accuracy,
           fill=verb,
           alpha=discourse_type)) +
  geom_hline(yintercept=0.5,linetype = "dashed", color="lightgrey")+
  geom_point(aes(shape=discourse_type),
             position=position_dodge2(width=.8,preserve = "single")) +
  geom_violin(data=context_blank_participant_accuracy %>% 
                mutate(verb = fct_relevel(verb, "yiwei", "juede")),
              position=position_dodge(width=.8)) +
  geom_boxplot(data=context_blank_participant_accuracy %>% 
                 mutate(verb = fct_relevel(verb, "yiwei", "juede")),
               width=0.1,
               position=position_dodge(width=.8),
               show.legend = FALSE) +
  theme_bw() +
  scale_fill_manual(values=cbPalette, guide = NULL) +
  ylim(0,1)+
  scale_alpha_discrete(range = c(0.4, 0.9), name="Discourse type") +
  scale_shape_manual(values=c("constrained"=22, "unconstrained"=24), name="Discourse type") +
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
context_blank_plot_violin
ggsave(context_blank_plot_violin, file="graphs/exp3_context-violin.pdf", width=6, height=4)

## proportion of yiwei bar graph ----
context_blank_yiwei_plot <- ggplot(data=context_blank_yiwei_summary %>% 
                                     mutate(verb = fct_relevel(verb, "yiwei", "juede")),
                                   aes(x=verb,
                                       y=prop_yiwei,
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
  labs(y="Proportion of yiwei") +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12))
context_blank_yiwei_plot
ggsave(context_blank_yiwei_plot, file="graphs/exp3_context_yiwei-bar.pdf", width=6, height=4)

## proportion of yiwei violin graph ----
context_blank_yiwei_plot_violin <- ggplot(data=context_blank_item_prop_yiwei %>% 
                                            mutate(verb = fct_relevel(verb, "yiwei", "juede")),
                                          aes(x=verb,
                                              y=prop_yiwei,
                                              fill=verb,
                                              alpha=discourse_type)) +
  geom_hline(yintercept=0.5,linetype = "dashed", color="lightgrey")+
  geom_point(aes(shape=discourse_type),
             position=position_dodge2(width=.8,preserve = "single")) +
  geom_violin(data=context_blank_participant_prop_yiwei %>% 
                mutate(verb = fct_relevel(verb, "yiwei", "juede")),
              position=position_dodge(width=.8)) +
  geom_boxplot(data=context_blank_participant_prop_yiwei %>% 
                 mutate(verb = fct_relevel(verb, "yiwei", "juede")),
               width=0.1,
               position=position_dodge(width=.8),
               show.legend = FALSE) +
  theme_bw() +
  scale_fill_manual(values=cbPalette, guide = NULL) +
  ylim(0,1)+
  scale_alpha_discrete(range = c(0.4, 0.9), name="Discourse type") +
  scale_shape_manual(values=c("constrained"=22, "unconstrained"=24), name="Discourse type") +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12)) +
  guides(alpha = guide_legend(override.aes = list(fill = "grey40")))+
  labs(x="Verb",
       y="Proportion of yiwei")
context_blank_yiwei_plot_violin
ggsave(context_blank_yiwei_plot_violin, file="graphs/exp3_context_yiwei-violin.pdf", width=6, height=4)

## proportion of each response type ----
ggplot(context_blank_type_summary,
       aes(x=verb_discourse,
           y=percent,
           fill=response_type)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = "Set2")

# lines connecting dots for individual means
ggplot(data=context_blank_participant_accuracy %>% 
         mutate(condition = fct_relevel(condition, "yiwei_contrastive", 
                                        "yiwei_unclear",
                                        "juede_contrastive",
                                        "juede_unclear"),
                verb = fct_relevel(verb, "yiwei", "juede")),
       aes(x=condition,
           y=accuracy,
           color=verb)) +
  geom_hline(yintercept=0.5,linetype = "dashed", color="lightgrey")+
  geom_line(aes(group=interaction(workerid,verb)),
            position=position_dodge2(width=.8,preserve="single"),
            color="black",
            alpha=0.5)+
  geom_point(position=position_dodge2(width=.8,preserve = "single"),
             alpha=0.6)+
  theme_bw() +
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

# lines connecting dots for individual means by item
ggplot(data=context_blank_item_accuracy %>% 
         mutate(condition = fct_relevel(condition, "yiwei_contrastive", 
                                        "yiwei_unclear",
                                        "juede_contrastive",
                                        "juede_unclear"),
                verb = fct_relevel(verb, "yiwei", "juede")),
       aes(x=condition,
           y=accuracy,
           color=verb)) +
  geom_hline(yintercept=0.5,linetype = "dashed", color="lightgrey")+
  geom_point(position=position_dodge2(width=.8,preserve = "single"),
             alpha=0.6)+
  theme_bw() +
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

# 3. Analysis ----
# sum coding
context_blank_clean_data$verb <- as.factor(context_blank_clean_data$verb)
contrasts(context_blank_clean_data$verb) <- contr.sum(2)
contrasts(context_blank_clean_data$verb)
context_blank_clean_data$discourse_type <- as.factor(context_blank_clean_data$discourse_type)
contrasts(context_blank_clean_data$discourse_type) <- contr.sum(2)
contrasts(context_blank_clean_data$discourse_type)

context_blank_model <- glmer(response_num ~ verb * discourse_type + (1|item_id) + (1+verb*discourse_type|workerid),
      data=context_blank_clean_data,
      family=binomial,
      control = glmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 2e5)
      ))
summary(context_blank_model)
VarCorr(context_blank_model)

emmeans(context_blank_model, ~ verb, type = "response")
pairs(emmeans(context_blank_model, ~ discourse_type|verb))
pairs(emmeans(context_blank_model, ~ verb|discourse_type))

simple_context_blank_model <- glmer(response_num ~ verb * discourse_type + (1|item_id) + (1|workerid),
                                 data=context_blank_clean_data,
                                 family=binomial,
                                 control = glmerControl(
                                   optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)
                                 ))
summary(simple_context_blank_model)

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
  filter(!grepl("practice", condition)) %>% 
  filter(condition!="filler") %>% 
  mutate(response_corr = case_when(condition !="filler" & verb==response ~ "correct",
                                   condition !="filler" & verb!=response ~ "incorrect",
                                   condition == "filler" ~ response),
         response_num = if_else(response_corr == "correct", 1, 0),
         discourse_type = sub(".*_", "",condition)) %>% 
  mutate(discourse_type = if_else(discourse_type == "contrastive", "constrained", "unconstrained"))
all_data <- bind_rows(lst(context_clean_data, context_blank_clean_data), .id="task") %>% 
  mutate(task = if_else(task == "context_clean_data", "2AFC", "Open-ended"),
         task = fct_relevel(task, "2AFC", "Open-ended")) 

all_summary <- all_data %>% 
  group_by(condition, verb, discourse_type, task) %>% 
  summarize(accuracy = mean(response_num),
            CILow = ci.low(response_num),
            CIHigh = ci.high(response_num)) %>% 
  ungroup() %>% 
  mutate(YMin = accuracy-CILow,
         YMax = accuracy+CIHigh,
         task = fct_relevel(task, "2AFC", "Open-ended"))

all_participant_accuracy <- all_data %>% 
  group_by(workerid, condition, verb, discourse_type, task) %>% 
  summarize(accuracy = mean(response_num),
            CILow = ci.low(response_num),
            CIHigh = ci.high(response_num)) %>% 
  ungroup() %>% 
  mutate(YMin = accuracy-CILow,
         YMax = accuracy+CIHigh,
         task = fct_relevel(task, "2AFC", "Open-ended"))

all_item_accuracy <- all_data %>% 
  group_by(item_id, condition, verb, discourse_type, task) %>% 
  summarize(accuracy = mean(response_num),
            CILow = ci.low(response_num),
            CIHigh = ci.high(response_num)) %>% 
  ungroup() %>% 
  mutate(YMin = accuracy-CILow,
         YMax = accuracy+CIHigh,
         task = fct_relevel(task, "2AFC", "Open-ended"))

## 4.2 plot ----
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
  facet_grid(.~task) +
  scale_fill_manual(values=cbPalette, guide = NULL) +
  scale_alpha_discrete(range = c(0.3, 0.9), name="Discourse type") +
  scale_shape_manual(values=c("constrained"=22, "unconstrained"=24), name="Discourse type") +
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
ggsave(all_plot_violin, file="graphs/all_context-violin.pdf", width=8, height=4)
