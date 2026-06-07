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
no_context_blank.data <- read.csv("../../data/4_no_context_blank/4_no_context_blank_main-trials.csv", header=TRUE) %>% 
  filter(!workerid %in% c("476")) # responded all in English

filler_answer <- read.csv("../../data/4_no_context_blank/filler_answer.csv", header=TRUE) %>% 
  select(c("item_id", "answer")) 

no_context_blank.data <- no_context_blank.data %>% 
  left_join(filler_answer, by="item_id") %>% 
  mutate(response_correct = 
           str_detect(response, str_replace_all(answer, "/","|"))
  )

# exclusion based on filler items
no_context_blank_filler.data <- subset(no_context_blank.data, condition=="filler")
no_context_blank_filler_summary <- no_context_blank_filler.data %>% 
  group_by(workerid) %>% 
  summarize(error_num = sum(!response_correct))
no_context_blank_eligible_subjects = no_context_blank_filler_summary$workerid[no_context_blank_filler_summary$error_num < 2]
length(no_context_blank_eligible_subjects) # 49
no_context_blank.data = subset(no_context_blank.data, workerid %in% no_context_blank_eligible_subjects)

# data cleaning
no_context_blank_clean_data <- no_context_blank.data %>% 
  filter(!grepl("practice", condition)) %>% 
  filter(condition!="filler") %>% 
  mutate(response_clean = case_when(
    str_detect(response, "以为|以為|以爲") ~ "yiwei",
    str_detect(response, "觉得|覺得") ~ "juede",
    TRUE ~ "other"
  ),
  response_type = case_when(
    str_detect(response, "以为|以為|以爲|当|當|假装|假裝|记成") ~ "contra",
    str_detect(response, "觉得|覺得|认为|認為|認爲|感觉|感覺|相信|猜|赞成|贊成") ~ "non-belief", # 赞成(贊成) is not a belief verb but mental-state verb
    str_detect(response, "知道|看到|发现|發現") ~ "factive-belief",
    str_detect(response, "怕|擔心|担心") ~ "factive-emotion",
    str_detect(response, "说|説|說|講|讲|喊|叫|强调|告訴|告诉|承认|确认") ~ "non-say",
    str_detect(response, "要|想") ~ "ambiguous want",
    TRUE ~ "other"
  ),
  response_num = if_else(response_clean == verb, 1, 0),
         discourse_type = sub(".*_", "",condition)) %>% 
  mutate(discourse_type = if_else(discourse_type == "contrastive", "constrained", "unconstrained"))

# save the clean dataset
write.csv(no_context_blank_clean_data, "../../data/4_no_context_blank/4_no_context_blank_main-trials_clean.csv", row.names=FALSE)

no_context_blank_summary <- no_context_blank_clean_data %>% 
  group_by(condition, verb, discourse_type) %>% 
  summarize(accuracy = mean(response_num),
            CILow = ci.low(response_num),
            CIHigh = ci.high(response_num)) %>% 
  ungroup() %>% 
  mutate(YMin = accuracy-CILow,
         YMax = accuracy+CIHigh)

no_context_blank_yiwei_summary <- no_context_blank_clean_data %>% 
  mutate(response_yiwei = if_else(response_clean == "yiwei", 1, 0)) %>% 
  group_by(condition, verb, discourse_type) %>% 
  summarize(prop_yiwei = mean(response_yiwei),
            CILow = ci.low(response_yiwei),
            CIHigh = ci.high(response_yiwei)) %>% 
  ungroup() %>% 
  mutate(YMin = prop_yiwei-CILow,
         YMax = prop_yiwei+CIHigh)

no_context_blank_type_summary <- no_context_blank_clean_data %>% 
  mutate(response_type = if_else(response_type == "non-emotion" | response_type == "non-say", "non-other", response_type )) %>% 
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

no_context_blank_participant_accuracy <- no_context_blank_clean_data %>% 
  group_by(workerid, condition, verb, discourse_type) %>% 
  summarize(accuracy = mean(response_num),
            CILow = ci.low(response_num),
            CIHigh = ci.high(response_num)) %>% 
  ungroup() %>% 
  mutate(YMin = accuracy-CILow,
         YMax = accuracy+CIHigh)

no_context_blank_item_accuracy <- no_context_blank_clean_data %>% 
  group_by(item_id, condition, verb, discourse_type) %>% 
  summarize(accuracy = mean(response_num),
            CILow = ci.low(response_num),
            CIHigh = ci.high(response_num)) %>% 
  ungroup() %>% 
  mutate(YMin = accuracy-CILow,
         YMax = accuracy+CIHigh)

no_context_blank_participant_prop_yiwei <- no_context_blank_clean_data %>% 
  mutate(response_yiwei = if_else(response_clean == "yiwei", 1, 0)) %>% 
  group_by(workerid, condition, verb, discourse_type) %>% 
  summarize(prop_yiwei = mean(response_yiwei),
            CILow = ci.low(response_yiwei),
            CIHigh = ci.high(response_yiwei)) %>% 
  ungroup() %>% 
  mutate(YMin = prop_yiwei-CILow,
         YMax = prop_yiwei+CIHigh)

no_context_blank_item_prop_yiwei <- no_context_blank_clean_data %>% 
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
no_context_blank_plot <- ggplot(data=no_context_blank_summary %>% 
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
no_context_blank_plot
ggsave(no_context_blank_plot, file="graphs/exp4_no_context-bar.pdf", width=7, height=4)

## accuracy violin graph ----
no_context_blank_plot_violin <- ggplot(data=no_context_blank_item_accuracy %>% 
         mutate(verb = fct_relevel(verb, "yiwei", "juede")),
       aes(x=verb,
           y=accuracy,
           fill=verb,
           alpha=discourse_type)) +
  geom_hline(yintercept=0.5,linetype = "dashed", color="lightgrey")+
  geom_point(aes(shape=discourse_type),
             position=position_dodge2(width=.8,preserve = "single")) +
  geom_violin(data=no_context_blank_participant_accuracy %>% 
                mutate(verb = fct_relevel(verb, "yiwei", "juede")),
              position=position_dodge(width=.8)) +
  geom_boxplot(data=no_context_blank_participant_accuracy %>% 
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
no_context_blank_plot_violin
ggsave(no_context_blank_plot_violin, file="graphs/exp4_no_context-violin.pdf", width=6, height=4)

## proportion of yiwei bar graph ----
no_context_blank_yiwei_plot <- ggplot(data=no_context_blank_yiwei_summary %>%
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
no_context_blank_yiwei_plot
ggsave(no_context_blank_yiwei_plot, file="graphs/exp4_no_context_yiwei-bar.pdf", width=7, height=4)

## proportion of yiwei violin graph ----
no_context_blank_yiwei_plot_violin <- ggplot(data=no_context_blank_item_prop_yiwei %>% 
                                         mutate(verb = fct_relevel(verb, "yiwei", "juede")),
                                       aes(x=verb,
                                           y=prop_yiwei,
                                           fill=verb,
                                           alpha=discourse_type)) +
  geom_hline(yintercept=0.5,linetype = "dashed", color="lightgrey")+
  geom_point(aes(shape=discourse_type),
             position=position_dodge2(width=.8,preserve = "single")) +
  geom_violin(data=no_context_blank_participant_prop_yiwei %>% 
                mutate(verb = fct_relevel(verb, "yiwei", "juede")),
              position=position_dodge(width=.8)) +
  geom_boxplot(data=no_context_blank_participant_prop_yiwei %>% 
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
no_context_blank_yiwei_plot_violin
ggsave(no_context_blank_yiwei_plot_violin, file="graphs/exp4_no_context_yiwei-violin.pdf", width=6, height=4)

## proportion of each response type ----
no_context_blank_type_plot <- ggplot(no_context_blank_type_summary,
                                     aes(x=discourse_type,
                                         y=percent,
                                         fill=response_type)) +
  geom_bar(stat="identity") +
  facet_grid(.~verb) +
  scale_fill_brewer(palette = "Set2",name = "Response type") +
  labs(x="Condition",
       y="Proportion of response type")
  # scale_alpha_discrete(range = c(0.4, 0.9), guide = NULL)
no_context_blank_type_plot
ggsave(no_context_blank_type_plot, file="graphs/exp4_no_context_type-bar.pdf", width=6, height=4)

# lines connecting dots for individual means
ggplot(data=no_context_blank_participant_accuracy %>% 
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

# dots for individual means by item
ggplot(data=no_context_blank_item_accuracy %>% 
         mutate(condition = fct_relevel(condition, "yiwei_contrastive", 
                                        "yiwei_unclear",
                                        "juede_contrastive",
                                        "juede_unclear"),
                verb = fct_relevel(verb, "yiwei", "juede")),
       aes(x=condition,
           y=accuracy,
           color=discourse_type)) +
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
no_context_blank_clean_data$verb <- as.factor(no_context_blank_clean_data$verb)
contrasts(no_context_blank_clean_data$verb) <- contr.sum(2)
contrasts(no_context_blank_clean_data$verb)
no_context_blank_clean_data$discourse_type <- as.factor(no_context_blank_clean_data$discourse_type)
contrasts(no_context_blank_clean_data$discourse_type) <- contr.sum(2)
contrasts(no_context_blank_clean_data$discourse_type)

no_context_blank_model <- glmer(response_num ~ verb * discourse_type + (1|item_id) + (1+verb*discourse_type|workerid),
      data=no_context_blank_clean_data,
      family=binomial,
      control = glmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 2e5)
      ))
summary(no_context_blank_model)
VarCorr(no_context_blank_model)

simple_no_context_blank_model <- glmer(response_num ~ verb * discourse_type + (1|item_id) + (1|workerid),
                                 data=no_context_blank_clean_data,
                                 family=binomial,
                                 control = glmerControl(
                                   optimizer = "bobyqa",
                                   optCtrl = list(maxfun = 2e5)
                                 ))
summary(simple_no_context_blank_model)

# 4. Combined with Exp3 ----
## 4.1 data ----
context_blank_clean_data <- read.csv("../../data/3_context_blank/3_context_blank_main-trials_clean_all-language.csv", header=TRUE) %>% 
  mutate(block_id = str(block_id))

all_data <- bind_rows(lst(context_blank_clean_data, no_context_blank_clean_data), .id="context") %>% 
  mutate(context = if_else(context == "context_blank_clean_data", "presence", "absence"),
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

all_type_summary <- all_data %>% 
  mutate(response_type = if_else(response_type == "non-emotion" | response_type == "non-say", "non-other", response_type)) %>% 
  group_by(condition, verb, discourse_type, response_type, context) %>% 
  summarize(count = n(), .groups="drop") %>% 
  group_by(condition, verb, discourse_type, context) %>% 
  mutate(percent = count/sum(count),
         verb = factor(verb, levels = c("yiwei", "juede")),
         discourse_type = factor(discourse_type, levels = c("constrained", "unconstrained")))

## 4.2 plot ----
### accuracy violin graph ----
context_type <- list("presence"="Context-Presence",
                     "absence"="Context-Absence")
context_labeller <- function(variable,value){
  return(context_type[value])
}

num <- data.frame(
  context=c("presence","absence"),
  label=c("N=46", "N=49")
)

all_plot_violin <- ggplot(data=all_item_accuracy %>% 
                            mutate(verb = fct_relevel(verb, "yiwei", "juede")),
                                 aes(x=verb,
                                     y=accuracy,
                                     # fill=verb,
                                     alpha=discourse_type)) +
  geom_hline(yintercept=0.5,linetype = "dashed", color="lightgrey")+
  geom_point(aes(shape=discourse_type),
             position=position_dodge2(width=.8,preserve = "single"),
             fill="#779CAB") +
  geom_violin(data=all_participant_accuracy %>% 
                mutate(verb = fct_relevel(verb, "yiwei", "juede")),
              position=position_dodge(width=.8),
              fill="#779CAB") +
  geom_boxplot(data=all_participant_accuracy %>% 
                 mutate(verb = fct_relevel(verb, "yiwei", "juede")),
               width=0.1,
               position=position_dodge(width=.8),
               fill="#779CAB",
               show.legend = FALSE) +
  theme_bw() +
  ylim(0,1) +
  geom_text(
    data = num,
    aes(x = Inf, y = -Inf, label = label),
    hjust = 1.1,
    vjust = -0.5,
    inherit.aes = FALSE
  ) +
  facet_grid(.~factor(context, levels=c("presence", "absence")),labeller=context_labeller) +
  # scale_fill_manual(values=cbPalette, guide = NULL) +
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
# ggsave(all_plot_violin, file="graphs/all_blank-violin.pdf", width=8, height=4)
ggsave(all_plot_violin, file="graphs/all_blank-violin_all-language_grey.png", width=8, height=4)

### proportion of each response type ----
all_type_plot <- ggplot(all_type_summary,
                        aes(x=discourse_type,
                            y=percent,
                            fill=response_type)) +
  geom_bar(stat="identity") +
  facet_grid(verb~context) +
  scale_fill_brewer(palette = "Set2", name="Response type") +
  labs(x="Condition",
       y="Proportion of response type") 
  # scale_alpha_discrete(range = c(0.4, 0.9), guide = NULL)
all_type_plot
ggsave(all_type_plot, file="graphs/all_blank_type-bar.pdf", width=6, height=4)

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
emmeans(all_model, ~ verb * discourse_type * context, type = "response")
contrast(
  emmeans(all_model, ~ verb * discourse_type | context),
  interaction = "pairwise"
)
pairs(emmeans(all_model, ~context|discourse_type))
pairs(emmeans(all_model, ~discourse_type|context))
pairs(emmeans(all_model, ~context|verb))
pairs(emmeans(all_model, ~verb|context))
