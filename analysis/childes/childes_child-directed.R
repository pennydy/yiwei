# load the library
library(childesr)
library(dplyr)
library(lme4)
library(emmeans)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggsignif)
library(ggpattern)
library(tidytext)
library(RColorBrewer)
library(viridis)
library(stringr)
library(irr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
sentencePalette <- c("#BEBADA", "#80B1D3") 
embedPalette <- c("#BEBADA", "#8DD3C7") 
embedfullPalette <- c("#E41A1C","#4DAF4A","#FFFF33")

# 1. Data ----
data <- read.csv("../../data/childes/zhidao_juede_no_readings_combined.csv", header=TRUE) %>% 
  filter(!grepl("Chang2/BookReading", file_name)) %>%
  select(!c("file_name", "speaker", "sentence"))

clean_data <- data %>% 
  mutate(id=row_number(),
         across(c(sentence_type_e, sentence_type_g, sentence_type_c, sentence_type_j3, embedded_sentence_type_e, embedded_sentence_type_g, embedded_sentence_type_c, embedded_sentence_type_j3),
                ~ if_else(.x == "3 how/why", "2 wh_question", .x)),
         across(c(embedded_sentence_type_e, embedded_sentence_type_g, embedded_sentence_type_c, embedded_sentence_type_j3),
                ~ if_else(.x == "3 base-generated wh/polar", "3 base_position", .x)),
         across(c(subject_j1, subject_j2),
                ~ case_when(.x == "4 self_reference" ~ "1 first_person",
                            .x == "5 自己" ~ "5 ambiguous",
                            TRUE ~ .x)))

# 2 Reliability ----
## 2.1 matrix subject ----
matrix_subject_reliability.data <- clean_data %>% 
  select(c(subject_j1, subject_j2, id)) %>% 
  # mutate(subject_j2 = if_else(subject_j2 == "", NA, subject_j2)) %>% # a more lenient criterion when change to NA 
  pivot_longer(cols = c(subject_j1, subject_j2),
               names_to = "coder_id",
               values_to = "value") %>% 
  pivot_wider(names_from = id,
              values_from = value) %>% 
  select(-coder_id) 

matrix_subject_reliability.data <- as.matrix(matrix_subject_reliability.data)
kripp.alpha(matrix_subject_reliability.data, method = "nominal")

## 2.2 matrix sentence ----
matrix_sentence_reliability.data <- clean_data %>% 
  select(c(sentence_type_e, sentence_type_g, sentence_type_c, sentence_type_j3, id)) %>% 
  pivot_longer(cols = c(sentence_type_e, sentence_type_g, sentence_type_c, sentence_type_j3),
               names_to = "coder_id",
               values_to = "value") %>% 
  pivot_wider(names_from = id,
              values_from = value) %>% 
  select(-coder_id) 

matrix_sentence_reliability.data <- as.matrix(matrix_sentence_reliability.data)
kripp.alpha(matrix_sentence_reliability.data, method = "nominal")

## 2.3 embedded sentence ----
embedded_sentence_reliability.data <- clean_data %>% 
  select(c(embedded_sentence_type_e, embedded_sentence_type_g, embedded_sentence_type_c, embedded_sentence_type_j3, id)) %>% 
  mutate(
    across(embedded_sentence_type_e:embedded_sentence_type_j3, ~ if_else(.x == "", "NONE", .x))
  ) %>% 
  pivot_longer(cols = c(embedded_sentence_type_e, embedded_sentence_type_g, embedded_sentence_type_c, embedded_sentence_type_j3),
               names_to = "coder_id",
               values_to = "value") %>% 
  pivot_wider(names_from = id,
              values_from = value) %>% 
  select(-coder_id) 

embedded_sentence_reliability.data <- as.matrix(embedded_sentence_reliability.data)
kripp.alpha(embedded_sentence_reliability.data, method = "nominal")

## 2.4 embedded CP ----
embedded_CP_reliability.data <- clean_data %>% 
  select(c(embedded_CP_j1, embedded_CP_j2, id)) %>% 
  pivot_longer(cols = c(embedded_CP_j1, embedded_CP_j2),
               names_to = "coder_id",
               values_to = "value") %>% 
  pivot_wider(names_from = id,
              values_from = value) %>% 
  select(-coder_id)

embedded_CP_reliability.data <- as.matrix(embedded_CP_reliability.data)
kripp.alpha(embedded_CP_reliability.data, method = "nominal")

# a more generous way, where 0 No (CP) AND 1 Adj are combined to 2 CP
embedded_CP_reliability_alt.data <- clean_data %>%
  mutate(embedded_CP_j1 = if_else(embedded_CP_j1 == "0 No" & embedded_content_j1 == "1 Adj", "2 CP", embedded_CP_j1),
         embedded_CP_j2 = if_else(embedded_CP_j2 == "0 No" & embedded_content_j2 == "1 Adj", "2 CP", embedded_CP_j2)) %>% 
  select(c(embedded_CP_j1, embedded_CP_j2, id)) %>% 
  pivot_longer(cols = c(embedded_CP_j1, embedded_CP_j2),
               names_to = "coder_id",
               values_to = "value") %>% 
  pivot_wider(names_from = id,
              values_from = value) %>% 
  select(-coder_id)

embedded_CP_reliability_alt.data <- as.matrix(embedded_CP_reliability_alt.data)
kripp.alpha(embedded_CP_reliability_alt.data, method = "nominal")

## 2.5 negation ----
negation_reliability.data <- clean_data %>% 
  select(c(matrix_negation_e, matrix_negation_g, matrix_negation_j1, id)) %>% 
  pivot_longer(cols = c(matrix_negation_e, matrix_negation_g, matrix_negation_j1),
               names_to = "coder_id",
               values_to = "value") %>% 
  pivot_wider(names_from = id,
              values_from = value) %>% 
  select(-coder_id)

negation_reliability.data <- as.matrix(negation_reliability.data)
kripp.alpha(negation_reliability.data, method = "nominal")

# 3. Distrubtions ----
# only using data that the two coders agreed on
## 3.1 matrix subject ----
matrix_subject.data <- clean_data %>% 
  select(c(id, verb, subject_j1, subject_j2)) %>% 
  mutate(agreement = if_else(subject_j1 == subject_j2, "same", "diff")) %>%
  filter(agreement == "same") %>% 
  select(!c(agreement, subject_j1)) %>% 
  rename(subject_type = subject_j2)

matrix_subject_summary <- matrix_subject.data %>% 
  group_by(verb, subject_type) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()

ggplot(matrix_subject_summary, 
       aes(x=verb,
           y=percent,
           fill=subject_type)) + 
  geom_col()+
  geom_text(aes(label=paste0(count, " (", round(percent, 1), "%)")),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 3)+
  # geom_label(aes(label=count,
  #                group = subject_type),
  #           position = position_stack(vjust = 0.5),
  #           fill = "white",
  #           color = "black",
  #           label.size = 0.3,
  #           label.padding = unit(0.15, "lines"),
  #           size = 3)+
  theme_bw()+
  scale_fill_brewer(palette = "Set2",
                    name="Matrix subject type") +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14))+
  labs(x="Verb",
       y="Percentage")

## 3.2 matrix sentence ----
matrix_sentence.data <- clean_data %>% 
  select(c(id, verb, sentence_type_e, sentence_type_g, sentence_type_c, sentence_type_j3)) %>% 
  rowwise() %>% 
  mutate(
    n_filled = sum(c_across(sentence_type_e:sentence_type_j3) != "" & !is.na(c_across(sentence_type_e:sentence_type_j3))), # cases where only 1 coder considers it incomplete and the other considers it complete 
    agreement = {
      vals <- c_across(sentence_type_e:sentence_type_j3)
      vals <- vals[vals != "" & !is.na(vals)]
      if_else(n_filled == 2 && length(unique(vals)) == 1, "same", "diff")
      },
    agreed_sentence_type = {
      vals <- c_across(sentence_type_e:sentence_type_j3)
      vals <- vals[vals != "" & !is.na(vals)]
      vals[1]
    }
    ) %>%
  ungroup() %>% 
  filter(agreement == "same") %>% 
  select(!c(agreement, sentence_type_e, sentence_type_g, sentence_type_c, sentence_type_j3, n_filled)) %>% 
  rename(matrix_sentence_type = agreed_sentence_type)

matrix_sentence_summary <- matrix_sentence.data %>% 
  group_by(verb, matrix_sentence_type) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()

ggplot(matrix_sentence_summary, 
       aes(x=verb,
           y=percent,
           fill=matrix_sentence_type)) + 
  geom_col()+
  geom_text(aes(label=paste0(count, " (", round(percent, 1), "%)")),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 3)+
  theme_bw()+
  scale_fill_brewer(palette = "Set3",
                    name="Matrix sentence type") +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14))+
  labs(x="Verb",
       y="Percentage")

## 3.3 embedded sentence ----
embedded_sentence.data <- clean_data %>% 
  select(c(id, verb, embedded_sentence_type_e, embedded_sentence_type_g, embedded_sentence_type_c, embedded_sentence_type_j3)) %>% 
  rowwise() %>% 
  mutate(
    agreement = {
      vals <- c_across(embedded_sentence_type_e:embedded_sentence_type_j3)
      vals <- vals[!is.na(vals)]
      if_else(length(unique(vals)) == 1, "same", "diff")
    },
    agreed_embedded_sentence_type = {
      vals <- c_across(embedded_sentence_type_e:embedded_sentence_type_j3)
      vals <- vals[!is.na(vals)]
      if_else(agreement == "same", vals[1], NA_character_)
    }
  ) %>%
  ungroup() %>% 
  filter(agreement == "same") %>% 
  select(!c(agreement, embedded_sentence_type_e, embedded_sentence_type_g, embedded_sentence_type_c, embedded_sentence_type_j3)) %>% 
  rename(embedded_sentence_type = agreed_embedded_sentence_type)

embedded_sentence_summary <- embedded_sentence.data %>% 
  group_by(verb, embedded_sentence_type) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()

ggplot(embedded_sentence_summary, 
       aes(x=verb,
           y=percent,
           fill=embedded_sentence_type)) + 
  geom_col()+
  geom_text(aes(label=paste0(count, " (", round(percent, 1), "%)")),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 3)+
  theme_bw()+
  scale_fill_brewer(palette = "Set3",
                    name="Embedded CP type") +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14))+
  labs(x="Verb",
       y="Percentage")

### 3.3.1 matrix and embedded sentence ----
matrix_embedded_sentence.data <- clean_data %>% 
  select(c(id, verb, 
           sentence_type_e, sentence_type_g, sentence_type_c, sentence_type_j3,
           embedded_sentence_type_e, embedded_sentence_type_g, embedded_sentence_type_c, embedded_sentence_type_j3)) %>% 
  mutate(
    across(embedded_sentence_type_e:embedded_sentence_type_j3, ~ if_else(.x == "", "NONE", .x))
  ) %>% 
  rowwise() %>% 
  mutate(
    matrix_n_filled = sum(c_across(sentence_type_e:sentence_type_j3) != "" & !is.na(c_across(sentence_type_e:sentence_type_j3))),
    matrix_agreement = {
      vals <- c_across(sentence_type_e:sentence_type_j3)
      vals <- vals[vals != "" & !is.na(vals)]
      if_else(matrix_n_filled == 2 && length(unique(vals)) == 1, "same", "diff")
    },
    embedded_agreement = {
      vals <- c_across(embedded_sentence_type_e:embedded_sentence_type_j3)
      vals <- vals[!is.na(vals)]
      if_else(length(unique(vals)) == 1, "same", "diff")
    },
    agreed_sentence_type = {
      vals <- c_across(sentence_type_e:sentence_type_j3)
      vals <- vals[vals != "" & !is.na(vals)]
      vals[1]
    },
    agreed_embedded_sentence_type = {
      vals <- c_across(embedded_sentence_type_e:embedded_sentence_type_j3)
      vals <- vals[!is.na(vals)]
      if_else(embedded_agreement == "same", vals[1], NA_character_)
    }
  ) %>%
  ungroup() %>% 
  filter(matrix_agreement == "same" & embedded_agreement == "same") %>% 
  select(!c(matrix_n_filled, matrix_agreement,sentence_type_e, sentence_type_g, sentence_type_c, sentence_type_j3,
            embedded_agreement, embedded_sentence_type_e, embedded_sentence_type_g, embedded_sentence_type_c, embedded_sentence_type_j3)) %>% 
  rename(matrix_sentence_type = agreed_sentence_type,
         embedded_sentence_type = agreed_embedded_sentence_type)

matrix_embedded_sentence_combined.data <- matrix_embedded_sentence.data %>%
  mutate(matrix_sentence_surface_type =
           case_when(matrix_sentence_type=="2 wh_question" & embedded_sentence_type=="3 base_position" ~ "0 declarative",
                     matrix_sentence_type=="1 polar_question" & embedded_sentence_type=="3 base_position" ~ "0 declarative",
                     TRUE ~ matrix_sentence_type),
         embedded_sentence_surface_type =
           case_when(matrix_sentence_type=="2 wh_question" & embedded_sentence_type=="3 base_position" ~ "3 base_position: wh",
                     matrix_sentence_type=="1 polar_question" & embedded_sentence_type=="3 base_position" ~ "3 base_position: polar",
                     TRUE ~ embedded_sentence_type)
           )

matrix_combined_summary <- matrix_embedded_sentence_combined.data %>% 
  group_by(verb, matrix_sentence_surface_type,matrix_sentence_type) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()

ggplot(matrix_combined_summary, 
       aes(x=verb,
           y=percent,
           fill=matrix_sentence_surface_type,
           pattern=matrix_sentence_type)) + 
  geom_col_pattern(
    color = "black",
    linewidth = 0.2,
    pattern_fill = "lightgrey",
    pattern_density = 0.1,
    pattern_size = 0.2,
    pattern_spacing = 0.03,
    pattern_alpha = 0.4
  )+
  scale_pattern_manual(
    values = c(
      "0 declarative" = "none",
      "1 polar_question" = "stripe",
      "2 wh_question" = "circle",
      "4 tag_question" = "none"),
    name="Matrix sentence type"
    )+
  geom_text(aes(label=paste0(count, " (", round(percent, 1), "%)")),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 3)+
  theme_bw()+
  scale_fill_brewer(palette = "Set3",
                    name="Matrix sentence type") +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14))+
  labs(x="Verb",
       y="Percentage")

embedded_combined_summary <- matrix_embedded_sentence_combined.data %>% 
  group_by(verb, embedded_sentence_surface_type) %>%
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()

ggplot(embedded_combined_summary %>% 
         mutate(embedded_sentence_surface_type = fct_relevel(
           embedded_sentence_surface_type,
           "0 declarative",
           "1 polar_question",
           "3 base_position: polar",
           "2 wh_question",
           "3 base_position: wh",
           "NONE"
         )), 
       aes(x=verb,
           y=percent,
           fill=embedded_sentence_surface_type,
           pattern=embedded_sentence_surface_type)) + 
  geom_col_pattern(
    color = "black",
    linewidth = 0.2,
    pattern_fill = "lightgrey",
    pattern_density = 0.1,
    pattern_size = 0.2,
    pattern_spacing = 0.03,
    pattern_alpha = 0.4
  )+
  scale_pattern_manual(
    values = c(
      "0 declarative" = "none",
      "1 polar_question" = "none",
      "2 wh_question" = "none",
      "3 base_position: polar" = "circle",
      "3 base_position: wh" = "circle",
      "NONE" = "none"),
    name="Embedded CP type"
  )+
  geom_text(aes(label=paste0(count, " (", round(percent, 1), "%)")),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 3)+
  theme_bw()+
  scale_fill_manual(
    values = c(
      "0 declarative" = "#8DD3C7",  # 1
      "1 polar_question" = "#FFFFB3",  # 2
      "3 base_position: polar" = "#FFFFB3",
      "2 wh_question" = "#BEBADA", # 3
      "3 base_position: wh" = "#BEBADA",
      "NONE" = "#80B1D3" # 5
    ),
    name = "Embedded CP type"
  )+
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14))+
  labs(x="Verb",
       y="Percentage")

## 3.4 negation ----
matrix_negation.data <- clean_data %>% 
  select(c(id, verb, matrix_negation_e, matrix_negation_j1, matrix_negation_g)) %>% 
  rowwise() %>% 
  mutate(
    agreement = {
      vals <- c_across(matrix_negation_e:matrix_negation_g)
      vals <- vals[!is.na(vals)]
      if_else(length(unique(vals)) == 1, "same", "diff")
    },
    agreed_negation_type = {
      vals <- c_across(matrix_negation_e:matrix_negation_g)
      vals <- vals[!is.na(vals)]
      if_else(agreement == "same", vals[1], NA_character_)
    }
  ) %>%
  ungroup() %>% 
  filter(agreement == "same") %>% 
  select(!c(agreement, matrix_negation_e, matrix_negation_j1, matrix_negation_g)) %>% 
  rename(negation_type = agreed_negation_type)

matrix_negation_summary <- matrix_negation.data %>% 
  mutate(negation = if_else(negation_type == "0 no negation", negation_type, "1 negation")) %>% 
  group_by(verb, negation) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()

ggplot(matrix_negation_summary, 
       aes(x=verb,
           y=percent,
           fill=negation)) + 
  geom_col()+
  geom_text(aes(label=paste0(count, " (", round(percent, 1), "%)")),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 3)+
  theme_bw()+
  scale_fill_brewer(palette = "Set3",
                    name="Matrix negation type") +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14))+
  labs(x="Verb",
       y="Percentage")

## 3.5 speech acts ----
subject_sentence.data <- clean_data %>% 
  select(c(id, verb,
           subject_j1, subject_j2,
           sentence_type_e, sentence_type_g, sentence_type_c, sentence_type_j3,
           embedded_sentence_type_e, embedded_sentence_type_g, embedded_sentence_type_c, embedded_sentence_type_j3,
           matrix_negation_e, matrix_negation_j1, matrix_negation_g)) %>% 
  mutate(subject_agreement = if_else(subject_j1 == subject_j2, "same", "diff"),
    across(embedded_sentence_type_e:embedded_sentence_type_j3, ~ if_else(.x == "", "NONE", .x))
  ) %>% 
  filter(subject_agreement=="same") %>% 
  select(!c(subject_agreement, subject_j1)) %>%
  rowwise() %>% 
  mutate(
    matrix_n_filled = sum(c_across(sentence_type_e:sentence_type_j3) != "" & !is.na(c_across(sentence_type_e:sentence_type_j3))),
    matrix_agreement = {
      vals <- c_across(sentence_type_e:sentence_type_j3)
      vals <- vals[vals != "" & !is.na(vals)]
      if_else(matrix_n_filled == 2 && length(unique(vals)) == 1, "same", "diff")
    },
    embedded_agreement = {
      vals <- c_across(embedded_sentence_type_e:embedded_sentence_type_j3)
      vals <- vals[!is.na(vals)]
      if_else(length(unique(vals)) == 1, "same", "diff")
    },
    negation_agreement = {
      vals <- c_across(matrix_negation_e:matrix_negation_g)
      vals <- vals[!is.na(vals)]
      if_else(length(unique(vals)) == 1, "same", "diff")
    },
    agreed_sentence_type = {
      vals <- c_across(sentence_type_e:sentence_type_j3)
      vals <- vals[vals != "" & !is.na(vals)]
      vals[1]
    },
    agreed_embedded_sentence_type = {
      vals <- c_across(embedded_sentence_type_e:embedded_sentence_type_j3)
      vals <- vals[!is.na(vals)]
      if_else(embedded_agreement == "same", vals[1], NA_character_)
    },
    agreed_negation_type = {
      vals <- c_across(matrix_negation_e:matrix_negation_g)
      vals <- vals[!is.na(vals)]
      if_else(negation_agreement == "same", vals[1], NA_character_)
    }
  ) %>%
  ungroup() %>% 
  filter(matrix_agreement == "same" & embedded_agreement == "same" & negation_agreement == "same") %>% 
  select(!c(matrix_n_filled, matrix_agreement,sentence_type_e, sentence_type_g, sentence_type_c, sentence_type_j3,
            embedded_agreement, embedded_sentence_type_e, embedded_sentence_type_g, embedded_sentence_type_c, embedded_sentence_type_j3,
            matrix_negation_e, matrix_negation_j1, matrix_negation_g,
            negation_agreement)) %>% 
  rename(subject_type = subject_j2,
         matrix_sentence_type = agreed_sentence_type,
         embedded_sentence_type = agreed_embedded_sentence_type,
         negation_type = agreed_negation_type)

speech_acts.data <- subject_sentence.data %>% 
  mutate(speech_act = case_when(
    subject_type %in% c("1 first_person", "0 dropped") & matrix_sentence_type == "0 declarative" & embedded_sentence_type == "NONE" & negation_type == "2 不" ~ "i don't know/think",
    subject_type == "1 first_person" & matrix_sentence_type == "0 declarative" & embedded_sentence_type == "0 declarative" ~ "i think/know P",
    subject_type == "2 second_person" & matrix_sentence_type == "1 polar_question" & embedded_sentence_type == "2 wh_question" ~ "do you know WH",
    subject_type == "2 second_person" & matrix_sentence_type == "1 polar_question" & embedded_sentence_type == "0 declarative" ~ "do you know/think P",
    subject_type == "2 second_person" & matrix_sentence_type == "1 polar_question" & embedded_sentence_type == "3 base_position" ~ "do you know/think P: base",
    subject_type == "2 second_person" & matrix_sentence_type == "2 wh_question" & embedded_sentence_type == "3 base_position" ~ "WH do you think: base",
    subject_type == "2 second_person" & matrix_sentence_type == "2 wh_question" & embedded_sentence_type == "0 declarative" ~ "WH do you know/think P",
    TRUE ~ "others"))

speech_acts_summary <- speech_acts.data %>% 
  group_by(verb, speech_act) %>%
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup() %>% 
  mutate(speech_act=fct_relevel(speech_act,
                                "do you know WH",
                                "WH do you think: base",
                                "WH do you know/think P",
                                "do you know/think P: base",
                                "do you know/think P",
                                "i don't know/think",
                                "others"))

ggplot(speech_acts_summary, 
       aes(x=verb,
           y=percent,
           fill=speech_act,
           pattern=speech_act)) + 
  geom_col_pattern(
    color = "black",
    linewidth = 0.2,
    pattern_fill = "lightgrey",
    pattern_density = 0.1,
    pattern_size = 0.2,
    pattern_spacing = 0.03,
    pattern_alpha = 0.6
  )+
  scale_pattern_manual(
    values = c(
      "do you know WH" = "none",
      "WH do you know/think P" = "none",
      "WH do you think: base" = "circle",
      "do you know/think P" = "none",
      "do you know/think P: base" = "circle",
      "i think/know P" = "none",
      "i don't know/think" = "none",
      "others" = "none"),
    name="Speech acts type"
  )+
  geom_text(aes(label=paste0(count, " (", round(percent, 1), "%)")),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 3)+
  theme_bw()+
  scale_fill_manual(
    values = c(
      "do you know WH" = "#CC79A7", # purple
      "WH do you think: base" = "#CC79A7",
      "WH do you know/think P" = "#F0E442",
      "do you know/think P" = "#56B4E9",  # blue
      "do you know/think P: base" = "#56B4E9",
      "i think/know P" = "#E69F00", # orange
      "i don't know/think" = "#009E73", # green
      "others" = "grey80"
    ),
    name="Speech acts type"
  ) +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14))+
  labs(x="Verb",
       y="Percentage")

# simple
speech_acts.data <- subject_sentence.data %>% 
  mutate(negation = if_else(negation_type == "0 no negation", "0 no negation", "1 yes negation")) %>% 
  mutate(speech_act = case_when(
    subject_type %in% c("1 first_person", "0 dropped") & matrix_sentence_type == "0 declarative" & embedded_sentence_type == "NONE" & negation == "1 yes negation" ~ "(i) don't know/think",
    subject_type %in% c("1 first_person") & matrix_sentence_type == "0 declarative" & embedded_sentence_type == "0 declarative" ~ "i think/know P",
    subject_type == "2 second_person" & matrix_sentence_type == "1 polar_question" & embedded_sentence_type == "2 wh_question" & negation == "0 no negation" ~ "do you know WH",
    subject_type == "2 second_person" & matrix_sentence_type == "1 polar_question" & embedded_sentence_type == "0 declarative" & negation == "0 no negation" ~ "do you know/think P",
    subject_type == "2 second_person" & matrix_sentence_type == "1 polar_question" & embedded_sentence_type == "3 base_position" & negation == "0 no negation" ~ "do you think/know P: base",
    subject_type == "2 second_person" & matrix_sentence_type == "2 wh_question" & embedded_sentence_type == "3 base_position" & negation == "0 no negation" ~ "do you think/know WH: base",
    subject_type %in% c("0 dropped", "2 second_person") & matrix_sentence_type == "1 polar_question" & embedded_sentence_type == "NONE" & negation == "0 no negation" ~ "(you) know/think?",
    matrix_sentence_type == "4 tag_question" ~ "tag question",
    subject_type %in% c("1 first_person", "0 dropped") & matrix_sentence_type == "0 declarative" & embedded_sentence_type == "NONE" & negation == "0 no negation" ~ "(i) know/think",
    
    TRUE ~ "others"))

speech_acts_summary <- speech_acts.data %>% 
  group_by(verb, speech_act) %>%
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup() %>% 
  mutate(speech_act=fct_relevel(speech_act,
                                "do you know WH",
                                "do you think/know WH: base",
                                "do you think/know P: base",
                                "do you know/think P",
                                "(i) don't know/think",
                                "(i) know/think",
                                "i think/know P",
                                "(you) know/think?",
                                "tag question",
                                "others"))

ggplot(speech_acts_summary, 
       aes(x=verb,
           y=percent,
           fill=speech_act,
           pattern=speech_act)) + 
  geom_col_pattern(
    color = "black",
    linewidth = 0.2,
    pattern_fill = "lightgrey",
    pattern_density = 0.1,
    pattern_size = 0.2,
    pattern_spacing = 0.03,
    pattern_alpha = 0.6
  )+
  scale_pattern_manual(
    values = c(
      "do you know WH" = "none",
      "WH do you know/think P" = "none",
      "do you think/know WH: base" = "circle",
      "do you know/think P" = "none",
      "do you think/know P: base" = "circle",
      "i think/know P" = "none",
      "(i) don't know/think" = "none",
      "(i) know/think" = "none",
      "(you) know/think?" = "none",
      "tag question" = "none",
      "others" = "none"),
    name="Speech acts type"
  )+
  geom_text(aes(label=paste0(count, " (", round(percent, 1), "%)")),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 3)+
  theme_bw()+
  scale_fill_manual(
    values = c(
      "do you know WH" = "#CC79A7", # purple
      "do you think/know WH: base" = "#CC79A7",
      "do you know/think P" = "#56B4E9",  # blue
      "do you think/know P: base" = "#56B4E9",
      "i think/know P" = "#E69F00", # orange
      "(i) don't know/think" = "#009E73", # green
      "(i) know/think" = "#B4CEB3",
      "(you) know/think?" = "#F0E442",
      "tag question" = "#D55E00",
      "others" = "grey80"
    ),
    name="Speech acts type"
  ) +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14))+
  labs(x="Verb",
       y="Percentage")

other_speech_acts.data <- speech_acts.data %>% 
  filter(speech_act=="others") %>% 
  mutate(speech_act = case_when(
    subject_type == "2 second_person" & matrix_sentence_type == "0 declarative" & embedded_sentence_type %in% c("1 polar_qustion", "2 wh_question") & negation == "1 yes negation" ~ "you don't think/know P/WH",
    subject_type == "2 second_person" & matrix_sentence_type == "0 declarative" & embedded_sentence_type %in% c("1 polar_qustion", "2 wh_question") & negation == "0 no negation" ~ "you think/know P/WH",
    subject_type == "2 second_person" & matrix_sentence_type == "0 declarative" & embedded_sentence_type == "NONE" & negation == "1 yes negation" ~ "you don't think/know",
    subject_type == "2 second_person" & matrix_sentence_type == "0 declarative" & negation == "0 no negation" ~ "you think/know P/WH",
    subject_type == "2 second_person" & matrix_sentence_type %in% c("1 polar_qustion", "2 wh_question") ~ "do/WH you (not) think/know (P/WH)",
    subject_type == "0 dropped" & matrix_sentence_type == "0 declarative" & embedded_sentence_type == "2 wh_question" ~ "(don't) think/know WH",
    subject_type == "1 first_person" & matrix_sentence_type == "0 declarative" & embedded_sentence_type %in% c("1 polar_question","2 wh_question") ~ "i (don't) think/know (P/WH)",
    subject_type == "3 third/NP" & matrix_sentence_type == "0 declarative" & embedded_sentence_type %in% c("1 polar_qustion", "2 wh_question", "NONE") ~ "she/he/NP (don't) think/know (P/WH)",
    subject_type == "3 third/NP" & matrix_sentence_type %in% c("1 polar_qustion", "2 wh_question") & embedded_sentence_type %in% c("0 declarative", "NONE") ~ "does/WH she//NP (don't) think/know (P)",
    TRUE ~ "others"))

other_speech_acts_summary <- other_speech_acts.data %>% 
  group_by(verb, speech_act) %>%
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()
