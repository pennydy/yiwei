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

# 1. Load data ----
data <- read.csv("../../data/childes/zhidao_juede_no_readings_combined.csv", header=TRUE)

# 2. Get annotations that differ ----
## 2.1 matrix subject ----
matrix_subject_diff.data <- data %>% 
  select(c("id", "verb", "file_name", "speaker", "sentence", "subject_j1", "subject_j2")) %>% 
  mutate(difference = if_else(subject_j1 == subject_j2, "same", "diff")) %>% 
  filter(difference == "diff") %>% 
  select(!difference)
# write.csv(matrix_subject_diff.data, "../../data/childes/annotation_diff/matrix_subject_diff.csv", row.names = FALSE)
nrow(matrix_subject_diff.data) / nrow(data) # overall: 0.02
matrix_subject_diff.data %>% 
  group_by(verb) %>% 
  summarize(percentage = n()/nrow(data)) # juede: 0.004, zhidao: 0.016

## 2.2 matrix sentence type ----
matrix_sentence_diff.data <- data %>% 
  select(c("id", "verb", "file_name", "speaker", "sentence", "sentence_type_e", 
           "sentence_type_g", "sentence_type_j3", "sentence_type_c")) %>% 
  mutate(difference = apply(
    select(., starts_with("sentence_type_")),
    1,
    function(x) {
      values <- x[!is.na(x)] 
      if (length(values) < 2) {
        "diff"
      } else {
        if_else(values[1] == values[2], "same", "diff")
      }
    }
  )) %>% 
  filter(difference == "diff") %>% 
  select(!difference)
# write.csv(matrix_sentence_diff.data, "../../data/childes/annotation_diff/matrix_sentence_diff.csv", row.names = FALSE)
nrow(matrix_sentence_diff.data) / nrow(data) # overall: 0.063
matrix_sentence_diff.data %>% 
  group_by(verb) %>% 
  summarize(percentage = n()/nrow(data)) # juede: 0.0156, zhidao: 0.0474

## 2.3 embedded sentence type ----
embedded_sentence_diff.data <- data %>% 
  select(c("id", "verb", "file_name", "speaker", "sentence", starts_with("embedded_sentence_type_"))) %>% 
  mutate(difference = apply(
    select(., starts_with("embedded_sentence_type_")),
    1,
    function(x) {
      values <- x[!is.na(x)] 
      if (length(values) < 2) {
        "diff"
      } else {
        if_else(values[1] == values[2], "same", "diff")
        }
    }
  )) %>% 
  filter(difference == "diff") %>% 
  select(!difference)
# write.csv(embedded_sentence_diff.data, "../../data/childes/annotation_diff/embedded_sentence_diff.csv", row.names = FALSE)
nrow(embedded_sentence_diff.data) / nrow(data) # overall: 0.051
embedded_sentence_diff.data %>% 
  group_by(verb) %>% 
  summarize(percentage = n()/nrow(data)) # juede: 0.0186, zhidao: 0.0323

## 2.4 matrix negation ----
matrix_negation_diff.data <- data %>% 
  select(c("id", "verb", "file_name", "speaker", "sentence", starts_with("matrix_negation_"))) %>% 
  mutate(difference = apply(
    select(., starts_with("matrix_negation_")),
    1,
    function(x) {
      values <- x[!is.na(x)] 
      if_else(values[1] == values[2], "same", "diff")
    }
  )) %>% 
  filter(difference == "diff") %>% 
  select(!difference)
# write.csv(matrix_negation_diff.data, "../../data/childes/annotation_diff/matrix_negation_diff.csv", row.names = FALSE)
nrow(matrix_negation_diff.data) / nrow(data) # overall: 0.001
matrix_negation_diff.data %>% 
  group_by(verb) %>% 
  summarize(percentage = n()/nrow(data)) # juede: 0.0186, zhidao: 0.0323

## 2.5 ma sentence final particle ----
ma_particle_diff.data <- data %>% 
  select(c("id", "verb", "file_name", "speaker", "sentence", starts_with("ma_particle_"))) %>% 
  mutate(difference = apply(
    select(., starts_with("ma_particle_")),
    1,
    function(x) {
      values <- x[!is.na(x)] 
      if_else(values[1] == values[2], "same", "diff")
    }
  )) %>% 
  filter(difference == "diff") %>% 
  select(!difference)
write.csv(ma_particle_diff.data, "../../data/childes/annotation_diff/ma_particle_diff.csv", row.names = FALSE)


# 3 Reliability ----
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

## 3.1 matrix subject ----
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

## 3.2 matrix sentence ----
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

## 3.3 embedded sentence ----
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

## 3.4 embedded CP ----
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

## 3.5 negation ----
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
