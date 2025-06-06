# load the library
library(childesr)
library(dplyr)
library(lme4)
library(emmeans)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggsignif)
library(tidytext)
library(RColorBrewer)
library(stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# access childes 
# d_chi <- get_transcripts(collection = "Chinese")
# head(d_chi)

# 0. load the data ----
# 16: repeated for correction; 23, 79, 63: incomplete; 129: question
annotation.data <- read.csv("../../data/yiwei.csv", header=TRUE) %>% 
  filter(!(id %in% c("16", "23", "129", "79", "63"))) 

# 1. summary ----
## children's output ----
child_summary <- annotation.data %>% 
  filter(speaker == "child")
### type of subject ----
child_matrix_subject <- data.frame(table(child_summary$subject)) 
colnames(child_matrix_subject) <- c("subject_type", "count")
child_matrix_subject 
child_embed_subject <- data.frame(table(child_summary$embedded_subject))
colnames(child_embed_subject) <- c("subject_type", "count")
child_embed_subject

### type of clause ----
child_matrix_clause <- data.frame(table(child_summary$sentence_type))
colnames(child_matrix_clause) <- c("clause_type", "count")
child_matrix_clause
child_embed_clause <- data.frame(table(child_summary$embedded_type))
colnames(child_embed_clause) <- c("clause_type", "count")
child_embed_clause

### discourse status ----
child_discourse <- data.frame(table(child_summary$in_context))
colnames(child_discourse) <- c("discourse_type", "count")
child_discourse <- child_discourse %>% 
  mutate(discourse_type = if_else(grepl("unclear", discourse_type, fixed=TRUE), "unclear", discourse_type)) %>% 
  group_by(discourse_type) %>% 
  summarize(count = sum(count)) %>% 
  ungroup()
child_discourse

child_discourse_summary <- child_discourse %>% 
  mutate(discourse_type = if_else(grepl("against", discourse_type), "unclear", discourse_type)) %>% 
  mutate(p_not_p = if_else(!(discourse_type %in% c("p", "unclear", "no")), "contrast", discourse_type)) %>% 
  group_by(p_not_p) %>% 
  summarize(count = sum(count)) %>% 
  ungroup()
child_discourse_summary

## children's input ----
adult_summary <- annotation.data %>% 
  filter(speaker != "child")
### type of subject ----
adult_matrix_subject <- data.frame(table(adult_summary$subject))
colnames(adult_matrix_subject) <- c("subject_type", "count")
adult_matrix_subject
adult_embed_subject <- data.frame(table(adult_summary$embedded_subject))
colnames(adult_embed_subject) <- c("subject_type", "count")
adult_embed_subject

### type of clause ----
adult_matrix_clause <- data.frame(table(adult_summary$sentence_type))
colnames(adult_matrix_clause) <- c("clause_type", "count")
adult_matrix_clause
adult_matrix_clause <- adult_matrix_clause %>% 
  mutate(clause_type = if_else(clause_type == "interrogative", clause_type , "declarative")) %>% 
  group_by(clause_type) %>% 
  summarize(count = sum(count)) %>% 
  ungroup()
adult_matrix_clause
adult_embed_clause <- data.frame(table(adult_summary$embedded_type))
colnames(adult_embed_clause) <- c("clause_type", "count")
adult_embed_clause
  

### discourse status ----
adult_discourse <- data.frame(table(adult_summary$in_context))
colnames(adult_discourse) <- c("discourse_type", "count")
adult_discourse <- adult_discourse %>% 
  mutate(discourse_type = if_else(grepl("unclear", discourse_type, fixed=TRUE), "unclear", discourse_type)) %>% 
  group_by(discourse_type) %>% 
  summarize(count = sum(count)) %>% 
  ungroup()
adult_discourse

adult_discourse_summary <- adult_discourse %>% 
  mutate(discourse_type = if_else(grepl("against", discourse_type), "unclear", discourse_type)) %>% 
  mutate(discourse_type = if_else(grepl("p_alt", discourse_type), "p_alt", discourse_type)) %>% 
  mutate(p_not_p = if_else(!(discourse_type %in% c("p", "unclear","no")), "contrast", discourse_type)) %>% 
  group_by(p_not_p) %>% 
  summarize(count = sum(count)) %>% 
  ungroup()
adult_discourse_summary


# 2. plot ----
## subject type ----
matrix_subject <- merge(child_matrix_subject, adult_matrix_subject, 
                 by=c("subject_type"),
                 all=TRUE) %>% 
  # replace(., is.na(.), 0) %>% 
  rename(output_subject = "count.x", # child
         input_subject = "count.y") %>%  # adult
  pivot_longer(cols=c("output_subject", "input_subject"), 
               names_to = "input_output",
               values_to = "count")

matrix_subject_plot<-ggplot(matrix_subject, 
                            aes(fill=subject_type, 
                                y=count,
                                x=input_output)) + 
  geom_bar(position="fill",
           stat="identity")+
  geom_label(aes(label = count,
                 group=factor(subject_type)), 
             # fill="white",
             position = position_fill(vjust = 0.5),
             show.legend = FALSE)+
  theme_bw()+
  scale_fill_brewer(palette = "Set3",
                    name="Subject Type") +
  scale_x_discrete(labels=c("input_subject" = "Children's input",
                            "output_subject" = "Children's output"))+
  theme(axis.title.x = element_blank())+
  labs(y="Proportion")
matrix_subject_plot
ggsave(matrix_subject_plot, file="graphs/matrix_subject_plot.pdf", width=7, height=4)


embed_subject <- merge(child_embed_subject, adult_embed_subject, 
                        by=c("subject_type"),
                        all=TRUE) %>% 
  replace(., is.na(.), 0) %>% 
  rename(output_subject = "count.x", # child
         input_subject = "count.y") %>%  # adult
  pivot_longer(cols=c("output_subject", "input_subject"), 
               names_to = "input_output",
               values_to = "count")

embed_subject_plot<-ggplot(embed_subject, 
                            aes(fill=subject_type, 
                                y=count,
                                x=input_output)) + 
  geom_bar(position="fill",
           stat="identity")+
  geom_label(aes(label = count,
                 group=factor(subject_type)), 
            position = position_fill(vjust = 0.5),
            show.legend = FALSE)+
  theme_bw()+
  scale_fill_brewer(palette = "Set3",
                    name="Subject Type") +
  scale_x_discrete(labels=c("input_subject" = "Children's input",
                            "output_subject" = "Children's output"))+
  theme(axis.title.x = element_blank())+
  labs(y="Proportion")
embed_subject_plot
ggsave(embed_subject_plot, file="graphs/embed_subject_plot.pdf", width=7, height=4)

## clause type ----
matrix_clause <- merge(child_matrix_clause, adult_matrix_clause, 
                        by=c("clause_type"),
                        all=TRUE) %>% 
  # replace(., is.na(.), 0) %>% 
  rename(output_clause = "count.x", # child
         input_clause = "count.y") %>%  # adult
  pivot_longer(cols=c("output_clause", "input_clause"), 
               names_to = "input_output",
               values_to = "count")

matrix_clause_plot<-ggplot(matrix_clause, 
                            aes(fill=clause_type, 
                                y=count,
                                x=input_output)) + 
  geom_bar(position="fill",
           stat="identity")+
  geom_label(aes(label = count,
                 group=factor(clause_type)), 
             # fill="white",
             position = position_fill(vjust = 0.5),
             show.legend = FALSE)+
  theme_bw()+
  scale_fill_brewer(palette = "Set3",
                    name="Clause Type") +
  scale_x_discrete(labels=c("input_clause" = "Children's input",
                            "output_clause" = "Children's output"))+
  theme(axis.title.x = element_blank())+
  labs(y="Proportion")
matrix_clause_plot
ggsave(matrix_clause_plot, file="graphs/matrix_clause_plot.pdf", width=7, height=4)


embed_clause <- merge(child_embed_clause, adult_embed_clause, 
                       by=c("clause_type"),
                       all=TRUE) %>% 
  # replace(., is.na(.), 0) %>% 
  rename(output_clause = "count.x", # child
         input_clause = "count.y") %>%  # adult
  pivot_longer(cols=c("output_clause", "input_clause"), 
               names_to = "input_output",
               values_to = "count")

embed_clause_plot<-ggplot(embed_clause, 
                           aes(fill=clause_type, 
                               y=count,
                               x=input_output)) + 
  geom_bar(position="fill",
           stat="identity")+
  geom_label(aes(label = count,
                 group=factor(clause_type)), 
             position = position_fill(vjust = 0.5),
             show.legend = FALSE)+
  theme_bw()+
  scale_fill_brewer(palette = "Set3",
                    name="Clause Type") +
  scale_x_discrete(labels=c("input_clause" = "Children's input",
                            "output_clause" = "Children's output"))+
  theme(axis.title.x = element_blank())+
  labs(y="Proportion")
embed_clause_plot
ggsave(embed_clause_plot, file="graphs/embed_clause_plot.pdf", width=7, height=4)

## discourse type ----
discourse_summary <- merge(child_discourse_summary, adult_discourse_summary, 
                        by=c("p_not_p"),
                        all=TRUE) %>% 
  # replace(., is.na(.), 0) %>% 
  rename(output_discourse_type = "count.x", # child
         input_discourse_type = "count.y") %>%  # adult
  pivot_longer(cols=c("output_discourse_type", "input_discourse_type"), 
               names_to = "input_output",
               values_to = "count")

discourse_plot<-ggplot(discourse_summary, 
                       aes(fill=p_not_p, 
                                y=count,
                                x=input_output)) + 
  geom_bar(position="fill",
           stat="identity")+
  geom_label(aes(label = count,
                 group=factor(p_not_p)), 
             position = position_fill(vjust = 0.5),
             show.legend = FALSE)+
  theme_bw()+
  scale_fill_brewer(palette = "Set3",
                    name="Dicourse Status") +
  scale_x_discrete(labels=c("input_subject" = "Children's input",
                            "output_subject" = "Children's output"))+
  theme(axis.title.x = element_blank())+
  labs(y="Proportion")
discourse_plot
ggsave(discourse_plot, file="graphs/discourse_plot.pdf", width=7, height=4)
