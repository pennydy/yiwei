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
library(stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# access childes 
# d_chi <- get_transcripts(collection = "Chinese")
# head(d_chi)

# 0. load the data ----
# 16: repeated for correction; 23, 79, 63: incomplete; 129: question
# 56?
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

### matrix subject x clause type ----
child_matrix_clause_subject <- child_summary %>% 
  mutate(sentence_type = if_else(sentence_type == "interrogative", sentence_type , "declarative")) %>% 
  group_by(subject, sentence_type) %>% 
  summarize(count=n()) %>% 
  ungroup()
child_matrix_clause_subject

### embed subject x clause type ----
child_embed_clause_subject <- child_summary %>% 
  group_by(embedded_subject, embedded_type) %>% 
  summarize(count=n()) %>% 
  ungroup()
child_embed_clause_subject

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
  mutate(discourse_type = if_else(grepl("against", discourse_type), "inferable", discourse_type)) %>% 
  mutate(p_not_p = if_else(!(discourse_type %in% c("p", "unclear", "no", "inferable")), "contrast", discourse_type)) %>% 
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
  
### matrix subject x clause type ----
adult_matrix_clause_subject <- adult_summary %>% 
  mutate(sentence_type = if_else(sentence_type == "interrogative", sentence_type , "declarative")) %>% 
  group_by(subject, sentence_type) %>% 
  summarize(count=n()) %>% 
  ungroup()
adult_matrix_clause_subject

### embed subject x clause type ----
adult_embed_clause_subject <- adult_summary %>% 
  group_by(embedded_subject, embedded_type) %>% 
  summarize(count=n()) %>% 
  ungroup()
adult_embed_clause_subject

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
  mutate(discourse_type = if_else(grepl("against", discourse_type), "inferable", discourse_type)) %>% 
  mutate(discourse_type = if_else(grepl("p_alt", discourse_type), "p_alt", discourse_type)) %>% 
  mutate(p_not_p = if_else(!(discourse_type %in% c("p", "unclear","no", "inferable")), "contrast", discourse_type)) %>% 
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
             position = position_fill(vjust = 0.5),
             show.legend = FALSE)+
  theme_bw()+
  scale_fill_brewer(palette = "Set2",
                    name="Subject Type",
                    labels=c("Dropped", "First person", "Second person", "Other", "Unclear")) +
  scale_x_discrete(labels=c("input_subject" = "Children's input",
                            "output_subject" = "Children's output"))+
  theme(legend.position = "top",
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14))+
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
  geom_bar(position=position_fill(),
           stat="identity")+
  geom_label(aes(label = count,
                 group=factor(subject_type)), 
            position = position_fill(vjust = 0.5),
            show.legend = FALSE)+
  theme_bw()+
  scale_fill_brewer(palette = "Set2",
                    name="Subject Type",
                    labels = c("Dropped", "First person", "Second person", "Other")) +
  scale_x_discrete(labels=c("input_subject" = "Child-ambient speech",
                            "output_subject" = "Children's production"))+
  theme(legend.position = "top",
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14))+
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
                            aes(y=count,
                                x=input_output,
                                pattern=clause_type)) + 
  geom_col_pattern(position="fill",
                   pattern_spacing = 0.02,
                   pattern_frequency = 5,
                   pattern_fill = "black",
                   pattern_angle=45,
                   pattern_alpha=0.4,
                   alpha=0.3,
                   linewidth=1)+
  geom_label(aes(label = count,
                 group=factor(clause_type)),
             position = position_fill(vjust = 0.5),
             show.legend = FALSE)+
  theme_bw()+
  scale_pattern_manual(values=c(declarative = 'stripe', 
                                interrogative = 'none'),
                       name = "Clause Type") +
  scale_x_discrete(labels=c("input_clause" = "Child-ambient speech",
                            "output_clause" = "Children's production"))+
  theme(legend.position = "top",
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14))+
  labs(y="Proportion")
matrix_clause_plot
ggsave(matrix_clause_plot, file="graphs/matrix_clause_plot.pdf", width=7, height=4)


embed_clause <- merge(child_embed_clause, adult_embed_clause, 
                       by=c("clause_type"),
                       all=TRUE) %>% 
  rename(output_clause = "count.x", # child
         input_clause = "count.y") %>%  # adult
  pivot_longer(cols=c("output_clause", "input_clause"), 
               names_to = "input_output",
               values_to = "count")

embed_clause_plot<-ggplot(embed_clause, 
                           aes(pattern=clause_type, 
                               y=count,
                               x=input_output)) + 
  geom_col_pattern(position="fill",
                   pattern_spacing = 0.02,
                   pattern_frequency = 5,
                   pattern_fill = "black",
                   pattern_angle=45,
                   pattern_alpha=0.4,
                   alpha=0.3,
                   linewidth=1)+
  geom_label(aes(label = count,
                 group=factor(clause_type)), 
             position = position_fill(vjust = 0.5),
             show.legend = FALSE)+
  theme_bw()+
  scale_pattern_manual(values=c(declarative = 'stripe',
                                interrogative = 'none',
                                unclear = 'weave'),
                       name="Clause Type") +
  scale_x_discrete(labels=c("input_clause" = "Child-ambient speech",
                            "output_clause" = "Children's production"))+
  theme(legend.position = "top",
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14))+
  labs(y="Proportion")
embed_clause_plot
ggsave(embed_clause_plot, file="graphs/embed_clause_plot.pdf", width=7, height=4)

## matrix subject x clause type ----
input_output_names <- list(
  "input" = "Child-ambient speech",
  "output" = "Children's production"
)

input_output_labeller <- function(variable, value){
  return(input_output_names[value])
}

matrix_clause_subject <- merge(child_matrix_clause_subject, adult_matrix_clause_subject,
                               by=c("subject", "sentence_type"),
                        all=TRUE) %>% 
  rename(output = "count.x", # child
         input = "count.y") %>%  # adult
  pivot_longer(cols=c("output", "input"), 
               names_to = "input_output",
               values_to = "count")
matrix_clause_subject

matrix_clause_subject_plot<-ggplot(matrix_clause_subject, 
                          aes(fill=subject, 
                              y=count,
                              x=sentence_type)) + 
  geom_bar(position=position_dodge2(preserve = "single"),
           stat="identity")+
  theme_bw()+
  facet_grid(.~input_output, labeller=labeller(input_output=input_output_labeller))+
  scale_fill_brewer(palette = "Set2",
                    name="Matrix subject",
                    labels = c("Dropped", "First person", "Second person", "Others", "Unclear")) +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14))+
  labs(x="Matrix clause type",
       y="Counts")
matrix_clause_subject_plot
ggsave(matrix_clause_subject_plot, file="graphs/matrix_clause_subject_plot.pdf", width=7, height=4)

## embed subject x clause type ----
embed_clause_subject <- merge(child_embed_clause_subject, adult_embed_clause_subject,
                               by=c("embedded_subject", "embedded_type"),
                               all=TRUE) %>% 
  rename(output = "count.x", # child
         input = "count.y") %>%  # adult
  pivot_longer(cols=c("output", "input"), 
               names_to = "input_output",
               values_to = "count")
embed_clause_subject

embed_clause_subject_plot<-ggplot(embed_clause_subject, 
                                   aes(fill=embedded_subject, 
                                       y=count,
                                       x=embedded_type)) + 
  geom_bar(position=position_dodge2(preserve = "single"),
           stat="identity")+
  theme_bw()+
  facet_grid(.~input_output, labeller=labeller(input_output=input_output_labeller))+
  scale_fill_brewer(palette = "Set2",
                    name="Matrix subject",
                    labels = c("Dropped", "First person", "Second person", "Others", "Unclear")) +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14))+
  labs(x="Embedded clause type",
       y="Counts")
embed_clause_subject_plot
ggsave(embed_clause_subject_plot, file="graphs/embed_clause_subject_plot.pdf", width=7, height=4)

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
  scale_x_discrete(labels=c("input_discourse_type" = "Child-ambient speech",
                            "output_discourse_type" = "Children's production"))+
  theme(axis.title.x = element_blank(),
        legend.position = "top",
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14))+
  labs(y="Proportion")
discourse_plot
ggsave(discourse_plot, file="graphs/discourse_plot.pdf", width=7, height=4)
