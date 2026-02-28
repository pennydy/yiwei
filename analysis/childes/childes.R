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
sentencePalette <- c("#BEBADA", "#80B1D3") 
# access childes 
# d_chi <- get_transcripts(collection = "Chinese")
# head(d_chi)

# 0. load the data ----
# 16: repeated for correction; 20, 23, 79, 63, 81: incomplete; 129: question
# 56?
yiwei <- read.csv("../../data/childes/yiwei_new.csv", header=TRUE) %>% 
  filter(!(id %in% c("16", "20", "23", "129", "63", "79", "81", "99", "131","284", "292","300"))) 

juede <- read.csv("../../data/childes/juede_new.csv", header=TRUE) %>% 
  filter(!id %in% c( "23", "24", "36", "45","50", "84","85","88","96","104","105","107","108", "466","467", "779", "781","791", "793", "794","795","800", "801", "805", "806","807","808","811","818","823","824","829","840","844","845","846","849","850","853","854","855","856","858","2627") )


zhidao <- read.csv("../../data/childes/zhidao.csv", header=TRUE) %>% 
  filter(!id %in% c("481", "482", "483") )

annotation.data <- bind_rows(lst(yiwei, zhidao, juede), .id="verb")

# 1. summary ----
## children's output ----
child_summary <- annotation.data %>% 
  filter(speaker == "child") %>% 
  mutate(subject_type = if_else(subject == "NP", "third_person", subject)) %>% 
  mutate(embedded_subject_type = if_else(embedded_subject == "NP", "third_person", embedded_subject))

annotation.data %>%
  mutate(speaker_type = if_else(speaker == "child", "child", "adult")) %>% 
  select(verb, speaker_type) %>%
  group_by(speaker_type, verb) %>% 
  summarize(count = n()) %>% 
  ungroup()

### type of subject ----
child_matrix_subject <- child_summary %>% 
  select(verb, subject_type) %>% 
  group_by(verb, subject_type) %>%
  summarize(count = n()) %>% 
  ungroup()

child_embed_subject <- child_summary %>% 
  select(verb, embedded_subject_type) %>% 
  group_by(verb, embedded_subject_type) %>%
  summarize(count = n()) %>% 
  ungroup()

### type of clause ----
child_matrix_clause <- child_summary %>% 
  select(verb, sentence_type) %>% 
  group_by(verb, sentence_type) %>% 
  summarize(count = n()) %>% 
  ungroup()

child_embed_clause <- child_summary %>% 
  select(verb, embedded_type) %>% 
  group_by(verb, embedded_type) %>% 
  summarize(count = n()) %>% 
  ungroup()

### matrix subject x clause type ----
child_matrix_clause_subject <- child_summary %>% 
  group_by(verb,subject_type, sentence_type) %>% 
  summarize(count=n()) %>% 
  ungroup()
child_matrix_clause_subject

### embed subject x clause type ----
child_embed_clause_subject <- child_summary %>% 
  group_by(verb, embedded_subject_type, embedded_type) %>% 
  summarize(count=n()) %>% 
  ungroup()
child_embed_clause_subject

### discourse status ----
child_discourse <- child_summary %>% 
  mutate(discourse_type = if_else(grepl("unclear", in_context, fixed=TRUE), "unclear", in_context)) %>% 
  select(verb, discourse_type) %>% 
  group_by(verb, discourse_type) %>%
  summarize(count = n()) %>% 
  ungroup()
child_discourse

child_discourse_summary <- child_discourse %>% 
  mutate(discourse_type = if_else(grepl("against", discourse_type), "inferable", discourse_type)) %>% 
  mutate(p_not_p = if_else(!(discourse_type %in% c("p", "unclear", "no", "inferable")), "contrast", discourse_type)) %>% 
  group_by(verb, p_not_p) %>% 
  summarize(count = sum(count)) %>% 
  ungroup()
child_discourse_summary

## children's input ----
adult_summary <- annotation.data %>% 
  filter(speaker != "child") %>% 
  mutate(subject_type = if_else(subject == "NP", "third_person", subject)) %>% 
  mutate(embedded_subject_type = if_else(embedded_subject == "NP", "third_person", embedded_subject))

### type of subject ----
adult_matrix_subject <- adult_summary %>% 
  select(verb, subject_type) %>% 
  group_by(verb, subject_type) %>%
  summarize(count = n()) %>% 
  ungroup()

adult_embed_subject <- adult_summary %>% 
  select(verb, embedded_subject_type) %>% 
  group_by(verb, embedded_subject_type) %>%
  summarize(count = n()) %>% 
  ungroup()

### type of clause ----
adult_matrix_clause <- adult_summary %>% 
  select(verb, sentence_type) %>% 
  group_by(verb, sentence_type) %>% 
  summarize(count = n()) %>% 
  ungroup()

adult_embed_clause <- adult_summary %>% 
  select(verb, embedded_type) %>% 
  group_by(verb, embedded_type) %>% 
  summarize(count = n()) %>% 
  ungroup()
  
### matrix subject x clause type ----
adult_matrix_clause_subject <- adult_summary %>% 
  mutate(sentence_type = if_else(sentence_type=="rhetorical", "polar", sentence_type)) %>%
  mutate(sentence_type = if_else(sentence_type %in% c("polar", "how-wh_question","tag_question", "wh_question"), "interrogative", sentence_type)) %>%  # just two levels for now
  mutate(subject_type = if_else(subject_type == "third_person", "others", subject_type)) %>% 
  group_by(verb, subject_type, sentence_type) %>% 
  summarize(count=n()) %>% 
  ungroup()
adult_matrix_clause_subject

adult_matrix_clause_subject_plot<-ggplot(adult_matrix_clause_subject, 
                                         aes(fill=sentence_type, 
                                       y=count,
                                       x=subject_type)) + 
  geom_bar(position=position_dodge2(preserve = "single"),
           stat="identity")+
  # geom_bar(position="fill",
  #          stat="identity")+
  # geom_label(aes(label = count,
  #                group=factor(sentence_type)),
  #            position = position_fill(vjust = 0.5),
  #            show.legend = FALSE)+
  theme_bw()+
  facet_grid(.~verb)+
  # scale_fill_brewer(palette = "Set3",
  #                   name="Matrix clause type") +
  scale_fill_manual(values=sentencePalette,
                    name="Matrix clause type") +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        # axis.text.x = element_text(angle = 45,hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14))+
  scale_x_discrete(labels = c("1st person", "2nd person", "others", "dropped")) +
  labs(x="Matrix subject type",
       y="Counts")
adult_matrix_clause_subject_plot
ggsave(adult_matrix_clause_subject_plot, file="graphs/matrix_type-subject_plot.pdf", width=8, height=3)


### embed subject x clause type ----
adult_embed_clause_subject <- adult_summary %>% 
  # mutate(sentence_type = if_else(sentence_type == "rhetorical", "polar", sentence_type)) %>% 
  mutate(embedded_subject_type = if_else(str_detect(embedded_subject_type, "demonstrative"),"third_person" , embedded_subject_type)) %>%
  group_by(verb, embedded_subject_type, embedded_type) %>% 
  summarize(count=n()) %>% 
  ungroup()
adult_embed_clause_subject


adult_embed_clause_subject_plot<-ggplot(adult_embed_clause_subject %>% 
                                           filter(verb!="zhidao"), 
                                         aes(fill=embedded_type, 
                                             y=count,
                                             x=embedded_subject_type)) + 
  geom_bar(position=position_dodge2(preserve = "single"),
           stat="identity")+
  theme_bw()+
  facet_grid(.~verb)+
  scale_fill_brewer(palette = "Set3",
                    name="Embedded clause type") +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(angle = 45,hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14))+
  labs(x="Embedded subject type",
       y="Counts")
adult_embed_clause_subject_plot


### matrix and embedded subjects vs. matrix clause type ----
adult_all_subject_matrix_clause_type <- adult_summary %>% 
  filter(verb!="zhidao") %>% 
  mutate(embedded_subject_type = if_else(str_detect(embedded_subject_type, "demonstrative"), "third_person" , embedded_subject_type),
         subject_combine = paste(subject_type,embedded_subject_type,sep="-")) %>% 
  # count(verb, subject_type, embedded_subject_type, sentence_type) %>%
  # group_by(verb, subject_type, embedded_subject_type) %>% 
  group_by(verb,subject_combine,sentence_type) %>% 
  # summarize(proportion=n/sum(n)) %>% 
  summarize(count=n()) %>% 
  ungroup()
adult_all_subject_matrix_clause_type

# adult_all_subject_matrix_clause_type <- adult_all_subject_matrix_clause_type %>% 
#   pivot_longer(cols=c(subject_type, embedded_subject_type),
#                 names_to = "subject_role",
#                 values_to = "subject_type_value") %>% 
#   mutate(subject_role = recode(subject_role,
#                                subject_type = "matrix",
#                                embedded_subject_type = "embedded"))

ggplot(adult_all_subject_matrix_clause_type, 
       aes(x=subject_combine,
           y=count,
         fill=sentence_type)) + 
  # geom_bar(stat="identity",position="stack")+
  geom_bar(stat = "identity",
           position = position_dodge2(width = 0.8, preserve = "single")) +
  theme_bw()+
  facet_grid(.~verb)+
  scale_fill_brewer(palette = "Set3",
                    name="Sentence type") +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(angle = 45,hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14))+
  labs(x="Subject type",
       y="Counts")

### discourse status ----
adult_discourse <- as.data.frame(
  table(adult_summary$sentence_type,
        adult_summary$in_context,
        adult_summary$verb))
colnames(adult_discourse) <- c("sentence_type","discourse_type","verb","count")
adult_discourse <- adult_discourse %>% 
  filter(verb!="zhidao") %>% 
  filter(count != 0) %>% 
  mutate(sentence_type = if_else(sentence_type %in% c("polar", "wh_question", "how-wh_question", "rhetorical", "tag_question"), "interrogative", sentence_type)) %>% # two levels for now
  mutate(discourse_type = if_else(grepl("against", discourse_type, fixed=TRUE), "not_p", discourse_type)) %>% 
  mutate(discourse_type = if_else(discourse_type %in% c("not_p", "p_alt", "disagreement", "pretend_play", "contrast_p", "uncertainty"), "contrast", discourse_type)) %>%
  mutate(discourse_type = if_else(grepl("answer",discourse_type,fixed=TRUE), "p", discourse_type))

adult_discourse_summary <- adult_discourse %>% 
  group_by(verb, sentence_type, discourse_type) %>% 
  summarize(count = sum(count)) %>% 
  ungroup()
adult_discourse_summary

adult_discourse_summary <- adult_discourse_summary %>% 
  mutate(verb = fct_relevel(verb,"yiwei", "juede"),
         discourse_type=case_when(discourse_type=="no" ~ "unsupported",
                                  discourse_type=="repeating" ~ "others",
                                  TRUE ~ discourse_type)) %>% 
  mutate(discourse_type = fct_relevel(discourse_type, "contrast",
                                      "p", "unsupported", "p?", "others"))
  

adult_matrix_clause_discourse_plot <- ggplot(adult_discourse_summary, 
       aes(fill=sentence_type, 
           y=count,
           x=discourse_type)) + 
  geom_bar(position=position_dodge2(preserve = "single"),
           stat="identity")+
  theme_bw()+
  facet_grid(.~verb)+
  # scale_fill_brewer(palette = "Set3",
  #                   name="Matrix clause type") +
  scale_fill_manual(values=sentencePalette,
                    name="Matrix clause type") +
  theme(legend.position = c(0.4, 0.70),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=10),
        legend.text = element_text(size=10),
        legend.title = element_text(size=10),
        axis.title.y = element_text(size = 14))+
  labs(x="Matrix discourse type",
       y="Counts")
adult_matrix_clause_discourse_plot
ggsave(adult_matrix_clause_discourse_plot, file="graphs/matrix_type-discourse_plot.pdf", width=7, height=3)

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
