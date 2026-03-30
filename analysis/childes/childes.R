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
embedPalette <- c("#BEBADA", "#8DD3C7") 
embedfullPalette <- c("#E41A1C","#4DAF4A","#FFFF33")
# access childes 
# d_chi <- get_transcripts(collection = "Chinese")
# head(d_chi)

# 0. load the data ----
# 16, 99: repeated for correction; 2, 20, 23, 63, 79, 81: incomplete; 
# 131: question no CP; 
# 284, 292, 300: incomplete, speech error
yiwei <- read.csv("../../data/childes/yiwei.csv", header=TRUE) %>% 
  filter(!(id %in% c("2", "16", "20", "23", "63", "79", "81", "99", 
                     "284", "292", "300")))
# 38, 39: book reading
yiwei <- yiwei %>% 
  filter(!grepl("Chang2/BookReading", file_name))

# 24, 36, 104, 105, 466, 467, 781, 791, 794, 795, 800, 801, 805, 806, 807, 808, 818, 823, 824, 829, 840, 844, 845, 846, 849, 850, 853, 854, 855, 856, 858: subjective feeling, to feel (sleepy, comfortable, hungry, strange, cold, difficult, scared)
# 45, 50, 108, 779, 811: incomplete
# 88, 107, 793, 2627: question no CP; 
juede <- read.csv("../../data/childes/juede.csv", header=TRUE) %>%
  filter(!id %in% c("45","50", "108", "779", "811")) %>% 
  filter(!id %in% c("24", "36", "104","105", "466","467", "781","791", "794","795","800", "801", "805", "806","807","808","818","823","824","829","840","844","845","846","849","850","853","854","855","856","858")) 
  # filter(!id %in% c("88", "107", "793", "2627"))
# 1107, 1652, 1676, 1738, 1855, 1892, 1912, 1925, 2159, 2163, 2507: book reading
juede <- juede %>% 
  filter(!grepl("reading", file_name, ignore.case=TRUE))

zhidao <- read.csv("../../data/childes/zhidao.csv", header=TRUE) %>% 
  filter(!id %in% c("481", "482", "483") )
zhidao <- zhidao %>% 
  filter(!grepl("reading", file_name, ignore.case=TRUE))

annotation.data <- bind_rows(lst(yiwei, zhidao, juede), .id="verb") %>% 
  mutate(subject_type = if_else(subject %in% c("NP", "third_person"), "others", subject),
         embedded_subject_type = if_else(embedded_subject %in% c("NP", "third_person","demonstrative","demonstrative_NP"), "others", embedded_subject),
         sentence_type = if_else(sentence_type=="rhetorical", "polar", sentence_type),
         embedded_type = if_else(embedded_type=="rhetorical", "polar", embedded_type))

# 1. summary ----
## children's output ----
child_summary <- annotation.data %>% 
  filter(speaker == "child")

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
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()

child_embed_subject <- child_summary %>% 
  select(verb, embedded_subject_type) %>% 
  group_by(verb, embedded_subject_type) %>%
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()

### type of clause ----
child_matrix_clause <- child_summary %>% 
  select(verb, sentence_type) %>% 
  group_by(verb, sentence_type) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()

child_embed_clause <- child_summary %>% 
  select(verb, embedded_type) %>% 
  group_by(verb, embedded_type) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()

### adverbs ----
child_adverbs <- child_summary %>% 
  mutate(adverb_annotation = case_when(grepl("还|刚|原|本来", adverb) ~ "past", 
                                       adverb == "no" ~ "no",
                                       TRUE ~ "others")) %>% 
  group_by(verb, adverb_annotation) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()

### embedded tense, aspect, modal ----
child_embedded_tam <- child_summary %>% 
  mutate(embedded_modal_annotation = if_else(embedded_modal == "no", "no", "yes"),
         embedded_tense_annotation = if_else(embedded_aspect == "no", "no", "yes"),
         embedded_tam = if_else((embedded_modal_annotation=="yes" | embedded_tense_annotation == "yes"),"yes", "no")) %>% 
  group_by(verb, embedded_tam) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()

### matrix subject x clause type ----
child_matrix_clause_subject <- child_summary %>% 
  group_by(verb,subject_type, sentence_type) %>% 
  summarize(count=n()) %>% 
  ungroup()
child_matrix_clause_subject

### embed subject x embed clause type ----
child_embed_clause_subject <- child_summary %>% 
  group_by(verb, embedded_subject_type, embedded_type) %>% 
  summarize(count=n()) %>% 
  ungroup()
child_embed_clause_subject

### discourse status ----
# child_discourse <- child_summary %>% 
#   mutate(discourse_type = if_else(grepl("unclear", in_context, fixed=TRUE), "unclear", in_context)) %>% 
#   select(verb, discourse_type) %>% 
#   group_by(verb, discourse_type) %>%
#   summarize(count = n()) %>% 
#   ungroup()
# child_discourse
# 
# child_discourse_summary <- child_discourse %>% 
#   mutate(discourse_type = if_else(grepl("against", discourse_type), "inferable", discourse_type)) %>% 
#   mutate(p_not_p = if_else(!(discourse_type %in% c("p", "unclear", "no", "inferable")), "contrast", discourse_type)) %>% 
#   group_by(verb, p_not_p) %>% 
#   summarize(count = sum(count)) %>% 
#   ungroup()
# child_discourse_summary

child_discourse <- as.data.frame(
  table(child_summary$sentence_type,
        child_summary$in_context,
        child_summary$verb))
colnames(child_discourse) <- c("sentence_type","discourse_type","verb","count")
child_discourse <- child_discourse %>% 
  filter(verb!="zhidao") %>% 
  filter(count != 0) %>% 
  mutate(major_sentence_type = if_else(sentence_type %in% c("polar", "wh_question", "how-wh_question", "rhetorical", "tag_question"), "interrogative", sentence_type)) %>% # two levels for now
  mutate(discourse_type = if_else(grepl("against", discourse_type, fixed=TRUE), "inferred", discourse_type)) %>% 
  mutate(discourse_type = if_else(discourse_type %in% c("not_p", "p_alt", "disagreement", "pretend_play", "contrast_p", "uncertainty"), "contrast", discourse_type)) %>%
  mutate(discourse_type = if_else(grepl("answer", discourse_type,fixed=TRUE), "p", discourse_type))

child_discourse <- child_discourse %>% 
  mutate(discourse_type=case_when(discourse_type=="no" ~ "unsupported",
                                  discourse_type=="repeating" ~ "others",
                                  discourse_type=="not_p?" ~ "others",
                                  discourse_type=="unclear" ~ "others",
                                  TRUE ~ discourse_type))

child_discourse_summary <- child_discourse %>% 
  group_by(verb, discourse_type) %>% 
  summarize(count = sum(count)) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()
  
child_discourse_clause_summary <- child_discourse %>% 
  group_by(verb, major_sentence_type, discourse_type) %>% 
  summarize(count = sum(count)) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()

child_discourse_clause_summary <- child_discourse_clause_summary %>% 
  mutate(verb = fct_relevel(verb,"yiwei", "juede")) %>% 
  mutate(discourse_type = fct_relevel(discourse_type, "contrast", "inferred",
                                      "p", "unsupported", "p?", "others"))

## children's input ----
adult_summary <- annotation.data %>% 
  filter(speaker != "child")

### type of subject ----
adult_matrix_subject <- adult_summary %>% 
  select(verb, subject_type) %>% 
  group_by(verb, subject_type) %>%
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()

adult_embed_subject <- adult_summary %>%
  mutate(sum=n()) %>% 
  select(verb, embedded_subject_type) %>% 
  group_by(verb, embedded_subject_type) %>%
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()

### type of clause ----
adult_matrix_clause <- adult_summary %>% 
  select(verb, sentence_type) %>% 
  group_by(verb, sentence_type) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()

adult_embed_clause <- adult_summary %>% 
  select(verb, embedded_type) %>% 
  group_by(verb, embedded_type) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()

### adverbs ----
adult_adverbs <- adult_summary %>% 
  mutate(adverb_annotation = case_when(grepl("还|刚|原|本来", adverb) ~ "past", 
                                       adverb == "no" ~ "no",
                                       TRUE ~ "others")) %>% 
  group_by(verb, adverb_annotation) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()

adult_adverbs_past <- adult_summary %>% 
  mutate(adverb_annotation = case_when(grepl("还|刚|原|本来", adverb) ~ "past", 
                                       adverb == "no" ~ "no",
                                       TRUE ~ "others")) %>% 
  filter(adverb_annotation == "past") %>% 
  group_by(verb, subject_type) %>% 
  summarize(count = n()) %>% 
  ungroup()

### embedded tense, aspect, modal ----
adult_embedded_tam <- adult_summary %>% 
  mutate(embedded_modal_annotation = if_else(embedded_modal == "no", "no", "yes"),
         embedded_tense_annotation = if_else(embedded_aspect == "no", "no", "yes"),
         embedded_tam = if_else((embedded_modal_annotation=="yes" | embedded_tense_annotation == "yes"),"yes", "no")) %>% 
  group_by(verb, embedded_tam) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()

### discourse status ----
adult_discourse <- as.data.frame(
  table(adult_summary$sentence_type,
        adult_summary$in_context,
        adult_summary$verb))
colnames(adult_discourse) <- c("sentence_type","discourse_type","verb","count")
adult_discourse <- adult_discourse %>% 
  filter(verb!="zhidao") %>% 
  filter(count != 0) %>% 
  mutate(major_sentence_type = if_else(sentence_type %in% c("polar", "wh_question", "how-wh_question", "rhetorical", "tag_question"), "interrogative", sentence_type)) %>% # two levels for now
  mutate(discourse_type = if_else(grepl("against", discourse_type, fixed=TRUE), "inferred", discourse_type)) %>% 
  mutate(discourse_type = if_else(discourse_type=="direct_p?", "p?", discourse_type)) %>% 
  mutate(discourse_type = if_else(discourse_type %in% c("not_p", "p_alt", "disagreement", "pretend_play", "contrast_p", "uncertainty"), "contrast", discourse_type)) %>%
  mutate(discourse_type = if_else(grepl("answer",discourse_type,fixed=TRUE), "p", discourse_type))

adult_discourse <- adult_discourse %>% 
  mutate(discourse_type=case_when(discourse_type=="no" ~ "unsupported",
                                  discourse_type=="repeating" ~ "others",
                                  discourse_type=="not_p?" ~ "others",
                                  discourse_type=="unclear" ~ "others",
                                  TRUE ~ discourse_type)) 
adult_discourse_summary <- adult_discourse %>% 
  group_by(verb, discourse_type) %>% 
  summarize(count = sum(count)) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()

adult_discourse_clause_summary <- adult_discourse %>% 
  group_by(verb, major_sentence_type, discourse_type) %>% 
  summarize(count = sum(count)) %>% 
  ungroup() %>% 
  group_by(verb) %>% 
  mutate(percent = count/sum(count) * 100) %>% 
  ungroup()

adult_discourse_clause_summary <- adult_discourse_clause_summary %>% 
  mutate(verb = fct_relevel(verb,"yiwei", "juede")) %>% 
  mutate(discourse_type = fct_relevel(discourse_type, "contrast", "inferred",
                                      "p", "unsupported", "p?", "others"))

# 2. plot ----
### adult matrix subject x clause type ----
adult_matrix_clause_subject <- adult_summary %>% 
  mutate(major_sentence_type = if_else(sentence_type %in% c("polar", "how-wh_question","tag_question", "wh_question"), "interrogative", sentence_type)) %>% # two major levels
  group_by(verb, subject_type, sentence_type, major_sentence_type) %>% 
  summarize(count=n()) %>% 
  ungroup()

adult_matrix_clause_subject <- adult_matrix_clause_subject %>% 
  filter(verb!="zhidao") %>% 
  mutate(subject_type = fct_relevel(subject_type, "first_person", "second_person", "others","dropped"),
         verb = fct_relevel(verb, "yiwei", "juede"),
         sentence_type = fct_relevel(sentence_type, "declarative", "polar", "wh_question", "how-wh_question", "tag_question"))

adult_matrix_clause_subject_plot<-ggplot(adult_matrix_clause_subject, 
                                         aes(fill=sentence_type, 
                                       y=count,
                                       x=subject_type)) + 
  geom_bar(position=position_dodge2(preserve = "single"),
           stat="identity")+
  theme_bw()+
  facet_grid(.~verb)+
  scale_fill_brewer(palette = "Set3",
                    name="Matrix clause type",
                    labels = c("declarative"="Declarative",
                               "polar"="Polar",
                               "wh_question"="Wh-question",
                               "how-wh_question"="How/why",
                               "tag_question"="Tag question")) +
  # scale_fill_manual(values=sentencePalette,
  #                   name="Matrix clause type") +
  theme(legend.position = "top",
        # legend.position = c(0.85,0.7),
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
ggsave(adult_matrix_clause_subject_plot, file="graphs/matrix_type-subject_plot.pdf", width=7, height=4)

adult_subject_matrix_clause_plot<-ggplot(adult_matrix_clause_subject, 
                                         aes(fill=subject_type, 
                                             y=count,
                                             x=sentence_type)) + 
  geom_bar(position=position_dodge2(preserve = "single"),
           stat="identity")+
  theme_bw()+
  facet_grid(.~verb)+
  scale_fill_brewer(palette = "Set2",
                    name="Matrix subject type",
                    labels = c("first_person"="1st person", 
                               "second_person"="2nd person", 
                               "others"="others", 
                               "dropped"="dropped")) +
  theme(legend.position = "top",
        # legend.position = c(0.85,0.7),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        # axis.text.x = element_text(angle = 45,hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14))+
  scale_x_discrete(labels = c("declarative"="Declarative",
                              "polar"="Polar",
                              "wh_question"="Wh-\nquestion",
                              "how-wh_question"="How/why",
                              "tag_question"="Tag\nquestion")) +
  labs(x="Matrix clause type",
       y="Counts")
adult_subject_matrix_clause_plot
ggsave(adult_subject_matrix_clause_plot, file="graphs/subject-matrix_type_plot.pdf", width=7, height=4)

### child matrix subject x clause type ----
child_matrix_clause_subject <- child_summary %>% 
  mutate(major_sentence_type = if_else(sentence_type %in% c("polar", "how-wh_question","tag_question", "wh_question"), "interrogative", sentence_type)) %>% # two major levels
  group_by(verb, subject_type, sentence_type, major_sentence_type) %>% 
  summarize(count=n()) %>% 
  ungroup()

child_matrix_clause_subject <- child_matrix_clause_subject %>% 
  filter(verb!="zhidao") %>% 
  mutate(subject_type = fct_relevel(subject_type, "first_person", "second_person", "others","dropped"),
         verb = fct_relevel(verb, "yiwei", "juede"),
         sentence_type = fct_relevel(sentence_type, "declarative", "polar", "wh_question"))

child_subject_matrix_clause_plot<-ggplot(child_matrix_clause_subject, 
                                         aes(fill=subject_type, 
                                             y=count,
                                             x=sentence_type)) + 
  geom_bar(position=position_dodge2(preserve = "single"),
           stat="identity")+
  theme_bw()+
  facet_grid(.~verb)+
  scale_fill_brewer(palette = "Set2",
                    name="Matrix subject type",
                    labels = c("first_person"="1st person", 
                               "second_person"="2nd person", 
                               "others"="others", 
                               "dropped"="dropped")) +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14))+
  scale_x_discrete(labels = c("declarative"="Declarative",
                              "polar"="Polar",
                              "wh_question"="Wh-question")) +
  labs(x="Matrix clause type",
       y="Counts")
child_subject_matrix_clause_plot
ggsave(child_subject_matrix_clause_plot, file="graphs/child_subject-matrix_type_plot.pdf", width=7, height=4)

### adult embedded subject x clause type ----
adult_embed_clause_subject <- adult_summary %>% 
  mutate(major_embedded_type = if_else(embedded_type %in% c("polar", "how-wh_question","tag_question", "wh_question"), "interrogative", embedded_type)) %>% # two major levels
  group_by(verb, embedded_subject_type, embedded_type, major_embedded_type) %>% 
  summarize(count=n()) %>% 
  ungroup()
adult_embed_clause_subject

adult_embed_clause_subject <- adult_embed_clause_subject %>% 
  filter(verb!="zhidao") %>% 
  mutate(embedded_subject_type = fct_relevel(embedded_subject_type, "first_person", "second_person", "others","dropped","none"),
         verb = fct_relevel(verb, "yiwei", "juede"),
         embedded_type = fct_relevel(embedded_type, "declarative", "none"))

adult_embed_clause_subject_plot<-ggplot(adult_embed_clause_subject, 
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
        axis.text.x = element_text(size = 12),
        # axis.text.x = element_text(angle = 45,hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14))+
  scale_x_discrete(labels = c("first_person"="1st\nperson",
                              "second_person"="2nd\nperson",
                              "others"="others",
                              "dropped"="dropped",
                              "none"="none")) +
  labs(x="Embedded subject type",
       y="Counts")
adult_embed_clause_subject_plot
ggsave(adult_embed_clause_subject_plot, file="graphs/embed_type-subject_plot.pdf", width=7, height=4)

adult_subject_embed_clause_plot<-ggplot(adult_embed_clause_subject, 
                                        aes(fill=embedded_subject_type, 
                                            y=count,
                                            x=embedded_type)) + 
  geom_bar(position=position_dodge2(preserve = "single"),
           stat="identity")+
  theme_bw()+
  facet_grid(.~verb)+
  scale_fill_brewer(palette = "Set2",
                    name="Embedded subject type",
                    labels=c("first_person"="1st person",
                             "second_person"="2nd person",
                             "others"="others",
                             "dropped"="dropped",
                             "none"="none")) +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14)) +
  labs(x="Embedded clause type",
       y="Counts")+
  scale_x_discrete(labels = c("declarative"="Declarative",
                              "none"="None"))
adult_subject_embed_clause_plot
ggsave(adult_subject_embed_clause_plot, file="graphs/subject-embed_type_plot.pdf", width=7, height=4)

### child embedded subject x clause type ----
child_embed_clause_subject <- adult_summary %>% 
  mutate(major_embedded_type = if_else(embedded_type %in% c("polar", "how-wh_question","tag_question", "wh_question"), "interrogative", embedded_type)) %>% # two major levels
  group_by(verb, embedded_subject_type, embedded_type, major_embedded_type) %>% 
  summarize(count=n()) %>% 
  ungroup()
child_embed_clause_subject

child_embed_clause_subject <- child_embed_clause_subject %>% 
  filter(verb!="zhidao") %>% 
  mutate(embedded_subject_type = fct_relevel(embedded_subject_type, "first_person", "second_person", "others","dropped","none"),
         verb = fct_relevel(verb, "yiwei", "juede"),
         embedded_type = fct_relevel(embedded_type, "declarative", "none"))

child_subject_embed_clause_plot<-ggplot(child_embed_clause_subject, 
                                        aes(fill=embedded_subject_type, 
                                            y=count,
                                            x=embedded_type)) + 
  geom_bar(position=position_dodge2(preserve = "single"),
           stat="identity")+
  theme_bw()+
  facet_grid(.~verb)+
  scale_fill_brewer(palette = "Set2",
                    name="Embedded subject type",
                    labels=c("first_person"="1st person",
                             "second_person"="2nd person",
                             "others"="others",
                             "dropped"="dropped",
                             "none"="none")) +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14)) +
  labs(x="Embedded clause type",
       y="Counts")+
  scale_x_discrete(labels = c("declarative"="Declarative",
                              "none"="None"))
child_subject_embed_clause_plot
ggsave(child_subject_embed_clause_plot, file="graphs/child_subject-embed_type_plot.pdf", width=7, height=4)

### adult matrix x embedded subject type ----
adult_subjects <- adult_summary %>% 
  mutate(major_sentence_type = if_else(sentence_type %in% c("polar", "how-wh_question","tag_question", "wh_question"), "interrogative", sentence_type)) %>% # two major levels
  group_by(verb, subject_type, embedded_subject_type) %>% 
  summarize(count=n()) %>% 
  ungroup()
adult_subjects

adult_subjects <- adult_subjects %>% 
  filter(verb!="zhidao") %>% 
  mutate(subject_type=fct_relevel(subject_type, "first_person", "second_person", "others","dropped"),
           embedded_subject_type = fct_relevel(embedded_subject_type, "first_person", "second_person", "others","dropped","none"),
         verb = fct_relevel(verb, "yiwei", "juede"))

adult_subjects_plot<-ggplot(adult_subjects,
                            aes(fill=subject_type,
                                y=count,
                                x=embedded_subject_type)) + 
  geom_bar(position=position_dodge2(preserve = "single"),
           stat="identity")+
  theme_bw()+
  facet_grid(.~verb)+
  scale_fill_brewer(palette = "Set2",
                    name="Matrix subject type",
                    labels=c("first_person"="1st person",
                             "second_person"="2nd person",
                             "others"="others",
                             "dropped"="dropped")) +
  theme(legend.position = "top",
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        axis.title.y = element_text(size = 14)) +
  labs(x="Embedded subject type",
       y="Counts")+
  scale_x_discrete(labels = c("first_person"="1st person",
                              "second_person"="2nd person",
                              "others"="others",
                              "dropped"="dropped",
                              "none"="none"))
adult_subjects_plot

### adult matrix and embedded subjects vs. matrix clause type ----
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

### adult discourse status ----
adult_matrix_clause_discourse_plot <- ggplot(adult_discourse_clause_summary, 
       aes(fill=major_sentence_type, 
           y=count,
           x=discourse_type)) + 
  geom_bar(position=position_dodge2(preserve = "single"),
           stat="identity")+
  theme_bw()+
  facet_grid(.~verb)+
  scale_fill_manual(values=sentencePalette,
                    name="Matrix clause type") +
  theme(legend.position = c(0.35, 0.75),
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
ggsave(adult_matrix_clause_discourse_plot, file="graphs/matrix_type-discourse_plot.pdf", width=7, height=4)

### child discourse status ----
child_matrix_clause_discourse_plot <- ggplot(child_discourse_clause_summary, 
                                             aes(fill=major_sentence_type, 
                                                 y=count,
                                                 x=discourse_type)) + 
  geom_bar(position=position_dodge2(preserve = "single"),
           stat="identity")+
  theme_bw()+
  facet_grid(.~verb)+
  scale_fill_manual(values=sentencePalette,
                    name="Matrix clause type") +
  theme(legend.position = c(0.35, 0.75),
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
child_matrix_clause_discourse_plot
ggsave(child_matrix_clause_discourse_plot, file="graphs/child_matrix_type-discourse_plot.pdf", width=7, height=4)

## child and adult matrix subject type ----
matrix_subject <- merge(child_matrix_subject, adult_matrix_subject, 
                 by=c("subject_type", "verb"),
                 all=TRUE) %>% 
  replace(., is.na(.), 0) %>%
  select(-c("percent.x", "percent.y")) %>% 
  rename(output_subject = "count.x", # child
         input_subject = "count.y") %>% 
  pivot_longer(cols=c("output_subject", "input_subject"), 
               names_to = "input_output",
               values_to = "count")

matrix_subject <- matrix_subject %>% 
  filter(verb!="zhidao") %>% 
  mutate(subject_type = fct_relevel(subject_type, "first_person", "second_person", "others","dropped","unclear"),
         verb = fct_relevel(verb, "yiwei", "juede"))

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
                    labels=c("first_person"="1st person",
                             "second_person"="2nd person",
                             "dropped"="dropped", 
                             "other"="other",
                             "unclear"="unclear")) +
  scale_x_discrete(labels=c("input_subject" = "Child-ambient\nspeech",
                            "output_subject" = "Child's\nproduction"))+
  facet_grid(.~verb) +
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

## child and adult embedded subject type ----
embed_subject <- merge(child_embed_subject, adult_embed_subject, 
                        by=c("embedded_subject_type", "verb"),
                        all=TRUE) %>% 
  replace(., is.na(.), 0) %>% 
  select(-c("percent.x", "percent.y")) %>% 
  rename(output_subject = "count.x", # child
         input_subject = "count.y") %>%  # adult
  pivot_longer(cols=c("output_subject", "input_subject"), 
               names_to = "input_output",
               values_to = "count")

embed_subject <- embed_subject %>% 
  filter(verb!="zhidao") %>% 
  mutate(embedded_subject_type = fct_relevel(embedded_subject_type, "first_person", "second_person", "others","dropped"),
         verb = fct_relevel(verb, "yiwei", "juede"))

embed_subject_plot<-ggplot(embed_subject, 
                            aes(fill=embedded_subject_type, 
                                y=count,
                                x=input_output)) + 
  geom_bar(position=position_fill(),
           stat="identity")+
  geom_label(aes(label = count,
                 group=factor(embedded_subject_type)), 
            position = position_fill(vjust = 0.5),
            show.legend = FALSE)+
  theme_bw()+
  scale_fill_brewer(palette = "Set2",
                    name="Subject Type",
                    labels = c("first_person"="1st person",
                                "second_person"="2nd person",
                                "dropped"="dropped", 
                                "other"="other")) +
  scale_x_discrete(labels=c("input_subject" = "Child-ambient\nspeech",
                            "output_subject" = "Child's\nproduction"))+
  facet_grid(.~verb) +
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

## child and adult matrix clause type ----
matrix_clause <- merge(child_matrix_clause, adult_matrix_clause, 
                        by=c("sentence_type", "verb"),
                        all=TRUE) %>% 
  replace(., is.na(.), 0) %>%
  select(-c("percent.x", "percent.y")) %>% 
  rename(output_clause = "count.x", # child
         input_clause = "count.y") %>%  # adult
  pivot_longer(cols=c("output_clause", "input_clause"), 
               names_to = "input_output",
               values_to = "count")

matrix_clause <- matrix_clause %>% 
  filter(verb!="zhidao") %>% 
  mutate(sentence_type = fct_relevel(sentence_type, "declarative", "polar", "wh_question", "how-wh_question", "tag_question"),
         verb = fct_relevel(verb, "yiwei", "juede"))

matrix_clause_plot<-ggplot(matrix_clause, 
                            aes(y=count,
                                x=input_output,
                                fill=sentence_type,
                                pattern=sentence_type)) + 
  geom_col_pattern(aes(pattern_density=sentence_type),
                   position="fill",
                   color="black",
                   alpha=0.3,
                   pattern="stripe",
                   show.legend = FALSE)+
  geom_label(aes(label = count,
                 group=factor(sentence_type)),
             position = position_fill(vjust = 0.5),
             alpha=0.7,
             size=3)+
  theme_bw()+
  scale_pattern_manual(values=c(polar = 'stripe',
                                wh_question = 'wave',
                                tag_question = "stripe",
                                "how-wh_question" = "weave",
                                demonstrative = 'none'),
                       name = "Clause Type",
                       guide="none") +
  scale_fill_brewer(palette = "Set1",
                    name="Clause Type",
                    labels=c("declarative"="Declarative",
                             "polar"="Polar",
                             "wh_question"="Wh-question",
                             "how-wh_question"="How/why",
                             "tag_question"="Tag question"))+
  scale_x_discrete(labels=c("input_clause" = "Child-ambient\nspeech",
                            "output_clause" = "Children's\nproduction"))+
  facet_grid(.~verb) +
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
matrix_clause_plot
ggsave(matrix_clause_plot, file="graphs/matrix_clause_plot.pdf", width=7, height=4)

### child and adult embedded clause type ----
embed_clause <- merge(child_embed_clause, adult_embed_clause, 
                       by=c("embedded_type","verb"),
                       all=TRUE) %>% 
  replace(., is.na(.), 0) %>%
  select(-c("percent.x", "percent.y")) %>% 
  rename(output_clause = "count.x", # child
         input_clause = "count.y") %>%  # adult
  pivot_longer(cols=c("output_clause", "input_clause"), 
               names_to = "input_output",
               values_to = "count")

embed_clause <- embed_clause %>% 
  filter(verb!="zhidao") %>% 
  mutate(embedded_type = fct_relevel(embedded_type, "declarative", "wh_question", "none"),
         verb = fct_relevel(verb, "yiwei", "juede"))

embed_clause_plot<-ggplot(embed_clause, 
                           aes(pattern=embedded_type, 
                               y=count,
                               x=input_output,
                               fill=embedded_type)) + 
  geom_col_pattern(aes(pattern_density=embedded_type),
                   position="fill",
                   color="black",
                   alpha=0.3,
                   pattern="stripe",
                   show.legend = FALSE)+
  geom_label(aes(label = count,
                 group=factor(embedded_type)), 
             position = position_fill(vjust = 0.5),
             size=3)+
  theme_bw()+
  scale_fill_manual(values=embedfullPalette,
                    name="Clause Type",
                    labels=c("declarative"="Declarative",
                             "wh_question"="Wh-question",
                             "none"="None"))+
  scale_pattern_manual(values=c(declarative = 'stripe',
                                interrogative = 'none',
                                unclear = 'weave'),
                       name="Clause Type") +
  scale_x_discrete(labels=c("input_clause" = "Child-ambient\nspeech",
                            "output_clause" = "Children's\nproduction"))+
  facet_grid(.~verb)+
  theme(legend.position = "top",
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(size = 12),
        strip.text.x = element_text(size=10),
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
