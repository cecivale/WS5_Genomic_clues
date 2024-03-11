library(ape)
library(tidyverse)
library(stringr)

seqs <- read.FASTA("WS5_Genomic_clues_Moose/step2_sequencing/files/moose_sequences_v0.fasta")
seqs_labels <- tibble(original = names(seqs),
                      id_country = str_split(original, "\\|", simplify = TRUE)[,1],
                      id = str_split(original, "\\|", simplify = TRUE)[,2],
                      sp = str_split(original, "\\|", simplify = TRUE)[,3],
                      date = date(str_split(original, "\\|", simplify = TRUE)[,4])) %>%
  mutate(id = str_replace_all(case_when(id == "" ~ id_country,
                        TRUE ~ id), "_|-|Riyadh|Jeddah", ""))

dt <- ymd("2024-03-01") - max(seqs_labels$date)

ggplot(seqs_labels) +
  geom_histogram(aes(ymd(date))) +
  scale_x_date(date_breaks = "1 year")

seqs_labels <- seqs_labels %>% mutate(new_date = date + dt,
                                      new_sp = ifelse(sp == "camel", "moose", sp),
                                      new = paste(id, new_sp, new_date, sep = "|"))

ggplot(seqs_labels) +
  geom_histogram(aes(ymd(new_date), fill = sp)) +
  scale_x_date(date_breaks = "1 year")

names(seqs) <- seqs_labels$new

write.FASTA(seqs, "WS5_Genomic_clues_Moose/step2_sequencing/files/moose_sequences.fasta")
