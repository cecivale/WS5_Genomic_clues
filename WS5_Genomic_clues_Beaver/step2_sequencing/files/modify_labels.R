library(ape)
library(tidyverse)
libarry(stringr)

seqs <- read.FASTA("/Users/ceciliav/Documents/conferences/SVEPM/2024 Uppsala/WS5_Genomic_clues/WS5_Genomic_clues_Beaver/step2_sequencing/files/beaver_sequences_v0.fasta")
seqs_labels <- tibble(original = names(seqs),
                      id = str_split(original, "\\|", simplify = TRUE)[,1],
                      sp = str_split(original, "\\|", simplify = TRUE)[,2],
                      date = date(str_split(original, "\\|", simplify = TRUE)[,3]))

dt <- ymd("2024-03-01") - max(seqs_labels$date)

ggplot(seqs_labels) +
  geom_histogram(aes(ymd(date))) +
  scale_x_date(date_breaks = "1 year")

seqs_labels <- seqs_labels %>% mutate(new_date = date + dt,
                                      new_id = str_sub(id, 3, -1),
                                      new = paste(new_id, sp, new_date, sep = "|"))

ggplot(seqs_labels) +
  geom_histogram(aes(ymd(new_date))) +
  scale_x_date(date_breaks = "1 year")

names(seqs) <- seqs_labels$new

write.FASTA(seqs, "/Users/ceciliav/Documents/conferences/SVEPM/2024 Uppsala/WS5_Genomic_clues/WS5_Genomic_clues_Beaver/step2_sequencing/files/beaver_sequences.fasta")
