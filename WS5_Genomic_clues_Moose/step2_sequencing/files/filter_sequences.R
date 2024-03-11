# Script to compute number of missing nucleotides in each sequence of an alignment
# plot an histogram, and keep sequences with at least 90% of nucleotides

library(tidyverse) 
library(ape) # good package to work with sequences and trees in R

# Read alignment
aln <- read.FASTA("moose_aligned.fasta")


# Get number of missing nucleotides per sequences
t_l <- lapply(1:length(aln), function(i){
  seq <- as.character.DNAbin(aln[[i]])
  t <- tibble(label = names(aln[i]),
              nt = sum(seq != "-"), 
              n = sum(seq == "-"), 
              p = nt/length(seq))
})
t <- bind_rows(t_l)

# Plot histogram of missing nucleotides
ggplot(t) +
  geom_histogram(aes(n)) +
  geom_vline(aes(xintercept = 0.1 * length(aln[[1]])), linetype = 2) +
  theme_minimal() +
  ylab("Number of sequences") +
  xlab("Number of missing nucleotides in the sequence")

# Filter only sequences with more than 90% of the nucleotides
t_filt <- t %>% filter(p >= 0.9)

# Select those sequences in the alignment
aln_filt <- aln[t_filt$label]

# Save new filtered alignment
write.FASTA(aln_filt, "moose_aligned_filtered.fasta")



