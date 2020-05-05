library(rentrez)
library(ggplot2)
library(tibble)
set_entrez_key("c20bee62610f497c2766ffd7e8430abc0408")
setwd("C:/Users/tyden46/Desktop")
myTerm='"Severe acute respiratory syndrome coronavirus 2"[Organism] OR SARS-CoV-2[All Fields]"'
search_month <- function(month, term){
  query <- paste(term, " AND ", month)
  print(query)
  entrez_search(db="nuccore", term=query, retmax=0)$count
}

month <- c('(2020/01/01[PDAT] : 2020/01/31[PDAT])',
           '(2020/02/01[PDAT] : 2020/02/29[PDAT])',
           '(2020/03/01[PDAT] : 2020/03/31[PDAT])', 
           '(2020/04/01[PDAT] : 2020/04/30[PDAT])')
sequences <- sapply(month, search_month, term=myTerm, USE.NAMES=FALSE)
month=factor(c("January", "February", "March", "April"))
month_levels <- c(
  "January", "February", "March", "April"
)
myMonths <- factor(month, levels = month_levels)
df=tibble(month=myMonths, sequences=sequences)
ggplot(data=df, aes(x=month, y=sequences, group=1)) +
  geom_line()+
  geom_point()+
  xlab("Month") +
  ylab(paste("Number of Sequences in NCBI's", "Nucleotide Database", sep="\n")) +
  ggtitle(paste("Number of Available Sequences", "for SARS-Cov-2 by Month", sep="\n")) +
  theme(text = element_text(size = 30),
        legend.position = "right", plot.title = element_text(hjust = 0.5, size=30),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 30))
ggsave("SequencesAvailable.png", plot = last_plot(), height = 10, width=15, device = "png", dpi = 500, limitsize = FALSE)

