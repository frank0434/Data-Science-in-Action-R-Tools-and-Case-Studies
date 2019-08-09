

library(tidyverse)
library(ggalluvial)

main_theme = theme(panel.background=element_blank(),
                   panel.grid=element_blank(),
                   axis.line.x=element_line(size=.5, colour="black"),
                   axis.line.y=element_line(size=.5, colour="black"),
                   axis.ticks=element_line(color="black"),
                   axis.text=element_text(color="black", size=7),
                   legend.position="right",
                   legend.background=element_blank(),
                   legend.key=element_blank(),
                   legend.text= element_text(size=7),
                   text=element_text(family="sans", size=7))
otu <- read.delim('./TipsTricks/T2genus_top20_alluvium.txt', sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
Taxonomy_levels <- c('g__Lactobacillus', 'g__unidentified', 'g__Bacill', 'g__Kroppenstedti', 'g__Lentibacillus', 
                     'g__Oceanobacillus', 'g__Staphylococcus','g__Anaerosalibacter', 'g__Pseudogracilibacil', 
                     'g__Anaerobacillus', 'g__Pseudomonas', "g__Thermoactinomyces", "g__Saccharopolyspora", 
                     "g__Acetobacter", "g__Clostridium_sensu_stricto_18", "g__Massilia", "g__Brevibacterium", 
                     "g__Nesterenkonia", "g__Streptococcus", "g__Acinetobacter", "Others")
variable_levels <- c("D2", "D4", "D6","D8", 'D9', 'D14', 'D19', 'D24', 'D29', 'D35')
otu <- otu %>% 
  mutate(Taxonomy = as.factor(Taxonomy),
         Taxonomy = fct_relevel(Taxonomy, Taxonomy_levels),
         variable = as.factor(variable),
         variable = fct_relevel(variable, variable_levels))

p = ggplot(otu, aes(x = variable , y = value*100, alluvium = Taxonomy)) +
  geom_alluvium(aes(fill = Taxonomy), alpha = .75) +
  main_theme + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Genus changes during fermentation in T2")
p <- p +
  scale_fill_manual(values =  rev(c('blue', 'orange', 'green', 'yellow', 'red', 'hotpink', 'cyan','purple', 'burlywood1', 'skyblue', 'gray', "#E64B35B2", "#4DBBD5B2", "#00A087B2", "#3C5488B2", "#F39B7FB2", "#8491B4B2", "#91D1C2B2", "#DC0000B2", "#7E6148B2", "#B09C85B2")))
p
ggsave("tax_alluvium_genus18.pdf", p, width = 8, height = 5)