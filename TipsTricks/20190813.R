mt<-function(otu,env){
     library(vegan)
     library(dplyr)
     vars <- colnames(env)
     models<-list()
     for (i in seq_along(vars)){
         otu_bray<-vegdist(otu,method = "bray")
         env_dis<-vegdist(env[vars[i]],method = "euclidean")
         model <- mantel(otu_bray,env_dis, permutations=999)
         name <- vars[i]
         statistic <- model$statistic
         signif <- model$signif
         models[[i]] <- data.frame(name = name, statistic = statistic, signif = signif, row.names = NULL)
       }
     models %>%  bind_rows()
}
otu<-read.delim('./TipsTricks/otu.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, na.strings = c("NA"), check.names = FALSE)
env<-read.delim('./TipsTricks/env.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, na.strings = c("NA"), check.names = FALSE)
mantRpTotal<-mt(otu,env)
