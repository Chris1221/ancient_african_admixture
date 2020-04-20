library(xtable)

k <- read.table("~/repos/dirmig/data/kay/kays_stats.txt", h = T)

a = c( rep(1, 10),
       rep(2, 12),
       rep(3, 3),
       rep(4, 1),
       rep(5, 2),
       rep(6, 4))

captions = c("Putatively migrated segments of a Yoruban are closer to Out of Africa groups than a comparable Yoruban.",
             "Both Yorubans share more alleles with OoA populations than San or Mbuti. The individual used to ascertain segments shares more alleles than a comparable individual.",
             "The Yoruban used to ascertain segment is more closely related to OoA groups than a comparable Yoruban.",
             "No difference in allele sharing with Vindija Neanderthal.",
             "No difference in allele sharing with Vindija Neanderthal over Mbuti baseline.",
             "No increased affinity to Vindija Neanderthal over Altai, as would be expected if the source of any Neanderthal ancestry was Eurasian.")


k$a <- a

l = list()

for(ana in c(1:6)){
  ka <- k %>% 
    filter(a == ana) %>% 
    mutate(Dstat = D) %>%
    mutate(D = paste0("D(", W, ", ", X, ", ", Y, ", ", Z, ")")) %>%
    mutate(D = gsub(D, pattern = ".DG", replacement = "")) %>%
    mutate(D = gsub(D, pattern= "S_", replacement = "")) %>%
    mutate(D = gsub(D, pattern = ".REF", replacement = "")) %>%
    mutate(D = gsub(D, pattern = "_published", replacement = "")) %>%
    mutate(D = gsub(D, pattern = "_", replacement = "")) %>%
    select(D, Dstat, Zscore)
  
  colnames(ka) <- c("Statistic", "D", "Z")
  l[[ana]] <- ka
  print(xtable(ka, digits = 3, caption = captions[ana], label = paste0("dstats:a", ana)), include.rownames = FALSE, file = "~/repos/dirmig/table/kay_tables.tex", append = T)
}


all <- do.call(rbind, l)
summary_table <- all[c(9,10, 26:32),]

annotation = c("In the subset of variants in putatievly migrated segments, there is excessive allele sharing between the test Yoruban and Out of Africa groups (French and Han).",
               "",
               "The test Yoruban is no closer to the Vindija Neanderthal than a comparable Yoruban.",
               "",
               "Neither Yoruban shows increased Neanderthal ancestry over the Mbuti baseline.",
               "Neither Yoruban shows more affinity to the Vindija Neanderthal than the Altai, as would be expected if introgression came from a Eurasian vehicle.",
               "", 
               "", 
               "")

summary_table$Interpretation <- annotation

label <- "table:dstats_summary"
legend <- "Summary of relevant D statistics. Putatively migrated segments were isolated in Yoruba-1 (the test Yoruban) and compared against variation in Yoruba-2 (comparable Yoruban). French, Yoruban, Han, and Mbuti individuals are from the Simons Genome Diversity Panel (sample ID {\tt S_Sample-n.DG} in the Reich Human Origins Dataset), while Vindija and Altai are the published versions in the same dataset. Chimp ({\tt Chimp.REF}) was used as an outgroup for all analyses."
print(xtable(summary_table, digits = 3, caption = legend, label = label), include.rownames = FALSE, file = "~/repos/dirmig/table/kay_summary_table.tex", append = T)

