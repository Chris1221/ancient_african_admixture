library(smcsmcTools)
library(dplyr)

f3 <- read.table("~/repos/dirmig/data/dstats/a_couple_f3.txt", h = T, stringsAsFactors = F)
f3$B[f3$B == "Vindija.DG"] = "S_Vindija-1.DG"
f3$Source[f3$Source == "All"] = "All Variants"
f3$B <- ids_to_names(f3$B)
ggplot(f3, aes(fct_reorder(B, f3), f3, color = abs(Zscore) > 2)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_errorbar(aes(ymin = f3 - 2 * stderr, ymax = f3 + 2 * stderr)) +
  facet_wrap(~Source, nrow = 1) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") + 
  ylab("f3(Yoruba-1, Eurasian, Yoruba-2)") + 
  xlab("")


ggsave("~/repos/dirmig/plot/f3.pdf", height = 4, width = 8)

f3 <- read.table("~/repos/dirmig/data/dstats/a_couple_more_f3.txt", h = T, stringsAsFactors = F) %>%
  filter(B != "Vindija.DG")
f3$Source[f3$Source == "All"] = "All Variants"
f3$B <- ids_to_names(f3$B)
ggplot(f3, aes(fct_reorder(B, f3), f3, color = abs(Zscore) > 2)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_errorbar(aes(ymin = f3 - 2 * stderr, ymax = f3 + 2 * stderr)) +
  facet_wrap(~Source, nrow = 1) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") + 
  ylab("f3(Yoruba-2, Eurasian, Yoruba-1)") + 
  xlab("")


ggsave("~/repos/dirmig/plot/f3_admix.pdf", height = 4, width = 8)
