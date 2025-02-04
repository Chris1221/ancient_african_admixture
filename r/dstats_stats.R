library(smcsmcTools)
library(data.table)
library(dplyr)
library(countrycode)
library(ggplot2)
library(heatmaply)
library(ggpubr)
library(xtable)

anno <- fread("../lib/v37.2.1240K_HumanOrigins.clean4.anno")

exclude <- c("Ancestor.REF", "Href.REF", "Chimp.REF", "Gorilla.REF", "Href", "Href_HO", "Gorilla", "Orang", "Chimp_HO", "Macaque", "Marmoset")
clean_anno <- anno %>% 
  select(`Instance ID`, `Master ID`, `Group Label`, `Average of 95.4% date range in calBP (defined as 1950 CE)`, Country, Lat., Long.) %>%
  dplyr::rename(age = `Average of 95.4% date range in calBP (defined as 1950 CE)`, iid = `Instance ID`, mid = `Master ID`, group = `Group Label`, lat = Lat., long = Long., country = Country) %>%
  filter(!(iid %in% exclude)) %>% 
  mutate(age = as.numeric(age)) %>%
  mutate(continent = countrycode(sourcevar = country, origin = "country.name", destination = "continent"))

# Cleaning up the continents by dealing with the edge cases

clean_anno$continent[clean_anno$country == "Abkhazia"] <- "Europe"
clean_anno$continent[clean_anno$country == "Czechoslovakia"] <- "Europe"
clean_anno$continent[clean_anno$country == "Micronesia"] <- "Oceania"
clean_anno$continent[clean_anno$country == "BotswanaOrNamibia"] <- "Africa"
clean_anno$continent[clean_anno$country == "Western Sahara (Morocco)"] <- "Africa"
clean_anno$continent[clean_anno$country == "Orkney Islands"] <- "Europe"
clean_anno$continent[clean_anno$country == "Canary Islands"] <- "Africa"


clean_anno$continent[clean_anno$group == "LateDorset.SG"] <- "Americas"
clean_anno$continent[clean_anno$group == "MiddleDorset.SG"] <- "Americas"
clean_anno$continent[clean_anno$group == "Thule.SG"] <- "Americas"

# Add an age category

clean_anno$period <- ""
clean_anno$period[clean_anno$age <= 1000] <- "Modern"
clean_anno$period[clean_anno$age > 1000 & clean_anno$age <= 11700] <- "Holocene"
clean_anno$period[clean_anno$age > 11700] <- "Pleistocene"

continent_lookup <- clean_anno %>% select(group, continent, period) %>% unique

all = fread("~/repos/dirmig/data/dstats/d_all.txt") 
colnames(all) <- c("W", "X", "Y", "Z", "D", "STERR", "z", "na", "nb", "nt")
all$type = "All"

segs = fread("~/repos/dirmig/data/dstats/d_segs.txt") 
colnames(segs) <- c("W", "X", "Y", "Z", "D", "STERR", "z", "na", "nb", "nt")
segs$type = "Segs"

ggplot( rbind(segs, all), aes(x = D, fill= type, group = type))  + 
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.6, binwidth = 0.001) + 
  theme_bw() + 
  xlim(c(-0.15, 0.15))
theme(legend.position = "bottom", legend.title = element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
panel.background = element_blank(), axis.line = element_line(colour = "black")) + ylab("Density") + xlab("Z Score")
ggsave("~/repos/dirmig/plot/dstats_hist.pdf")

joined <- rbind(segs, all) 

joined$archaic = "Human"
joined$archaic[grepl("Nean", joined$Y)] = "Neanderthal"
joined$archaic[grepl("Deni", joined$Y)] = "Denisovan"

joined <- merge(joined, continent_lookup, by.x = "Y", by.y = "group")

ggplot(joined %>% filter(archaic != "Human"), aes(x = W, y = D, col = type)) + 
  geom_boxplot() +
  facet_wrap(~archaic, nrow = 2) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text.x = element_text(size=4, angle = 45, hjust = 1)) + 
  theme(legend.position = "bottom", legend.title = element_blank()) +
  xlab("")


ggsave("~/repos/dirmig/plot/dstats_archaic.pdf")



ggplot(joined, aes(sample = z, shape = period)) + stat_qq() + stat_qq_line() + facet_wrap(~type, nrow = 2, strip.position = "top") +   theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(axis.text.x = element_text(size=4, angle = 45, hjust = 1)) + 
  theme(legend.position = "bottom", legend.title = element_blank()) + 
  ylab("Observed Z Score") + 
  xlab("Expected Z Score")

ggsave("~/repos/dirmig/plot/dstats_qqplot.pdf")


# Find the average per population
# this takes a while
joined$lang <- ""
for(lang in list_language_families()){
  l = list_samples_from_lang(lang)
  for(i in 1:nrow(joined)){
    if(joined$sample[i] %in% l) joined$lang[i] <- lang
    
    if(i %% 1000 == 0) print(i)
  }
}

summary = joined %>% group_by(lang, period, continent, Y, type) %>% summarise(mean = mean(D), sd = sd(D))


##### Top 10 table
merged <- merge(all, segs, by = c("W", "X", "Y", "Z"), suffixes = c(".all", ".segs"))

top20 <- merged %>% 
  filter(!grepl(Y, pattern = "Chimp")) %>%
  filter(na.segs + nb.segs > 1000) %>%
  #mutate(W = paste0("D(", W, ", ", X, ", ", Y, ", ", Z, ")")) %>%
  select(W, Y, D.segs, STERR.segs, z.segs, D.all, STERR.all, z.all) %>%
  arrange(-D.segs) %>%
  filter(!grepl(Y, pattern= "LowCov")) %>%
  head(20) 

top20$W <- lapply(top20$W, ids_to_names)
colnames(top20) <- c("African", "Eurasian", "D", "Std. Err.", "Z", "D", "Std. Err", "Z")
label = "dstats:top20"
legend = "Top 20 D Statistics for D(Test African, Partner African; Eurasian, Chimp) calculated for variants in putatively migrated segments. Statistics with less than a thousand combined variants were excluded. Segments were isolated in the first individual (ID-1) and compared to the second individual (ID-2). Segments were isolated using a Han Chinese individual to estimate migration."
print(xtable(top20, digits = 3, caption = legend, label = label), file = "~/repos/dirmig/table/top20.tex", include.rownames = FALSE,  append = T)


### Modern populations
top20m <- merged %>% 
  filter(!grepl(Y, pattern = "Chimp")) %>%
  filter(na.segs + nb.segs > 10000) %>%
  #mutate(W = paste0("D(", W, ", ", X, ", ", Y, ", ", Z, ")")) %>%
  select(W, Y, D.segs, STERR.segs, z.segs, D.all, STERR.all, z.all) %>%
  arrange(-D.segs) %>%
  filter(!grepl(Y, pattern= "LowCov")) %>%
  head(20) 

top20m$W <- lapply(top20m$W, ids_to_names)
colnames(top20m) <- c("African", "Eurasian", "D", "Std. Err.", "Z", "D", "Std. Err", "Z")
label = "dstats:top20m"
legend = "Top 20 D Statistics for D(Test African, Partner African; Eurasian, Chimp) calculated for variants in putatively migrated segments with more than a combined 10,000 variants. Segments were isolated in a test individual (Sample-1) and compared to a partner individual (Sample-2) from the Simons Genome Diversity Panel. Segments were isolated using a Han Chinese individual to estimate migration, and the populations isolated are representative of Han affinity."
print(xtable(top20m, digits = 3, caption = legend, label = label), include.rownames = FALSE,  append = T)
