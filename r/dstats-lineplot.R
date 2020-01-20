library(smcsmcTools)
library(data.table)
library(dplyr)
library(countrycode)
library(ggplot2)

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

s <- rbind(segs, all) %>%
  merge(continent_lookup, by.x = "Y", by.y = "group", all.x = T, all.y = F) %>%
  filter(!(is.na(continent))) %>% 
  group_by(type, W, continent, period) %>% 
  dplyr::summarise (means = mean(D), sds = sd(D)) %>% 
  arrange(-means)

s$period <- factor(s$period, levels = c("Modern", "Holocene", "Pleistocene"))

s$W <- gsub(s$W, pattern = "S_", replacement = "")
s$W <- gsub(s$W, pattern = ".DG", replacement = "")
s$W <- factor(s$W, levels = unique(s$W))
ggplot(s, aes(x = W, y = means, color = type, shape = as.factor(continent))) + 
  geom_point() +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  geom_errorbar(aes(ymin = means - sds, ymax= means + sds), alpha = 0.25) +
  #scale_color_manual(guide = F) +
  ylab(expression("D ± SD")) + 
  xlab("") +
  geom_jitter() + 
  facet_wrap(~period) +
  theme(legend.position = "top",legend.title = element_blank())

ggsave("~/repos/dirmig/plot/dstats-lineplot.eps", dpi = 300, width = 15.8, height = 7.11, unit = "in") 

