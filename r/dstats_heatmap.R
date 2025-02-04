library(smcsmcTools)
library(data.table)
library(dplyr)
library(countrycode)
library(ggplot2)
library(heatmaply)

anno <- fread("lib/v37.2.1240K_HumanOrigins.clean4.anno")

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

continent_lookup <- clean_anno %>% select(group, continent, period, country) %>% unique

all = fread("~/repos/dirmig/data/dstats/d_all.txt") 
colnames(all) <- c("W", "X", "Y", "Z", "D", "STERR", "z", "na", "nb", "nt")
all$type = "All"

segs = fread("~/repos/dirmig/data/dstats/d_segs.txt") 
colnames(segs) <- c("W", "X", "Y", "Z", "D", "STERR", "z", "na", "nb", "nt")
segs$type = "Segs"

s <- rbind(segs, all) 
s <- segs
s <- merge(s, continent_lookup, by.x = "Y", by.y = "group") %>% filter(period == "Pleistocene")
s$W <- gsub(s$W, pattern = "S_", replacement = "")
s$W <- gsub(s$W, pattern = ".DG", replacement = "")
s$W <- factor(s$W, levels = unique(s$W))

n_african <- length(unique(s$W))
n_eur <- length(unique(s$Y))

mat <- matrix(, nrow = n_african, ncol = n_eur)

# I NEED TO CALCULATE THE MISSING ONES....
for(a in 1:n_african){
  for(e in 1:n_eur){
    d = s$D[s$W == unique(s$W)[a] & s$Y == unique(s$Y)[e]]
    if(length(d) > 0){
      mat[a, e] = unique(d)
    } else {
      mat[a,e] = 0
    }
  }
}

rownames(mat) <- unique(s$W)
colnames(mat) <- unique(s$Y)

Sys.setenv(MAPBOX_TOKEN = 11122223333444)
heatmaply(mat, k_col = NA, fontsize_col = 5, fontsize_row = 7, file = "../plot/ancient_dstats_seg.pdf")

#Export it from ggplot because orca is trash

if(F){
  # This is all of them in heatmaply
  s <- rbind(segs, all) 
  s <- segs
  s <- merge(s, continent_lookup, by.x = "Y", by.y = "group")
  s$W <- gsub(s$W, pattern = "S_", replacement = "")
  s$W <- gsub(s$W, pattern = ".DG", replacement = "")
  s$W <- factor(s$W, levels = unique(s$W))
  
  n_african <- length(unique(s$W))
  n_eur <- length(unique(s$Y))
  
  mat <- matrix(, nrow = n_african, ncol = n_eur)
  
  # I NEED TO CALCULATE THE MISSING ONES....
  for(a in 1:n_african){
    for(e in 1:n_eur){
      d = s$D[s$W == unique(s$W)[a] & s$Y == unique(s$Y)[e]]
      if(length(d) > 0){
        mat[a, e] = unique(d)
      } else {
        mat[a,e] = 0
      }
    }
  }
  
  rownames(mat) <- unique(s$W)
  colnames(mat) <- unique(s$Y)
  
  Sys.setenv(MAPBOX_TOKEN = 11122223333444)
  heatmaply(mat, k_col = NA, fontsize_col = 5, fontsize_row = 7)
  
  #Export it from ggplot because orca is trash
  
}

if(F){
  
  ggplot(rbind(all, segs), aes(W, Y, fill = D)) + 
    geom_tile() + 
    facet_grid(. ~ type, scales = "free", space = "free") + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    theme(axis.text.x = element_text(size = 4)) + 
    theme(axis.text.y = element_text(size = 4)) +
    ylab("Eurasian Populations") + 
    xlab("African Individuals") + 
    theme(legend.text = element_text(angle = 90, hjust = 1)) + 
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
}

if(F){
  #Looking at Bell beaker
  s <- segs
  s <- merge(s, continent_lookup, by.x = "Y", by.y = "group")
  s %>% filter(W == "S_Yoruba-1.DG") %>% group_by(country, type, period) %>% summarise(mean = mean(D), sd = sd(D)) -> s2
  ggplot(s2, aes(x = fct_reorder(country, mean), y = mean, ymin = mean-sd, ymax=mean+sd)) + geom_point() + geom_errorbar() + facet_wrap(~period) + coord_flip() + theme(axis.text.y = element_text(size = 5))
  ggplot(s %>% filter(W == "S_Yoruba-1"), aes(x = fct_reorder(country, mean), y = mean, ymin = mean-sd, ymax=mean+sd)) + geom_point() + geom_errorbar() + facet_wrap(~period) + coord_flip() + theme(axis.text.y = element_text(size = 5))
  ggplot(s %>% filter(country == "Japan"), aes(x = Y, y = D)) + geom_point()
  # Works in ALL and SEGS????????
  ggplot(s %>% filter(Y == "Japan_Jomon.SG_LowCov"), aes(x = fct_reorder(W, D), y = D, ymin = D-STERR, ymax=D+STERR)) + geom_point() + geom_errorbar() + coord_flip() + ggtitle("Japanese Joman Period Contribution to African Populations D(Jomon, Chimp; A1, A2)")
  
  # Looking at ALL the Y for a particular W                                                                                                                                                                              
  
  s %>% filter(W == "S_Yoruba-1.DG") %>% group_by(Y, type, period) %>% summarise(D = D, STDERR = STDERR) -> s2
  
  s <- segs
  s <- merge(s, continent_lookup, by.x = "Y", by.y = "group")
  ggplot(s %>% filter(W == "S_Yoruba-1.DG"), aes(x = fct_reorder(Y, D), y = D, ymin = D-STERR, ymax=D+STERR)) + geom_point() + geom_errorbar() + facet_wrap(~period) + coord_flip() + theme(axis.text.y = element_text(size = 1))
  
  s <- rbind(segs, all)
  s <- merge(s, continent_lookup, by.x = "Y", by.y = "group")
  ggplot(s %>% filter(W == "S_Yoruba-1.DG") %>% 
           filter(!grepl(Y, pattern = "LowCov")), 
         aes(x = fct_reorder(Y, D),col=type, y = D, ymin = D-STERR, ymax=D+STERR)) + 
    geom_point() + 
    geom_errorbar() + 
    xlab("World Population") + 
    ylab("D ± Standard Error") + 
    geom_hline(yintercept = 0.0) + 
    facet_grid(continent~period, scales = "free", space = "free_y") + 
    coord_flip() + 
    theme(axis.text.y = element_text(size = 1)) + 
    theme(legend.position = "none")
  ggsave("plot/yri_d_stats.pdf")
}

#ggsave("~/repos/dirmig/plot/dstats-lineplot.png", dpi = 300, height = 15.8, width = 7.11, unit = "in") 

