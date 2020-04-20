library(smcsmcTools)
library(data.table)
library(dplyr)
library(countrycode)
library(ggplot2)
library(heatmaply)
library(ggpubr)
library(xtable)

construct_reich_df = function(
  .anno = "~/repos/dirmig/lib/v37.2.1240K_HumanOrigins.clean4.anno"
){
  anno <- fread(.anno)

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
  
  #ggplot( rbind(segs, all), aes(x = D, fill= type, group = type))  + 
    geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.6, binwidth = 0.001) + 
    theme_bw() + 
    xlim(c(-0.15, 0.15))
  theme(legend.position = "bottom", legend.title = element_blank()) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  panel.background = element_blank(), axis.line = element_line(colour = "black")) + ylab("Density") + xlab("Z Score")
  #ggsave("~/repos/dirmig/plot/dstats_hist.pdf")
  
  joined <- rbind(segs, all) 
  
  joined$archaic = "Human"
  joined$archaic[grepl("Nean", joined$Y)] = "Neanderthal"
  joined$archaic[grepl("Deni", joined$Y)] = "Denisovan"
  
  joined <- merge(joined, continent_lookup, by.x = "Y", by.y = "group")
  
  #ggplot(joined %>% filter(archaic != "Human"), aes(x = W, y = D, col = type)) + 
    geom_boxplot() +
    facet_wrap(~archaic, nrow = 2) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    theme(axis.text.x = element_text(size=4, angle = 45, hjust = 1)) + 
    theme(legend.position = "bottom", legend.title = element_blank()) +
    xlab("")
  
  
  #ggplot(joined, aes(sample = z, shape = period)) + stat_qq() + stat_qq_line() + facet_wrap(~type, nrow = 2, strip.position = "top") +   theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    theme(axis.text.x = element_text(size=4, angle = 45, hjust = 1)) + 
    theme(legend.position = "bottom", legend.title = element_blank()) + 
    ylab("Observed Z Score") + 
    xlab("Expected Z Score")


}










##### Archaic bar plot
archaic_bar_plot <- ggdensity(joined %>% filter(archaic != "Human"), x = "D",  bins = 20, fill = "type", rug = T) + theme(legend.title = element_blank()) + scale_fill_manual(values = c("blue", "red"))
modern_bar_plot <- ggdensity(joined %>% filter(archaic == "Human"), x = "D", fill = "type", bins = 50, xlim = c(-0.1, 0.1), rug = T)+ scale_fill_manual(values = c("blue", "red"))
ggarrange(archaic_bar_plot, modern_bar_plot, common.legend = T)


#Looking at Bell beaker
if(F){
s <- segs
s <- merge(s, continent_lookup, by.x = "Y", by.y = "group")
s %>% filter(W == "S_Yoruba-1.DG") %>% group_by(country, type, period) %>% summarise(mean = mean(D), sd = sd(D)) -> s2
ggplot(s2, aes(x = fct_reorder(country, mean), y = mean, ymin = mean-sd, ymax=mean+sd)) + geom_point() + geom_errorbar() + facet_wrap(~period) + coord_flip() + theme(axis.text.y = element_text(size = 5))
ggplot(s %>% filter(W == "S_Yoruba-1"), aes(x = fct_reorder(country, mean), y = mean, ymin = mean-sd, ymax=mean+sd)) + geom_point() + geom_errorbar() + facet_wrap(~period) + coord_flip() + theme(axis.text.y = element_text(size = 5))
ggplot(s %>% filter(country == "Japan"), aes(x = Y, y = D)) + geom_point()
# Works in ALL and SEGS????????
ggplot(s %>% filter(Y == "Japan_Jomon.SG_LowCov"), aes(x = fct_reorder(W, D), y = D, ymin = D-STERR, ymax=D+STERR)) + geom_point() + geom_errorbar() + coord_flip() + ggtitle("Japanese Joman Period Contribution to African Populations D(Jomon, Chimp; A1, A2)")
}
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
  facet_grid(~continent, scales = "free", space = "free_y") +
  theme(axis.text.y = element_text(size = 1)) + 
  theme(legend.position = "none", panel.grid = element_blank())



dotchart <- ggdotchart(s %>% filter(W == "S_Yoruba-1.DG") %>% filter(!grepl(Y, pattern = "LowCov")), x  = "Y", y = "D", col = "type", shape = "period") + theme(axis.text.x = element_blank(), axis.ticks = element_blank()) + facet_wrap(~continent, nrow = 1) + scale_color_manual(values = c("blue", "red"), labels = c("All Markers", "Putatively Migrated Segments")) +
    theme(legend.title = element_blank(), legend.position = "top") + ylab("D(Test Yoruban, Yoruban; Y, Chimp)") +
  geom_hline(yintercept = 0) + scale_shape_discrete(labels = c("Holocene (1-10kya)", "Modern (<1kya)", "Paleolithic (>10kya)"))


#### ----
joined$id <- lapply(joined$W, ids_to_names) %>% unlist
joined = joined %>% filter(period == "Pleistocene", type == "Segs") %>% mutate(label = if_else(archaic == "Human", "", "*"))
ancientheatmap <- ggplot(joined %>% 
                           filter(period == "Pleistocene", type == "Segs"), 
                         aes(x = fct_reorder(Y, D), 
                             y = reorder(id, D), 
                             fill = D)) + 
  geom_tile() + 
  theme(axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank()) + 
  ylab("African Population") + 
  xlab("Ancient Sample") + 
  scale_fill_gradient(breaks = c(-0.1, -0.05, 0, 0.05, 0.1, 0.15)) +
  annotate("segment", x = 1, xend = 12, y = 0, yend = 0, color = "red", size = 3, lty = 1) +
  geom_text(x = 10, y = -2, label = "Neanderthals and Denisovans") + 
  theme(legend.text = element_text(angle = 45, hjust = 1),
        legend.position = "top")

ancientheatmap



##########


bottom <- ggarrange( ancientheatmap, archaic_bar_plot, modern_bar_plot, nrow = 1, widths = c(3,1,1), legend = "bottom", common.legend = T, labels = c("b", "c", "d")) 
ggarrange(dotchart, bottom, nrow = 2, legend = "top", labels = c("a", ""))

dotchart / (ancientheatmap + modern_bar_plot + archaic_bar_plot) + plot_annotation(tag_levels = "a") & 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(face = "bold", hjust = 0, vjust = 0))

ggsave("~/repos/dirmig/plot/new_dstats.pdf", height = 6, width = 12)
