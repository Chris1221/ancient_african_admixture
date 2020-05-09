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

  exclude <- c("Ancestor.REF", "Href.REF", "Chimp.REF", "Gorilla.REF", "Href", "Href_HO", "Gorilla", "Orang", "Chimp_HO", "Macaque", "Marmoset", "Denisova_light", "Denisova_published.DG")
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
  
  joined <- rbind(segs, all) 
  
  joined$archaic = "Human"
  joined$archaic[grepl("Nean", joined$Y)] = "Neanderthal"
  joined$archaic[grepl("Deni", joined$Y)] = "Denisovan"
  
  joined <- merge(joined, continent_lookup, by.x = "Y", by.y = "group")
  
  return(joined)

}

reich_archaic_density = function(){
  joined = construct_reich_df()
  archaic_bar_plot <- gghistogram(joined %>% filter(archaic != "Human"), x = "D",  bins = 30, fill = "type", rug = T, xlim = c(-0.15, 0.15)) + 
    scale_fill_manual(values = c("blue", "red")) +
    scale_color_manual(values = c("blue", "red")) + 
    theme(legend.title = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(angle=45,hjust=1))
  return(archaic_bar_plot)
}

reich_modern_density = function(){
  joined = construct_reich_df()
  modern_bar_plot <- gghistogram(joined %>% filter(archaic == "Human"), x = "D", fill = "type", xlim = c(-0.15, 0.15), bins = 100, rug = T)+ 
    scale_fill_manual(values = c("blue", "red")) +
    scale_color_manual(values = c("blue", "red")) +
    theme(legend.title = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(angle=45,hjust=1))

  return(modern_bar_plot)
}



#Looking at Bell beaker
reich_all_dotchart = function(){
  joined = construct_reich_df()
  s <- joined
  
  s$period = factor(s$period, levels = c("Modern", "Holocene", "Pleistocene"))
  
  dotchart <- ggdotchart(s %>% filter(W == "S_Yoruba-1.DG") %>% filter(!grepl(Y, pattern = "LowCov")), x  = "Y", y = "D", col = "type", shape = "period") + 
    theme(axis.text.x = element_blank(), axis.ticks = element_blank()) + 
    facet_wrap(~continent, nrow = 1) + 
    scale_color_manual(values = c("blue", "red"), labels = c("All Markers", "Putatively Migrated Segments")) +
    theme(legend.title = element_blank(), legend.position = "top") + ylab("D(Test Yoruban, Yoruban; Y, Chimp)") +
    geom_hline(yintercept = 0) + 
    scale_shape_discrete(labels = c("Modern (<1kya)", "Holocene (1-10kya)","Paleolithic (>10kya)"))
  
  return(dotchart)
  
}

#### ----

reich_ancientheatmap = function(){
  joined = construct_reich_df()
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
          legend.position = "right")
  
  return(ancientheatmap)
}



##########

plot_reich_figure = function(dotchart, ancientheatmap, modern_bar_plot, archaic_bar_plot){
 p = dotchart / (ancientheatmap + (modern_bar_plot / archaic_bar_plot + plot_layout(guides = "collect"))) + plot_annotation(tag_levels = "a") & 
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(face = "bold", hjust = 0, vjust = 0)) 
 return(p)
}
