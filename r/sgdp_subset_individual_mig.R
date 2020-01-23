# Supplemental figure

library(smcsmcTools)
library(rjson)
library(GGally)
library(dplyr)


config = fromJSON(file = "~/repos/eurasian-backmigration/analyses/sgdp_replication.json")
source = names_from_config(config$source) # from tools
source_strings = vapply(source, ids_to_names, character(1)) %>% as.vector
sink = names_from_config(config$sink) # from tools
sink_strings = vapply(sink, ids_to_names, character(1)) %>% as.vector
smc2_path = "~/repos/eurasian-backmigration/v2/data/sgdp_subset/"
msmc_path = "~/repos/eurasian-backmigration/v2/data/sgdp_subset/"

plots = list()

i = 1

#This is stupidly complicated but I don't think its worth a multi-plot function

files = list.files(smc2_path, pattern = "S_Han-1.S_Yoruba-1.out", full.names = T)
list_of_smcsmc <- lapply(files, smcsmc)
dfs = 0
for(smc in list_of_smcsmc){
  seed = stri_extract_all_regex(smc@file, "[0-9]{3,}") %>% unlist
  if(!is.data.frame(dfs)){
    dfs = plot(smc, return_df = T) %>% mutate(seed = seed) %>% mutate(pop = "Yoruba")
  } else {
    dfs = rbind(dfs, plot(smc, return_df = T) %>% mutate(seed = seed) %>% mutate(pop = "Yoruba")) 
  }
}

files = list.files(smc2_path, pattern = "S_Han-1.S_Mbuti-1.out", full.names = T)
list_of_smcsmc <- lapply(files, smcsmc)
for(smc in list_of_smcsmc){
  seed = stri_extract_all_regex(smc@file, "[0-9]{3,}") %>% unlist
  if(!is.data.frame(dfs)){
    dfs = plot(smc, return_df = T) %>% mutate(seed = seed) %>% mutate(pop = "Mbuti")
  } else {
    dfs = rbind(dfs, plot(smc, return_df = T) %>% mutate(seed = seed) %>% mutate(pop = "Mbuti")) 
  }
}

files = list.files(smc2_path, pattern = "S_Han-1.S_Khomani_San-1.out", full.names = T)
list_of_smcsmc <- lapply(files, smcsmc)
for(smc in list_of_smcsmc){
  seed = stri_extract_all_regex(smc@file, "[0-9]{3,}") %>% unlist
  if(!is.data.frame(dfs)){
    dfs = plot(smc, return_df = T) %>% mutate(seed = seed) %>% mutate(pop = "Khomani San")
  } else {
    dfs = rbind(dfs, plot(smc, return_df = T) %>% mutate(seed = seed) %>% mutate(pop = "Khomani San")) 
  }
}

summary <- dfs %>%
  group_by(Start, From, pop) %>%
  summarise(mean = mean(Rate),
            sd = sd(Rate))

ggplot(data = summary, aes(x = Start*g, fill = pop, linetype = From, y = mean, ymin=mean-sd, ymax=mean+sd)) + 
  geom_step(aes(col = pop)) +
  geom_ribbon(stat="stepribbon",
              alpha = 0.3) + 
  scale_y_continuous(limits = ylim, 
                     labels = label_comma()) +
  scale_x_log10(limits = xlim,
                labels = label_comma(scale = 0.001)) + 
  scale_color_manual(values = c('blue', 'purple', "red"), labels = c("Khomani San", "Mbuti", "Yoruba")) +
  ylab("Proportion Replaced per Generation") + 
  xlab("Thousands of Years before Present") + 
  scale_linetype_manual(values = c(2,1), labels = c("Eur to Afr", "Afr to Eur")) + 
  scale_fill_manual(values = c('blue', 'purple', "red"), labels = c("Khomani San", "Mbuti", "Yoruba")) + 
  #scale_fill_manual(values = c('blue', 'red', 'black'), labels = c("Afr to Eur", "Eur to Afr")) +
  theme_bw() + 
  theme(legend.position = "right",
        legend.title = element_blank(), 
        panel.grid = element_blank())
    
ggsave("~/repos/dirmig/plot/mig/sgdp_subet_three_pop.pdf", height = 9.61, width = 11.3, unit = "in")

