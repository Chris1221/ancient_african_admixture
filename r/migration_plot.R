
#' @description Produces Figure 1a
sgdp_three_pop_curve = function(){
config = fromJSON(file = "~/repos/eurasian-backmigration/analyses/sgdp_replication.json")
source = names_from_config(config$source) # from tools
source_strings = vapply(source, ids_to_names, character(1)) %>% as.vector
sink = names_from_config(config$sink) # from tools
sink_strings = vapply(sink, ids_to_names, character(1)) %>% as.vector
smc2_path = "~/repos/dirmig/data/sgdp_subset/"
old_path <- "~/repos/ancient_migration/data/full_africa"
msmc_path = "~/repos/eurasian-backmigration/v2/data/sgdp/"

plots = list()

i = 1

#This is stupidly complicated but I don't think its worth a multi-plot function

sample = "S_Yoruba-1.out"
#files = c(list.files(smc2_path, pattern = paste0("S_Han-1.", sample), full.names = T),
          #list.files(old_path, pattern = sample, full.names = T))
files <- list.files(smc2_path, pattern = paste0("S_Han-1.", sample), full.names = T)
list_of_smcsmc <- lapply(files, smcsmc)
dfs = 0
for(smc in list_of_smcsmc){
  #seed = stri_extract_all_regex(smc@file, "[0-9]{3,}") %>% unlist
  if(!is.data.frame(dfs)){
    dfs = plot(smc, return_df = T) %>% mutate(seed = seed) %>% mutate(pop = "Yoruba")
  } else {
    dfs = rbind(dfs, plot(smc, return_df = T) %>% mutate(seed = seed) %>% mutate(pop = "Yoruba")) 
  }
}

sample = "S_Mbuti-1.out"
#files = c(list.files(smc2_path, pattern = paste0("S_Han-1.", sample), full.names = T),
          #list.files(old_path, pattern = sample, full.names = T))

files <- list.files(smc2_path, pattern = paste0("S_Han-1.", sample), full.names = T)
list_of_smcsmc <- lapply(files, smcsmc)
for(smc in list_of_smcsmc){
  #seed = stri_extract_all_regex(smc@file, "[0-9]{3,}") %>% unlist
  if(!is.data.frame(dfs)){
    dfs = plot(smc, return_df = T) %>% mutate(seed = seed) %>% mutate(pop = "Mbuti")
  } else {
    dfs = rbind(dfs, plot(smc, return_df = T) %>% mutate(seed = seed) %>% mutate(pop = "Mbuti")) 
  }
}

sample = "S_Khomani_San-1.out"
#files = c(list.files(smc2_path, pattern = paste0("S_Han-1.", sample), full.names = T),
#          list.files(old_path, pattern = sample, full.names = T))

files <- list.files(smc2_path, pattern = paste0("S_Han-1.", sample), full.names = T)
list_of_smcsmc <- lapply(files, smcsmc)
for(smc in list_of_smcsmc){
  #seed = stri_extract_all_regex(smc@file, "[0-9]{3,}") %>% unlist
  if(!is.data.frame(dfs)){
    dfs = plot(smc, return_df = T) %>% mutate(seed = seed) %>% mutate(pop = "Khomani San")
  } else {
    dfs = rbind(dfs, plot(smc, return_df = T) %>% mutate(seed = seed) %>% mutate(pop = "Khomani San")) 
  }
}

sample = "S_Mozabite-1.out"
#files = c(list.files(smc2_path, pattern = paste0("S_Han-1.", sample), full.names = T),
#
          #ist.files(old_path, pattern = sample, full.names = T))
          #
files <- list.files(smc2_path, pattern = paste0("S_Han-1.", sample), full.names = T)
list_of_smcsmc <- lapply(files, smcsmc)
for(smc in list_of_smcsmc){
  #seed = stri_extract_all_regex(smc@file, "[0-9]{3,}") %>% unlist
  if(!is.data.frame(dfs)){
    dfs = plot(smc, return_df = T) %>% mutate(seed = seed) %>% mutate(pop = "Mozabite")
  } else {
    dfs = rbind(dfs, plot(smc, return_df = T) %>% mutate(seed = seed) %>% mutate(pop = "Mozabite")) 
  }
}

summary <- dfs %>%
  group_by(Start, From, pop) %>%
  summarise(mean = mean(Rate),
            sd = sd(Rate))

ylims = c(0, 4e-4)
summary$pop <- factor(summary$pop, levels = c("Yoruba", "Mbuti", "Khomani San"))
sgdp <- ggplot(data = summary, aes(x = Start*g, fill = pop, linetype = From, y = mean, ymin=mean-sd, ymax=mean+sd)) + 
  geom_rect(aes(xmin = 1e5, xmax=3e5, ymin=0, ymax=ylims[2]), fill = "lightgrey", alpha = 0.05, inherit.aes = F) +
  geom_step(aes(col = pop)) +
  geom_ribbon(stat="stepribbon",
              alpha = 0.3) + 
  scale_y_continuous(limits = ylims, 
                     labels = label_comma(scale = 10000)) +
  scale_x_log10(breaks = c(1e4, 3e4, 1e5, 3e5),
                limits = c(1e4,3e5),
                labels = label_comma(scale = 0.001, suffix = "k")) + 
  scale_color_manual(values = c('red', 'blue', "green", "orange"), labels = c("Yoruba", "Mbuti", "Khomani San", "Mozabite")) +
  ylab(TeX("Migration Rate (10^{-4})")) + 
  xlab("Years ago") + 
  scale_linetype_manual(values = c(2,1), labels = c("Afr to Eur", "Eur to Afr")) + 
  scale_fill_manual(values = c('red', 'blue', "green", "orange"), labels = c("Yoruba", "Mbuti", "Khomani San", "Mozabite")) + 
  #scale_fill_manual(values = c('blue', 'red', 'black'), labels = c("Afr to Eur", "Eur to Afr")) +
  theme_bw()  +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.grid = element_blank(),
        aspect.ratio = 1) +
  annotation_logticks() + 
  guides(colour = guide_legend(order = 1), 
         linetype = guide_legend(order = 2, override.aes = list(fill = NA)),
         fill = guide_legend(order = 1))
  #theme(legend.position = "none",
   #     legend.title = element_blank(), 
  #      panel.grid.minor = element_blank())
  #      
  return(sgdp)

}

#' @description Produces FIgure 1b
hgdp_three_pop_curve = function(){
  config = fromJSON(file = "~/repos/eurasian-backmigration/analyses/hgdp_physically_phased.json")
  source = names_from_config(config$source) # from tools
  source_strings = ids_to_names(source, "hgdp") %>% as.vector
  sink = names_from_config(config$sink) # from tools
  sink_strings = ids_to_names(sink, "hgdp") %>% as.vector
  smc2_path = "~/repos/dirmig/data/hgdp_low_mig/"
  
  plots = list()
  i = 1
  
  #This is stupidly complicated but I don't think its worth a multi-plot function
  
  sample = "HGDP00930"
  files <- list.files(smc2_path, pattern = paste0("HGDP00774.", sample, ".out"), full.names = T)
  list_of_smcsmc <- lapply(files, smcsmc)
  dfs = 0
  for(smc in list_of_smcsmc){
    #seed = stri_extract_all_regex(smc@file, "[0-9]{3,}") %>% unlist
    if(!is.data.frame(dfs)){
      dfs = plot(smc, return_df = T) %>% mutate(seed = seed) %>% mutate(pop = "Yoruba")
    } else {
      dfs = rbind(dfs, plot(smc, return_df = T) %>% mutate(seed = seed) %>% mutate(pop = "Yoruba")) 
    }
  }
  
  sample = "HGDP00450"
  files <- list.files(smc2_path, pattern = paste0("HGDP00774.", sample, ".out"), full.names = T)
  list_of_smcsmc <- lapply(files, smcsmc)
  for(smc in list_of_smcsmc){
    #seed = stri_extract_all_regex(smc@file, "[0-9]{3,}") %>% unlist
    if(!is.data.frame(dfs)){
      dfs = plot(smc, return_df = T) %>% mutate(seed = seed) %>% mutate(pop = "Mbuti")
    } else {
      dfs = rbind(dfs, plot(smc, return_df = T) %>% mutate(seed = seed) %>% mutate(pop = "Mbuti")) 
    }
  }
  
  sample = "HGDP01029"
  files <- list.files(smc2_path, pattern = paste0("HGDP00774.", sample, ".out"), full.names = T)
  list_of_smcsmc <- lapply(files, smcsmc)
  for(smc in list_of_smcsmc){
    #seed = stri_extract_all_regex(smc@file, "[0-9]{3,}") %>% unlist
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
  
  ylims = c(0, 4e-4)
  hgdp <- ggplot(data = summary, aes(x = Start*g, fill = pop, linetype = From, y = mean, ymax = mean+sd, ymin=mean-sd)) + 
    geom_rect(aes(xmin = 1e5, xmax=3e5, ymin=0, ymax=ylims[2]), fill = "lightgrey", alpha = 0.05, inherit.aes = F) +
    geom_step(aes(col = pop)) +
    geom_ribbon(stat = "stepribbon", alpha = 0.3) +
    scale_y_continuous(limits = ylims, 
                       labels = label_comma(scale=10000)) +
    scale_x_log10(breaks = c(1e4, 3e4, 1e5, 3e5), 
                  limits = c(1e4,3e5),
                  labels = label_comma(scale = 0.001, suffix = "k")) + 
    scale_color_manual(values = c('green', 'blue', "red", "orange"), labels = c("Khomani San", "Mbuti", "Yoruba", "Mozabite")) +
    #ylab(TeX("Migration Rate (10^{-4})")) + 
    ylab("") + 
    xlab("Years ago") + 
    scale_linetype_manual(values = c(2,1), labels = c("Afr to Eur", "Eur to Afr")) + 
    scale_fill_manual(values = c('green', 'blue', "red", "orange"), labels = c("Khomani San", "Mbuti", "Yoruba", "Mozabite")) + 
    #scale_fill_manual(values = c('blue', 'red', 'black'), labels = c("Afr to Eur", "Eur to Afr")) +
    theme_bw()  +
    theme(legend.position = "none",
          panel.grid = element_blank(),
          legend.title = element_blank(),
          aspect.ratio = 1) +
    annotation_logticks()
  
  return(hgdp)
}

#' @description SGDP MSMC X-Coal Curve
sgdp_msmc = function(){
  config = fromJSON(file = "~/repos/eurasian-backmigration/analyses/sgdp_replication.json")
  source = names_from_config(config$source) # from tools
  source_strings = vapply(source, ids_to_names, character(1)) %>% as.vector
  sink = names_from_config(config$sink) # from tools
  sink_strings = vapply(sink, ids_to_names, character(1)) %>% as.vector
  seed = "1791095846"
  smc2_path = "~/repos/eurasian-backmigration/v2/data/sgdp_subset/"
  msmc_path = "~/repos/eurasian-backmigration/v2/data/sgdp_subset/"
  
  
  list_of_msmc <- 0
  for (si in sink){
    for (so in source){
      msmc_file = msmc(file = paste0(msmc_path, so, ".", si, ".final.txt"))
      
      if(!is.data.frame(list_of_msmc)){
        list_of_msmc <- plot(msmc_file, return_df = T, lines = c("lambda_01"), ylim = c(0, 1)) %>%
          mutate(sink = si) %>%
          mutate(source = so)
      } else {
        list_of_msmc <- rbind(list_of_msmc, plot(msmc_file, return_df = T, lines = c("lambda_01"), ylim = c(0, 1)) %>% 
                                mutate(sink = si) %>%
                                mutate(source = so))
      }}}
  
  sgdp_xcoal <- ggplot(data = list_of_msmc %>% filter(source == "S_Han-1", sink != "S_Biaka-1"), aes(x = left_time_boundary, y = value, col = sink)) + 
    geom_step() + 
    scale_x_log10(breaks = c(1e4, 3e4, 1e5, 3e5),
                  labels = label_comma(scale = 0.001, suffix = "k"), 
                  limits = c(1e4,3e5)) + 
    #geom_hline(yintercept = 0.5, alpha = 0.2)+ 
    theme_bw() + 
    scale_color_manual(values = c("green", "blue", "red")) +
    xlab("Years ago") + 
    ylab("RCCR") + 
    theme(legend.position = "none",
          legend.title = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 1) + 
    annotation_logticks(sides = "b")
  
  return(sgdp_xcoal)
  
  
  #papuan_xcoal <- ggplot(data = list_of_msmc %>% filter(source == "S_Papuan-1", sink != "S_Biaka-1"), aes(x = left_time_boundary, y = value, col = sink)) + 
  #  geom_vline(xintercept = seq(1e4,1e5,1e4), col = "grey", alpha = 0.3) +
  #  geom_vline(xintercept = seq(1e5,1e6,1e5), col = "grey", alpha = 0.3) +
  #  geom_step() + 
  #  scale_x_log10(labels = label_comma(scale = 0.001)) + 
  #  scale_color_manual(values = c("blue", "green", "red")) +
  ##  geom_hline(yintercept = 0.5, alpha = 0.2)+ 
  #  theme_bw() + 
  #  xlab("Years ago") + 
  #  ylab("Relative X-Coal Rate") + 
  #  theme(legend.title = element_blank(),
  #        panel.grid = element_blank()) 
  
  #ggsave("~/repos/dirmig/plot/mig/han_xcoal.pdf", height = 4, width = 4, units = "in")
}

#' @description HGDP MSMC X-Coal curve
hgdp_msmc = function(){
config = fromJSON(file = "~/repos/eurasian-backmigration/analyses/hgdp_physically_phased.json")
source = names_from_config(config$source) # from tools
source_strings = vapply(source, ids_to_names, character(1)) %>% as.vector
sink = names_from_config(config$sink) # from tools
sink_strings = vapply(sink, ids_to_names, character(1)) %>% as.vector
seed = "1791095846"
msmc_path = "~/repos/dirmig/data/hgdp_low_mig/"


list_of_msmc <- 0
for (si in sink){
  for (so in source){
    msmc_file = msmc(file = paste0(msmc_path, so, ".", si, ".final.txt"))
    
    if(!is.data.frame(list_of_msmc)){
      list_of_msmc <- plot(msmc_file, return_df = T, lines = c("lambda_01"), ylim = c(0, 1)) %>%
        mutate(sink = si) %>%
        mutate(source = so)
    } else {
      list_of_msmc <- rbind(list_of_msmc, plot(msmc_file, return_df = T, lines = c("lambda_01"), ylim = c(0, 1)) %>% 
                              mutate(sink = si) %>%
                              mutate(source = so))
    }}}


list_of_msmc$sink <- ids_to_names(list_of_msmc$sink, "hgdp")
list_of_msmc$source <- ids_to_names(list_of_msmc$source, "hgdp")

hgdp_xcoal <- ggplot(data = list_of_msmc %>% filter(source == "Han", sink != "Biaka"), aes(x = left_time_boundary, y = value, col = sink)) + 
  geom_step() + 
  scale_x_log10(breaks = c(1e4, 3e4, 1e5, 3e5),
                labels = label_comma(scale = 0.001, suffix = "k"), 
                limits = c(1e4,3e5)) + 
  #geom_hline(yintercept = 0.5, alpha = 0.2)+ 
  theme_bw() + 
  scale_color_manual(values = c("blue", "green", "red")) +
  xlab("Years ago") + 
  ylab("RCCR") + 
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.grid = element_blank(),
        aspect.ratio = 1) + 
  annotation_logticks(sides = "b") 

}

#' @description SGDP Boxplot 
sgdp_boxplot = function(){

config = fromJSON(file = "~/repos/eurasian-backmigration/analyses/whole_sgdp.json")
source = names_from_config(config$source) # from tools
source_strings = vapply(source, ids_to_names, character(1)) %>% as.vector
sink = names_from_config(config$sink) # from tools
sink_strings = vapply(sink, ids_to_names, character(1)) %>% as.vector
seed = "1791095846"
smc2_path = "~/repos/dirmig/data/sgdp/"
msmc_path = "~/repos/dirmig/data/sgdp/"

n_source = length(source)
n_sink = length(sink)

mat = matrix(, nrow = n_source, ncol = n_sink)
eur_mat = matrix(, nrow = n_source, ncol = n_sink)

for (so in c(1:n_source)){
  for (si in c(1:n_sink)){
    smcsmc_file = new("smcsmc", file = paste0(smc2_path, seed, ".", source[so], ".", sink[si], ".out"))
    mat[so, si] = avg_migr(smcsmc_file@file, ancient = 70000, modern = 30000, g = 29)$integrated[2]
  }}

for (so in c(1:n_source)){
  for (si in c(1:n_sink)){
    smcsmc_file = new("smcsmc", file = paste0(smc2_path, seed, ".", source[so], ".", sink[si], ".out"))
    eur_mat[so, si] = avg_migr(smcsmc_file@file, ancient = 70000, modern = 30000, g = 29)$integrated[1]
  }}

colnames(mat) <- sink_strings
rownames(mat) <- source_strings
colnames(eur_mat) <- sink_strings
rownames(eur_mat) <- source_strings


df <- as.data.frame(mat)


df_lang <- t(mat) %>% as.data.frame
df_lang$pop <- rownames(df_lang)
df_lang$sample <- paste0("S_", df_lang$pop, "-1")
df_lang$sample[df_lang$pop == "Dinka"] = "B_Dinka-3"
df_lang$sample[df_lang$pop == "Ju hoan North"] = "S_Ju_hoan_North-1"
df_lang$sample[df_lang$pop == "Khomani San"] = "S_Khomani_San-1"
df_lang$sample[df_lang$pop == "Somali"] = "S_Somani-1"
df_lang$lang <- ""
for(i in 1:nrow(df_lang)){
  df_lang$lang[i] <- which_lang(df_lang$sample[i])
}
df_lang <- df_lang %>% gather(comparison, migration, French:Papuan)

df_lang$lang[df_lang$lang == "Khoesan"] = "Khoe-San"

df_lang$lang <- factor(df_lang$lang, levels = c("Afroasiatic", "Nilo-Saharan", "Niger-Kordofanian", "Khoe-San"),)

boxplot <- ggplot(df_lang, aes(y = migration, x = lang, col = comparison)) + 
  geom_boxplot() + 
  ylab("IMF") + 
  xlab("") + 
  theme_bw() + 
  scale_color_manual(values = c("purple", "orange", "brown")) +
  theme(legend.position = "top", legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        aspect.ratio = 1/3)
  #annotate(geom="text", x=2.5, y=0.2, label="Mbuti",
  #                                     color="blue") +
  #annotate("segment", x = 2.7, xend = 2.9, y = 0.2, yend = 0.24, colour = "blue", size=0.5, alpha=1, arrow=arrow())

  return(boxplot)

}

#' @description Three simulation plots
figure_1_sim_plots = function(){
s1 <- smcsmc(paste0("~/repos/dirmig/data/spvaryingmig_confident/backward_60000_10000_0.5_2.out"))
s2 <- smcsmc(paste0("~/repos/dirmig/data/spvaryingmig_confident/bidirectional_60000_10000_0.5_2.out"))
s3 <- smcsmc(paste0("~/repos/dirmig/data/spvaryingmig_confident/forward_60000_10000_0.5_2.out"))

df1 <- plot(s1, return_df = T, xlim = c(1e4, 3e5)) %>% mutate(run = "Backwards", From = as.numeric(From))
df2 <- plot(s2, return_df = T, xlim = c(1e4, 3e5)) %>% mutate(run = "Bidirectional", From = as.numeric(From))
df3 <- plot(s3, return_df = T, xlim = c(1e4, 3e5)) %>% mutate(run = "Forwards", From = as.numeric(From))

df <- bind_rows(df1, df2, df3)

mid = 60000
sim1 = ggplot(df1, aes(x = Start*29, y = Rate, col = as.factor(From), lty = factor(From)))  +
  geom_vline(xintercept = c(as.numeric(mid) + 5000, as.numeric(mid) - 5000), col = "grey", linetype = 2) +
  geom_step() + 
  scale_x_log10(
    breaks = c(1e4, 3e4, 1e5, 3e5),
    labels = label_comma(scale = 0.001, suffix = "k"),
    limits = c(1e4, 3e5)
  ) +
  scale_y_continuous(limits = c(0, 4e-4), labels = label_comma(scale = 10000)) +
  scale_color_manual(values = c("black", "black")) +
  scale_linetype_manual(values = c(2, 1)) + 
  theme_bw() +
  xlab("Years ago") + 
  ylab("") + 
  theme(legend.position = "none", 
        panel.grid = element_blank(),
        aspect.ratio = 1) +
  annotation_logticks()

sim2 = ggplot(df2, aes(x = Start*29, y = Rate, col = as.factor(From), lty = factor(From)))  +
  geom_vline(xintercept = c(as.numeric(mid) + 5000, as.numeric(mid) - 5000), col = "grey", linetype = 2) +
  geom_step() + 
  scale_x_log10(
    breaks = c(1e4, 3e4, 1e5, 3e5),
    labels = label_comma(scale = 0.001, suffix = "k"),
    limits = c(1e4, 3e5)
  ) +
  scale_y_continuous(limits = c(0, 4e-4), labels = label_comma(scale = 10000)) +
  scale_color_manual(values = c("black", "black")) + 
  scale_linetype_manual(values = c(2, 1)) +
  theme_bw() +
  xlab("Years ago") + 
  ylab("") + 
  theme(legend.position = "none", 
        panel.grid = element_blank(),
        aspect.ratio = 1) +
  annotation_logticks()

sim3 = ggplot(df3, aes(x = Start*29, y = Rate, col = as.factor(From), lty = factor(From)))  +
  #geom_vline(xintercept = seq(10000,100000, by = 10000), col = "grey", alpha = 0.3) + 
  #geom_vline(xintercept = seq(1e5,1e6,by=1e5), col = "grey", alpha = 0.3) + 
  geom_vline(xintercept = c(as.numeric(mid) + 5000, as.numeric(mid) - 5000), col = "grey", linetype = 2) +
  geom_step() + 
  scale_x_log10(
    breaks = c(1e4, 3e4, 1e5, 3e5),
    labels = label_comma(scale = 0.001, suffix = "k"),
    limits = c(1e4, 3e5)
  ) +
  scale_y_continuous(limits = c(0, 4e-4), labels = label_comma(scale = 10000)) +
  scale_color_manual(values = c("black", "black")) +
  scale_linetype_manual(values = c(2, 1)) +
  theme_bw() +
  xlab("Years ago") + 
  ylab("") + 
  theme(legend.position = "none", 
        panel.grid = element_blank(),
        aspect.ratio = 1) +
  annotation_logticks()

  return(list(sim1, sim2, sim3))
}

migration_plot = function(sgdp, hgdp, sgdp_msmc, hgdp_msmc, boxplot, sims1, sims2, sims3){
layout = "
IIIIIIIIII
AABBEEFFGG
AABBEEFFGG
AABBEEFFGG
AABBEEFFGG
CCDDHHHHHH
CCDDHHHHHH
CCDDHHHHHH
CCDDHHHHHH
"

p <-  sgdp + hgdp + theme(legend.position = "none") +sgdp_msmc + hgdp_msmc  + sims1  + sims2  +  sims3  + boxplot + guide_area()  +plot_layout(design = layout, guides = "collect") 
p <- p & theme(legend.box = "horizontal") 
p <- p +  plot_annotation(tag_levels = "a")
  #theme(plot.tag.position = c(0, 1),
   #     text = element_text(size = 10),
    #    plot.tag = element_text(size = 12, hjust = 0, vjust = 0))
#top <- ggarrange(sgdp, han_xcoal, papuan_xcoal, ncol = 3, labels = c("a.", "b.", "c."), legend = "top", common.legend = T)
#ggarrange(top, boxplot, nrow = 2, labels = c("", "d."), legend = "none")
#ggsave(p, file = "~/repos/dirmig/plot/new_mig_plot.pdf", width = 12, height = 6)
return(p)
}
