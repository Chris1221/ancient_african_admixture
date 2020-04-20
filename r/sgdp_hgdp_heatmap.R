library(smcsmcTools)
library(ggplot2)
library(rjson)

# SGDP
config = fromJSON(file = "~/repos/dirmig/analyses/sgdp_replication.json")
source = names_from_config(config$source) # from tools
sink = names_from_config(config$sink) # from tools
seed = "1791095846"
smc2_path = "~/repos/dirmig/data/sgdp_subset/"

SGDP_African <- matrix(, nrow = length(source), ncol = length(sink))
SGDP_Eurasian <- matrix(, nrow = length(source), ncol = length(sink))

i = 1
for(so in source){ 
  j = 1
  for(si in sink){
    SGDP_African[i, j] = avg_migr(paste0(path, seed, ".", so, ".", si, ".out"), ancient = 100000, modern = 0, g = 29)$integrated[1]
    SGDP_Eurasian[i, j] =  avg_migr(paste0(path, seed, ".", so, ".", si, ".out"), ancient = 100000, modern = 0, g = 29)$integrated[2]
    j = j + 1}
  i = i + 1
}

colnames(SGDP_African) <- ids_to_names(sink)
rownames(SGDP_African) <- ids_to_names(source)
colnames(SGDP_Eurasian) <- ids_to_names(sink)
rownames(SGDP_Eurasian) <- ids_to_names(source)
sgdp <- reshape2::melt(SGDP_African) %>% mutate(ds = "SGDP") %>% mutate(Var1 = as.character(Var1), Var2 = as.character(Var2), pop = "Forwards")
sgdp$Var2[sgdp$Var2 == "Khomani San"] = "San"

sgdp_e <- reshape2::melt(SGDP_Eurasian) %>% mutate(ds = "SGDP") %>% mutate(Var1 = as.character(Var1), Var2 = as.character(Var2), pop = "Backwards")
sgdp_e$Var2[sgdp_e$Var2 == "Khomani San"] = "San"


# HGDP

config = fromJSON(file = "~/repos/dirmig/analyses/hgdp_physically_phased.json")
source = names_from_config(config$source) # from tools
sink = names_from_config(config$sink) # from tools
seed = "1791095846"
smc2_path = "~/repos/dirmig/data/hgdp_low_mig/"

HGDP_African <- matrix(, nrow = length(source), ncol = length(sink))
HGDP_Eurasian <- matrix(, nrow = length(source), ncol = length(sink))

i = 1
for(so in source){ 
  j = 1
  for(si in sink){
    HGDP_African[i, j] = avg_migr(paste0(smc2_path, seed, ".", so, ".", si, ".out"), ancient = 100000, modern = 0, g = 29)$integrated[1]
    HGDP_Eurasian[i, j] =  avg_migr(paste0(smc2_path, seed, ".", so, ".", si, ".out"), ancient = 100000, modern = 0, g = 29)$integrated[2]
    j = j + 1}
  i = i + 1
}

colnames(HGDP_African) <- ids_to_names(sink, "hgdp")
rownames(HGDP_African) <- ids_to_names(source, "hgdp")
colnames(HGDP_Eurasian) <- ids_to_names(sink, "hgdp")
rownames(HGDP_Eurasian) <- ids_to_names(source, "hgdp")
hgdp <- reshape2::melt(HGDP_African) %>% mutate(ds = "HGDP") %>% mutate(Var1 = as.character(Var1), Var2 = as.character(Var2), pop = "Forwards") %>%
  filter(Var1 != "PapuanSepik")
hgdp_e <- reshape2::melt(HGDP_Eurasian) %>% mutate(ds = "HGDP") %>% mutate(Var1 = as.character(Var1), Var2 = as.character(Var2), pop = "Backwards") %>%
  filter(Var1 != "PapuanSepik")

hgdp$Var1[hgdp$Var1 == "PapuanHighlands"] = "Papuan"
hgdp_e$Var1[hgdp_e$Var1 == "PapuanHighlands"] = "Papuan"
## Combined


both <- rbind(sgdp, sgdp_e, hgdp, hgdp_e)


comparison <- ggplot(both, aes(x = Var1, y = Var2, fill = value, label = value)) + 
  geom_tile() + 
  geom_text(aes(label = round(value, 3)), col = "white", size = 3) +
  ylab("") +
  xlab("") +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = "left",
        legend.title = element_blank()) +
  facet_grid(pop ~ ds)


ggsave(comparison, file = "~/repos/dirmig/plot/hgdp_sgdp_heatmap.pdf", height = 4, width = 10)
