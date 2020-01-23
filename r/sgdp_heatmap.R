# Migration heatmap.

library(dplyr)
library(smcsmcTools)
library(rjson)
library(heatmaply)

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
    mat[so, si] = avg_migr(smcsmc_file@file, ancient = 100000, modern = 0, g = 29)$integrated[2]
  }}

for (so in c(1:n_source)){
  for (si in c(1:n_sink)){
    smcsmc_file = new("smcsmc", file = paste0(smc2_path, seed, ".", source[so], ".", sink[si], ".out"))
    eur_mat[so, si] = avg_migr(smcsmc_file@file, ancient = 100000, modern = 0, g = 29)$integrated[1]
  }}

colnames(mat) <- sink_strings
rownames(mat) <- source_strings
colnames(eur_mat) <- sink_strings
rownames(eur_mat) <- source_strings

setwd("~/repos/dirmig")
Sys.setenv(MAPBOX_TOKEN = 11122223333444)
heatmaply(mat, dendrogram = "column", limits = c(0, 1), file = "plot/sgdp_heatmap_af.pdf")
heatmaply(eur_mat, dendrogram = "column", limits = c(0, 1), file = "plot/sgdp_heatmap_eur.pdf")



# Because of bug with heatmaply, have to save it through Rstudio then edit in illustrator. (but not actually, orca works now)
# 
# 
# SOme t tests
df <- as.data.frame(mat)
t.test(df["Papuan",] %>% unlist %>% as.vector, df["Han",] %>% unlist %>% as.vector, paired = T)


df_no_high <- df
df_no_high$Mozabite <- NULL
df_no_high$Somali <- NULL
df_no_high$Saharawi <- NULL
df_no_high$Mbuti <- NULL
df_no_high$`Khomani San` <- NULL
df_no_high$`Ju hoan North` <- NULL

t.test(df_no_high["French",] %>% unlist %>% as.vector, df_no_high["Han",] %>% unlist %>% as.vector, paired = T)

# and a plot for the t tests
# again, very messy, but dont ever need it again

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

df_lang$lang <- factor(df_lang$lang, levels = c("Afroasiatic", "Nilo-Saharan", "Niger-Kordofanian", "Khoesan"),)

ggplot(df_lang, aes(y = migration, x = comparison, col = comparison)) + 
  stat_compare_means(comparisons = list(c("Han", "French"), c("French", "Papuan"), c("Han", "Papuan")), aes(label = ..p.signif..), method = "t.test", paired = T) + 
  geom_boxplot() + facet_wrap(~lang, nrow = 1) + 
  theme_bw() + 
  theme(legend.position = "none", panel.grid = element_blank()) + 
  ylab("Integrated Migration over 100ky") + 
  xlab("")

ggsave("~/repos/dirmig/plot/mig/sgdp_averages.pdf", height = 5.61, width = 12.5, units = "in")


# A table
# 
eur_lang_df <- t(eur_mat) %>% as.data.frame
eur_lang_df$pop <- rownames(eur_lang_df)
eur_lang_df$sample <- paste0("S_", eur_lang_df$pop, "-1")
eur_lang_df$sample[eur_lang_df$pop == "Dinka"] = "B_Dinka-3"
eur_lang_df$sample[eur_lang_df$pop == "Ju hoan North"] = "S_Ju_hoan_North-1"
eur_lang_df$sample[eur_lang_df$pop == "Khomani San"] = "S_Khomani_San-1"
eur_lang_df$sample[eur_lang_df$pop == "Somali"] = "S_Somani-1"
eur_lang_df$lang <- ""
for(i in 1:nrow(eur_lang_df)){
  eur_lang_df$lang[i] <- which_lang(eur_lang_df$sample[i])
}
eur_lang_df <- eur_lang_df %>% gather(comparison, migration, French:Papuan)

eur_lang_df$lang <- factor(eur_lang_df$lang, levels = c("Afroasiatic", "Nilo-Saharan", "Niger-Kordofanian", "Khoesan"))

ggplot(eur_lang_df, aes(y = migration, x = comparison, col = comparison)) + 
  stat_compare_means(comparisons = list(c("Han", "French"), c("French", "Papuan"), c("Han", "Papuan")), aes(label = ..p.signif..), method = "t.test", paired = T) + 
  geom_boxplot() + facet_wrap(~lang, nrow = 1) + 
  theme_bw() + 
  theme(legend.position = "none", panel.grid = element_blank()) + 
  ylab("Integrated Migration over 100ky") + 
  xlab("")

ggsave("~/repos/dirmig/plot/mig/sgdp_eur_averages.pdf", height = 5.61, width = 12.5, units = "in")


summary <- df_lang %>% group_by(lang, comparison) %>% summarise(mean = paste0( round(mean(migration), 3), "(", sd = round(sd(migration), 3), ")"))
eur_summary <- eur_lang_df %>% group_by(lang, comparison) %>% summarise(mean = paste0( round(mean(migration), 3), "(", sd = round(sd(migration), 3), ")"))
table = merge(summary, eur_summary, by = c("lang", "comparison"))

colnames(table) <- c("Language Family", "Partner Population", "Average Migration to Africa (Standard Deviation)", "Average Migration to Eurasia (Standard Deviation)")

print( xtable(table, label = "average_sgdp_migration_table", caption = "Average plus or minus standard deviation integrated migration from Eurasian to African populations in the last 100 thousand years (ky)"), include.rownames = F)
