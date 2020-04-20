# Plots dealing with IMF in SGDP.

#' @description Produces matrices of average IMF over replicates.
make_imf_matrices = function(config_file = "~/repos/dirmig/analyses/whole_sgdp.json", data_path = "~/repos/dirmig/data/sgdp/", pop = "sgdp"){
  
  config = fromJSON(file = config_file)
  source = names_from_config(config$source) # from tools
  source_strings = ids_to_names(source, pop)
  #source_strings = vapply(source, ids_to_names, character(1)) %>% as.vector
  sink = names_from_config(config$sink) # from tools
  #sink_strings = vapply(sink, ids_to_names, character(1)) %>% as.vector
  sink_strings = ids_to_names(sink, pop)
  seed = "1791095846"
  smc2_path = data_path
  seeds = c("1791095846", "946286477", "2135392492")
  
  n_source = length(source)
  n_sink = length(sink)
  
  list_of_mats <- list()
  list_of_eur_mats <- list()
  i = 1
  
  for(seed in seeds){
    mat = matrix(, nrow = n_source, ncol = n_sink)
    eur_mat = matrix(, nrow = n_source, ncol = n_sink)
    
  
    for (so in c(1:n_source)){
      for (si in c(1:n_sink)){
        smcsmc_file = new("smcsmc", file = paste0(smc2_path, seed, ".", source[so], ".", sink[si], ".out"))
        mat[so, si] = avg_migr(smcsmc_file@file, ancient = 70000, modern = 40000, g = 29)$integrated[2]
      }}
    
    for (so in c(1:n_source)){
      for (si in c(1:n_sink)){
        smcsmc_file = new("smcsmc", file = paste0(smc2_path, seed, ".", source[so], ".", sink[si], ".out"))
        eur_mat[so, si] = avg_migr(smcsmc_file@file, ancient = 70000, modern = 40000, g = 29)$integrated[1]
      }}
    
    colnames(mat) <- sink_strings
    rownames(mat) <- source_strings
    colnames(eur_mat) <- sink_strings
    rownames(eur_mat) <- source_strings
    
    list_of_mats[[i]] = mat
    list_of_eur_mats[[i]] = eur_mat
    i = i+1
  }
  
  mat = apply(simplify2array(list_of_mats), c(1,2), mean)
  eur_mat = apply(simplify2array(list_of_eur_mats), c(1,2), mean)
  return(list(mat, eur_mat))
}

#' @description Produces a dataframe with IMF by language group.
#' @param i 1 = African, 2 = European.
make_df_lang = function(i = 1){
  mats = make_imf_matrices()
  mat = mats[[i]]
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
  
  df_lang$lang <- factor(df_lang$lang, levels = c("Afroasiatic", "Nilo-Saharan", "Niger-Kordofanian", "Khoesan"))
  
  return(df_lang)
}

#' @description Heatmap of both average Afr and Eur IMF
imf_heatmap = function(pop = "sgdp"){
  
  if (pop == "hgdp"){
    mats = make_imf_matrices(config_file = "~/repos/dirmig/analyses/hgdp_physically_phased.json", 
                             data_path = "~/repos/dirmig/data/hgdp_low_mig/",
                             pop = "hgdp")

  } else{
    mats = make_imf_matrices()
  }
  
  mat = mats[[1]]
  eur_mat = mats[[2]]
  
  lims = c(min(eur_mat), max(mat))


  a <- reshape2::melt(mat) %>% as.data.frame
  
  a_plot <- ggplot(a, aes(x = fct_reorder(factor(Var2),value), y = Var1, fill = value)) + 
    geom_tile() + 
    geom_text(aes(label=round(value, 3)), color="white") +
    xlab("African Group") +
    ylab("Eurasian Group") +
    scale_fill_gradient(limits = lims) + 
    theme_bw() + 
    theme(legend.position = "none") + 
    theme(panel.border = element_blank(), 
          panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  e <- reshape2::melt(eur_mat) %>% as.data.frame
  
  e_plot <- ggplot(e, aes(x = fct_reorder(factor(Var2),value), y = Var1, fill = value)) + 
    geom_tile() + 
    geom_text(aes(label=round(value, 3)), color="white") +
    xlab("African Group") +
    ylab("Eurasian Group") +
    scale_fill_gradient(limits = lims) + 
    theme_bw() + 
    theme(legend.position = "none") + 
    theme(panel.border = element_blank(), 
          panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  p <- ggmatrix(list(a_plot, e_plot), nrow = 2, ncol = 1, ylab = "Eurasian Group", xlab = "African Group", yAxisLabels = c("African Migration", "Eurasian Migration"))
  #ggsave(p, file = "~/repos/dirmig/plot/mig/integrated_sgdp.pdf", height = 5.18, width = 14.9, unit = "in")
  return(p)
}

#' @description Paired t test for average IMF by donor population.
sgdp_imf_t_test = function() {
  mats = make_imf_matrices()
  mat = mats[[1]]
  df <- as.data.frame(mat)
  t.test(df["Papuan",] %>% unlist %>% as.vector, df["Han",] %>% unlist %>% as.vector, paired = T) %>% print
  t.test(df["Papuan",] %>% unlist %>% as.vector, df["French",] %>% unlist %>% as.vector, paired = T) %>% print
  t.test(df["French",] %>% unlist %>% as.vector, df["Han",] %>% unlist %>% as.vector, paired = T) %>% print

  df_lang = make_df_lang(1)
  t.test(df_lang$migration[df_lang$comparison == "Han" & df_lang$lang != "Afroasiatic"], df_lang$migration[df_lang$comparison == "French"& df_lang$lang != "Afroasiatic"], paired = T) %>% print
}

sgdp_imf_t_test_table = function(){
  df_lang = make_df_lang(1)
  df_lang %>% pairwise_t_test(migration ~ lang) %>%
    select(group1, group2, p.adj) -> pt
  
  colnames(pt) = c("Language Family", "Comparison Family", "Adjusted P Value")
  
  i = 1
  l1l = vector()
  l2l = vector()
  est = vector()
  p = vector()
  for(l1 in unique(df_lang$lang)){
    for(l2 in unique(df_lang$lang)){
      if(l1 != l2){
        t <- t.test(df_lang$migration[df_lang$lang == l1], df_lang$migration[df_lang$lang == l2])
        diff = (t$estimate[1]-t$estimate[2]) %>% round(3)
        ci = paste0(
          " (",
          t$conf.int[1] %>% round(3),
          "-",
          t$conf.int[2] %>% round(3), 
          ")")
        est[i] = paste0(diff, ci)
        
        p[i] = t$p.value
        l1l[i] = l1
        l2l[i] = l2
        i = i + 1
          
      }
    }
  }
  
  mat = make_imf_matrices()[[1]]
  mat[,colnames(mat) %in% ids_to_names(list_samples_from_lang("Niger-Kordofanian"))] %>% 
    t %>%
    melt %>%
    mutate(cahg = Var1 %in% c("Mbuti", "Biaka")) -> cahg
  
  t = t.test(cahg$value[cahg$cahg], cahg$value[!cahg$cahg])
  diff = (t$estimate[1]-t$estimate[2]) %>% round(3)
  ci = paste0(
    " (",
    t$conf.int[1] %>% round(3),
    "-",
    t$conf.int[2] %>% round(3), 
    ")")
  est[i] = paste0(diff, ci)
  
  p[i] = t$p.value
  l1l[i] = "CAHG"
  l2l[i] = "Niger-Kordofanian"
  
  
  
  df = data.frame(l1l, l2l, est, p)
  
  df$p = scientific( p.adjust(df$p, method = "fdr") )
  
  colnames(df) <- c("Language Family", "Comparison Family", "Mean Difference (95% CI)", "Adjusted P")
  
  print(xtable(df, 
         caption = "Two tailed T test for differences in integrated migration fraction (IMF) between language groups in the SGDP. Averaged over three technical replicates to account for the influence of stochastic sampling variation. P values adjusted using Benjamini Hochburg's false discovery rate correction for multiple comparisons. Abbreviations: Central African Hunter Gatherers (CAHG) include Mbuti and Biaka, Confidence Interval (CI).",
         label = "table:sgdp_pairwise",
         align = "lllrr"),
        include.rownames = F,
        math.style.exponents = T,
        math.style.negative = T)
  
}

#' @description Simple boxplot of the IMFs. Not the one in the first figure.
simple_imf_boxplot = function(){ 
  
  df_lang = make_df_lang(1)
  p <- ggplot(df_lang, aes(y = migration, x = comparison, col = comparison)) + 
    stat_compare_means(comparisons = list(c("Han", "French"), c("French", "Papuan"), c("Han", "Papuan")), aes(label = ..p.signif..), method = "t.test", paired = T) + 
    geom_boxplot() + facet_wrap(~lang, nrow = 1) + 
    theme_bw() + 
    theme(legend.position = "none", panel.grid = element_blank()) + 
    ylab("Integrated Migration over 100ky") + 
    xlab("")
  
  eur_lang_df = make_df_lang(2)
  q <- ggplot(eur_lang_df, aes(y = migration, x = comparison, col = comparison)) + 
    stat_compare_means(comparisons = list(c("Han", "French"), c("French", "Papuan"), c("Han", "Papuan")), aes(label = ..p.signif..), method = "t.test", paired = T) + 
    geom_boxplot() + facet_wrap(~lang, nrow = 1) + 
    theme_bw() + 
    theme(legend.position = "none", panel.grid = element_blank()) + 
    ylab("Integrated Migration over 100ky") + 
    xlab("")
  
  return(p/q)

}

#' @description Supplemental table of average IMF by language family.
sgdp_imf_averages_table = function(){
  df_lang = make_df_lang(1)
  eur_lang_df = make_df_lang(2)
  
  summary <- df_lang %>% group_by(lang, comparison) %>% summarise(mean = paste0( round(mean(migration), 3), "(", sd = round(sd(migration), 3), ")"))
  eur_summary <- eur_lang_df %>% group_by(lang, comparison) %>% summarise(mean = paste0( round(mean(migration), 3), "(", sd = round(sd(migration), 3), ")"))
  
  all <- df_lang %>% group_by(lang) %>% summarise(mean = paste0( round(mean(migration), 3), "(", sd = round(sd(migration), 3), ")")) %>% mutate(comparison = "All")
  eur_all <- eur_lang_df %>% group_by(lang) %>% summarise(mean = paste0( round(mean(migration), 3), "(", sd = round(sd(migration), 3), ")")) %>% mutate(comparison = "All")
  
  summary = rbind(as.data.frame(summary), as.data.frame(all))
  eur_summary = rbind(as.data.frame(eur_summary), as.data.frame(eur_all))
  
  table = merge(summary, eur_summary, by = c("lang", "comparison"))
  
  
  colnames(table) <- c("Language Family", "Partner Population", "Average Migration to Africa (Standard Deviation)", "Average Migration to Eurasia (Standard Deviation)")
  
  print( xtable(table, label = "average_sgdp_migration_table", caption = "Average plus or minus standard deviation integrated migration fraction (IMF) from Eurasian to African populations in the Simons Genome Diversity Panel between 40-70kya. Abbreviations: Integrated Migration Fraction (IMF), Stanard Deviation (SD), EUR (Eurasian), AFR (African)"), include.rownames = F)
}

