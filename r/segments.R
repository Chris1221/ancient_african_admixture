length_plot = function(){
  output= list()
  config = fromJSON(file = "~/repos/dirmig/analyses/whole_sgdp.json")
  source = names_from_config(config$source) # from tools
  source_strings = vapply(source, ids_to_names, character(1)) %>% as.vector
  sink = names_from_config(config$sink_segs) # from tools
  sink_strings = vapply(sink, ids_to_names, character(1)) %>% as.vector 
  
  sink <- sink[sink != "B_Dinka-3"]
  sink <- sink[sink != "S_Somali-1"]
  
  path <- "~/repos/dirmig/data/segments/"
  
  i = 1
  j = 1
  dfs <- list()
  sum_list <- list()
  for(so in source){
    for(si in sink){
      segs <- fread(paste0(path, so, ".", si, ".bed")) %>% mutate(diff = right - left)
      sum_list[[i]] = c(so, si, mean(segs$diff), sd(segs$diff), sum(segs$diff))
      i = i + 1
    }
    df <- t(data.frame(sum_list))
    row.names(df) <- NULL
    colnames(df) <- c("Source", "Sink", "Mean", "SD", "Sum")
    dfs[[j]] <- as.data.frame(df, stringsAsFactors = F) %>% mutate(Mean = as.numeric(Mean)/1e3, SD = as.numeric(SD)/1e3, Sum = as.numeric(Sum)/1e6)
    i = 1
    j = j + 1
  }
  
  df <- merge(dfs[[1]], dfs[[2]], by = "Sink", suffixes = c(".French", ".Han")) %>% select(-Source.French, -Source.Han)
  df2 <- merge(df, dfs[[3]], by = "Sink") %>% select(-Source) %>%
    mutate(French = paste0(round(Mean.French, 3), " (", round(SD.French, 3), ")")) %>%
    mutate(Han = paste0(round(Mean.Han, 3), " (", round(SD.Han, 3), ")" )) %>%
    mutate(Papuan = paste0(round(Mean, 3), " (", round(SD, 3), ")")) %>%
    select(Sink, French, Sum.French, Han, Sum.Han, Papuan, Sum) %>% arrange(French)
  
  
  colnames(df2) <- c("African Population", "Mean (SD)", "Total (Mb)", "Mean (SD)", "Total (Mb)","Mean (SD)", "Total (Mb)")
  
  df2$`African Population` <- lapply(df2$`African Population`, ids_to_names)
  
  label = "table:lengths"
  legend = "Summary of the length distribution for putatively migrated segments in different African individuals. Means and standard deviations are given in kilobases (kb) while the total length of all segments is given in megabases (Mb)."
  
  output[["table"]] = print(xtable(df2, digits = 3, caption = legend, label = label), include.rownames = FALSE,  append = T)
  
  
  #######
  i = 1
  dfs <- NULL
  for(so in source){
    for(si in sink){
      segs <- fread(paste0(path, so, ".", si, ".bed")) %>% mutate(diff = right - left) %>% select(diff)
      segs$so = ids_to_names(so)
      segs$si = ids_to_names(si)
      if(is.null(dfs)){
        dfs <- segs
      } else {
        dfs <- rbind(dfs, segs)
      }
    }
  }
  
  real <- ggplot(dfs, aes(diff, col = si)) + geom_freqpoly(bins = 50) + scale_y_log10() + facet_wrap(~so) + scale_x_continuous(limits = c(0, 3e6), labels = label_comma(scale = 1e-6)) + theme_bw() + theme( legend.title = element_blank())  + xlab("Length of Admixture Tracts (Mb)") + ylab("Count")
  #ggsave(real, file = "~/repos/dirmig/plot/tract_lengths.pdf", height= 6, width = 10)
  
  
  dfm <- data.frame(m = seq(0, 1, by = 0.001))%>% mutate(l_50k = 1/((1 - m) *1e-8 * ( (50000/29)-1)),l_55k = 1/((1 - m) *1e-8 * ( (55000/29)-1)) , l_60k = 1/((1 - m) *1e-8 * ( (60000/29)-1)), l_65k = 1/((1 - m) *1e-8 * ( (65000/29)-1)), l_70k = 1/((1 - m) *1e-8 * ( (70000/29)-1)), l_40k = 1/((1 - m) *1e-8 * ( (40000/29)-1)), l_45k = 1/((1 - m) *1e-8 * ( (45000/29)-1)))
  dfm <- dfm %>% pivot_longer(l_50k:l_45k) %>% arrange(name)
  
  theory <- ggplot(dfm, aes(x = m, y = value, col = name)) + geom_line() + scale_y_log10(limit = c(8e4, 3e5), labels = label_comma(scale = 1e-3)) + scale_x_continuous(limits = c(0.5, 0.85)) + geom_hline(yintercept = c(170000, 200000)) + annotate("rect", xmin = 0.55, xmax = 0.8, ymin = 170000, ymax = 200000, alpha = .2, fill = "yellow") + theme_bw() + ylab("Expeted Tract Length (kb)") + xlab("IMF") + scale_color_manual(values = c('#1b9e77','#d95f02','#7570b3','#e7298a','#66a61e','#e6ab02','#a6761d'), labels = c(40, 45, 50, 55, 60, 65, 70, 75), name = "Age of Single-pulse Migration") + theme(legend.position = "top", legend.title = element_blank()) + guides(col = guide_legend(nrow = 1))
  #ggsave(theory, file = "~/repos/dirmig/plot/age_by_m.pdf")
  
  lengths <- ggarrange(theory, real,nrow = 2, labels = "auto") 
  output[["plot"]] = lengths
  return(output)
  #ggsave(lengths, file = "~/repos/dirmig/plot/both_length.pdf")
}
