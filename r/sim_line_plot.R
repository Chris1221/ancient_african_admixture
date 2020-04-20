# Just plotting 5 examples that we have now for the various models. 

simulation_line_plot = function(){
  scenarios = c("backward", "forward", "bidirectional")#, "backward")#, "bidirectional", "realistic")
  mplots <- list()
  neplots <- list()
  i <- 1
  
  migs = c("0.0", "0.1", "0.3", "0.5", "0.7", "0.9")
  situations = c(2)
  midpoints <- c("40000", "50000", "60000", "70000")
  
  g = 29
  matrices <- list()
  
  m <- list()
  ne = list()
  j = 1
  
  mat <- matrix(, nrow = length(midpoints), ncol = length(migs))
  emat <- matrix(, nrow = length(midpoints), ncol = length(migs))
  
  
  lists = list()
  elists = list()
  
  for (sit in situations){
    i = 1
    plots <- list()
    eplots <- list()
    for (s in scenarios){
      j = 1
      for(mid in midpoints){
        k = 1
        for(mig in migs){
          emat[j, k] <- avg_migr( file = paste0("~/repos/dirmig/data/spvaryingmig/", s, "_", mid, "_10000_", mig, "_", sit, ".out"), ancient = 100000, modern = 0, g = 29)$integrated[1]
          mat[j,k] <- avg_migr( file = paste0("~/repos/dirmig/data/spvaryingmig/", s, "_", mid, "_10000_", mig, "_", sit, ".out"), ancient = 100000, modern = 0, g = 29)$integrated[2]
          k = k+1
        }
        j = j + 1
      }
      
      rownames(mat) <- midpoints
      colnames(mat) <- factor(round(1-exp(-as.numeric(migs)), 3))
      rownames(emat) <- midpoints
      colnames(emat) <- factor(round(1-exp(-as.numeric(migs)), 3))
      
      lists[[s]] = as.data.frame(mat)
      elists[[s]] = emat
      
      
      
      df <- reshape2::melt(t(mat)) %>% as.data.frame() %>%  mutate(Var1 = as.character(Var1))
      df2 <- reshape2::melt(t(emat)) %>% as.data.frame() %>%  mutate(Var1 = as.character(Var1))
      plots[[i]] <- ggplot(df, aes(x = factor(Var1), y = Var2, fill = value)) + 
        geom_tile() + 
        geom_text(aes(label=round(value, 3)), color="white") +
        xlab("Amount of Migration \n(Proportion Replaced per Generation)") +
        ylab("Time of Migration \n(years before present)") +
        scale_fill_gradient(limits = c(0, 0.6)) + 
        theme_bw() + 
        theme(legend.position = "none",
              panel.border = element_blank(), 
              panel.grid = element_blank(),
              axis.title.y = element_text(size = 15),
              axis.title.x = element_text(size = 15))
      
      eplots[[i]] <- ggplot(df2, aes(x = factor(Var1), y = Var2, fill = value)) + 
        geom_tile() + 
        geom_text(aes(label=round(value, 3)), color="white") +
        xlab("Amount of Migration \n(Proportion Replaced per Generation)") +
        ylab("Time of Migration \n(years before present)") +
        scale_fill_gradient(limits = c(0, 0.6)) + 
        theme_bw() + 
        theme(legend.position = "none",
              panel.border = element_blank(), 
              panel.grid = element_blank(),
              axis.title.y = element_text(size = 15),
              axis.title.x = element_text(size = 15))
      
      i = i + 1
      
    }
  }
  
  mids = list(); j = 1
  dfs = list(); i = 1
  edfs = list()
  
  midpoints = c("40000", "50000", "60000", "70000")
  
  for(midpoint in midpoints){
    for(s in scenarios){
      simulated_m = colnames(lists[[s]])
      recovered_m = lists[[s]][midpoint,] %>% as.vector %>% as.numeric
      
      esimulated_m = colnames(elists[[s]])
      erecovered_m = elists[[s]][midpoint,] %>% as.vector %>% as.numeric
      
      
      dfs[[s]] = data.frame(sit = s, 
                        simulated_m = simulated_m,
                        recovered_m = recovered_m,
                        source = "a",
                        sim = 'sim',
                        mid = midpoint)
      
      edfs[[s]] = data.frame(sit = s, 
                            simulated_m = esimulated_m,
                            recovered_m = erecovered_m,
                            source = "e",
                            sim = "sim",
                            mid = midpoint)
      i = i + 1
  }
  
  # Add in truth lines
  bind_rows(dfs) %>% mutate(recovered_m = 0.543, source = "a", sim = "real", mid = "YRI-Han Inference") -> bm
  bind_rows(dfs) %>% mutate(recovered_m = 0.13, source = "e", sim = "real", mid = "YRI-Han Inference") -> fm
  
  df = bind_rows(dfs, edfs, bm, fm) %>% mutate(simulated_m = as.factor(simulated_m), sit = as.factor(sit)) 
  levels(df$sit) <- c("Backward", "Symmetric", "Forward")
  
    mids[[j]] = df
    j = j + 1
  
  }
  
  df = bind_rows(mids)
  df$source <- as.factor(df$source)
  levels(df$source) =  c("Recovered Backwards", "Recovered Forwards")
  
  p <- ggplot(df, aes(x = simulated_m, y = recovered_m, group = interaction(source, sim, mid), col= interaction(mid), sim), lty = as.factor(sim)) + 
    geom_point(aes(size = sim)) + 
    geom_line(aes(lty = sim)) + 
    facet_grid(source~sit) + 
    scale_linetype_manual(values = c(5, 1), labels = c("SGDP Yoruban-Han", "Simulation")) +
    scale_color_manual(values = c("blue", "green", "red", "brown", "black"), breaks = c("40000", "50000", "60000", "70000")) + 
    scale_size_manual(values = c(-1, 2), guide = F) +
    theme_bw() + 
    ylab("Recovered IMF") +
    xlab("Simulated IMF") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top",
          aspect.ratio = 1,
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.title = element_blank())
  
  return(p)
  
}

#p <- ggplot(df, aes(x=simulated_m, y = recovered_m, group = interaction(source,sim), col = mid, lty = source)) + 
#  geom_point(aes(size = sim)) + 
#  geom_line() + 
#  ylim(c(0, 0.6)) +
#  facet_wrap(~sit) + 
#  #scale_color_manual(values = c("red", "blue"), labels = c("Backwards", "Forward")) +
  #scale_linetype_manual(values = c(2, 1), labels = c("SGDP Yoruban-Han", "Simulation")) +
  #scale_size_manual(values = c(-1, 2), guide = F) +
#  xlab("Simulated True Migration") + 
#  ylab("Recovered Migration") +
#  theme_bw() + 
#  theme(axis.text.x = element_text(angle = 45, hjust = 1),
#        legend.position = "top",
#        aspect.ratio = 1,
##        panel.grid.major.x = element_blank(),
#        panel.grid.minor.x = element_blank(),
#        legend.title = element_blank())

#ggsave(p, file = "~/repos/dirmig/plot/sim_line_plot.pdf", h = 6, w = 10)
