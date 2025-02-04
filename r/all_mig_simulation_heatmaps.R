# Just plotting 5 examples that we have now for the various models. 
library(smcsmcTools)
library(data.table)
library(dplyr)
library(ggplot2)
library(GGally)

scenarios = c("backward", "forward", "bidirectional")#, "backward")#, "bidirectional", "realistic")
mplots <- list()
neplots <- list()
i <- 1

migs = c("0.0", "0.1", "0.3", "0.5", "0.7", "0.9")
situations = c(0,2,4)
midpoints <- c("40000", "50000", "60000", "70000")

g = 29
matrices <- list()

m <- list()
ne = list()
j = 1

mat <- matrix(, nrow = length(midpoints), ncol = length(migs))
emat <- matrix(, nrow = length(midpoints), ncol = length(migs))


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
  p <- ggmatrix(c(plots, eplots), nrow=3, ncol = 2, byrow = F, xlab = "Amount of Migration (Proportion Replaced per Generation)", ylab = "Time of Migration (years before present)", yAxisLabels = c("Backwards", "Forwards", "Bidirectional"), xAxisLabels = c("Inferred Backwards Migration", "Inferred Forwards Migration")) + theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12))
  ggsave(p, file = paste0("~/repos/dirmig/plot/sims/recovered_migration_", sit, ".pdf"), height = 4, width = 10, units = "in")
}
