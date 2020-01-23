library(dplyr)
library(data.table)
library(ggplot2)
library(magrittr)
library(GGally)

pop0 = "Pop1"
pop1 = "Pop2"
main = ""
plot_diff = F
plotly = F
xlim = c(1e4, 1e5, 1e6)
labels = c("10", "100","1000")
ylim = c(8,11)
t0 = 10000
t1 = 1500000
return = F
previous = NULL

dur <- c(30000)
prop <- c(0.5, 0.8, 1.25)
mid <- c(60000)

all_path <- "~/repos/eurasian-backmigration/data/simulations_fixed/"
single_path <- "~/repos/eurasian-backmigration/data/li_and_durbin/"
truth_path <- "~/repos/eurasian-backmigration/data/simulations_truth/"



plots <- list()
i = 1

for(d in dur){
  for(pr in prop){
    for(m in mid){
      name = paste0(m, "_", d, "_", pr)
      name1 = paste0(name, ".out")
      name2 = paste0(name, "_dip1.out")
      df1 <- fread(paste0(all_path, name1)) %>% filter(Iter == max(Iter))
      df2 <- fread(paste0(single_path, name2)) %>% filter(Iter == max(Iter))
      df2$From = 2
      truth <- fread(paste0(truth_path, name, ".csv"))
      
      for( j in 1:nrow(truth)){
        tryCatch({
          truth$End[j] = truth$time[j+1]
        }, error = function(e) {
          truth$End[j] = NA
        })
      }
      
      df <- rbind(df1, df2)
      
      df <- df %>% 
        filter(Type == "Coal") %>% 
        filter(Clump== -1)
      
      g = 29
      df$From <- as.factor(df$From)
      p <- ggplot() + 
        geom_step(
          data = df,
          aes(x = Start*g, 
              y = Ne, 
              group = From, 
              col = From,
              linetype = From)) + 
        geom_step(data = truth,
                  aes(x = End*g,
                      y = ceu_ne*N0,
                      color = "black"))+
        geom_step(data = truth,
                  aes(x = End*g,
                      y = yri_ne*N0,
                      color = "black")) +
        scale_x_log10(limits=c(1e4, 1e6), breaks = c(1e4, 1e5, 1e6), labels = c("10", "100", "1000")) + 
        scale_y_continuous(trans = 'log10', limits = c(3e3, 1e5)) +
        xlab("Time (thousands of years)") + 
        ylab("Effective Population Size") + 
        scale_linetype_manual(values=c(1,1,4,1,1), guide=FALSE)+
        scale_color_manual(values = c('blue', 'red', 'red', 'darkgrey', 'darkgrey'), labels = c("Eurasian", "African", "One-Population African", 'Truth', 'CEU Truth')) + 
        
        theme_light() +
        theme(legend.position = "bottom", legend.title = element_blank(), legend.spacing.x = unit(0.5, 'cm')) +
        guides(color = guide_legend(override.aes = list(linetype = c(1, 1, 4, 1))))
      
      plots[[i]] <- p
      i = i +1
      
    } 
  }
}

leg <- grab_legend(plots[[1]])

pm <- ggmatrix(
  plots,
  nrow = 1,
  ncol = 3,
  c(0.4, 0.55, 0.71),
  ylab = "Effective Population Size",
  xlab = "Thousands of Years before Present",
  byrow= T,
  legend = leg
)

pm + theme(legend.position = "bottom", legend.title = element_blank(), panel.grid = element_blank())

ggsave("~/repos/dirmig/plot/ne/PLACEHOLDER_sims.pdf", height = 4.22, width = 8.93, units = "in")





#ggsave("~/repos/eurasian-backmigration/plot/li_and_durbin.png", height = 7, width = 7)
