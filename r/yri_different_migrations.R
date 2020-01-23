# Just plotting 5 examples that we have now for the various models. 
library(smcsmcTools)
library(data.table)
library(dplyr)
library(ggplot2)
library(GGally)

avg_migr <- function(
  file,
  ancient,
  modern,
  g
){
  # Checks and imports 
  `%>%` <- dplyr::`%>%`
  
  assertthat::assert_that(  
    is.character(file) &&
      is.numeric(g) && 
      is.numeric(ancient) && 
      is.numeric(modern))
  
  # Read and set parameters. 
  df <- data.table::fread(file, h = T) %>%
    dplyr::filter(Type == "Migr") %>% 
    dplyr::filter(Iter == max(Iter)) %>% 
    dplyr::filter(Start < (ancient / g)) %>%
    dplyr::filter(End > (modern / g)) %>%
    dplyr::mutate(Diff = End - Start) %>%
    dplyr::mutate(totalepoch = Rate * Diff) %>%
    dplyr::group_by(From) %>%
    dplyr::summarise(sum = sum(totalepoch), integrated = 1-exp(-sum(totalepoch)))
  
  return(df)
  
}

scenarios = c("backward")#, "backward")#, "bidirectional", "realistic")
plots <- list()
i <- 1

#migs =c("S_Khomani_San-1", "S_Han-1", "S_French-1", "S_Karitiana-1", "S_Yoruba-1" )
migs = c("0.0", "0.5", "1.0", "2.0", "4.0", "5.0", "10.0")
#migs_yri = c("S_Papuan-1", "S_Papuan-8", "S_Papuan-11", "S_Mixe-2")
migs_yri = c(1000, 5000, 10000)

xlim = c(1e4, 1e5, 1e6)
labels = c("10", "100","1000")
type = c("Coal", "Migr")
ylim = c(0, 5e-4)
g = 30
for(t in ("Coal")){
  for (s in scenarios){
    for(mid in migs){
      for(mig in migs_yri){
        #path <- paste0("~/repos/eurasian-backmigration/data/papuan_low_mig/1791095846.", mid, ".", mig)
        path <- paste0("~/repos/eurasian-backmigration/data/yrisymig/yri_", mid, "_", mig)
        df <- fread(paste0(path, ".out"), h = T)
        
        if(t == "Migr"){
        
          #truth <- fread(paste0("~/repos/eurasian-backmigration/data/simulations_spvaryingmig/", s, "_60000_10000_", mig, ".csv"), h = T)
          #df$Start[df$Start == 0] <- 1
          #df$Rate[df$Rate > max(ylim)] <- max(ylim)
          total_mig = avg_migr(paste0(path, ".out"), ancient = 100000, modern = 1, g = 29)
          ceu_mig = as.character(round(total_mig$integrated[total_mig$From == 1], 3))
          yri_mig = as.character(round(total_mig$integrated[total_mig$From == 0], 3))
          p <-ggplot(data = df %>% filter(Type == "Migr") %>% filter(Iter == max(Iter))  %>% filter(Clump == -1)) + 
            #geom_step(data = truth,
            #          aes(x = time*29,
            #              y = ceu_ne  * 14312), col = "grey") +
            #geom_step(data = truth,
            #          aes(x = time*29, 
            #              y = yri_ne * 14312), col = "grey") + 
            geom_step(
              aes(x = Start*g, 
                  y = Rate, 
                  group = factor(From), 
                  col = factor(From) )) +
            #geom_vline(xintercept = 60e3, col = "red", linetype = "dotted" ) + 
            scale_x_log10(limits=c(min(xlim),max(xlim)),breaks=xlim, labels = labels) + 
            #scale_y_log10(limits = c(1e3,3e4)) + 
            scale_y_continuous(limits = ylim) +
            xlab("Time (thousands of years)") + 
            #annotate("text", x = 30000, y = 4.75e-4, label = paste0("sum(M[1/0]) == ", yri_mig), col = "#002147", parse = TRUE) +
            #annotate("text", x = 250000, y = 4.75e-4, label = paste0("sum(M[0/1]) == ", ceu_mig), col = "green", parse = TRUE) + 
            ylab("Mij (% Population / Generation)") + 
            scale_color_manual(values = c('blue', 'red', 'grey', 'black'), labels = c("CEU to YRI", "YRI to CEU", "Simulated YRI to CEU Migration")) + 
            labs(color = "") +
            theme_bw() +
            theme(legend.position = "bottom") +
            theme(strip.text.x = element_text(size = 12),
              strip.text.y = element_text(size = 12))
          plots[[i]] <- p
          i = i+1
        
        } else if(t == "Coal"){
          ylim = c(1e3, 3e4)
          df$Ne[df$Ne > max(ylim)] <- max(ylim)
          
          
          #truth <- fread(paste0("~/repos/eurasian-backmigration/data/simulations_spvaryingmig/", s, "_60000_10000_", mig, ".csv"), h = T)
          #df$Start[df$Start == 0] <- 1
          #df$Rate[df$Rate > max(ylim)] <- max(ylim)
          p <-ggplot(data = df %>% filter(Type == t) %>% filter(Iter == max(Iter))  %>% filter(Clump == -1)) + 
            #geom_step(data = truth,
            #          aes(x = time*29,
            #              y = ceu_ne  * 14312), col = "grey") +
            #geom_step(data = truth,
            #          aes(x = time*29, 
            #              y = yri_ne * 14312), col = "grey") + 
            geom_step(
              aes(x = Start*g, 
                  y = Ne, 
                  group = factor(From), 
                  col = factor(From) )) +
            #geom_vline(xintercept = 120e3, col = "red", linetype = "dotted" ) + 
            #geom_vline(xintercept = 150e3, col = "blue", linetype = "dotted") + 
            #annotate(geom = "text", x = 300e3, y = 3000, label = "120kya", col = "red") +
            #annotate(geom = "text", x = 300e3, y = 2000, label = "150kya", col = "blue") +
            scale_x_log10(limits=c(min(xlim),max(xlim)),breaks=xlim, labels = labels) + 
            scale_y_log10(limits = c(3e3, 3e4)) + 
            xlab("Time (thousands of years)") + 
            ylab("Mij (% Population / Generation)") + 
            scale_color_manual(values = c('blue', 'red', 'grey', 'black'), labels = c("CEU to YRI", "YRI to CEU", "Simulated YRI to CEU Migration")) + 
            labs(color = "") +
            theme_bw() +
            theme(legend.position = "bottom") +
            theme(strip.text.x = element_text(size = 12),
                  strip.text.y = element_text(size = 12))

          plots[[i]] <- p
          i = i+1
        }
      }
    }
  }
}
ggmatrix(plots, nrow = 3, ncol = 7, byrow = F, yAxisLabels = migs_yri, ylab = "Effective Population Size", xlab = "Thousands of Years before Present", xAxisLabels = migs)
#ggsave("~/repos/dirmig/plot/mig/yri_different_starting_migs.pdf", height = 5.61, width = 15.7, units = "in")
ggsave("~/repos/dirmig/plot/mig/yri_different_starting_migs_ne.pdf", height = 5.61, width = 15.7, units = "in")

# 
# ### TEMP
# if(F){
# msmc <- fread("~/repos/eurasian-backmigration/data/msmc/debug.final.txt", sep = "\t", head = T)
# msmc$rel <- 2 * (msmc$lambda_01 / (msmc$lambda_00 + msmc$lambda_11))
# 
# rmin = 0
# rmax = 1
# tmax = 1e6
# tmin = 1e3
# msmc %>% mutate(scaled_rel = ((rel - rmin)/(rmax-rmin)) * (tmax - tmin) + tmin) -> msmc
# msmc <- melt(msmc, id.vars = c("time_index", "left_time_boundary", "right_time_boundary"))
# 
# truth <- fread("~/repos/eurasian-backmigration/data/scenario_simulations/forward_60000_30000_1.25.csv")
# 
# truth %>% select(time, ceu_ne, yri_ne) -> truth2
# truth2 <- melt(truth2, id.vars = "time")
# ggplot(msmc %>% filter(variable == c("lambda_00", "lambda_11")), aes(x = (left_time_boundary / 1.25e-8) * 29)) + 
#   geom_step(aes(y = (1/ value) / (2*1.25e-8), col = variable)) +
#   geom_step(aes(x = time*29, y = value*14312, col = variable), data = truth2) + 
#   geom_step(aes(y = value, col = "green"), data = msmc %>% filter(variable == "scaled_rel")) +
#   scale_y_log10(limits = c(1e3, 3e6)) + 
#   scale_color_manual(values = c("darkgrey", "green", "blue", "red", "grey"), labels = c("CEU Truth", "(scaled) Relative x-Coal", "MSMC CEU", "MSMC YRI", "YRI Truth")) +
#   #scale_linetype_manual(values = c(1,10,1,1,1)) +
#   scale_x_log10(limits = c(1e4, 1e6)) + 
#   ggtitle("Estimated effective pop size") + 
#   ylab("Population Size") + 
#   xlab("Time") + 
#   theme_bw() +   theme(legend.position = "bottom", legend.title = element_blank()) 
# 
# ## With the smc2 inference
# ## 
# ## 
# ## 
# path <- paste0("~/repos/eurasian-backmigration/data/scenario_simulations/forward_60000_30000_1.25")
# df <- fread(paste0(path, ".out"), h = T) %>%
#   filter(Iter == max(Iter)) %>% filter(Type == "Coal")
# ggplot(msmc %>% filter(variable == c("lambda_00", "lambda_11")), aes(x = (left_time_boundary / 1.25e-8) * 29)) + 
#   geom_step(aes(y = (1/ value) / (2*1.25e-8), col = variable)) +
#   geom_step(aes(y = Ne, x = Start*29, col = factor(From)), data = df) + 
#   geom_step(aes(x = time*29, y = value*14312, col = variable), data = truth2) + 
#   scale_y_log10(limits = c(1e3, 3e6)) + 
#   scale_color_manual(values = c("blue", "green", "black", "red", "pink", "black")) + #, labels = c("SMC2 CEU", "SMC2 YRI", "CEU Truth", "MSMC CEU", "MSMC ", "YRI Truth")) +
#   #scale_linetype_manual(values = c(1,10,1,1,1)) +
#   scale_x_log10(limits = c(1e4, 1e6)) + 
#   ggtitle("Estimated effective pop size") + 
#   ylab("Population Size") + 
#   xlab("Time") + 
#   theme_bw() +   theme(legend.position = "bottom", legend.title = element_blank()) 
# 
# ## Iterations
# ## 
# df <- fread(paste0(path, ".out"), h = T) 
# 
# ggplot(df %>% filter(Type == "Migr") %>% filter(From == 1) %>% filter(Iter > 10), aes(x = Start*29, y = Rate, col = factor(Iter))) + 
#   scale_color_brewer() + 
#   geom_step() + 
#   scale_x_log10() + 
#   scale_y_continuous(limits = c(0, 4e-4))
# }
#  