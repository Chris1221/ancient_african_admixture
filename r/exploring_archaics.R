library(smcsmcTools)


# So pop 1 is neanderthal
# So the migration from pop 1 to pop 0 is backwards in time, forwards in time is migration from CEU to neanderthal
#   So the migration plot makes sense...
age = 0
xlim = c(1e4, 1e6)
vind = list()
yrivind <- smcsmc("~/repos/dirmig/data/adna/yri-vind.out")
vind[[2]] <- plot(yrivind, pops = c("Yoruban", "Vindija Neanderthal"), times = c(0, age), xlim = xlim)  + geom_vline(xintercept = 40e3, lty = 2, col = "blue") + geom_vline(xintercept = 150e3, lty = 2, col = "blue") + geom_vline(xintercept = 120e3, col = "red", lty = 2)
vind[[1]] <-plot(yrivind, type = "ne", ylim = c(1e2, 1e5), xlim = xlim, pops = c("Yoruban", "Vindija Neanderthal"), times = c(0, age))

# Larger, more distinct introgression in Eurasians, which makes sense
ceuvind <- smcsmc("~/repos/dirmig/data/adna/ceu-vind.out")
vind[[4]] <- plot(ceuvind, xlim = xlim, times = c(age, age))  + geom_vline(xintercept = 40e3, lty = 2, col = "blue") + geom_vline(xintercept = 150e3, lty = 2, col = "blue") + geom_vline(xintercept = 120e3, col = "red", lty = 2) 
vind[[3]] <- plot(ceuvind, type = "ne", ylim = c(1e2, 1e5), xlim = xlim, times = c(0, age))

# This one is VERY STRANGE, clearly something has gone very wrong
hanvind <- smcsmc("~/repos/dirmig/data/adna/han-vind.out")
vind[[6]] <- plot(hanvind, ylim = c(0, 1), xlim = xlim, times = c(age, age))
vind[[5]] <- plot(hanvind, type = "ne", ylim = c(1e2, 1e5), xlim = xlim, times = c(0, age))

papvind <- smcsmc("~/repos/dirmig/data/adna/pap-vind.out")
vind[[8]] <- plot(papvind, xlim = xlim, times = c(age, age))   + geom_vline(xintercept = 40e3, lty = 2, col = "blue") + geom_vline(xintercept = 150e3, lty = 2, col = "blue") + geom_vline(xintercept = 120e3, col = "red", lty = 2)
vind[[7]] <- plot(papvind, type = "ne", ylim = c(1e2, 1e5), times = c(0, age), xlim = xlim)
ggmatrix(vind, nrow =2, ncol = 4, byrow = F, xAxisLabels = c("Yoruba", "French", "Han", "Papuan"), title = "Vindija Neanderthal")
ggsave("~/repos/dirmig/plot/adna/vind.pdf", height = 8, width = 14)


# THe UI things are very strange, extraordinarily high rates of migration
age = 40e3
ui <- list()
yriui <- smcsmc("~/repos/dirmig/data/adna/yri-ui.out")
ui[[2]] <- plot(yriui, ylim = c(0, 5e-3), xlim = c(1e4, 1e6), times = c(age, age)) 
ui[[1]] <- plot(yriui, ylim = c(1e2, 1e6), type = "ne", times = c(0, age))

sanui <- smcsmc("~/repos/dirmig/data/adna/san-ui.out")
ui[[4]] <- plot(sanui, ylim = c(0, 5e-3), xlim = c(1e4, 1e6), times = c(age, age))
ui[[3]] <- plot(sanui, ylim = c(1e3, 1e6), type = "ne", times = c(0, age))
ggmatrix(ui, nrow =2, ncol = 2, byrow = F, xAxisLabels = c("Yoruba", "Khomani San"), title = "Ust-Ishim Man")

