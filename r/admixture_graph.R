library(smcsmcTools)
library(admixturegraph)

dstats <- f4stats(read.table("~/repos/dirmig/data/dstats/graph2.txt", h = T, stringsAsFactors = F))      
colnames(dstats) = c("W", "X", "Y", "Z", "D", "StdErr", "Z.value", "na", "nb", "nt")
dstats[dstats=="Vindija.DG"] = "S_Vindija-1.DG"
names <- dstats %>% mutate(W = ids_to_names(W), Y = ids_to_names(Y), X = ids_to_names(X), Z=ids_to_names(Z))
names[names == "Khomani San"] = "San"
dstats = f4stats(names)

## Plot with admixture
leaves = c("Vindija",
           "French", 
           "Han",
           "Mbuti",
           "Biaka",
           "Yoruba",
           "San")
inner_nodes = c("archaicSplit",
                "OoA",
                "eastWestSplit",
                "sanSplit",
                "ghostSplit",
                "mbutiSplit",
                "pygmySplit",
                "backMigration")
edges <- parent_edges(c(edge("Mbuti", "mbutiSplit"),
                        edge("Biaka", "mbutiSplit"),
                        edge("mbutiSplit", "pygmySplit"),
                        edge("OoA", "pygmySplit"),
                        edge("pygmySplit", "sanSplit"),
                        edge("San", "sanSplit"),
                        edge("sanSplit", "archaicSplit"),
                        #edge("backMigration", "OoA"),
                        edge("Yoruba", "backMigration"),
                       edge("ghostSplit", "OoA"),
                       edge("eastWestSplit", "ghostSplit"),
                        #edge("ghostDonation", "ghostSplit"),
                        edge("French", "eastWestSplit"),
                        edge("Han", "eastWestSplit"),
                        #edge("eastWestSplit", "ghostSplit"),
                        edge("Vindija", "archaicSplit"),
                        admixture_edge("backMigration", "ghostSplit", "OoA", "alpha")))

just_bm <- agraph(leaves, inner_nodes, edges)
plot(just_bm, title = "With Eurasian Admixture")

f_just_bm <- fit_graph(dstats, just_bm)

## Error = 9307, cant fit alpha


leaves = c("Vindija",
           "French", 
           "Han",
           "Mbuti",
           "Biaka",
           "Yoruba",
           "San")
inner_nodes = c("archaicSplit",
                "OoA",
                "eastWestSplit",
                "sanSplit",
                "mbutiSplit",
                "pygmySplit")
edges <- parent_edges(c(edge("Mbuti", "mbutiSplit"),
                        edge("Biaka", "mbutiSplit"),
                        edge("mbutiSplit", "pygmySplit"),
                        edge("OoA", "pygmySplit"),
                        edge("pygmySplit", "sanSplit"),
                        edge("San", "sanSplit"),
                        edge("sanSplit", "archaicSplit"),
                        #edge("backMigration", "OoA"),
                        edge("Yoruba", "OoA"),
                        #edge("ghostSplit", "OoA"),
                        edge("eastWestSplit", "OoA"),
                        #edge("ghostDonation", "ghostSplit"),
                        edge("French", "eastWestSplit"),
                        edge("Han", "eastWestSplit"),
                        #edge("eastWestSplit", "ghostSplit"),
                        edge("Vindija", "archaicSplit")))
                        #admixture_edge("backMigration", "ghostSplit", "OoA", "alpha")))

no_bm <- agraph(leaves, inner_nodes, edges)
plot(no_bm, title = "No admixture")

f_no_bm <- fit_graph(dstats, just_bm) # error 9307


### Incorporating archaic admixture....
leaves = c("Vindija",
           "French", 
           "Han",
           "Mbuti",
           "Biaka",
           "Yoruba",
           "San")
inner_nodes = c("archaicSplit",
                "OoA",
                "eastWestSplit",
                "sanSplit",
                "ghostSplit",
                "mbutiSplit",
                "pygmySplit",
                "ndthAdmix",
                "ndthDonation",
                "backMigration")
edges <- parent_edges(c(edge("Mbuti", "mbutiSplit"),
                        edge("Biaka", "mbutiSplit"),
                        edge("mbutiSplit", "pygmySplit"),
                        edge("OoA", "pygmySplit"),
                        edge("pygmySplit", "sanSplit"),
                        edge("San", "sanSplit"),
                        edge("sanSplit", "archaicSplit"),
                        #edge("backMigration", "OoA"),
                        edge("Yoruba", "backMigration"),
                        edge("ghostSplit", "OoA"),
                        edge("eastWestSplit", "ndthAdmix"),
                        #edge("ghostDonation", "ghostSplit"),
                        edge("French", "eastWestSplit"),
                        edge("Han", "eastWestSplit"),
                        #edge("eastWestSplit", "ghostSplit"),
                        edge("Vindija", "ndthDonation"),
                        edge("ndthDonation","archaicSplit"),
                        admixture_edge("backMigration", "ghostSplit", "OoA", "alpha"),
                        admixture_edge("ndthAdmix", "ghostSplit", "ndthDonation", "beta")))

aa <- agraph(leaves, inner_nodes, edges)
plot(aa, title = "Eurasian Admixture with Neanderthals")

f_aa <- fit_graph(dstats, aa)


### With recent Biaka admixture
leaves = c("Vindija",
"French", 
"Han",
"Mbuti",
"Biaka",
"Yoruba",
"San")
inner_nodes = c("archaicSplit",
                "OoA",
                "eastWestSplit",
                "sanSplit",
                "ghostSplit",
                "mbutiSplit",
                "pygmySplit",
                "biakaSink",
                "biakaSource",
                "ndthAdmix",
                "ndthDonation",
                "backMigration")
edges <- parent_edges(c(edge("Mbuti", "mbutiSplit"),
                        edge("Biaka", "biakaSink"),
                        edge("mbutiSplit", "pygmySplit"),
                        edge("OoA", "pygmySplit"),
                        edge("pygmySplit", "sanSplit"),
                        edge("San", "sanSplit"),
                        edge("sanSplit", "archaicSplit"),
                        #edge("backMigration", "OoA"),
                        edge("Yoruba", "biakaSource"),
                        edge("biakaSource", "backMigration"),
                        edge("ghostSplit", "OoA"),
                        edge("eastWestSplit", "ndthAdmix"),
                        #edge("ghostDonation", "ghostSplit"),
                        edge("French", "eastWestSplit"),
                        edge("Han", "eastWestSplit"),
                        #edge("backMigration", "biakaSource"),
                        #edge("biakaSource", "OoA"),
                        #edge("eastWestSplit", "ghostSplit"),
                        edge("Vindija", "ndthDonation"),
                        edge("ndthDonation","archaicSplit"),
                        admixture_edge("backMigration", "ghostSplit", "OoA", "alpha"),
                        admixture_edge("ndthAdmix", "ghostSplit", "ndthDonation", "beta"),
                        admixture_edge("biakaSink", "biakaSource", "mbutiSplit", "gamma")))

w_biaka <- agraph(leaves, inner_nodes, edges)
plot(w_biaka, title = "Back Migration with Neanderthal and Biaka admixture", show_admixture_labels = T)

f_w_biaka <- fit_graph(dstats, w_biaka) # 1812.498


# Same thing but without the edge
leaves = c("Vindija",
           "French", 
           "Han",
           "Mbuti",
           "Biaka",
           "Yoruba",
           "San")
inner_nodes = c("archaicSplit",
                "OoA",
                "eastWestSplit",
                "sanSplit",
                "ghostSplit",
                "mbutiSplit",
                "pygmySplit",
                "biakaSink",
                "biakaSource",
                "ndthAdmix",
                "ndthDonation",
                "backMigration")
edges <- parent_edges(c(edge("Mbuti", "mbutiSplit"),
                        edge("Biaka", "biakaSink"),
                        edge("mbutiSplit", "pygmySplit"),
                        edge("OoA", "pygmySplit"),
                        edge("pygmySplit", "sanSplit"),
                        edge("San", "sanSplit"),
                        edge("sanSplit", "archaicSplit"),
                        #edge("backMigration", "OoA"),
                        edge("Yoruba", "backMigration"),
                        edge("ghostSplit", "OoA"),
                        edge("eastWestSplit", "ndthAdmix"),
                        #edge("ghostDonation", "ghostSplit"),
                        edge("French", "eastWestSplit"),
                        edge("Han", "eastWestSplit"),
                        edge("backMigration", "biakaSource"),
                        edge("biakaSource", "OoA"),
                        #edge("eastWestSplit", "ghostSplit"),
                        edge("Vindija", "ndthDonation"),
                        edge("ndthDonation","archaicSplit"),
                        #admixture_edge("backMigration", "ghostSplit", "biakaSource", "alpha"),
                        admixture_edge("ndthAdmix", "ghostSplit", "ndthDonation", "beta"),
                        admixture_edge("biakaSink", "biakaSource", "mbutiSplit", "gamma")))

w_biaka_no_bm <- agraph(leaves, inner_nodes, edges)
plot(w_biaka_no_bm, title = "Neanderthal and Biaka Admixture without Backmigration")

f_w_biaka_no_bm <- fit_graph(dstats, w_biaka_no_bm) #2100


# Adding recent admixture with vindija
leaves = c("Vindija",
           "French", 
           "Han",
           "Mbuti",
           "Biaka",
           "Yoruba",
           "San")
inner_nodes = c("archaicSplit",
                "OoA",
                "ndthEarly",
                "laterNdth",
                "eastWestSplit",
                "sanSplit",
                "ghostSplit",
                "mbutiSplit",
                "pygmySplit",
                "biakaSink",
                "biakaSource",
                "ndthAdmix",
                "ndthDonation",
                "backMigration")
edges <- parent_edges(c(edge("Mbuti", "mbutiSplit"),
                        edge("Biaka", "biakaSink"),
                        edge("mbutiSplit", "pygmySplit"),
                        edge("OoA", "pygmySplit"),
                        edge("pygmySplit", "sanSplit"),
                        edge("San", "sanSplit"),
                        edge("sanSplit", "archaicSplit"),
                        #edge("backMigration", "OoA"),
                        edge("Yoruba", "backMigration"),
                        edge("ghostSplit", "OoA"),
                        edge("eastWestSplit", "ndthAdmix"),
                        #edge("ghostDonation", "ghostSplit"),
                        edge("French", "eastWestSplit"),
                        edge("Han", "laterNdth"),
                        edge("backMigration", "biakaSource"),
                        edge("biakaSource", "OoA"),
                        #edge("eastWestSplit", "ghostSplit"),
                        edge("ndthEarly", "ndthDonation"),
                        edge("Vindija", "ndthEarly"),
                        edge("ndthDonation","archaicSplit"),
                        admixture_edge("backMigration", "ghostSplit", "biakaSource", "alpha"),
                        admixture_edge("ndthAdmix", "ghostSplit", "ndthDonation", "beta"),
                        admixture_edge("biakaSink", "biakaSource", "mbutiSplit", "gamma"),
                        admixture_edge("laterNdth", "ndthEarly", "eastWestSplit", "kappa")))

w_biaka_late_ndth <- agraph(leaves, inner_nodes, edges)
plot(w_biaka_late_ndth, title = "Later Neanderthal and Biaka Admixture without Backmigration", show_admixture_labels = T)

f_w_biaka_late_ndth <- fit_graph(dstats, w_biaka_late_ndth) 


# No backmigration but two pulses of neanderthal admixture and recent biaka.
leaves = c("Vindija",
           "French", 
           "Han",
           "Mbuti",
           "Biaka",
           "Yoruba",
           "San")
inner_nodes = c("archaicSplit",
                "OoA",
                "ndthEarly",
                "laterNdth",
                "eastWestSplit",
                "sanSplit",
                "ghostSplit",
                "mbutiSplit",
                "pygmySplit",
                "biakaSink",
                "biakaSource",
                "ndthAdmix",
                "ndthDonation",
                "backMigration")
edges <- parent_edges(c(edge("Mbuti", "mbutiSplit"),
                        edge("Biaka", "biakaSink"),
                        edge("mbutiSplit", "pygmySplit"),
                        edge("OoA", "pygmySplit"),
                        edge("pygmySplit", "sanSplit"),
                        edge("San", "sanSplit"),
                        edge("sanSplit", "archaicSplit"),
                        #edge("backMigration", "OoA"),
                        edge("Yoruba", "backMigration"),
                        edge("ghostSplit", "OoA"),
                        edge("eastWestSplit", "ndthAdmix"),
                        #edge("ghostDonation", "ghostSplit"),
                        edge("French", "eastWestSplit"),
                        edge("Han", "laterNdth"),
                        edge("backMigration", "biakaSource"),
                        edge("biakaSource", "OoA"),
                        #edge("eastWestSplit", "ghostSplit"),
                        edge("ndthEarly", "ndthDonation"),
                        edge("Vindija", "ndthEarly"),
                        edge("ndthDonation","archaicSplit"),
                        #admixture_edge("backMigration", "ghostSplit", "biakaSource", "alpha"),
                        admixture_edge("ndthAdmix", "ghostSplit", "ndthDonation", "beta"),
                        admixture_edge("biakaSink", "biakaSource", "mbutiSplit", "gamma"),
                        admixture_edge("laterNdth", "ndthEarly", "eastWestSplit", "kappa")))

w_biaka_no_bm_late_ndth <- agraph(leaves, inner_nodes, edges)
plot(w_biaka_no_bm_late_ndth, title = "No backmigration but 2 ndth, late biaka", show_admixture_labels = T)

f_w_biaka_no_bm_late_ndth <- fit_graph(dstats, w_biaka_no_bm_late_ndth) 
