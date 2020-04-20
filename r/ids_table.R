# Supplemental figure

library(smcsmcTools)
library(rjson)
library(GGally)
library(dplyr)
library(cowplot)
library(grid)
library(gridExtra)
library(ggplot2)
library(xtable)


config = fromJSON(file = "~/repos/dirmig/analyses/whole_sgdp.json")
hgdp_config <- fromJSON(file = "~/repos/dirmig/analyses/hgdp_physically_phased.json")


source = names_from_config(config$source)
sink = names_from_config(config$sink)

hsource = names_from_config(hgdp_config$source)
hsink = names_from_config(hgdp_config$sink)

df = data.frame( Name = ids_to_names(c(source, sink)), ID = c(source, sink), Source = "SGDP", stringsAsFactors = F)
hdf = data.frame(Name = ids_to_names(c(hsource, hsink), "hgdp"), ID = c(hsource, hsink), Source = "HGDP", stringsAsFactors = F)

odf <- rbind(df, hdf)

odf <- rbind(odf, 
             c("Vindija", "Vindija.DG", "Pruefer et al. 2017"),
             c("Altai", "Altai_published.DG", "Pruefer et al. 2013"),
             c("Denisovan", "Deniosva_published.DG", "Myers et al 2012"))

label = "samples"
caption = "Sample IDs of the individuals used in this article and relevant resources. SGDP = Simons Genome Diversity Project, HGDP = Human Genome Diversity Panel."

print(xtable(odf, label = label, caption = caption), include.rownames = F)
