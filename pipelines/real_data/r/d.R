.libPaths( c( .libPaths(), "~/R/lib64/R/library/") )

args = commandArgs(T)

library(dplyr)
library(admixr)

setwd("~/well/eigenstrat/")

pop = args[1]
eur_subset = args[2]

filter <- eigenstrat("sgdp")

#filter <- eigenstrat("small-sgdp")
modif_snps <- relabel(
	filter,
	Chane = c("S_Chane-1"),
	Karitiana = c("A_Karitiana-4", "B_Karitiana-3", "S_Karitiana-1", "S_Karitiana-2"),
	Mayan = c("S_Mayan-1", "S_Mayan-2"),
	Mixe = c("B_Mixe-1", "S_Mixe-2", "S_Mixe-3"),
	Mixtec = c("S_Mixtec-1", "S_Mixtec-2"),
	Piapoco = c("S_Piapoco-1", "S_Piapoco-2"),
	Pima = c("S_Pima-1", "S_Pima-2"),
	Quechua = c("S_Quechua-1", "S_Quechua-2", "S_Quechua-3"),
	Surui = c("S_Surui-1", "S_Surui-2"),
        Zapotec = c("S_Zapotec-1", "S_Zapotec-2"),
        Aleut = c("S_Aleut-1", "S_Aleut-2"),
        Altaian = c("S_Altaian-1"),
        Chukchi = c("S_Chukchi-1"),
	Eskimo_Chaplin = c("S_Eskimo_Chaplin-1"),
	Eskimo_Naukan = c("S_Eskimo_Naukan-1", "S_Eskimo_Naukan-2"),
       	Eskimo_Sireniki = c("S_Eskimo_Sireniki-1", "S_Eskimo_Sireniki-2"),
        Even = c("S_Even-1", "S_Even-2", "S_Even-3"),
        Itelman = c("S_Itelman-1"),
	Kyrgyz = c("S_Kyrgyz-1", "S_Kyrgyz-2"),
        Mansi = c("S_Mansi-1", "S_Mansi-2"),
        Mongola = c("S_Mongola-1", "S_Mongola-2"),
        Tlingit = c("S_Tlingit-1", "S_Tlingit-2"),
        Tubalar = c("S_Tubalar-1", "S_Tubalar-2"),
	Ulchi = c("S_Ulchi-1", "S_Ulchi-2"),
        Yakut = c("S_Yakut-1", "S_Yakut-2"),
        Ami = c("S_Ami-1","S_Ami-2"),
        Atayal = c("S_Atayal-1"),
        Burmese = c("S_Burmese-1", "S_Burmese-2"),
       	Cambodian = c("S_Cambodian-1", "S_Cambodian-2"),
        Dai = c("A_Dai-5", "B_Dai-4", "S_Dai-1", "S_Dai-2", "S_Dai-3"),
       	Daur = c("S_Daur-2"),
        Han = c("A_Han-4", "B_Han-3", "S_Han-1", "S_Han-2"),
	Hezhen = c("S_Hezhen-1", "S_Hezhen-2"),
        Japanese = c("S_Japanese-1", "S_Japanese-2", "S_Japanese-3"),
	Kinh = c("S_Kinh-1", "S_Kinh-2"),
        Korean = c("S_Korean-1", "S_Korean-2"),
        Lahu = c("S_Lahu-1", "S_Lahu-2"),
        Miao = c("S_Miao-1", "S_Miao-2"),
        Naxi = c("S_Naxi-1", "S_Naxi-2", "S_Naxi-3"),
	Oroqen = c("S_Oroqen-1", "S_Oroqen-2"),
        She = c("S_She-1", "S_She-2"),
        Thai = c("S_Thai-1", "S_Thai-2"),
        Tu = c("S_Tu-1", "S_Tu-2"),
        Tujia = c("S_Tujia-1", "S_Tujia-2"),
        Uygur = c("S_Uygur-1", "S_Uygur-2"),
        Xibo = c("S_Xibo-1", "S_Xibo-2"),
        Yi = c("S_Yi-1", "S_Yi-2"),
        Australian = c("B_Australian-4", "S_Australian-1", "S_Australian-5"),
        Bougainville = c("S_Bougainville-1", "S_Bougainville-2"),
	Dusun = c("S_Dusun-1", "S_Dusun-2"),
        Hawaiian = c("S_Hawaiian-1"),
        Igorot = c("S_Igorot-1", "S_Igorot-2"),
	Maori = c("S_Maori-1"),
        Papuan = c("A_Papuan-16", "B_Papuan-15", "S_Papuan-1", "S_Papuan-10",
	 "S_Papuan-11", "S_Papuan-12", "S_Papuan-13", "S_Papuan-14", "S_Papuan-2",
	 "S_Papuan-3", "S_Papuan-4", "S_Papuan-5", "S_Papuan-6", "S_Papuan-7",
	 "S_Papuan-8", "S_Papuan-9"),
	Balochi = c("S_Balochi-1"),
        Bengali = c("S_Bengali-1", "S_Bengali-2"),
	Brahmin = c("S_Brahmin-1", "S_Brahmin-2"),
        Brahui = c("S_Brahui-1", "S_Brahui-2"),
        Burusho = c("S_Burusho-1", "S_Burusho-2"),
        Hazara = c("S_Hazara-1", "S_Hazara-2"),
        Irula = c("S_Irula-1", "S_Irula-2"),
	Kalash = c("S_Kalash-1", "S_Kalash-2"),
        Kapu = c("S_Kapu-1", "S_Kapu-2"),
        Khonda_Dora = c("S_Khonda_Dora-1"),
	Kusunda = c("S_Kusunda-1", "S_Kusunda-2"),
        Madiga = c("S_Madiga-1", "S_Madiga-2"),
        Makrani = c("S_Makrani-1", "S_Makrani-2"), 
	Mala = c("S_Mala-2", "S_Mala-3"),
       Pathan = c("S_Pathan-1", "S_Pathan-2"),
       Punjabi = c("S_Punjabi-1", "S_Punjabi-2", "S_Punjabi-3", "S_Punjabi-4"),
       Relli = c("S_Relli-1", "S_Relli-2"),
       Sindhi = c("S_Sindhi-1", "S_Sindhi-2"),
       Yadava = c("S_Yadava-1", "S_Yadava-2"),
       Abkhasian = c("S_Abkhasian-1", "S_Abkhasian-2"),
       Adygei = c("S_Adygei-1", "S_Adygei-2"),
	Albanian = c("S_Albanian-1"),
       Armenian = c("S_Armenian-1", "S_Armenian-2"),
       Basque = c("S_Basque-1", "S_Basque-2"),
       Bedouin = c("S_BedouinB-1", "S_BedouinB-2"),
       Bergamo = c("S_Bergamo-2"),
       Bulgarian = c("S_Bulgarian-1", "S_Bulgarian-2"),
       Chechen = c("S_Chechen-1"),
       Czech = c("S_Czech-2"),
       Druze = c("S_Druze-1", "S_Druze-2"),
       English = c("S_English-1", "S_English-2"),
       Estonian = c("S_Estonian-1", "S_Estonian-2"),
       Finnish = c("S_Finnish-1", "S_Finnish-2", "S_Finnish-3"),
	French = c("B_French-3", "S_French-1", "S_French-2"),
        Georgian = c("S_Georgian-1", "S_Georgian-2"),
	Greek = c("S_Greek-1", "S_Greek-2"),
        Hungarian = c("S_Hungarian-1", "S_Hungarian-2"),
        Icelandic = c("S_Icelandic-1", "S_Icelandic-2"),
        Iranian = c("S_Iranian-1", "S_Iranian-2"),
        Iraqi_Jew = c("S_Iraqi_Jew-1", "S_Iraqi_Jew-2"),
        Jordanian = c("S_Jordanian-1", "S_Jordanian-2", "S_Jordanian-3"),
	Lezgin = c("S_Lezgin-1", "S_Lezgin-2"),
       	North_Ossetian = c("S_North_Ossetian-1", "S_North_Ossetian-2"),
	Orcadian = c("S_Orcadian-1", "S_Orcadian-2"),
        Palestinian = c("S_Palestinian-1", "S_Palestinian-2", "S_Palestinian-3"),
        Polish = c("S_Polish-1"),
        Russian = c("S_Russian-1", "S_Russian-2"),
	Samaritan = c("S_Samaritan-1"),
        Sardinian = c("B_Sardinian-3", "S_Sardinian-1", "S_Sardinian-2"),
	Spanish = c("S_Spanish-1", "S_Spanish-2"),
        Tajik = c("S_Tajik-1", "S_Tajik-2"),
        Turkish = c("S_Turkish-1", "S_Turkish-2"),
        Tuscan = c("S_Tuscan-1", "S_Tuscan-2"),
        Yemenite_Jew = c("S_Yemenite_Jew-1","S_Yemenite_Jew-2"),
#	BantuHerero = c("S_BantuHerero-1", "S_BantuHerero-2"),
#        BantuKenya = c("S_BantuKenya-1", "S_BantuKenya-2"),
#	BantuTswana = c("S_BantuTswana-1", "S_BantuTswana-2"),
#        Biaka = c("S_Biaka-1", "S_Biaka-2"),
#	Dinka = c("B_Dinka-3", "S_Dinka-1", "S_Dinka-2"),
#        Esan = c("S_Esan-1", "S_Esan-2"),
#  	Gambian = c("S_Gambian-1", "S_Gambian-2"),
        Ju_hoan_North = c("B_Ju_hoan_North-4", "S_Ju_hoan_North-1", "S_Ju_hoan_North-2", "S_Ju_hoan_North-3")
#	Khomani_San = c("S_Khomani_San-1", "S_Khomani_San-2"),
#        Luhya = c("S_Luhya-1", "S_Luhya-2"),
#        Luo = c("S_Luo-1", "S_Luo-2"),
#	Mandenka = c("A_Mandenka-4", "B_Mandenka-3", "S_Mandenka-1", "S_Mandenka-2"),
#  	Masai = c("S_Masai-1", "S_Masai-2"),
        #Mbuti = c("B_Mbuti-4", "S_Mbuti-1", "S_Mbuti-2", "S_Mbuti-3")
#  	Mende = c("S_Mende-1", "S_Mende-2"),
#        Mozabite = c("S_Mozabite-1", "S_Mozabite-2"),
#  	Saharawi = c("S_Saharawi-1", "S_Saharawi-2"),
#        Somali = c("S_Somali-1"),
#        Yoruba = c("B_Yoruba-3", "S_Yoruba-1", "S_Yoruba-2"),
#	Vindija33.19 = c("Vindija33.19")
	)


#sgdp_vind
modif_snps %>% filter_bed(paste0("~/repos/ancient_migration/analyses/full_africa_segments/f0_t1_", pop, ".out")) -> modif_segs
print("Done the recoding")

eur <- c("Chane", "Mayan",
       "Mixe", "Mixtec", "Piapoco", "Pima", "Quechua", "Surui", "Zapotec",
       "Aleut", "Altaian", "Chukchi", "Eskimo_Chaplin", "Eskimo_Naukan",
       "Eskimo_Sireniki", "Even", "Itelman", "Kyrgyz", "Mansi", "Mongola",
       "Tlingit", "Tubalar", "Ulchi", "Yakut", "Ami", "Atayal", "Burmese",
       "Cambodian", "Dai", "Daur", "Han", "Hezhen", "Japanese",
       "Kinh", "Korean", "Lahu", "Miao", "Naxi", "Oroqen", "She", "Thai",
       "Tu", "Tujia", "Uygur", "Xibo", "Yi", "Australian", "Bougainville",
       "Dusun", "Hawaiian", "Igorot", "Maori", "Papuan", "Balochi",
       "Bengali", "Brahmin", "Brahui", "Burusho", "Hazara", "Irula",
       "Kalash", "Kapu", "Khonda_Dora", "Kusunda", "Madiga", "Makrani",
       "Mala", "Pathan", "Punjabi", "Relli", "Sindhi", "Yadava", "Abkhasian",
       "Adygei", "Albanian", "Armenian", "Basque", "Bedouin", "Bergamo",
       "Bulgarian", "Chechen", "Czech", "Druze", "English", "Estonian",
       "Finnish", "French", "Georgian", "Greek", "Hungarian", "Icelandic",
       "Iranian", "Iraqi_Jew", "Jordanian", "Lezgin", "North_Ossetian",
       "Orcadian", "Palestinian", "Polish", "Russian", "Samaritan",
       "Sardinian", "Spanish", "Tajik", "Turkish", "Tuscan", "Yemenite_Jew")

afr <- c("BantuHerero", "BantuKenya", "BantuTswana",
	 "Biaka", "Dinka", "Esan", "Gambian", "Ju_hoan_North",
	 "Luhya", "Luo", "Mandenka",
	 "Masai", "Mbuti", "Mende", "Mozabite", "Saharawi", "Somali", "Yoruba")

eur_0 = c("Chane")
afr_0 = c("Dinka")

afr_1 <- c("BantuHerero", "BantuKenya", "BantuTswana")
afr_2 <- c("Biaka", "Dinka", "Esan", "Gambian", "Ju_hoan_North")
afr_3 <- c("Luhya", "Luo", "Mandenka")
afr_4 <- c("Masai", "Mbuti", "Mende", "Mozabite", "Saharawi", "Somali", "Yoruba")

eur_1 = c("Chane", "Mayan",
       "Mixe", "Mixtec", "Piapoco", "Pima", "Quechua", "Surui", "Zapotec")
eur_2 = c("Aleut", "Altaian", "Chukchi", "Eskimo_Chaplin", "Eskimo_Naukan",
       "Eskimo_Sireniki", "Even", "Itelman", "Kyrgyz", "Mansi", "Mongola")
eur_3 = c("Tlingit", "Tubalar", "Ulchi", "Yakut", "Ami", "Atayal", "Burmese")

eur_4 = c("Cambodian", "Dai", "Daur", "Han", "Hezhen", "Japanese",
       "Kinh", "Korean", "Lahu", "Miao", "Naxi", "Oroqen", "She", "Thai")
eur_5 = c("Tu", "Tujia", "Uygur", "Xibo", "Yi", "Australian", "Bougainville",
       "Dusun", "Hawaiian", "Igorot", "Maori", "Papuan", "Balochi")

eur_6 = c("Bengali", "Brahmin", "Brahui", "Burusho", "Hazara", "Irula",
       "Kalash", "Kapu", "Khonda_Dora", "Kusunda", "Madiga", "Makrani")
eur_7 = c("Mala", "Pathan", "Punjabi", "Relli", "Sindhi", "Yadava", "Abkhasian",
       "Adygei", "Albanian", "Armenian", "Basque", "Bedouin", "Bergamo")
eur_8 = c("Bulgarian", "Chechen", "Czech", "Druze", "English", "Estonian")

eur_9 = c( "Finnish", "French", "Georgian", "Greek", "Hungarian", "Icelandic",
       "Iranian", "Iraqi_Jew", "Jordanian", "Lezgin", "North_Ossetian")
eur_10 = c("Orcadian", "Palestinian", "Polish", "Russian", "Samaritan",
       "Sardinian", "Spanish", "Tajik", "Turkish", "Tuscan", "Yemenite_Jew")

eur_subset <- c("Bougainville", "English", "Yakut", "Iraqi_Jew")
afr_subset <- c("Yoruba", "Mbuti", "Mozabite", "Luo", "Ju_hoan_North")

eur_array = list(eur_1, eur_2, eur_3, eur_4)

#d_statistics = vector("list", 2000)
#i = 1
#for (e in eur) {
#	for (a in afr) {

e = eval(parse(text = paste0("eur_", args[2])))

if ( grepl( x = pop, pattern = "-1" ) ) {
	c = gsub(pop, pattern="-1", replacement="-2")
} else if ( grepl( x = pop, pattern = "-2" ) ) {
	c = gsub(pop, pattern="-2", replacement="-1")
} else { 
	stop("Cant find a good partner")
}


whole = d(
   W = pop,
   X = c,
   Y = e,
   Z = "Ju_hoan_North",
   data = modif_snps
)

seg = d(
   W = pop,
   X = c,
   Y = e,
   Z = "Ju_hoan_North",
   data = modif_segs
)

print("Finished the d statistic step...")
write.table(whole, file = paste0("~/repos/ancient_migration/analyses/d-statistics/new_results/d-all.txt"), sep = " ", append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
write.table(seg, file = paste0("~/repos/ancient_migration/analyses/d-statistics/new_results/d-seg.txt"), sep = " ", append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)

print("Finished writing....")
