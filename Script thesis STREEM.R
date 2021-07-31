rm(list=ls())
# Join between Dinar and Gerlak datasets, Number of wars
setwd("/Users/michelequaranta/Desktop/Thesis STREEM/Furnished Datasets")
library(readxl)
treaty<-read_excel("treaty cv.xlsx")
codes<-read.csv2("COW country codes.csv",sep=",")
library(data.table)
treaty <- data.table(treaty)
codes <- data.table(codes)
codes <- unique(codes)
treaty <- treaty[2:nrow(treaty), ]
library(tidyverse)
library(tidyr)
library(haven)
library(dplyr)
X2_infosharing <- read_dta("~/Downloads/2-infosharing.dta")
X2_infosharing <- data.table(X2_infosharing)
X2_infosharing <- X2_infosharing[!is.na(X2_infosharing$sharingtype) & !is.na(X2_infosharing$sharingfrequency), ]
X2_infosharing <- X2_infosharing %>% rename("Treaty Name" = "document_name")
X2_infosharing$`Treaty Name` <- recode(X2_infosharing$`Treaty Name`, "Treaty between the Hungarian Peopleâs Republic and the Republic of Austria concerning the regulation of water economy questions" = "Treaty between the Hungarian People’s Republic and the Republic of Austria concerning the regulation of water economy questions" , "Agreement between the Union of Soviet Socialist Republics and the People's Republic of China on joint research operations to determine the natural resources of the Amur River Basin and the prospects for development of its productive potentialities and on" = "Agreement between the Union of Soviet Socialist Republics and the People's Republic of China on joint research operations to determine the natural resources of the Amur River Basin and the prospects for development of its productive potentialities and on panning and survey operations to prepare a scheme for the multi-purpose exploitation of the Argun River and the Upper Amur River" , "Agreement between the government of the Federal People's Republic of Yugoslavia and the government of the People's Republic of Albania concerning water economy questions, together with the statue of the Yugoslav-Albanian Water economic commission and with" = "Agreement between the government of the Federal People's Republic of Yugoslavia and the government of the People's Republic of Albania concerning water economy questions, together with the statue of the Yugoslav-Albanian Water economic commission" , "Agreement concerning the Niger River Commission and the Navigation and Transport on the River Niger Revised on 2nd February, 1968 and 15th June, 1973" = "Act regarding navigation and economic co-operation between the states of the Niger Basin")
X2_infosharing$`Treaty Name` <- recode(X2_infosharing$`Treaty Name`, "Agreement between the Republic of Kazakhstan, Republic of Kyrgyzstan, Republic of Uzbekistan, Republic of Tajikistan and Turkmenistan on Cooperation in the Area of Joint Management, Utilization and Protection of Interstate Water Resources" = "Agreement Between the Republic of Kazakhstan , the Republic of Kirgyzstan, the Republic of Uzbekistan, the Republic of Tajikistan and Turkmenistan On Cooperation in the Field of Joint Water Resources Management and Conservation of Interstate Sources" , "Amended agreement between His Majesty's government of Nepal and the government of India concerning the Kosi Project" = "Agreement between the government of India and the government of Nepal on the Kosi project" , "Agreement between the Peopleâs Republic of Bulgaria and the Republic of Turkey concerning cooperation in the use of the waters of rivers flowing through the territory of both countries" = "Agreement between the People’s Republic of Bulgaria and the Republic of Turkey concerning cooperation in the use of the waters of rivers flowing through the territory of both countries" , "Agreement between Iran and Iraq Concerning Frontier Commissioners" = "Agreement between Iran and Iraq concerning the use of frontier watercourses, and protocol" , "Agreement between the government of the Socialist Republic of Romania and the government of the Union of Soviet Socialist Republics on the joint construction of the Stinca-Costesti Hydraulic Engineering Scheme on the River Prut and the establishment of th" = "Agreement between the Government of the Socialist Republic of Romania and the Government of the Union of Soviet Socialist Republics on the joint construction of the Stinca-Costesti Hydraulic Engineering Scheme" , "Treaty between the Czech Republic Government and the Slovak Republic government on mutual relations and principles of cooperation in agriculture, food industry, forestry and water economy under the conditions of the customs union" = "Treaty between the Czech Republic and the Slovak Republic Government on mutual relations and principles of cooperation in agriculture, food industry, forestry and water economy under the conditions of the customs union" , "Complementary agreement to the basic scientific and technical cooperation agreement between the government of the Eastern Republic of Uruguay and the Federal Republic of Brazil on cooperation in the area of water resources" = "Complementary settlement to the agreement of cooperation between the government of the Eastern Republic of Uruguay and the government of the Federal Republic of Brazil for the use of natural resources and the development of the Cuareim river basin" , "Tripartite Interim Agreement between the Republic of Mozambique and the Republic of South Africa and the Kingdom of Swaziland for co-operation on the protection and sustainable utilisation of the water resources of the Incomati and Maputo watercourses" = "Agreement between the government of the Republic of South Africa, the government of the Kingdom of Swaziland and the government of the People's Republic of Mozambique relative to the establishment of a tripartite permanent technical committee" , "Reglamento para la Administracion del Canal de Zarumilla y la utilizacion de sus aguas" = "Agreement on the criteria for the rehabilitation or reconstruction of the headworks and ancillary works of the Zarumilla Canal" , "Senegal River Water Charter" = "Convention relating to the statute of the Senegal river")
dinar_gerlak <- left_join(X2_infosharing, treaty, by = "Treaty Name")
dinar_gerlak <- dinar_gerlak[!is.na(dinar_gerlak$`Signatory 1`), ]
dinar_gerlak <- dinar_gerlak[!is.na(dinar_gerlak$`Signatory 2`), ]
dinar_gerlak <- dinar_gerlak[!is.na(dinar_gerlak$`CV Precip`), ]
dinar_gerlak$`Signatory 1` <- recode(dinar_gerlak$`Signatory 1`, "Yugoslavia (Former)" ="Yugoslavia" , "Laos, People's Democratic Republic of" ="Laos", "Cambodia (Kampuchea)" ="Cambodia", "Tanzania, United Republic of" ="Tanzania", "Gambia, The" ="Gambia")
dinar_gerlak$`Signatory 2` <- recode(dinar_gerlak$`Signatory 2`, "Yugoslavia (Former)" ="Yugoslavia" , "Laos, People's Democratic Republic of" ="Laos", "Cambodia (Kampuchea)" ="Cambodia", "Tanzania, United Republic of" ="Tanzania", "Gambia, The" ="Gambia")
dinar_gerlak <- data.frame(dinar_gerlak)
dinar_gerlak$sharebord <- c(1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,1,0,1,0,0,0,0,0,1,0,0,1,1,0,1,1,1,0,0,0,1,0,0,0,1,0,0,1,1,0,0,1,1,1,1,1,1,0,1,1,1,1,1,0,1,1,0,0,1,1,1,1,1,1,1,1,1,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,1,1,0,0,0,1,0,0,0,0,0,0,1,1,1,1,0,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
dinar_gerlak <- dinar_gerlak %>% rename("Signatory 1" = "Signatory.1")
dinar_gerlak <- dinar_gerlak %>% rename("Signatory 2" = "Signatory.2")
dinar_gerlak <- dinar_gerlak %>% rename("CV Precip" = "CV.Precip")



setwd("..")
dyadic_mid <- read_dta("Conflict Data/DYDMID3.1/dyadic mid 3.1_may 2018.dta")
dyadic_mid <- dyadic_mid[, c("disno","statea","stateb")]
dyadic_mid <- unique(dyadic_mid, by=c("disno","statea","stateb"))
table_A <- left_join(dyadic_mid, codes, by = c("statea" = "CCode"))
table_B <- left_join(dyadic_mid, codes, by = c("stateb" = "CCode"))
war_table_trial1 <- left_join(table_A, table_B, by = c("statea", "stateb","disno"))
war_table_trial1 <-as.data.table(war_table_trial1)
www <- war_table_trial1[, .N , by=c("statea", "stateb")]
cds <- inner_join(www,war_table_trial1, by=c("statea", "stateb"))
cds <- data.table(cds)
cds <- unique(cds, by=c("StateNme.x","StateNme.y"))
cds <- rename(cds, "Signatory 1" = "StateNme.x")
cds <- rename(cds, "Signatory 2" = "StateNme.y")
cds <- cds %>% mutate(`Signatory 1` = as.character(`Signatory 1`))
cds <- cds %>% mutate(`Signatory 2` = as.character(`Signatory 2`))
dinar_gerlak <- dinar_gerlak %>% left_join(select(cds, N, "Signatory 1", "Signatory 2"), by=c("Signatory 1","Signatory 2"))
dinar_gerlak <- dinar_gerlak %>% rename("n_disputes" = "N")
dinar_gerlak$`Signatory 1` <- recode(dinar_gerlak$`Signatory 1`, "Union of Soviet Socialist Republics" = "Russia")
dinar_gerlak$`Signatory 2` <- recode(dinar_gerlak$`Signatory 2`, "Union of Soviet Socialist Republics" = "Russia")


#directed_dyadic_war_may_2018 <- read_dta("Conflict Data/Dyadic Interstate War Dataset/directed dyadic war may 2018.dta")
#directed_dyadic_war_may_2018 <- data.table(directed_dyadic_war_may_2018)
#dd <- directed_dyadic_war_may_2018[, c("warnum","statea","stateb")]
#dyadic_unique <- unique(dd)
#dyadic_unique <- dyadic_unique %>% mutate(statea = as.integer(statea))
#dyadic_unique <- dyadic_unique %>% mutate(stateb = as.integer(stateb))
#table_A <- left_join(dyadic_unique, codes, by = c("statea" = "CCode"))
#table_A_unique <- unique(table_A)
#table_B <- left_join(dyadic_unique, codes, by= c("stateb" = "CCode"))
#table_B_unique <- unique(table_B)
#war_table_trial1 <- left_join(table_A_unique, table_B_unique, by = c("statea", "stateb","warnum"))
#war_table_trial1 <-as.data.table(war_table_trial1)
#www <- war_table_trial1[, .N , by=c("statea", "stateb")]
#cds <- inner_join(www,war_table_trial1, by=c("statea", "stateb"))

# final merge between war_table and treaty, creation of n° of wars variable
#cds <- rename(cds, "Signatory 1" = "StateNme.x")
#cds <- rename(cds, "Signatory 2" = "StateNme.y")
#cds <- cds %>% mutate(`Signatory 1` = as.character(`Signatory 1`))
#cds <- cds %>% mutate(`Signatory 2` = as.character(`Signatory 2`))
#finalT <- treaty %>% left_join(select(cds, N, "Signatory 1", "Signatory 2"), by=c("Signatory 1","Signatory 2"))
#finalT <- finalT %>% rename("n_wars" = "N")

# CPI score
setwd("/Users/michelequaranta/Desktop/Thesis STREEM/Corruption Data/2018_CPI_FullResults")
cpi_2018 <- read_excel("2018_CPI_FullDataSet.xlsx")
cpi_2018 <- data.table(cpi_2018)
cpi_2018 <- cpi_2018[, c(1,4)]
colnames(cpi_2018) <- c("Country", "CPI Score 2018")
cpi_2018 <- cpi_2018[-c(1,2) ,] 
cpi_2018$Country <- recode(cpi_2018$Country, "Cote d'Ivoire" = "Ivory Coast")
treaty_cpi1 <- left_join(dinar_gerlak, cpi_2018, by = c("Signatory 1" = "Country"))
finalT <- left_join(treaty_cpi1, cpi_2018, by = c("Signatory 2" = "Country"))

# Trade importance 
setwd("/Users/michelequaranta/Desktop/Thesis STREEM/Trade Data")
#trade <- data.table()
#for (x in 1:156){
#  trade <- rbind(trade, read_excel("Trade (imp+exp).xlsx", x))
#}

trade <- read_excel("Trade (imp+exp) secondaverage.xlsx")
trade <- data.table(trade)
trade_clean <- trade[, c("Year","Trade Flow","Reporter","Partner","Trade Value (US$)")]
trade_clean <- data.table(trade_clean)

# Average 2012-2018 values 
trade_clean <- trade_clean[trade_clean$Year %in% c(2012,2013,2014,2015,2016,2017,2018),]
trade_clean <- trade_clean[trade_clean$`Trade Flow` %in% c("Import","Export"),]
trade_clean <- rename(trade_clean, "Signatory 1" = "Reporter")
trade_clean <- rename(trade_clean, "Signatory 2" = "Partner")
trade_clean$`Signatory 1` <- recode(trade_clean$`Signatory 1`, "Russian Federation" ="Russia" , "USA" ="United States of America", "Rep. of Moldova" ="Moldova", "United Rep. of Tanzania" ="Tanzania", "Bolivia (Plurinational State of)" ="Bolivia")
trade_clean$`Signatory 2` <- recode(trade_clean$`Signatory 2`, "Russian Federation" ="Russia" , "Rep. of Moldova" ="Moldova", "United Rep. of Tanzania" ="Tanzania", "Bolivia (Plurinational State of)" ="Bolivia", "	Lao People's Dem. Rep." = "Laos", "Viet Nam" = "Vietnam")
trade_clean <- trade_clean %>% group_by(`Year`,`Signatory 1`,`Signatory 2`) %>% mutate(imp_exp = sum(`Trade Value (US$)`))
trade_clean <- trade_clean %>% group_by(`Signatory 1`,`Signatory 2`) %>% mutate(imp_exp_avg = mean(`imp_exp`))
trade_clean <- trade_clean[, c("Signatory 1","Signatory 2","imp_exp_avg")]
trade_clean <- unique(trade_clean)
trade_clean$`Signatory 1` <- recode(trade_clean$`Signatory 1`, "C√¥te d'Ivoire" ="Ivory Coast", "Lao People's Dem. Rep." ="Laos")
trade_clean$`Signatory 2` <- recode(trade_clean$`Signatory 2`, "C√¥te d'Ivoire" ="Ivory Coast" , "Eswatini" ="Swaziland")
trade_clean_rev <- trade_clean
trade_clean_rev[,"Signatory 1"] <- trade_clean[,"Signatory 2"] 
trade_clean_rev[,"Signatory 2"] <- trade_clean[,"Signatory 1"]
treaty_trade_double <- rbind(trade_clean, trade_clean_rev)
treaty_trade_complete <- finalT %>% left_join(treaty_trade_double, by=c("Signatory 1","Signatory 2"))



# 2018 year values
#trade_clean <- trade_clean[trade_clean$Year == "2018",]
#trade_clean <- trade_clean[trade_clean$`Trade Flow` %in% c("Import","Export"),]
#trade_clean <- rename(trade_clean, "Signatory 1" = "Reporter")
#trade_clean <- rename(trade_clean, "Signatory 2" = "Partner")
#trade_clean$`Signatory 1` <- recode(trade_clean$`Signatory 1`, "Russian Federation" ="Russia" , "USA" ="United States of America", "Rep. of Moldova" ="Moldova", "United Rep. of Tanzania" ="Tanzania", "Bolivia (Plurinational State of)" ="Bolivia")
#trade_clean$`Signatory 2` <- recode(trade_clean$`Signatory 2`, "Russian Federation" ="Russia" , "Rep. of Moldova" ="Moldova", "United Rep. of Tanzania" ="Tanzania", "Bolivia (Plurinational State of)" ="Bolivia", "	Lao People's Dem. Rep." = "Laos", "Viet Nam" = "Vietnam")
#finalT$`Signatory 2` <- recode(finalT$`Signatory 2`, "Union of Soviet Socialist Republics" = "Russia")
#treaty_trade <- select(finalT, "Signatory 1", "Signatory 2") %>% left_join(select(trade_clean, "Trade Flow", "Signatory 1", "Signatory 2","Trade Value (US$)"), by = c("Signatory 1","Signatory 2")) # take away from finalT basins, and eventually join impexp table with original finalT
#treaty_trade <- as.data.table(treaty_trade)
#treaty_trade_unique <- unique(treaty_trade)
#treaty_trade_final <- treaty_trade_unique %>% group_by(`Signatory 1`,`Signatory 2`) %>% mutate(imp_exp = sum(`Trade Value (US$)`))
#treaty_left <- treaty_trade_final[, c("Signatory 1","Signatory 2","imp_exp")]
#treaty_right <- treaty_left
#treaty_right[,"Signatory 1"] <- treaty_left[,"Signatory 2"] 
#treaty_right[,"Signatory 2"] <- treaty_left[,"Signatory 1"]
#treaty_left <- treaty_left %>% drop_na(imp_exp)
#treaty_right <- treaty_right %>% drop_na(imp_exp)
#treaty_trade_nona <- rbind(treaty_left, treaty_right)
#treaty_trade_nona <- unique(treaty_trade_nona)
#treaty_trade_complete <- finalT %>% left_join(treaty_trade_nona, by=c("Signatory 1","Signatory 2"))


# Now, import GDP data (GDP average 2012-2018)
setwd("/Users/michelequaranta/Desktop/Thesis STREEM/Power Data")
gdp_table <- read_excel("GDP data (annual country level 1960-2018).xls")
gdp_table <- data.table(gdp_table)
colnames(gdp_table) <- c("Country Name", "Country Code", "Indicator Name", "Indicator Code", "1960", "1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")
gdp_table <- gdp_table[-c(1,2,3) ,] 
gdp_table <- gdp_table[, c("Country Name","2012","2013","2014","2015","2016","2017","2018")]
gdp_table$`2012` <- as.numeric(as.character(gdp_table$`2012`))
gdp_table$`2013` <- as.numeric(as.character(gdp_table$`2013`))
gdp_table$`2014` <- as.numeric(as.character(gdp_table$`2014`))
gdp_table$`2015` <- as.numeric(as.character(gdp_table$`2015`))
gdp_table$`2016` <- as.numeric(as.character(gdp_table$`2016`))
gdp_table$`2017` <- as.numeric(as.character(gdp_table$`2017`))
gdp_table$`2018` <- as.numeric(as.character(gdp_table$`2018`))
gdp_table_mean <- data.frame("Country Name" = gdp_table[,1], gdp_mean=rowMeans(gdp_table[,-1], na.rm = TRUE))
gdp_table_mean$`Country.Name` <- recode(gdp_table_mean$`Country.Name`, "Gambia, The" = "Gambia", "Cambodia (Kampuchea)" = "Cambodia", "Tanzania, United Republic of" = "Tanzania", "Eswatini" = "Swaziland", "Laos, People's Democratic Republic of" = "Laos")
treaty_gdp1 <- left_join(treaty_trade_complete, gdp_table_mean, by = c("Signatory 1" = "Country.Name"))
treaty_gdp <- left_join(treaty_gdp1, gdp_table_mean, by = c("Signatory 2" = "Country.Name"))
treaty_gdp <- treaty_gdp %>% mutate(gdp_mean_dyad = `gdp_mean.x` + `gdp_mean.y`) %>% mutate(trade_imp = imp_exp_avg / gdp_mean_dyad) 



# With 2018 GDP values 
#gdp_table <- gdp_table[, -c(2:62)]
#check names of country, then join
#gdp_table$`Country Name` <- recode(gdp_table$`Country Name`, "Gambia, The" = "Gambia", "Cambodia (Kampuchea)" = "Cambodia", "Tanzania, United Republic of" = "Tanzania")
#treaty_gdp1 <- left_join(treaty_trade_complete, gdp_table, by = c("Signatory 1" = "Country Name"))
#treaty_gdp <- left_join(treaty_gdp1, gdp_table, by = c("Signatory 2" = "Country Name"))
#treaty_gdp$`2018.x` <- as.numeric(as.character(treaty_gdp$`2018.x`))
#treaty_gdp$`2018.y` <- as.numeric(as.character(treaty_gdp$`2018.y`))
#treaty_gdp <- treaty_gdp %>% mutate(gdp_2018 = `2018.x` + `2018.y`) %>% mutate(trade_imp = imp_exp / gdp_2018) 

# Power Asymmetry
final_dataset <- treaty_gdp %>% mutate(power_ass = ifelse(`gdp_mean.x` > `gdp_mean.y`, `gdp_mean.x` / `gdp_mean.y`, `gdp_mean.y` / `gdp_mean.x`))
# Now clean it from all columns which you don't need and export it in stata to do your regressions
final_dataset <- final_dataset[, c("sharingtype","sharingfrequency","CV Precip","shareboard","n_disputes","CPI Score 2018.x","CPI Score 2018.y","trade_imp","power_ass")] 
final_dataset <- final_dataset %>% rename("cv_precip" = "CV Precip" , "cpi_score_2018x" = "CPI Score 2018.x", "cpi_score_2018y" = "CPI Score 2018.y")
final_dataset$cv_precip <- as.numeric(as.character(final_dataset$cv_precip))
final_dataset$cpi_score_2018x <- as.numeric(as.character(final_dataset$cpi_score_2018x))
final_dataset$cpi_score_2018y <- as.numeric(as.character(final_dataset$cpi_score_2018y))
library(foreign)
write.dta(final_dataset, "/Users/michelequaranta/Desktop/Thesis STREEM/Furnished Datasets/final_dataset.dta")



#final_dataset <- final_dataset[, -c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,22,23,24,25,26,27,28,29,30,31,36,37,38,39)]
# (With n_wars version) Now clean it from all columns which you don't need and export it in stata to do your regressions
#final_dataset <- final_dataset[, -c(1,11,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39)]
#final_dataset <- final_dataset %>% rename("cv_precip" = "CV Precip" , "cpi_score_2018x" = "CPI Score 2018.x", "cpi_score_2018y" = "CPI Score 2018.y")
#final_dataset$cpi_score_2018x <- as.numeric(as.character(final_dataset$cpi_score_2018x))
#final_dataset$cpi_score_2018y <- as.numeric(as.character(final_dataset$cpi_score_2018y))
#final_dataset$cv_precip <- as.numeric(as.character(final_dataset$cv_precip))


