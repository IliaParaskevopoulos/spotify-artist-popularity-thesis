

#Set up ------------------------------------------------------------------
setwd("C:/Users/Ilias/Desktop/Thesis Files")

list.of.packages <- c("data.table", "dplyr", "caTools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(data.table)
library(dplyr)
library(caTools)

#load data
artists <- read.table('parsed-meta-data.csv', sep = '\t', encoding = 'UTF-8', fill = T, header=T,
                      na.strings = c('NA','None'), quote='')
artists$no.column<- NULL
dataframe1 <- artists[,c("name","code2", "gender", "record_label", "image_url","cover_url","press_contact","booking_agent","cmstats_num_sp_playlists", "cmstats_sp_playlist_total_reach", "cmstats_sp_popularity", "cmstats_sp_monthly_listeners", "cmstats_sp_followers")] 

#number of observations
nrow(dataframe1)

names(dataframe1)[2] <- "country"
names(dataframe1)[5:6] <- c("avatar", "background")
names(dataframe1)[9:13] <- c("playlist_count","playlist_reach","popularity_score","monthly_listeners","followers")

#count missings
dataframe1$na_count <- apply(dataframe1, 1, function(name) sum(is.na(name)))

#Remove duplicate rows
dataframe2 <- unique(dataframe1)
nrow2 <- nrow(dataframe2)

#Number of rows removed because they were duplicates:
nrow1 <- nrow(dataframe1)
n_duplicates_removed <- nrow1-nrow2
print(n_duplicates_removed)


#remove rows with gender & country missing
dataframe3<-dataframe2[(!is.na(dataframe2$gender) & !is.na(dataframe2$country)),]
nrow3 <- nrow(dataframe3)


#country classification

dataframe3$country_classified[dataframe3$country == 'dk' |
                                dataframe3$country == 'se' |
                                dataframe3$country == 'fi' |
                                dataframe3$country == 'no' |
                                dataframe3$country == 'is' |
                                dataframe3$country == 'fo' |
                                dataframe3$country == 'gl' |
                                dataframe3$country == 'ax'] <- "nordics"
dataframe3$country_classified[dataframe3$country == 'be' |
                                dataframe3$country == 'nl' |
                                dataframe3$country == 'lu'] <- "benelux"
dataframe3$country_classified[dataframe3$country == 'rs' |
                                dataframe3$country == 'ua' |
                                dataframe3$country == 'rs' |
                                dataframe3$country == 'al' |
                                dataframe3$country == 'mk' |
                                dataframe3$country == 'me' |
                                dataframe3$country == 'ba' |
                                dataframe3$country == 'hr' |
                                dataframe3$country == 'ro' |
                                dataframe3$country == 'bg' |
                                dataframe3$country == 'by' |
                                dataframe3$country == 'lt' |
                                dataframe3$country == 'lv' |
                                dataframe3$country == 'ee' |
                                dataframe3$country == 'pl' |
                                dataframe3$country == 'ru' |
                                dataframe3$country == 'sk' |
                                dataframe3$country == 'gr' |
                                dataframe3$country == 'si' |
                                dataframe3$country == 'yu' |
                                dataframe3$country == 'hu' |
                                dataframe3$country == 'cz']<- "east europe"
dataframe3$country_classified[dataframe3$country == 'rs' |
                                dataframe3$country == 'ar' |
                                dataframe3$country == 'co' |
                                dataframe3$country == 'uy' |
                                dataframe3$country == 'py' |
                                dataframe3$country == 'pe' |
                                dataframe3$country == 'cl' |
                                dataframe3$country == 'bo' |
                                dataframe3$country == 'br' |
                                dataframe3$country == 'ec' |
                                dataframe3$country == 've' |
                                dataframe3$country == 'sr' |
                                dataframe3$country == 'gy'] <- "south america"
dataframe3$country_classified[dataframe3$country == 'it' |
                                dataframe3$country == 'sm' ] <- "italy"
dataframe3$country_classified[dataframe3$country == 'ie' |
                                dataframe3$country == 'gb' ] <- "great britain & ireland"
dataframe3$country_classified[dataframe3$country == 'us' ] <- "united states"
dataframe3$country_classified[dataframe3$country == 'de' |
                                dataframe3$country == 'ch' |
                                dataframe3$country == 'at' ] <- "germany & austria & switzerland"
dataframe3$country_classified[dataframe3$country == 'fr' ] <- "france"
dataframe3$country_classified[dataframe3$country == 'es' |
                              dataframe3$country == 'pt'] <- "spain & portugal"


dataframe3$country_classified[dataframe3$country == 'au' |
                                dataframe3$country == 'nz'] <- "oceania"

dataframe3$country_classified[dataframe3$country == 'ir' |
                                dataframe3$country == 'iq' |
                                dataframe3$country == 'sy' |
                                dataframe3$country == 'lb' |
                                dataframe3$country == 'jo' |
                                dataframe3$country == 'sa' |
                                dataframe3$country == 'ae' |
                                dataframe3$country == 'bh' |
                                dataframe3$country == 'kw' |
                                dataframe3$country == 'qa' |
                                dataframe3$country == 'om' |
                                dataframe3$country == 'eg' |
                                dataframe3$country == 'am' |
                                dataframe3$country == 'az' |
                                dataframe3$country == 'il' |
                                dataframe3$country == 'tr' |
                                dataframe3$country == 'ye'] <- "middle east"

dataframe3$country_classified[dataframe3$country == 'zw' |
                                dataframe3$country == 'zm' |
                                dataframe3$country == 'za' |
                                dataframe3$country == 'tn' |
                                dataframe3$country == 'sn' |
                                dataframe3$country == 'ng' |
                                dataframe3$country == 'na' |
                                dataframe3$country == 'gh' |
                                dataframe3$country == 'ma' |
                                dataframe3$country == 'ml' |
                                dataframe3$country == 'gn' |
                                dataframe3$country == 'cd' |
                                dataframe3$country == 'eh' |
                                dataframe3$country == 'ao' |
                                dataframe3$country == 'mz'] <- "africa"
dataframe3$country_classified[dataframe3$country == 'tt' |
                                dataframe3$country == 'do' |
                                dataframe3$country == 'pr' |
                                dataframe3$country == 'ht' |
                                dataframe3$country == 'hn' |
                                dataframe3$country == 'cu' |
                                dataframe3$country == 'gp' |
                                dataframe3$country == 'jm'] <- "carribean"
dataframe3$country_classified[dataframe3$country == 'mx' |
                                dataframe3$country == 'pa' |
                                dataframe3$country == 'ht' |
                                dataframe3$country == 'gt' |
                                dataframe3$country == 'gm'] <- "central america"
dataframe3$country_classified[dataframe3$country == 'tw' |
                                dataframe3$country == 'cn' |
                                dataframe3$country == 'jp' |
                                dataframe3$country == 'kr' |
                                dataframe3$country == 'kp' |
                                dataframe3$country == 'vn' |
                                dataframe3$country == 'kh' |
                                dataframe3$country == 'th' |
                                dataframe3$country == 'sg' |
                                dataframe3$country == 'ph' |
                                dataframe3$country == 'hk' |
                                dataframe3$country == 'my' |
                                dataframe3$country == 'id' |
                                dataframe3$country == 'mo' |
                                dataframe3$country == 'bd' |
                                dataframe3$country == 'pk' |
                                dataframe3$country == 'in' |
                                dataframe3$country == 'lk' |
                                dataframe3$country == 'mu' |
                                dataframe3$country == 'kz' |
                                dataframe3$country == 'la' ] <- "asia"

#eastern europe: ua, by, lt, lv, ee, pl, sk, cz
#south america: br , ar, co, uy, py, pe, cl, bo, ec, ve,sr, gy
#middle east ir, iq, sy, lb, jo, sa, ae, bh, kw, qa, om, eg, ye, il, tr
#africa

a <- table(dataframe3$country_classified)
a
plot(a)

# popularity score check 0 <= popularity score <= 100
dataframe4<-dataframe3[(dataframe3$popularity_score <= 100 & dataframe3$popularity_score >= 0),]
nrow4 <- nrow(dataframe4)

# variable restrictions check
dataframe4<-dataframe4[(dataframe4$followers >= 0),]
dataframe4<-dataframe4[(dataframe4$monthly_listeners >= 0),]
dataframe4<-dataframe4[(dataframe4$playlist_reach >= 0),]
dataframe4<-dataframe4[(dataframe4$playlist_count >= 0),]
nrow4 <- nrow(dataframe4)

# press_contact &booking_agent operationalization
dataframe4$press_contact[is.na(dataframe4$press_contact)] <- 0
dataframe4$booking_agent[is.na(dataframe4$booking_agent)] <- 0
dataframe4$press_contact[!dataframe4$press_contact == 0] <- 1
dataframe4$booking_agent[!dataframe4$booking_agent == 0] <- 1


dataframe4 <- dataframe4[,c("name","country_classified", "gender", "record_label","press_contact","booking_agent","playlist_reach","playlist_count","monghtly_listeners", "followers")] 

View(dataframe4)
summary(dataframe4)

a <- table(dataframe4$press_contact)
a
plot(a)

a <- table(dataframe4$booking_agent)
a
plot(a)

a <- table(dataframe4$monghtly_listeners)
plot(a)


