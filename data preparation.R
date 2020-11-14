

#Set up ------------------------------------------------------------------
setwd("C:/Users/Ilias/Desktop/Thesis Files")

list.of.packages <- c("data.table", "dplyr", "caTools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(data.table)
library(dplyr)
library(caTools)
library("matrixStats")
library("microbenchmark")
library("PerformanceAnalytics")
library("moments")



#load Data
artists <- read.table('parsed-meta-data.csv', sep = '\t', encoding = 'UTF-8', fill = T, header=T,
                      na.strings = c('NA','None'), quote='')
artists$no.column<- NULL
artists <- artists[,c("name","code2","gender", "record_label","press_contact","booking_agent",
                         "cmstats_num_sp_playlists", "cmstats_sp_playlist_total_reach", 
                         "cmstats_sp_popularity", "cmstats_sp_monthly_listeners", 
                         "cmstats_sp_followers")] 

#number of observations
nrow(artists)

names(artists)[2] <- "country"
names(artists)[7:11] <- c("playlist_count","playlist_reach","popularity_score","monthly_listeners","followers")

#count Missings per row - to be deleted
#artists$na_count <- apply(artists, 1, function(name) sum(is.na(name)))

#remove Duplicate rows
artists <- unique(artists)
nrow(artists)

#POPULARITY check 0 <= popularity score <= 100
artists<-artists[(artists$popularity_score <= 100 & artists$popularity_score >= 0),]
#Variable restrictions check
artists<-artists[(artists$followers >= 0),]
artists<-artists[(artists$monthly_listeners >= 0),]
artists<-artists[(artists$playlist_reach >= 0),]
artists<-artists[(artists$playlist_count >= 0),]
nrow(artists)


#remove Rows with GENDER & COUNTRY missing
#artists<-artists[(!is.na(artists$record_label) & !is.na(artists$name)),]
artists<-artists[(!is.na(artists$gender) & !is.na(artists$country)),]
nrow(artists)

artists<-artists[(!is.na(artists$record_label)),]
nrow(artists)

#####artists<-artists[(!is.na(artists$gender) & !is.na(artists$country) & !is.na(artists$record_label)),]

#COUNTRY Classification
artists$country_classified[artists$country == 'dk' |
                              artists$country == 'se' |
                              artists$country == 'fi' |
                              artists$country == 'no' |
                              artists$country == 'is' |
                              artists$country == 'fo' |
                              artists$country == 'gl' |
                              artists$country == 'ax'] <- "nordics"
artists$country_classified[artists$country == 'be' |
                              artists$country == 'nl' |
                              artists$country == 'lu'] <- "benelux"
artists$country_classified[artists$country == 'rs' |
                              artists$country == 'ua' |
                              artists$country == 'rs' |
                              artists$country == 'al' |
                              artists$country == 'mk' |
                              artists$country == 'me' |
                              artists$country == 'ba' |
                              artists$country == 'hr' |
                              artists$country == 'ro' |
                              artists$country == 'bg' |
                              artists$country == 'by' |
                              artists$country == 'lt' |
                              artists$country == 'lv' |
                              artists$country == 'ee' |
                              artists$country == 'pl' |
                              artists$country == 'ru' |
                              artists$country == 'sk' |
                              artists$country == 'gr' |
                              artists$country == 'si' |
                              artists$country == 'yu' |
                              artists$country == 'hu' |
                              artists$country == 'cz']<- "east europe"
artists$country_classified[artists$country == 'rs' |
                              artists$country == 'ar' |
                              artists$country == 'co' |
                              artists$country == 'uy' |
                              artists$country == 'py' |
                              artists$country == 'pe' |
                              artists$country == 'cl' |
                              artists$country == 'bo' |
                              artists$country == 'br' |
                              artists$country == 'ec' |
                              artists$country == 've' |
                              artists$country == 'sr' |
                              artists$country == 'gy'] <- "south america"
artists$country_classified[artists$country == 'it' |
                              artists$country == 'sm' ] <- "italy"
artists$country_classified[artists$country == 'ie' |
                              artists$country == 'gb' ] <- "great britain & ireland"
artists$country_classified[artists$country == 'us' ] <- "united states"
artists$country_classified[artists$country == 'de' |
                              artists$country == 'ch' |
                              artists$country == 'at' ] <- "germany & austria & switzerland"
artists$country_classified[artists$country == 'fr' ] <- "france"
artists$country_classified[artists$country == 'es' |
                              artists$country == 'pt' ] <- "spain & portugal"
artists$country_classified[artists$country == 'au' |
                              artists$country == 'nz'] <- "oceania"
artists$country_classified[artists$country == 'ir' |
                              artists$country == 'iq' |
                              artists$country == 'sy' |
                              artists$country == 'lb' |
                              artists$country == 'jo' |
                              artists$country == 'sa' |
                              artists$country == 'ae' |
                              artists$country == 'bh' |
                              artists$country == 'kw' |
                              artists$country == 'qa' |
                              artists$country == 'om' |
                              artists$country == 'eg' |
                              artists$country == 'am' |
                              artists$country == 'az' |
                              artists$country == 'il' |
                              artists$country == 'tr' |
                              artists$country == 'ye'] <- "middle east"
artists$country_classified[artists$country == 'zw' |
                              artists$country == 'zm' |
                              artists$country == 'za' |
                              artists$country == 'tn' |
                              artists$country == 'sn' |
                              artists$country == 'ng' |
                              artists$country == 'na' |
                              artists$country == 'gh' |
                              artists$country == 'ma' |
                              artists$country == 'ml' |
                              artists$country == 'gn' |
                              artists$country == 'cd' |
                              artists$country == 'eh' |
                              artists$country == 'ao' |
                              artists$country == 'mz'] <- "africa"
artists$country_classified[artists$country == 'tt' |
                              artists$country == 'do' |
                              artists$country == 'pr' |
                              artists$country == 'ht' |
                              artists$country == 'hn' |
                              artists$country == 'cu' |
                              artists$country == 'gp' |
                              artists$country == 'jm'] <- "carribean"
artists$country_classified[artists$country == 'mx' |
                              artists$country == 'pa' |
                              artists$country == 'ht' |
                              artists$country == 'gt' |
                              artists$country == 'gm'] <- "central america"
artists$country_classified[artists$country == 'tw' |
                              artists$country == 'cn' |
                              artists$country == 'jp' |
                              artists$country == 'kr' |
                              artists$country == 'kp' |
                              artists$country == 'vn' |
                              artists$country == 'kh' |
                              artists$country == 'th' |
                              artists$country == 'sg' |
                              artists$country == 'ph' |
                              artists$country == 'hk' |
                              artists$country == 'my' |
                              artists$country == 'id' |
                              artists$country == 'mo' |
                              artists$country == 'bd' |
                              artists$country == 'pk' |
                              artists$country == 'in' |
                              artists$country == 'lk' |
                              artists$country == 'mu' |
                              artists$country == 'kz' |
                              artists$country == 'la' ] <- "asia"


#PRESS CONTACT & BOOKING AGENT operationalization
artists$press_contact[is.na(artists$press_contact)] <- 0
artists$booking_agent[is.na(artists$booking_agent)] <- 0
artists$press_contact[!artists$press_contact == 0] <- 1
artists$booking_agent[!artists$booking_agent == 0] <- 1

labels_warner = c('Warner Music' = 'warner[ ]music|warner[ ]home|warner[ ]special|warner[ ]strategic|warner[.]esp',
                  'Asylum Records' = 'asylum[ ]records|([|]|^)atlantic[ ]records|atlantic[ ]|([|]|^)atlantic([|]|$)|elektra[ ]records|([|]|^)elektra([|]|$)|warner[ ]music[ ]nashville|warner[ ]bros|elektra[ ]nashville',
                  'Big Beats Records' = 'big[ ]beat[ ]|([|]|^)big beat([|]|$)',
                  'Canvasback Music' = 'canvasback',
                  'Parlophone Label Group' = 'parlophone|FFR[ ]records|([|]|^)FFRR([|]|$)|virgin[ ]classics|emi[ ]classics|[ ]erato[ ]|([|]|^)erato|warner[ ]classics',
                  'Reprise Records' = '([|]|^)reprise[ ]',
                  'Fueled By Ramen' = 'Fueled[ ]by[ ]ramen',
                  'Nonesuch Records' = 'nonesuch[ ]records|([|]|^)nonesuch([|]|$)',
                  'Rhino Entertainment' = 'rhino[ ]entertainment|([|]|^)rhino',
                  'Roadrunner Records' = '([|]|^)roadrunner',
                  'Sire Records' = '([|]|^)sire[ ]records|([|]|^)sire[ ]|([|]|^)sire[ ]([|]|$)',
                  'East West' = 'east[ ]west|eastwest',
                  'Warner (all combined)' = '([|]|^)warner|[(]warner[)]|asylum[ ]records|big[ ]beat[ ]records|canvasback[ ]music|parlophone[ ]label[ ]group|reprise[ ]records|fueled[ ]by[ ]ramen|nonesuch[ ]records|rhino[ ]entertainment|roadrunner[ ]records|sire[ ]records|east[ ]west')
labels_universal = c('Universal Music Group' = '([|]|^)universal|([|]|^)universal[ ]music[ ]japan|([|]|^)universal[ ]sigma|([|]|^)universal[ ]international|([|]|^)geneon[ ]universal|nbcuniversal|universal[ ]licensing[ ]music|([|]|^)universal[ ]music[ ]|universal[ ]music[ ]spain|universal[ ]m..z.k|([|]|^)universal records|([|]|^)universal[ ]records[ ]|([|]|^)universal[ ]republic[ ]records',
                     'Capital Music Group' = 'capitol|astralwerks|blue[ ]{0,2}note|([|]|^)caroline[ ]|deep[ ]{0,2}well|([|]|^)harvest|([|]|^)metamorphosis|motown|quality[ ]{0,2}control|([|]|^)virgin[ ]|([|]|^)virgin([|]|$)',
                     'Decca Classics' = 'decca|ecm([ ]|$|[|])|([|]|^)mercury|([|]|^)mercury[ ]classics|([|]|^)mercury[ ]records',
                     'Def Jam Recordings' = 'def[ ]{0,2}jam|artium|g.o.o.d|([|]|^)good([|]|$)|good[ ]records',
                     'Deutsche Grammophon' = 'deutsche[ ]grammophon|grammophon',
                     'Eagle Rock Entertainment' = 'eagle[ ]rock|eagle[ ]records', 
                     'EMI' = 'emi[ ]|([|]|^)emi|([|]|^)emi[ ]music|emi[-]',
                     'Interscope' = 'interscope|geffen|A[&]M|([|]|^)222([|]|$)|aftermath|dreamville|insomniac[ ]|kidinakorner|shady[ ]{0,2}records|([|]|^)shady',
                     'Island Records' = '([|]|^)island[ ]records|4th & Broadway|universal[ ]island|([|]|^)island[ ]|([|]|^)island([|]|$)',
                     'Polydor Records' = '([|]|^)polydor[ ]|([|]|^)polydor[ ]([|]|$)|([|]|^)fiction[ ]records|([|]|^)fiction([|]|$)|polydor', 
                     'Republic Records' = '([|]|^)republic[ ]records|universal[ ]republic|([|]|^)american[ ]recordings|([|]|^)Brushfire[ ]records|([|]|^)casablanca[ ]records|([|]|^)cash[ ]money[ ]records|john[ ]varvatos|([|]|^)lava[ ]records|lightworkers|([|]|^)the[ ]voice', 
                     'Republic Records 2' = '([|]|^)republic[ ]records|([|]|^)american[ ]recordings|([|]|^)Brushfire[ ]records|([|]|^)casablanca[ ]records|([|]|^)cash[ ]money[ ]records|john[ ]varvatos|([|]|^)lava[ ]records|lightworkers|([|]|^)the[ ]voice', 
                     'Universal Music Enterprises' = '([|]|^)universal[ ]|([|]|^)universal([|]|$)|T[-]{0,1}boy',
                     'Universal Music Group Nashville' = 'capitol[ ]{0,1}records[ ]{0,1}nashville|emi[ ]{0,1}records[ ]{0,1}nashville|mca[ ]{0,1}nashville|mercury[ ]{0,1}nashville|show[-]{0,1}dog',
                     'Universal Music Latin Entertainment' = 'capitol[ ]{0,1}latin|disa[ ]{0,1}records|fonovisa|machete[ ]{0,1}music|universal[ ]{0,1}music[ ]{0,1}latino',
                     'Verve Label Group' = 'verve[ ]label[ ]group|verve[ ]{0,1}records|decca[ ]{0,1}gold|universal[ ]{0,1}music[ ]{0,1}classics|decca[ ]{0,1}broadway|verve[ ]{0,1}group|([|]|&)verve([|]|$)|verve[ ]{0,1}music[ ]{0,1}group', 
                     'PM:AM Recordings' = 'PM[:]AM|pm[ ]{0,1}am',
                     'Spinefarm Records' = 'spinefarm',
                     'SpinnUp' = 'SpinnUp',
                     'Disques Barclay' = 'disques[ ]{0,1}barclay|([|]|^)barclay',
                     'Varese Sarabande' = 'var.se',
                     'Universal Music (combined)' = 'varese[ ]sarabande|disques[ ]barclay|spinnup|spinefarm[ ]records|pm:am[ ]recordings|verve[ ]label[ ]group|universal[ ]music[ ]latin[ ]entertainment|universal[ ]music[ ]group[ ]nashville|universal[ ]music[ ]enterprises|republic[ ]records|polydor[ ]records|island[ ]records|interscope|emi|eagle[ ]rock[ ]entertainment|deutsche[ ]grammophon|def[ ]jam[ ]recordings|decca[ ]classics|capitol[ ]music[ ]group|universal[ ]music[ ]group')
labels_sony = c('Columbia Records'='CBS[ ]columbia|([|]|^)columbia|hypnotize[ ]minds',
                'Columbia Records 2' = 'dreamville[ ]entertainment|small.*giant|startime[ ]international|blue[ ]propaganda', 
                'RCA Records' = '([|]|^)rca|([|]|^)bystorm.*entertainment|([|]|^)nappy[ ]boy|([|]|^)j[ ]records', 
                'Epic Records' = '([|]|^)epic|([|]|^)battery|([|]|^)freebandz|([|]|^)bad[ ]{0,1}boy[ ]records|([|]|^)volcano|vested[ ]in[ ]culture', 
                'Sony Music Nashville' = 'sony[ ]music|([|]|^)arista|([|]|^)columbia[ ]nashville|rca[ ]records[ ]nashville', 
                'Zomba Music Group' = '([|]|^)zomba|([|]|^)jive[ ]records|([|]|^)verity|([|]|^)silvertone',
                'RED Music Distribution' = '([|]|^)red[ ]music[ ]|odd[ ]future|([|]|^)red[ ]ink|cinematic[ ]music|([|]|^)reach[ ]records', 
                'Legacy Recordings' = '([|]|^)legacy[ ]recordings|([|]|^)laface', 
                'Sony Music Latin' = 'sony.*latin',
                'Ariola Records' = 'ariola',
                'Sony Masterworks' = 'sony[ ]masterworks|([|]|^)bluebird|([|]|^)okeh|portrait[ ]records|([|]|^)portrait|([|]|^)arte[ ]nova|sony[ ]classical|flying[ ]buddha|([|]|^)masterworks', 
                'Provident Label Group' = '([|]|^)provident|essential[ ]records|flicker[ ]records|beach[ ]street|reunion[ ]records|essential[ ]worship',
                'Century Media Records' = 'century[ ]media|([|]|^)century record|people[ ]like[ ]you|insideout[ ]music|superball[ ]music',
                'Sony Music Entertainment' = 'Sony[ ]BMG|([|]|^)BMG|columbia[ ]music|sony[ ]music|Columbia[ ]records|RCA[ ]Records|Epic[ ]Records|Sony[ ]Music[ ]Nashville|Zomba[ ]Music[ ]Group|RED[ ]Music[ ]Distribution[ ]|Legacy[ ]Recordings|Sony[ ]Music[ ]Latin|Ariola[ ]Records|Sony[ ]Masterworks|Provident[ ]Label[ ]Group|Century[ ]Media[ ]Records')

label_iter=c(labels_warner, labels_universal, labels_sony)
artists <- data.table(artists)
artists[, major_label:=0]


for (lbl in label_iter) {
  artists[grepl(lbl, record_label, ignore.case=TRUE), major_label:=1]
}
View(artists)

plot(artists$popularity_score,artists$record_label)

artists$album_label[artists$record_label=="" | artists$record_label==","] <- NA
artists$major_label[is.na(artists$record_label)] <- NA
artists$major_label[artists$major_label==1] <- "Major label"
artists$major_label[artists$major_label==0] <- "Indie label"












#____________________________ Data Preparation ____________________________

#Plots
#DV- Playlist Count = Logarithmic
plot(artists$popularity_score,artists$playlist_count)
#DV- Playlist Reach = Logarithmic
plot(artists$popularity_score,artists$playlist_reach)
#Playlist Count- Playlist Reach = Linear
plot(artists$playlist_count,artists$playlist_reach)
#DV- Gender 
plot(artists$popularity_score,artists$gender)
#DV- Press Contact 
plot(artists$popularity_score,artists$press_contact)
#DV- Booking Agent 
plot(artists$popularity_score,artists$booking_agent)



#DESCRIPTIVES
summary(artists)
sd(artists$popularity_score, na.rm=TRUE)
sd(artists$playlist_count, na.rm=TRUE)
sd(artists$playlist_reach, na.rm=TRUE)
sd(artists$gender, na.rm=TRUE)

artists$press_contact <- as.integer(artists$press_contact)  
artists$booking_agent <- as.integer(artists$booking_agent)
sd(artists$press_contact, na.rm=TRUE)
sd(artists$booking_agent, na.rm=TRUE)

cor.test(artists$gender, artists$popularity_score, method = "pearson")
cor.test(artists$press_contact, artists$popularity_score, method = "pearson")
cor.test(artists$booking_agent, artists$popularity_score, method = "pearson")
cor.test(artists$playlist_count, artists$popularity_score, method = "pearson")
cor.test(artists$playlist_reach, artists$popularity_score, method = "pearson")

#colSds(as.matrix(artists[sapply(artists, is.numeric)]))

#LOG Transform REACH & COUNT https://www.pluralsight.com/guides/normalizing-data-r
artists$playlist_reach_norm <- log(artists$playlist_reach)
hist(artists$playlist_reach_norm)
artists$playlist_count_norm <- log(artists$playlist_count)
hist(artists$playlist_count_norm)

View(artists)

#Check Variable Normality, and normalize the Non-Normal Variables
skewness(artists$popularity_score, na.rm = TRUE) 
kyrtosis(artists$popularity_score, na.rm = TRUE) 
skewness(artists$followers, na.rm = TRUE)
skewness(artists$monthly_listeners, na.rm = TRUE) 


## testing code
model <- lm(artists$popularity_score ~ artists$playlist_reach, data = artists)
model
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(model)

corr.test(artists)

dataframe9 <- data.table(artists)
dataframe9[, major_label:=0]
View(dataframe9)
