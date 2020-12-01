
file.path(R.home("bin"), "R")

#Set up ------------------------------------------------------------------
setwd("C:/Users/Ilias/Desktop/Thesis Files")

list.of.packages <- c("data.table", "dplyr", "caTools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

install.packages("finalfit")
library(finalfit) 

library(data.table)
#install.packages("RDocumentation")
#install.packages("FreqProf")
#install.packages("rioja")
library(rioja)
library(dplyr)
library(caTools)
library(zoo)
library("tidyverse")
#install.packages("dplyr")  #maybe i dont need this
#library("matrixStats") #maybe i dont need this
#library("microbenchmark")
#library("PerformanceAnalytics") #maybe i dont need this
#library("moments") 




#load Data
artists <- read.table('parsed-meta-data.csv', sep = '\t', encoding = 'UTF-8', fill = T, header=T,
                      na.strings = c('NA','None'), quote='')
artists$no.column<- NULL
artists <- artists[,c("name","code2","gender", "record_label","press_contact","booking_agent",
"cmstats_sp_popularity", "cmstats_sp_monthly_listeners","cmstats_sp_followers", 
"cmstats_sp_popularity_rank", "cmstats_sp_monthly_listeners_rank","cmstats_sp_followers_rank", 
"cmstats_sp_where_people_listen", "cmstats_num_sp_playlists", "cmstats_sp_playlist_total_reach",
"cmstats_deezer_fans", "cmstats_deezer_fans_rank",
"cmstats_ycs_subscribers", "cmstats_ycs_subscribers_rank",
"cmstats_ycs_views", "cmstats_ycs_views_rank",
"cmstats_youtube_daily_video_views" , "cmstats_youtube_daily_video_views_rank",
"cmstats_youtube_monthly_video_views","cmstats_youtube_monthly_video_views_rank",
"cmstats_youtube_daily_video_views", "cmstats_youtube_daily_video_views_rank",     
"cmstats_youtube_monthly_video_views", "cmstats_youtube_monthly_video_views_rank",
"cmstats_pandora_listeners_28_day" , "cmstats_pandora_listeners_28_day_rank",
"cmstats_pandora_lifetime_stations_added" , "cmstats_pandora_lifetime_stations_added_rank",  
"cmstats_pandora_lifetime_streams" , "cmstats_pandora_lifetime_streams_rank", 
"cmstats_twitch_followers", "cmstats_twitch_followers_rank", "cmstats_twitch_views",
"cmstats_twitch_views_rank","cmstats_twitch_monthly_viewer_hours", "cmstats_twitch_monthly_viewer_hours_rank",
"cmstats_twitch_weekly_viewer_hours", "cmstats_twitch_weekly_viewer_hours_rank",
"cmstats_tiktok_followers", "cmstats_tiktok_followers_rank",
"cmstats_tiktok_likes", "cmstats_tiktok_likes_rank",
"cmstats_ins_followers", "cmstats_ins_followers_rank"
 )] 

#deezer fans, instagram followers, youtube subscribers, tiktok followers, pandora monthly listeners
#number of observations
nrow(artists)

missing_plot(artists)
missing_pattern(artists)


# rename dataset columns --------------------------------------------------



names(artists)[2] <- "country"
names(artists)[7:9] <- c("popularity_score", "monthly_listeners", "followers")
names(artists)[10:12] <- c("popularity_score_rank", "monthly_listeners_rank", "followers_rank")
names(artists)[13:15] <- c("listener_location","playlist_count","playlist_reach")
names(artists)[16:17] <- c("deezer_fans","deezer_fans_rank") #DEEZER
names(artists)[18:19] <- c("youtube_subscribers","youtube_subscribers_rank") #YOUTUBE
names(artists)[20:21] <- c("youtube_views","youtube_views_rank") #YOUTUBE
names(artists)[18:19] <- c("youtube_subscribers","youtube_subscribers_rank") #YOUTUBE
names(artists)[20:21] <- c("youtube_views","youtube_views_rank") #YOUTUBE
names(artists)[22:23] <- c("youtube_daily_views","youtube_daily_views_rank") #YOUTUBE
names(artists)[24:25] <- c("youtube_monthly_video_views","youtube_monthly_views_rank") #YOUTUBE
names(artists)[26:27] <- c("twitch_followers","twitch_followers_rank") #TWITCH
names(artists)[28:29] <- c("twitch_views","twitch_views_rank") #TWITCH
names(artists)[30:31] <- c("twitch_monthly_viewers","twitch_monthly_viewers_rank") #TWITCH
names(artists)[32:33] <- c("twitch_weekly_viewers","twitch_weekly_viewers_rank") #TWITCH
names(artists)[34:35] <- c("pandora_monthly_listeners","pandora_monthly_listeners_rank") #PANDORA
names(artists)[36:37] <- c("pandora_stations","pandora_stations_rank") #PANDORA
names(artists)[38:39] <- c("pandora_streams","pandora_streams_rank") #PANDORA
names(artists)[40:41] <- c("tiktok_followers","tiktok_followers_rank") #TIKTOK
names(artists)[42:43] <- c("tiktok_likes","tiktok_likes_rank") #TIKTOK
names(artists)[44:45] <- c("instagram_followers","instagram_followers_rank") #INSTAGRAM

nrow(artists)

#DRAFT: artists$deezer_fans_new <- na.approx(artists$deezer_fans)
#DRAFT: approxfun(x = artists$deezer_fans, y = artists$youtube_subscribers,method = "linear", rule = 1, f = 0, ties = mean)



# remove missings, duplicate rows, and other checks -----------------------

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


#remove Rows with RECORD LABEL & COUNTRY missing
artists<-artists[(!is.na(artists$record_label) & !is.na(artists$name)),]
#artists<-artists[(!is.na(artists$gender) & !is.na(artists$country)),]
#nrow(artists)
#artists<-artists[(!is.na(artists$country)),]

#artists<-artists[(!is.na(artists$record_label)),]
nrow(artists)
View(artists)




# classify Country --------------------------------------------------------


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
                              artists$country == 'gb' ] <- "gb+ire"
artists$country_classified[artists$country == 'us' ] <- "us"
artists$country_classified[artists$country == 'de' |
                              artists$country == 'ch' |
                              artists$country == 'at' ] <- "ger+aust+switz"
artists$country_classified[artists$country == 'fr' ] <- "france"
artists$country_classified[artists$country == 'es' |
                              artists$country == 'pt' ] <- "spain+portugal"
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
                              artists$country == 'ye'] <- "mid east"
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
artists$country_classified[artists$country == 'ca'] <- "Canada"
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




# operationalize Press_Contact and Booking_Agent --------------------------


#PRESS CONTACT & BOOKING AGENT operationalization
artists$press_contact[is.na(artists$press_contact)] <- 0
artists$booking_agent[is.na(artists$booking_agent)] <- 0
artists$press_contact[!artists$press_contact == 0] <- 1
artists$booking_agent[!artists$booking_agent == 0] <- 1



# classify Record_Labels  ---------------------------------------------



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

artists$album_label[artists$record_label=="" | artists$record_label==","] <- NA
artists$major_label[is.na(artists$record_label)] <- NA
#artists$major_label[artists$major_label==1] <- "Major label"
#artists$major_label[artists$major_label==0] <- "Indie label"






# add Gender in dataset ---------------------------------------------------



artists$gender <- ifelse(artists$name == "María Yfeu",0, artists$gender)
artists$gender <- ifelse(artists$name == "Leatherface",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jason WhitehorAhmet Sendil",1, artists$gender)
artists$gender <- ifelse(artists$name == "Stimela",1, artists$gender)
artists$gender <- ifelse(artists$name == "Samo Zaen",0, artists$gender)
artists$gender <- ifelse(artists$name == "Cereus Bright",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Analogs",1, artists$gender)
artists$gender <- ifelse(artists$name == "Tørfisk",1, artists$gender)
artists$gender <- ifelse(artists$name == "Samo Zaen",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lauren McClinton",0, artists$gender)
artists$gender <- ifelse(artists$name == "Mark Alvarado",1, artists$gender)
artists$gender <- ifelse(artists$name == "Hoved",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dead Hand Projekt",1, artists$gender)
artists$gender <- ifelse(artists$name == "Frank's White Canvas",0, artists$gender)
artists$gender <- ifelse(artists$name == "The Funk Ark",1, artists$gender)
artists$gender <- ifelse(artists$name == "Tony Tonite",1, artists$gender)
artists$gender <- ifelse(artists$name == "Pontiacs",1, artists$gender)
artists$gender <- ifelse(artists$name == "Torro Torro",1, artists$gender)
artists$gender <- ifelse(artists$name == "Attack in Black",1, artists$gender)
artists$gender <- ifelse(artists$name == "Naïka",0, artists$gender)
artists$gender <- ifelse(artists$name == "Ippu Mitsui",1, artists$gender)
artists$gender <- ifelse(artists$name == "Central Cee",1, artists$gender)
artists$gender <- ifelse(artists$name == "Policias y Ladrones",1, artists$gender)
artists$gender <- ifelse(artists$name == "In Solitude",1, artists$gender)
artists$gender <- ifelse(artists$name == "Pää-äijät",1, artists$gender)
artists$gender <- ifelse(artists$name == "Swiernalis",1, artists$gender) 
artists$gender <- ifelse(artists$name == "Melody Noel",0, artists$gender)
artists$gender <- ifelse(artists$name == "Pala",1, artists$gender)
artists$gender <- ifelse(artists$name == "Toyboy & Robin",1, artists$gender)
artists$gender <- ifelse(artists$name == "Subwave",1, artists$gender)
artists$gender <- ifelse(artists$name == "Andrew Simple",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Black Atlantic",1, artists$gender)
artists$gender <- ifelse(artists$name == "LA Synthesis",1, artists$gender)
artists$gender <- ifelse(artists$name == "J.B.O.",1, artists$gender)
artists$gender <- ifelse(artists$name == "Instrumenti",1, artists$gender)
artists$gender <- ifelse(artists$name == "Musicologo Y Menes",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Interbeing",1, artists$gender)
artists$gender <- ifelse(artists$name == "Hightower",1, artists$gender)
artists$gender <- ifelse(artists$name == "JJ Demon",1, artists$gender)
artists$gender <- ifelse(artists$name == "Christian Smith",1, artists$gender)
artists$gender <- ifelse(artists$name == "Kepa",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Frowning Clouds",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lazer/Wulf",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ruby Escobar",0, artists$gender)
artists$gender <- ifelse(artists$name == "Tu Fazo",1, artists$gender)
artists$gender <- ifelse(artists$name == "Pop. 1280",1, artists$gender)
artists$gender <- ifelse(artists$name == "Prof.logik",1, artists$gender)
artists$gender <- ifelse(artists$name == "Kongh",1, artists$gender)
artists$gender <- ifelse(artists$name == "Biosphere",1, artists$gender)
artists$gender <- ifelse(artists$name == "Deafkids",1, artists$gender)
artists$gender <- ifelse(artists$name == "ill-esha",0, artists$gender)
artists$gender <- ifelse(artists$name == "Atencion Tsunami",1, artists$gender)
artists$gender <- ifelse(artists$name == "Main Attrakionz",1, artists$gender)
artists$gender <- ifelse(artists$name == "Spawn of Possession",1, artists$gender)
artists$gender <- ifelse(artists$name == "Leah James",0, artists$gender)
artists$gender <- ifelse(artists$name == "Running Death",1, artists$gender)
artists$gender <- ifelse(artists$name == "We Never Learned To Live",1, artists$gender)
artists$gender <- ifelse(artists$name == "Daal Band",1, artists$gender)
artists$gender <- ifelse(artists$name == "38 Spesh",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bonusbaby",0, artists$gender)
artists$gender <- ifelse(artists$name == "Martin Gutierrez",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lucky Monkey",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lucas Jussen",1, artists$gender)
artists$gender <- ifelse(artists$name == "Yung",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lucern Raze",1, artists$gender)
artists$gender <- ifelse(artists$name == "SickStrophe",1, artists$gender)
artists$gender <- ifelse(artists$name == "Florian Schirmacher",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Bacon Brothers",1, artists$gender)
artists$gender <- ifelse(artists$name == "Krauka",1, artists$gender)
artists$gender <- ifelse(artists$name == "Fetitxe",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lady Danville",0, artists$gender)
artists$gender <- ifelse(artists$name == "Afrika Bambaataa & The Soulsonic Force",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dead Empires",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dan Hyde",1, artists$gender)
artists$gender <- ifelse(artists$name == "Cemetery Drive",1, artists$gender)
artists$gender <- ifelse(artists$name == "Pueblo Cafe",1, artists$gender)
artists$gender <- ifelse(artists$name == "Empyrios",0, artists$gender)
artists$gender <- ifelse(artists$name == "Distance",1, artists$gender)
artists$gender <- ifelse(artists$name == "North Atlantic Drift",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bruno Helstroffer",1, artists$gender)
artists$gender <- ifelse(artists$name == "Fliehende Stürme",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jerrod",1, artists$gender)
artists$gender <- ifelse(artists$name == "Counterpunch",1, artists$gender)
artists$gender <- ifelse(artists$name == "Overtime Boyz",1, artists$gender)
artists$gender <- ifelse(artists$name == "Beer Bear",1, artists$gender)
artists$gender <- ifelse(artists$name == "Kingspade",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sister Sadie",0, artists$gender)
artists$gender <- ifelse(artists$name == "Shuicide Holla",1, artists$gender)
artists$gender <- ifelse(artists$name == "Oxbow",1, artists$gender)
artists$gender <- ifelse(artists$name == "Exarsis",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Cortinas",1, artists$gender)
artists$gender <- ifelse(artists$name == "Uhm Jung Hwa",0, artists$gender)
artists$gender <- ifelse(artists$name == "Autistic Youth",1, artists$gender)
artists$gender <- ifelse(artists$name == "Gelgia Caduff",0, artists$gender)
artists$gender <- ifelse(artists$name == "Esteble",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jonny Hefty & Jøden",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Hitcher",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jeremias Lawson",1, artists$gender)
artists$gender <- ifelse(artists$name == "So Sick Social Club",1, artists$gender)
artists$gender <- ifelse(artists$name == "Light Your Anchor",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Crawling",1, artists$gender)
artists$gender <- ifelse(artists$name == "Extol",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sinners & Saints",1, artists$gender)
artists$gender <- ifelse(artists$name == "David Marston",1, artists$gender)
artists$gender <- ifelse(artists$name == "Good Lovelies",0, artists$gender)
artists$gender <- ifelse(artists$name == "The Glorious Unseen",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Eyes In The Heat",1, artists$gender)
artists$gender <- ifelse(artists$name == "Gürkan Demirez",1, artists$gender)
artists$gender <- ifelse(artists$name == "Reagan Youth",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Black Marble Selection",1, artists$gender)
artists$gender <- ifelse(artists$name == "Trudy and the Romance",1, artists$gender)
artists$gender <- ifelse(artists$name == "Emmecosta",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jacek Stachursky",1, artists$gender)
artists$gender <- ifelse(artists$name == "City Of Glass",1, artists$gender)
artists$gender <- ifelse(artists$name == "Panychida",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dawen Wang",1, artists$gender)
artists$gender <- ifelse(artists$name == "Terrorfakt",1, artists$gender)
artists$gender <- ifelse(artists$name == "Tom. G",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bambu",1, artists$gender)
artists$gender <- ifelse(artists$name == "Monolithe",1, artists$gender)
artists$gender <- ifelse(artists$name == "S-TOOL",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sannia",0, artists$gender)
artists$gender <- ifelse(artists$name == "Convex",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dead Head",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bastard Grave",1, artists$gender)
artists$gender <- ifelse(artists$name == "Neuropa",1, artists$gender)
artists$gender <- ifelse(artists$name == "Voice Of The Seven Woods",1, artists$gender)
artists$gender <- ifelse(artists$name == "Rnie",1, artists$gender)
artists$gender <- ifelse(artists$name == "Made of Oak",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Celestics",1, artists$gender)
artists$gender <- ifelse(artists$name == "Our Last Enemy",1, artists$gender)
artists$gender <- ifelse(artists$name == "Javier Ojeda",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dizmas",1, artists$gender)
artists$gender <- ifelse(artists$name == "William Pascal",1, artists$gender)
artists$gender <- ifelse(artists$name == "Cantaritos De Oro",1, artists$gender)
artists$gender <- ifelse(artists$name == "Adrian Duffy & The Mayo Brothers",1, artists$gender)
artists$gender <- ifelse(artists$name == "Midnight Conspiracy",1, artists$gender)
artists$gender <- ifelse(artists$name == "Noelle Chiodo",1, artists$gender)
artists$gender <- ifelse(artists$name == "Valley of Chrome",1, artists$gender)
artists$gender <- ifelse(artists$name == "Snacky Chan",1, artists$gender)
artists$gender <- ifelse(artists$name == "Suely Façanha",0, artists$gender)
artists$gender <- ifelse(artists$name == "Jeanne Mascarenhas",1, artists$gender)
artists$gender <- ifelse(artists$name == "Caves of Steel",1, artists$gender)
artists$gender <- ifelse(artists$name == "Meggy",0, artists$gender)
artists$gender <- ifelse(artists$name == "SpectraSoul",1, artists$gender)
artists$gender <- ifelse(artists$name == "Memo an Miller",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Bonnevilles",1, artists$gender)
artists$gender <- ifelse(artists$name == "Los Bengala",1, artists$gender)
artists$gender <- ifelse(artists$name == "Julian Moon",0, artists$gender)
artists$gender <- ifelse(artists$name == "Eleni Hatzidou",0, artists$gender)
artists$gender <- ifelse(artists$name == "Sleeping Pulse",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ill Al Skratch",1, artists$gender)
artists$gender <- ifelse(artists$name == "Insist",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Young Professionals",1, artists$gender)
artists$gender <- ifelse(artists$name == "Alba Lua",1, artists$gender)
artists$gender <- ifelse(artists$name == "Metal Inquisitor",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Boy Will Drown",1, artists$gender)
artists$gender <- ifelse(artists$name == "Immortal",1, artists$gender)
artists$gender <- ifelse(artists$name == "Gallos",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mute Choir",1, artists$gender)
artists$gender <- ifelse(artists$name == "Deal Pacino",1, artists$gender)
artists$gender <- ifelse(artists$name == "Banda Kora",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jenny Oaks Baker",0, artists$gender)
artists$gender <- ifelse(artists$name == "Black Eyed Vermillion",1, artists$gender)
artists$gender <- ifelse(artists$name == "LightGuides",1, artists$gender)
artists$gender <- ifelse(artists$name == "Shadow System",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sleeping Bag",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Fad",1, artists$gender)
artists$gender <- ifelse(artists$name == "Tripnotic",1, artists$gender)
artists$gender <- ifelse(artists$name == "Amorphous Androgynous",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ecos del Rocio",1, artists$gender)
artists$gender <- ifelse(artists$name == "Nicolas Taboada",1, artists$gender)
artists$gender <- ifelse(artists$name == "German Carreño Y Su Orquesta",1, artists$gender)
artists$gender <- ifelse(artists$name == "Anthony Skinner",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lady Brian",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Honeysticks",1, artists$gender)
artists$gender <- ifelse(artists$name == "Rews",0, artists$gender)
artists$gender <- ifelse(artists$name == "Swearing At Motorists",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jan PerDu",1, artists$gender)
artists$gender <- ifelse(artists$name == "Nicklas Sahl",1, artists$gender)
artists$gender <- ifelse(artists$name == "Metin Kemal Kahraman",1, artists$gender)
artists$gender <- ifelse(artists$name == "GIANNI TAYLOR",1, artists$gender)
artists$gender <- ifelse(artists$name == "Noori",1, artists$gender)
artists$gender <- ifelse(artists$name == "Scott Pemberton",1, artists$gender)
artists$gender <- ifelse(artists$name == "Soilent Green",1, artists$gender)
artists$gender <- ifelse(artists$name == "Trayce",1, artists$gender)
artists$gender <- ifelse(artists$name == "Edgar Moreau",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jody",0, artists$gender)
artists$gender <- ifelse(artists$name == "Yajaira",0, artists$gender)
artists$gender <- ifelse(artists$name == "Xana Romeo",0, artists$gender)
artists$gender <- ifelse(artists$name == "Markus Adams",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sourvein",1, artists$gender)
artists$gender <- ifelse(artists$name == "David Baron",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Last Sleepless City",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Mike Flowers Pops",1, artists$gender)
artists$gender <- ifelse(artists$name == "Taigenz",1, artists$gender)
artists$gender <- ifelse(artists$name == "Pitbulking",1, artists$gender)
artists$gender <- ifelse(artists$name == "Wrinkle Neck Mules",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bökkers",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jah6",1, artists$gender)
artists$gender <- ifelse(artists$name == "Código Neurótico",1, artists$gender)
artists$gender <- ifelse(artists$name == "G-Mainey",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dry River",1, artists$gender)
artists$gender <- ifelse(artists$name == "Vio-Lence",1, artists$gender)
artists$gender <- ifelse(artists$name == "HAARPS",1, artists$gender)
artists$gender <- ifelse(artists$name == "Hector De Cesare",1, artists$gender)
artists$gender <- ifelse(artists$name == "Masala Coffee",1, artists$gender)
artists$gender <- ifelse(artists$name == "De Underjordiske",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bullet Bane",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Monty Alexander Trio",1, artists$gender)
artists$gender <- ifelse(artists$name == "Squad Five-O",1, artists$gender)
artists$gender <- ifelse(artists$name == "Living With Lions",1, artists$gender)
artists$gender <- ifelse(artists$name == "City Rain",1, artists$gender)
artists$gender <- ifelse(artists$name == "Deproducers",1, artists$gender)
artists$gender <- ifelse(artists$name == "Total Abuse",1, artists$gender)
artists$gender <- ifelse(artists$name == "Shimmer Johnson",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Clipse",1, artists$gender)
artists$gender <- ifelse(artists$name == "MYNC",1, artists$gender)
artists$gender <- ifelse(artists$name == "Phrantic",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ana Muller",0, artists$gender)
artists$gender <- ifelse(artists$name == "Johanna Borchert",0, artists$gender)
artists$gender <- ifelse(artists$name == "King Dreams",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Supervillains",1, artists$gender)
artists$gender <- ifelse(artists$name == "Alfred Hall",1, artists$gender)
artists$gender <- ifelse(artists$name == "Kang LaMont",1, artists$gender)
artists$gender <- ifelse(artists$name == "X Maleya",1, artists$gender)
artists$gender <- ifelse(artists$name == "IKINÄ",0, artists$gender)
artists$gender <- ifelse(artists$name == "Sam Vloemans",1, artists$gender)
artists$gender <- ifelse(artists$name == "Gypsyhawk",1, artists$gender)
artists$gender <- ifelse(artists$name == "Meernaa",0, artists$gender)
artists$gender <- ifelse(artists$name == "Deep Dhillon",1, artists$gender)
artists$gender <- ifelse(artists$name == "Desolate Shrine",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sweet Angel",0, artists$gender)
artists$gender <- ifelse(artists$name == "School '94",1, artists$gender)
artists$gender <- ifelse(artists$name == "Consider the Thief",1, artists$gender)
artists$gender <- ifelse(artists$name == "Camille Christel",0, artists$gender)
artists$gender <- ifelse(artists$name == "BerlinskiBeat",1, artists$gender)
artists$gender <- ifelse(artists$name == "All Your Sisters",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jay Tablet",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mike Liebo",1, artists$gender)
artists$gender <- ifelse(artists$name == "Contemplando Su Trono",1, artists$gender)
artists$gender <- ifelse(artists$name == "8 Ball Aitken",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Meänland",1, artists$gender)
artists$gender <- ifelse(artists$name == "Graveyard Train",1, artists$gender)
artists$gender <- ifelse(artists$name == "Gadget",1, artists$gender)
artists$gender <- ifelse(artists$name == "Imminent Starvation",1, artists$gender)
artists$gender <- ifelse(artists$name == "Charlie Lane",1, artists$gender)
artists$gender <- ifelse(artists$name == "Unhindered",1, artists$gender)
artists$gender <- ifelse(artists$name == "13th Monkey",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ultranoire",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dead End Tragedy",1, artists$gender)
artists$gender <- ifelse(artists$name == "Nick Earle",1, artists$gender)
artists$gender <- ifelse(artists$name == "GARZA",1, artists$gender)
artists$gender <- ifelse(artists$name == "Hnos Silva",1, artists$gender)
artists$gender <- ifelse(artists$name == "Xerox & Illumination",1, artists$gender)
artists$gender <- ifelse(artists$name == "Civil Disobedience",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ma-E",1, artists$gender)
artists$gender <- ifelse(artists$name == "Superheaven",1, artists$gender)
artists$gender <- ifelse(artists$name == "SuenaTron",1, artists$gender)
artists$gender <- ifelse(artists$name == "E A Terra Nunca Me Pareceu Tão Distante",1, artists$gender)
artists$gender <- ifelse(artists$name == "Milczenie Owiec",1, artists$gender)
artists$gender <- ifelse(artists$name == "J Young MDK",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lex Lu",0, artists$gender)
artists$gender <- ifelse(artists$name == "LostAlone",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dtwice",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bishu",1, artists$gender)
artists$gender <- ifelse(artists$name == "Los Viernes Swing Band",1, artists$gender)
artists$gender <- ifelse(artists$name == "Teresa Bergman",0, artists$gender)
artists$gender <- ifelse(artists$name == "TWINNS",1, artists$gender)
artists$gender <- ifelse(artists$name == "Quinn Cicala",1, artists$gender)
artists$gender <- ifelse(artists$name == "Pornopop",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sacral Rage",1, artists$gender)
artists$gender <- ifelse(artists$name == "PAPA TANK",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Younger Lovers",1, artists$gender)
artists$gender <- ifelse(artists$name == "iZaak",1, artists$gender)
artists$gender <- ifelse(artists$name == "Killing Me Inside",1, artists$gender)
artists$gender <- ifelse(artists$name == "Adred",1, artists$gender)
artists$gender <- ifelse(artists$name == "Larry Dunn",1, artists$gender)
artists$gender <- ifelse(artists$name == "Larry Dunn",1, artists$gender)
artists$gender <- ifelse(artists$name == "Marlene Kuntz",0, artists$gender)
artists$gender <- ifelse(artists$name == "Sandra Muente",0, artists$gender)
artists$gender <- ifelse(artists$name == "J-BEATZ",1, artists$gender)
artists$gender <- ifelse(artists$name == "Samson For President",1, artists$gender)
artists$gender <- ifelse(artists$name == "Undercover",1, artists$gender)
artists$gender <- ifelse(artists$name == "Painful by Kisses",1, artists$gender)
artists$gender <- ifelse(artists$name == "Miel De Montagne",1, artists$gender)
artists$gender <- ifelse(artists$name == "Josie Dunne",0, artists$gender)
artists$gender <- ifelse(artists$name == "Ray Fulcher",1, artists$gender)
artists$gender <- ifelse(artists$name == "Latasha Lee",0, artists$gender)
artists$gender <- ifelse(artists$name == "Alaya",0, artists$gender)
artists$gender <- ifelse(artists$name == "Scratch Acid",1, artists$gender)
artists$gender <- ifelse(artists$name == "Divinity Roxx",0, artists$gender)
artists$gender <- ifelse(artists$name == "Los Morunos",1, artists$gender)
artists$gender <- ifelse(artists$name == "Big Oso Loc",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dan Tepfer",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dzö-nga",1, artists$gender)
artists$gender <- ifelse(artists$name == "Abiotic",1, artists$gender)
artists$gender <- ifelse(artists$name == "Feral Children",1, artists$gender)
artists$gender <- ifelse(artists$name == "Kolpa",1, artists$gender)
artists$gender <- ifelse(artists$name == "Infuze",1, artists$gender)
artists$gender <- ifelse(artists$name == "Blackguard",1, artists$gender)
artists$gender <- ifelse(artists$name == "Playmen",1, artists$gender)
artists$gender <- ifelse(artists$name == "Likanen Etelä",1, artists$gender)
artists$gender <- ifelse(artists$name == "Conor Matthews",1, artists$gender)
artists$gender <- ifelse(artists$name == "Antun Opic",1, artists$gender)
artists$gender <- ifelse(artists$name == "Kim Gray",1, artists$gender)
artists$gender <- ifelse(artists$name == "Divine",1, artists$gender)
artists$gender <- ifelse(artists$name == "Korzus",1, artists$gender)
artists$gender <- ifelse(artists$name == "Shane-O",1, artists$gender)
artists$gender <- ifelse(artists$name == "TonyOzs",1, artists$gender)
artists$gender <- ifelse(artists$name == "Milking the Goatmachine",1, artists$gender)
artists$gender <- ifelse(artists$name == "UnityTX",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mikroboy",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mastic Scum",1, artists$gender)
artists$gender <- ifelse(artists$name == "Moda Loda Broda",1, artists$gender)
artists$gender <- ifelse(artists$name == "Faber Drive",1, artists$gender)
artists$gender <- ifelse(artists$name == "Saint Punk",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sharlota",0, artists$gender)
artists$gender <- ifelse(artists$name == "Five Mile Sniper",1, artists$gender)
artists$gender <- ifelse(artists$name == "Asia Whiteacre",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Rogue Element",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dirty Streets",1, artists$gender)
artists$gender <- ifelse(artists$name == "Preto & Branco",1, artists$gender)
artists$gender <- ifelse(artists$name == "AVGVSTVS",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dizraeli",1, artists$gender)
artists$gender <- ifelse(artists$name == "Belmar",1, artists$gender)
artists$gender <- ifelse(artists$name == "Pharao Black Magic",1, artists$gender)
artists$gender <- ifelse(artists$name == "Salut Salon",0, artists$gender)
artists$gender <- ifelse(artists$name == "Zen Baboon",1, artists$gender)
artists$gender <- ifelse(artists$name == "Liquid Liquid",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mevrouw Tamara",0, artists$gender)
artists$gender <- ifelse(artists$name == "Ramón Ayala",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ignite",1, artists$gender)
artists$gender <- ifelse(artists$name == "Vanessa Van Basten",0, artists$gender)
artists$gender <- ifelse(artists$name == "Kerry Hart",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lenny Ibizarre",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Tied",1, artists$gender)
artists$gender <- ifelse(artists$name == "dBridge",1, artists$gender)
artists$gender <- ifelse(artists$name == "Half the Animal",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dirty Deep",1, artists$gender)
artists$gender <- ifelse(artists$name == "Alibrorsh",1, artists$gender)
artists$gender <- ifelse(artists$name == "Seth Cook",1, artists$gender)
artists$gender <- ifelse(artists$name == "Old Gray",1, artists$gender)
artists$gender <- ifelse(artists$name == "Stanton Lanier",1, artists$gender)
artists$gender <- ifelse(artists$name == "Publicist UK",1, artists$gender)
artists$gender <- ifelse(artists$name == "MC Villain",1, artists$gender)
artists$gender <- ifelse(artists$name == "Tvärvägen",1, artists$gender)
artists$gender <- ifelse(artists$name == "Steff Reed",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dub Trio",1, artists$gender)
artists$gender <- ifelse(artists$name == "MD Electro",1, artists$gender)
artists$gender <- ifelse(artists$name == "Erdmöbel",1, artists$gender)
artists$gender <- ifelse(artists$name == "Timbah",1, artists$gender)
artists$gender <- ifelse(artists$name == "OWLS",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dj Ghosty",1, artists$gender)
artists$gender <- ifelse(artists$name == "Karpatt",1, artists$gender)
artists$gender <- ifelse(artists$name == "Banner Pilot",1, artists$gender)
artists$gender <- ifelse(artists$name == "Miljardid",1, artists$gender)
artists$gender <- ifelse(artists$name == "TC",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Desert Rose Band",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dance Cult",1, artists$gender)
artists$gender <- ifelse(artists$name == "jacey j.",1, artists$gender)
artists$gender <- ifelse(artists$name == "That Fucking Tank",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jimmy Vaughan",1, artists$gender)
artists$gender <- ifelse(artists$name == "Adult Jazz",1, artists$gender)
artists$gender <- ifelse(artists$name == "Tribali",1, artists$gender)
artists$gender <- ifelse(artists$name == "Junior Boy",1, artists$gender)
artists$gender <- ifelse(artists$name == "Phanee de Pool",1, artists$gender)
artists$gender <- ifelse(artists$name == "Carl-Éric Hudon",1, artists$gender)
artists$gender <- ifelse(artists$name == "Cam Beszkin",0, artists$gender)
artists$gender <- ifelse(artists$name == "Finnish Hockey Mafia",1, artists$gender)
artists$gender <- ifelse(artists$name == "Necrot",1, artists$gender)
artists$gender <- ifelse(artists$name == "Fora des sembrat",1, artists$gender)
artists$gender <- ifelse(artists$name == "iamchelseaiam",0, artists$gender)
artists$gender <- ifelse(artists$name == "PostScriptum",1, artists$gender)
artists$gender <- ifelse(artists$name == "LEE HI",0, artists$gender)
artists$gender <- ifelse(artists$name == "Kiwa",1, artists$gender)
artists$gender <- ifelse(artists$name == "4th & Orange",1, artists$gender)
artists$gender <- ifelse(artists$name == "Raw District",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dazastah",1, artists$gender)
artists$gender <- ifelse(artists$name == "Zebra and Snake",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bel Plaine",0, artists$gender)
artists$gender <- ifelse(artists$name == "MC Chucky",1, artists$gender)
artists$gender <- ifelse(artists$name == "Deep Inside",1, artists$gender)
artists$gender <- ifelse(artists$name == "Arreat Summit",1, artists$gender)
artists$gender <- ifelse(artists$name == "Marcos Vidal",1, artists$gender)
artists$gender <- ifelse(artists$name == "Trepidant'S",1, artists$gender)
artists$gender <- ifelse(artists$name == "Joakim",1, artists$gender)
artists$gender <- ifelse(artists$name == "Maskavo Roots",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jason Myles Goss",1, artists$gender)
artists$gender <- ifelse(artists$name == "Knightlife",1, artists$gender)
artists$gender <- ifelse(artists$name == "Second Best",1, artists$gender)
artists$gender <- ifelse(artists$name == "Age of Taurus",1, artists$gender)
artists$gender <- ifelse(artists$name == "Pre$$ure",1, artists$gender)
artists$gender <- ifelse(artists$name == "As Autumn Calls",1, artists$gender)
artists$gender <- ifelse(artists$name == "G. Bonson",1, artists$gender)
artists$gender <- ifelse(artists$name == "Wild Marmalade",1, artists$gender)
artists$gender <- ifelse(artists$name == "ikkubaru",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jazz Pistols",1, artists$gender)
artists$gender <- ifelse(artists$name == "Volga Tamöz",1, artists$gender)
artists$gender <- ifelse(artists$name == "Melody Angel",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mark Trammell Trio",1, artists$gender)
artists$gender <- ifelse(artists$name == "Kickin Valentina",1, artists$gender)
artists$gender <- ifelse(artists$name == "Psiko",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jännerwein",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lazy Ghost",1, artists$gender)
artists$gender <- ifelse(artists$name == "Nod One's Head",0, artists$gender)
artists$gender <- ifelse(artists$name == "ZRM",1, artists$gender)
artists$gender <- ifelse(artists$name == "Danilo Brito",1, artists$gender)
artists$gender <- ifelse(artists$name == "Allison",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Reason",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ensemble Initium",1, artists$gender)
artists$gender <- ifelse(artists$name == "Nano Infect",1, artists$gender)
artists$gender <- ifelse(artists$name == "Primavera de Praga",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dylan Holland",1, artists$gender)
artists$gender <- ifelse(artists$name == "James Bambu",1, artists$gender)
artists$gender <- ifelse(artists$name == "Marble Empire",1, artists$gender)
artists$gender <- ifelse(artists$name == "Honey Harper",1, artists$gender)
artists$gender <- ifelse(artists$name == "Arbor Lights",1, artists$gender)
artists$gender <- ifelse(artists$name == "Anatol Cyberia",1, artists$gender)
artists$gender <- ifelse(artists$name == "Andre Lodemann",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Bollock Brothers",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jookabox",1, artists$gender)
artists$gender <- ifelse(artists$name == "Wesley Bright and the Honeytones",1, artists$gender)
artists$gender <- ifelse(artists$name == "THE SIMILAR",1, artists$gender)
artists$gender <- ifelse(artists$name == "Crack Of Dawn",1, artists$gender)
artists$gender <- ifelse(artists$name == "Luke De-Sciscio",1, artists$gender)
artists$gender <- ifelse(artists$name == "Torsten Kanzler",1, artists$gender)
artists$gender <- ifelse(artists$name == "Kid Kopphausen",1, artists$gender)
artists$gender <- ifelse(artists$name == "Langhorns",1, artists$gender)
artists$gender <- ifelse(artists$name == "Alarma",1, artists$gender)
artists$gender <- ifelse(artists$name == "Manfredas",1, artists$gender)
artists$gender <- ifelse(artists$name == "White Fox Society",1, artists$gender)
artists$gender <- ifelse(artists$name == "Gutslit",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sayag Jazz Machine",1, artists$gender)
artists$gender <- ifelse(artists$name == "Blanks 77",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Kilborn Alley Blues Band",1, artists$gender)
artists$gender <- ifelse(artists$name == "Roger Sanchez",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sensu",0, artists$gender)
artists$gender <- ifelse(artists$name == "Super Ratones",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dalyton Santos",1, artists$gender)
artists$gender <- ifelse(artists$name == "Nahuatl Sound System",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lanks",1, artists$gender)
artists$gender <- ifelse(artists$name == "Alex Hager",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ayria",0, artists$gender)
artists$gender <- ifelse(artists$name == "Stanton Warriors",1, artists$gender)
artists$gender <- ifelse(artists$name == "Zeke Foster",1, artists$gender)
artists$gender <- ifelse(artists$name == "Poor Man's Whiskey",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bollmer",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jorn",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Tangerines",1, artists$gender)
artists$gender <- ifelse(artists$name == "Infernal Revulsion",1, artists$gender)
artists$gender <- ifelse(artists$name == "Hannah Clive",0, artists$gender)
artists$gender <- ifelse(artists$name == "Spellcaster",1, artists$gender)
artists$gender <- ifelse(artists$name == "Tristana",1, artists$gender)
artists$gender <- ifelse(artists$name == "Emily Rowed",1, artists$gender)
artists$gender <- ifelse(artists$name == "Aysun Gültekin",0, artists$gender)
artists$gender <- ifelse(artists$name == "Jason Mighty",1, artists$gender)
artists$gender <- ifelse(artists$name == "Guineafowl",1, artists$gender)
artists$gender <- ifelse(artists$name == "Vicious Vic",1, artists$gender)
artists$gender <- ifelse(artists$name == "Drunken Dolly",1, artists$gender)
artists$gender <- ifelse(artists$name == "Cikatri$",0, artists$gender)
artists$gender <- ifelse(artists$name == "Orlando B",1, artists$gender)
artists$gender <- ifelse(artists$name == "Passenger Peru",1, artists$gender)
artists$gender <- ifelse(artists$name == "Great News",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dizzy Dros",1, artists$gender)
artists$gender <- ifelse(artists$name == "Craig Knight",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dominika Sozanska",0, artists$gender)
artists$gender <- ifelse(artists$name == "DJ Bonez",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lars Behrenroth",1, artists$gender)
artists$gender <- ifelse(artists$name == "Turbostaat",1, artists$gender)
artists$gender <- ifelse(artists$name == "End Of Green",1, artists$gender)
artists$gender <- ifelse(artists$name == "Los Crema Paraiso",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dylan Earl",1, artists$gender)
artists$gender <- ifelse(artists$name == "In Real Life",1, artists$gender)
artists$gender <- ifelse(artists$name == "New Arcades",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mthunzi",1, artists$gender)
artists$gender <- ifelse(artists$name == "Maad T-Ray",1, artists$gender)
artists$gender <- ifelse(artists$name == "Salsangroove",1, artists$gender)
artists$gender <- ifelse(artists$name == "It Bites",1, artists$gender)
artists$gender <- ifelse(artists$name == "Hayla",0, artists$gender)
artists$gender <- ifelse(artists$name == "MYLK",0, artists$gender)
artists$gender <- ifelse(artists$name == "Dissident",1, artists$gender)
artists$gender <- ifelse(artists$name == "Carridale",1, artists$gender)
artists$gender <- ifelse(artists$name == "Simply Saucer",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bhanga Bangla",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lydia",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bike Tuff",1, artists$gender)
artists$gender <- ifelse(artists$name == "Burning Babylon",1, artists$gender)
artists$gender <- ifelse(artists$name == "Moop Mama",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jay y el Punto",1, artists$gender)
artists$gender <- ifelse(artists$name == "Angrej Ali",1, artists$gender)
artists$gender <- ifelse(artists$name == "Atlantida Project",0, artists$gender)
artists$gender <- ifelse(artists$name == "Esoterik",1, artists$gender)
artists$gender <- ifelse(artists$name == "R.O.E.",0, artists$gender)
artists$gender <- ifelse(artists$name == "The Physics House Band",1, artists$gender)
artists$gender <- ifelse(artists$name == "Weeping Wound",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mad Hawkes",0, artists$gender)
artists$gender <- ifelse(artists$name == "The Dead Rocks",1, artists$gender)
artists$gender <- ifelse(artists$name == "Hederos & Hellberg",1, artists$gender)
artists$gender <- ifelse(artists$name == "Blackbyrds",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Modern Times",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dias de Truta",1, artists$gender)
artists$gender <- ifelse(artists$name == "Southside Johnny And The Asbury Jukes",1, artists$gender)
artists$gender <- ifelse(artists$name == "Vaines",1, artists$gender)
artists$gender <- ifelse(artists$name == "Midnight Odyssey",1, artists$gender)
artists$gender <- ifelse(artists$name == "Hood Smoke",1, artists$gender)
artists$gender <- ifelse(artists$name == "Nadia Batson",0, artists$gender)
artists$gender <- ifelse(artists$name == "She's Only Sixteen",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ludo",1, artists$gender)
artists$gender <- ifelse(artists$name == "Pflames",1, artists$gender)
artists$gender <- ifelse(artists$name == "Macromism",1, artists$gender)
artists$gender <- ifelse(artists$name == "Chamaeleon",1, artists$gender)
artists$gender <- ifelse(artists$name == "Flotsam & Jetsam",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dusty Boots",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sebastian Zalo",1, artists$gender)
artists$gender <- ifelse(artists$name == "Carreyó",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lemot",1, artists$gender)
artists$gender <- ifelse(artists$name == "Héctor Hermosillo",1, artists$gender)
artists$gender <- ifelse(artists$name == "Arc In Round",1, artists$gender)
artists$gender <- ifelse(artists$name == "Windwaker",1, artists$gender)
artists$gender <- ifelse(artists$name == "Peyote",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ornamento y Delito",1, artists$gender)
artists$gender <- ifelse(artists$name == "Flor Amargo",0, artists$gender)
artists$gender <- ifelse(artists$name == "Tom Cochrane & Red Rider",1, artists$gender)
artists$gender <- ifelse(artists$name == "Clara Louise",0, artists$gender)
artists$gender <- ifelse(artists$name == "Carolyn Rodriguez",0, artists$gender)
artists$gender <- ifelse(artists$name == "Eric Strickland & the B Sides",1, artists$gender)
artists$gender <- ifelse(artists$name == "La Locomotora Negra",1, artists$gender)
artists$gender <- ifelse(artists$name == "Colectro",1, artists$gender)
artists$gender <- ifelse(artists$name == "Pedro Aranha",1, artists$gender)
artists$gender <- ifelse(artists$name == "DJ Vortex",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jesus Volt",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Beautiful Ones",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bleeker",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dr. Toast",1, artists$gender)
artists$gender <- ifelse(artists$name == "Peixes Voadores",1, artists$gender)
artists$gender <- ifelse(artists$name == "Rey Three Latino",1, artists$gender)
artists$gender <- ifelse(artists$name == "Billy Boy On Poison",1, artists$gender)
artists$gender <- ifelse(artists$name == "Blowsight",1, artists$gender)
artists$gender <- ifelse(artists$name == "Among Authors",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ehsan Khajehamiri",1, artists$gender)
artists$gender <- ifelse(artists$name == "Inkline",1, artists$gender)
artists$gender <- ifelse(artists$name == "X-tassy",1, artists$gender)
artists$gender <- ifelse(artists$name == "Kingdom Of Giants",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Happenings",1, artists$gender)
artists$gender <- ifelse(artists$name == "Fortune Howl",1, artists$gender)
artists$gender <- ifelse(artists$name == "Misery Index",1, artists$gender)
artists$gender <- ifelse(artists$name == "Leshak",1, artists$gender)
artists$gender <- ifelse(artists$name == "OHMYMEITING",0, artists$gender)
artists$gender <- ifelse(artists$name == "The Graviators",1, artists$gender)
artists$gender <- ifelse(artists$name == "NQ Arbuckle",1, artists$gender)
artists$gender <- ifelse(artists$name == "Brightwood",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dialects",1, artists$gender)
artists$gender <- ifelse(artists$name == "Andy Bianchini",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ressonadors",1, artists$gender)
artists$gender <- ifelse(artists$name == "Satan's Pilgrims",1, artists$gender)
artists$gender <- ifelse(artists$name == "Whiptongue",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Gants",1, artists$gender)
artists$gender <- ifelse(artists$name == "Blancmange",1, artists$gender)
artists$gender <- ifelse(artists$name == "Les Techno",1, artists$gender)
artists$gender <- ifelse(artists$name == "Stefano Pain",1, artists$gender)
artists$gender <- ifelse(artists$name == "Grasscut",1, artists$gender)
artists$gender <- ifelse(artists$name == "Pilot Speed",1, artists$gender)
artists$gender <- ifelse(artists$name == "Marvin & Guy",1, artists$gender)
artists$gender <- ifelse(artists$name == "Northern Faces",1, artists$gender)
artists$gender <- ifelse(artists$name == "Donkey Rollers",1, artists$gender)
artists$gender <- ifelse(artists$name == "Eyekonik",1, artists$gender)
artists$gender <- ifelse(artists$name == "Amerakin Overdose",1, artists$gender)
artists$gender <- ifelse(artists$name == "A Classic Education",1, artists$gender)
artists$gender <- ifelse(artists$name == "Die Paldauer",1, artists$gender)
artists$gender <- ifelse(artists$name == "To Rococo Rot",1, artists$gender)
artists$gender <- ifelse(artists$name == "Eye of the Enemy",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Whistles & The Bells",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jorge Nava",1, artists$gender)
artists$gender <- ifelse(artists$name == "Kid Mac",1, artists$gender)
artists$gender <- ifelse(artists$name == "Age Factory",1, artists$gender)
artists$gender <- ifelse(artists$name == "Hard-Ons",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ablaze My Sorrow",1, artists$gender)
artists$gender <- ifelse(artists$name == "Denouncement Pyre",1, artists$gender)
artists$gender <- ifelse(artists$name == "Humble the Poet",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sleepytime Gorilla Museum",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mai Cocopelli",0, artists$gender)
artists$gender <- ifelse(artists$name == "Perla Colombiana",1, artists$gender)
artists$gender <- ifelse(artists$name == "One Year Later",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ocean Avenue",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jack Gibbons",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jeremy Weinglass",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lirow",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Inazuma Sentai",1, artists$gender)
artists$gender <- ifelse(artists$name == "Obnoxious Youth",1, artists$gender)
artists$gender <- ifelse(artists$name == "In League",1, artists$gender)
artists$gender <- ifelse(artists$name == "CJay",1, artists$gender)
artists$gender <- ifelse(artists$name == "Singer Dr. B...",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bas Roos",1, artists$gender)
artists$gender <- ifelse(artists$name == "Renacer Perú",1, artists$gender)
artists$gender <- ifelse(artists$name == "Music Mafia",1, artists$gender)
artists$gender <- ifelse(artists$name == "Negative Gemini",1, artists$gender)
artists$gender <- ifelse(artists$name == "Seedy Jeezus",1, artists$gender)
artists$gender <- ifelse(artists$name == "Rick Tippe",1, artists$gender)
artists$gender <- ifelse(artists$name == "Darwin Lechler",1, artists$gender)
artists$gender <- ifelse(artists$name == "backinhumanform",1, artists$gender)
artists$gender <- ifelse(artists$name == "Goldsmoke",1, artists$gender)
artists$gender <- ifelse(artists$name == "Max Chinasky",1, artists$gender)
artists$gender <- ifelse(artists$name == "Milkman",1, artists$gender)
artists$gender <- ifelse(artists$name == "Amante Lacey",1, artists$gender)
artists$gender <- ifelse(artists$name == "Re Dupre",1, artists$gender)
artists$gender <- ifelse(artists$name == "Colours",1, artists$gender)
artists$gender <- ifelse(artists$name == "Coppelius",1, artists$gender)
artists$gender <- ifelse(artists$name == "Leslie Tay",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mental Issues",1, artists$gender)
artists$gender <- ifelse(artists$name == "Kudai",1, artists$gender)
artists$gender <- ifelse(artists$name == "Marc Antoine",1, artists$gender)
artists$gender <- ifelse(artists$name == "Hecate Enthroned",1, artists$gender)
artists$gender <- ifelse(artists$name == "Caïna",1, artists$gender)
artists$gender <- ifelse(artists$name == "Priory",1, artists$gender)
artists$gender <- ifelse(artists$name == "G. Love & Special Sauce",1, artists$gender)
artists$gender <- ifelse(artists$name == "Crazibiza",1, artists$gender)
artists$gender <- ifelse(artists$name == "Terrence Pearce",1, artists$gender)
artists$gender <- ifelse(artists$name == "Marta Sui Tubi",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dondria",1, artists$gender)
artists$gender <- ifelse(artists$name == "Minitel Rose",1, artists$gender)
artists$gender <- ifelse(artists$name == "Klubbheads",1, artists$gender)
artists$gender <- ifelse(artists$name == "Shane Smith & the Saints",1, artists$gender)
artists$gender <- ifelse(artists$name == "Baketha",1, artists$gender)
artists$gender <- ifelse(artists$name == "NUTRONIC",1, artists$gender)
artists$gender <- ifelse(artists$name == "Andy Salvanos",1, artists$gender)
artists$gender <- ifelse(artists$name == "M-Dot",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Walls",1, artists$gender)
#artists$gender <- ifelse(artists$name == "Francisco "El Chico" Elizalde",1, artists$gender)
artists$gender <- ifelse(artists$name == "Midget Loco",1, artists$gender)
artists$gender <- ifelse(artists$name == "Prkr",1, artists$gender)
artists$gender <- ifelse(artists$name == "Cosmic Silence",1, artists$gender)
artists$gender <- ifelse(artists$name == "Maximo Norte",1, artists$gender)
artists$gender <- ifelse(artists$name == "Un Planeta",1, artists$gender)
artists$gender <- ifelse(artists$name == "Cathedral",1, artists$gender)
artists$gender <- ifelse(artists$name == "Choir Boy",1, artists$gender)
artists$gender <- ifelse(artists$name == "ZZT",1, artists$gender)
artists$gender <- ifelse(artists$name == "Damaui",1, artists$gender)
artists$gender <- ifelse(artists$name == "18 Kilates",1, artists$gender)
artists$gender <- ifelse(artists$name == "Arkabuz",1, artists$gender)
artists$gender <- ifelse(artists$name == "Antix",1, artists$gender)
artists$gender <- ifelse(artists$name == "Marty Ray",1, artists$gender)
artists$gender <- ifelse(artists$name == "Cassandra Jenkins",0, artists$gender)
artists$gender <- ifelse(artists$name == "One Dimensional Man",1, artists$gender)
artists$gender <- ifelse(artists$name == "Josh X",1, artists$gender)
artists$gender <- ifelse(artists$name == "Cancer Bats",1, artists$gender)
artists$gender <- ifelse(artists$name == "Atom Rhumba",1, artists$gender)
artists$gender <- ifelse(artists$name == "Gudicarmas",1, artists$gender)
artists$gender <- ifelse(artists$name == "DJ Fudge",1, artists$gender)
artists$gender <- ifelse(artists$name == "Tribe of Benjamin",1, artists$gender)
artists$gender <- ifelse(artists$name == "Riky",1, artists$gender)
artists$gender <- ifelse(artists$name == "Justice Hmix",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ja Ja Dickicht",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bullmeister",1, artists$gender)
artists$gender <- ifelse(artists$name == "Romeofoxtrott",1, artists$gender)
artists$gender <- ifelse(artists$name == "John Calvin Abney",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sun K",1, artists$gender)
artists$gender <- ifelse(artists$name == "Wildling",1, artists$gender)
artists$gender <- ifelse(artists$name == "Yarol",1, artists$gender)
artists$gender <- ifelse(artists$name == "Johnny Manchild and the Poor Bastards",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bo Weber",1, artists$gender)
artists$gender <- ifelse(artists$name == "Pesawat",1, artists$gender)
artists$gender <- ifelse(artists$name == "Eddie Hale",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Modern Age Slavery",1, artists$gender)
artists$gender <- ifelse(artists$name == "Los Huracanes",1, artists$gender)
artists$gender <- ifelse(artists$name == "David Hasert",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Kinsey Report",1, artists$gender)
artists$gender <- ifelse(artists$name == "Califone",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lillix",0, artists$gender)
artists$gender <- ifelse(artists$name == "Jazzmeia Horn",0, artists$gender)
artists$gender <- ifelse(artists$name == "Maraton",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sigrid Moldestad",0, artists$gender)
artists$gender <- ifelse(artists$name == "Dan San",1, artists$gender)
artists$gender <- ifelse(artists$name == "Trash Talk",1, artists$gender)
artists$gender <- ifelse(artists$name == "Theudho",1, artists$gender)
artists$gender <- ifelse(artists$name == "DJ Slugo",1, artists$gender)
artists$gender <- ifelse(artists$name == "Steve Coleman and Five Elements",1, artists$gender)
artists$gender <- ifelse(artists$name == "Doomraiser",1, artists$gender)
artists$gender <- ifelse(artists$name == "Double Experience",1, artists$gender)
artists$gender <- ifelse(artists$name == "Extortionist",1, artists$gender)
artists$gender <- ifelse(artists$name == "Supercordas",1, artists$gender)
artists$gender <- ifelse(artists$name == "Devan Ibiza",1, artists$gender)
artists$gender <- ifelse(artists$name == "Roladex",1, artists$gender)
artists$gender <- ifelse(artists$name == "Joseph Sullinger",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ryanhood",1, artists$gender)
artists$gender <- ifelse(artists$name == "TDK",1, artists$gender)
artists$gender <- ifelse(artists$name == "ONYVAA",0, artists$gender)
artists$gender <- ifelse(artists$name == "RYBO",1, artists$gender)
artists$gender <- ifelse(artists$name == "DJ Eco",1, artists$gender)
artists$gender <- ifelse(artists$name == "Travis McCoy",1, artists$gender)
artists$gender <- ifelse(artists$name == "Billon",1, artists$gender)
artists$gender <- ifelse(artists$name == "Skye Strickler",1, artists$gender)
artists$gender <- ifelse(artists$name == "Hoku Zuttermeister",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Zolas",1, artists$gender)
artists$gender <- ifelse(artists$name == "J-Dooly",1, artists$gender)
artists$gender <- ifelse(artists$name == "BROOKLNN",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Cry",1, artists$gender)
artists$gender <- ifelse(artists$name == "We Still Dream",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Elected",1, artists$gender)
artists$gender <- ifelse(artists$name == "Harvey McKay",1, artists$gender)
artists$gender <- ifelse(artists$name == "SVNTOZ",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ney Faustini",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ronnie Day",1, artists$gender)
artists$gender <- ifelse(artists$name == "Claudia Hoyser",0, artists$gender)
artists$gender <- ifelse(artists$name == "Dusty Stray",1, artists$gender)
artists$gender <- ifelse(artists$name == "Panzer Flower",1, artists$gender)
artists$gender <- ifelse(artists$name == "Pink Turns Blue",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mountain Lakes",1, artists$gender)
artists$gender <- ifelse(artists$name == "Animal Kingdom",1, artists$gender)
artists$gender <- ifelse(artists$name == "S.T.I.C.S",1, artists$gender)
artists$gender <- ifelse(artists$name == "Erika Wennerstrom",0, artists$gender)
artists$gender <- ifelse(artists$name == "Big Ups",1, artists$gender)
artists$gender <- ifelse(artists$name == "KoxBox",1, artists$gender)
artists$gender <- ifelse(artists$name == "Julia Vari",0, artists$gender)
artists$gender <- ifelse(artists$name == "River Jones",1, artists$gender)
artists$gender <- ifelse(artists$name == "Death Tyrant",1, artists$gender)
artists$gender <- ifelse(artists$name == "All The Best Tapes",1, artists$gender)
artists$gender <- ifelse(artists$name == "Yung Tilla",1, artists$gender)
artists$gender <- ifelse(artists$name == "Johnny Stranger",1, artists$gender)
artists$gender <- ifelse(artists$name == "Orchid",1, artists$gender)
artists$gender <- ifelse(artists$name == "Los Cartageneros",1, artists$gender)
artists$gender <- ifelse(artists$name == "HypoFixx",1, artists$gender)
artists$gender <- ifelse(artists$name == "Smif-N-Wessun",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mark McKinney",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ray Foxx",1, artists$gender)
artists$gender <- ifelse(artists$name == "Beogradski Sindikat",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Sainte Catherines",1, artists$gender)
artists$gender <- ifelse(artists$name == "Barcenilla",1, artists$gender)
artists$gender <- ifelse(artists$name == "Heretic Klick",1, artists$gender)
artists$gender <- ifelse(artists$name == "Monkeybrain",1, artists$gender)
artists$gender <- ifelse(artists$name == "RESO",1, artists$gender)
artists$gender <- ifelse(artists$name == "Opal Ocean",1, artists$gender)
artists$gender <- ifelse(artists$name == "Eltin",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mattblú",1, artists$gender)
artists$gender <- ifelse(artists$name == "Summer Wars",1, artists$gender)
artists$gender <- ifelse(artists$name == "Henri Vuorenmaa",1, artists$gender)
artists$gender <- ifelse(artists$name == "Taylor Henderson",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ager Sonus",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Debonaires",1, artists$gender)
artists$gender <- ifelse(artists$name == "Basement Birds",1, artists$gender)
artists$gender <- ifelse(artists$name == "Aiden Lewis",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dena Mwana",0, artists$gender)
artists$gender <- ifelse(artists$name == "Triumphator",1, artists$gender)
artists$gender <- ifelse(artists$name == "Humbertiko",1, artists$gender)
artists$gender <- ifelse(artists$name == "Denis A",1, artists$gender)
artists$gender <- ifelse(artists$name == "benny mayne",1, artists$gender)
artists$gender <- ifelse(artists$name == "Kingston Rudieska",1, artists$gender)
artists$gender <- ifelse(artists$name == "Domkraft",1, artists$gender)
artists$gender <- ifelse(artists$name == "Da Beatminerz",1, artists$gender)
artists$gender <- ifelse(artists$name == "Early Graves",1, artists$gender)
artists$gender <- ifelse(artists$name == "Shelter Point",1, artists$gender)
artists$gender <- ifelse(artists$name == "MC Ceja",1, artists$gender)
artists$gender <- ifelse(artists$name == "Symbiz",1, artists$gender)
artists$gender <- ifelse(artists$name == "Adrian Modiggård",1, artists$gender)
artists$gender <- ifelse(artists$name == "Er305",1, artists$gender)
artists$gender <- ifelse(artists$name == "Thorpey",1, artists$gender)
artists$gender <- ifelse(artists$name == "David Lee Garza Y Los Musicales",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mustasch",1, artists$gender)
artists$gender <- ifelse(artists$name == "Brittany Pfantz",0, artists$gender)
artists$gender <- ifelse(artists$name == "Yung Von",1, artists$gender)
artists$gender <- ifelse(artists$name == "Candyland",0, artists$gender)
artists$gender <- ifelse(artists$name == "Slapdash",1, artists$gender)
artists$gender <- ifelse(artists$name == "She Past Away",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ironbird",1, artists$gender)
artists$gender <- ifelse(artists$name == "Aktaion",1, artists$gender)
artists$gender <- ifelse(artists$name == "TRICOT MACHINE",1, artists$gender)
artists$gender <- ifelse(artists$name == "Brent Cowles",1, artists$gender)
artists$gender <- ifelse(artists$name == "Emeric Imre",1, artists$gender)
artists$gender <- ifelse(artists$name == "Minuit Machine",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bugs",1, artists$gender)
artists$gender <- ifelse(artists$name == "Eskimo Joe",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ings",0, artists$gender)
artists$gender <- ifelse(artists$name == "Pete Morgan",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Brave",1, artists$gender)
artists$gender <- ifelse(artists$name == "Atiba",1, artists$gender)
artists$gender <- ifelse(artists$name == "Urban Surf Kings",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Queen's Head",1, artists$gender)
artists$gender <- ifelse(artists$name == "Cairo Knife Fight",1, artists$gender)
artists$gender <- ifelse(artists$name == "Total Chaos",1, artists$gender)
artists$gender <- ifelse(artists$name == "Johan Örjansson",1, artists$gender)
artists$gender <- ifelse(artists$name == "Los Mortero",1, artists$gender)
artists$gender <- ifelse(artists$name == "Weekend",1, artists$gender)
artists$gender <- ifelse(artists$name == "Abram Shook",1, artists$gender)
artists$gender <- ifelse(artists$name == "Soul Demise",1, artists$gender)
artists$gender <- ifelse(artists$name == "Heidevolk",1, artists$gender)
artists$gender <- ifelse(artists$name == "Voice Of A Generation",1, artists$gender)
artists$gender <- ifelse(artists$name == "Slaughter to Prevail",1, artists$gender)
artists$gender <- ifelse(artists$name == "Enisum",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Blasters",1, artists$gender)
artists$gender <- ifelse(artists$name == "Walter TV",1, artists$gender)
artists$gender <- ifelse(artists$name == "Suffer Like G Did",1, artists$gender)
artists$gender <- ifelse(artists$name == "An Horse",1, artists$gender)
artists$gender <- ifelse(artists$name == "Albeezy",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ativin",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mantar",1, artists$gender)
artists$gender <- ifelse(artists$name == "Frosti",1, artists$gender)
artists$gender <- ifelse(artists$name == "Thy Light",1, artists$gender)
artists$gender <- ifelse(artists$name == "Nasty",1, artists$gender)
artists$gender <- ifelse(artists$name == "Captain Planet",1, artists$gender)
artists$gender <- ifelse(artists$name == "Nitrogods",1, artists$gender)
artists$gender <- ifelse(artists$name == "DjSunnymega",1, artists$gender)
artists$gender <- ifelse(artists$name == "Viva Le Vox",1, artists$gender)
artists$gender <- ifelse(artists$name == "Stunde Null",1, artists$gender)
artists$gender <- ifelse(artists$name == "Cydeways",1, artists$gender)
artists$gender <- ifelse(artists$name == "Filulas Juz",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Heatwave",1, artists$gender)
artists$gender <- ifelse(artists$name == "Minor Poet",1, artists$gender)
artists$gender <- ifelse(artists$name == "Enno Cheng",0, artists$gender)
artists$gender <- ifelse(artists$name == "Random Hand",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Bad Engrish",1, artists$gender)
artists$gender <- ifelse(artists$name == "Coleman Hamilton",1, artists$gender)
artists$gender <- ifelse(artists$name == "Urban Strangers",1, artists$gender)
artists$gender <- ifelse(artists$name == "Vains Of Jenna",1, artists$gender)
artists$gender <- ifelse(artists$name == "JJ Shiplett",1, artists$gender)
artists$gender <- ifelse(artists$name == "Volldrauf",1, artists$gender)
artists$gender <- ifelse(artists$name == "Hacride",1, artists$gender)
artists$gender <- ifelse(artists$name == "Melanie",0, artists$gender)
artists$gender <- ifelse(artists$name == "B-Movie",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Sneekers",1, artists$gender)
artists$gender <- ifelse(artists$name == "Greg Keelor",1, artists$gender)
artists$gender <- ifelse(artists$name == "Last Chance Band",1, artists$gender)
artists$gender <- ifelse(artists$name == "Taylor Carson",1, artists$gender)
artists$gender <- ifelse(artists$name == "Gina Venier",1, artists$gender)
artists$gender <- ifelse(artists$name == "Wilderness Survival",1, artists$gender)
artists$gender <- ifelse(artists$name == "Luxury Grooves",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sinistarr",1, artists$gender)
artists$gender <- ifelse(artists$name == "GLXY",1, artists$gender)
artists$gender <- ifelse(artists$name == "Øresund Space Collective",1, artists$gender)
artists$gender <- ifelse(artists$name == "Van Dammes",1, artists$gender)
artists$gender <- ifelse(artists$name == "De Kift",1, artists$gender)
artists$gender <- ifelse(artists$name == "Banda Maré",1, artists$gender)
artists$gender <- ifelse(artists$name == "Thomas Brothers",1, artists$gender)
artists$gender <- ifelse(artists$name == "Gary Beck",1, artists$gender)
artists$gender <- ifelse(artists$name == "Press to MECO",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mia.",0, artists$gender)
artists$gender <- ifelse(artists$name == "The Messengers",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lorine Chia",0, artists$gender)
artists$gender <- ifelse(artists$name == "The Mothership",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bonde da Stronda",1, artists$gender)
artists$gender <- ifelse(artists$name == "Satan",1, artists$gender)
artists$gender <- ifelse(artists$name == "Reaping Asmodeia",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lifelover",1, artists$gender)
artists$gender <- ifelse(artists$name == "Hades",1, artists$gender)
artists$gender <- ifelse(artists$name == "Kasper Nyemann",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lavender Blush",0, artists$gender)
artists$gender <- ifelse(artists$name == "The Armed",1, artists$gender)
artists$gender <- ifelse(artists$name == "Chicago Toys",1, artists$gender)
artists$gender <- ifelse(artists$name == "In The Pines",1, artists$gender)
artists$gender <- ifelse(artists$name == "Madina Lake",1, artists$gender)
artists$gender <- ifelse(artists$name == "Flowers In The Dustbin",1, artists$gender)
artists$gender <- ifelse(artists$name == "Daniel Maloso",1, artists$gender)
artists$gender <- ifelse(artists$name == "Tobias Robertson",1, artists$gender)
artists$gender <- ifelse(artists$name == "Caulfield",1, artists$gender)
artists$gender <- ifelse(artists$name == "Asyndeton",1, artists$gender)
artists$gender <- ifelse(artists$name == "Per-Håkans",1, artists$gender)
artists$gender <- ifelse(artists$name == "Tobias Robertson",1, artists$gender)
artists$gender <- ifelse(artists$name == "Flesh Panthers",1, artists$gender)
artists$gender <- ifelse(artists$name == "Blackfield",1, artists$gender)
artists$gender <- ifelse(artists$name == "Minerve",1, artists$gender)
artists$gender <- ifelse(artists$name == "Blackthorn",1, artists$gender)
artists$gender <- ifelse(artists$name == "Carishma",0, artists$gender)
artists$gender <- ifelse(artists$name == "Molotow Soda",1, artists$gender)
artists$gender <- ifelse(artists$name == "KALI",0, artists$gender)
artists$gender <- ifelse(artists$name == "Tijuana Love",1, artists$gender)
artists$gender <- ifelse(artists$name == "Kaada",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bryan Kessler",1, artists$gender)
artists$gender <- ifelse(artists$name == "Hateform",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Ribeye Brothers",1, artists$gender)
artists$gender <- ifelse(artists$name == "Assembly of Dust",1, artists$gender)
artists$gender <- ifelse(artists$name == "Cassyette",0, artists$gender)
artists$gender <- ifelse(artists$name == "Turbo Fruits",1, artists$gender)
artists$gender <- ifelse(artists$name == "Kid Kenobi",1, artists$gender)
artists$gender <- ifelse(artists$name == "George Kwali",1, artists$gender)
artists$gender <- ifelse(artists$name == "Reeperbahn Kareem",1, artists$gender)
artists$gender <- ifelse(artists$name == "Pöbel & Gesocks",1, artists$gender)
artists$gender <- ifelse(artists$name == "Tin Can Phone",1, artists$gender)
artists$gender <- ifelse(artists$name == "MRKTS",1, artists$gender)
artists$gender <- ifelse(artists$name == "Preston Lovinggood",1, artists$gender)
artists$gender <- ifelse(artists$name == "RQTN",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Ocean",1, artists$gender)
artists$gender <- ifelse(artists$name == "JSTJR",1, artists$gender)
artists$gender <- ifelse(artists$name == "Nueva Era",1, artists$gender)
artists$gender <- ifelse(artists$name == "Solace",1, artists$gender)
artists$gender <- ifelse(artists$name == "Aperio",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bravestation",1, artists$gender)
artists$gender <- ifelse(artists$name == "Melech Mechaya",1, artists$gender)
artists$gender <- ifelse(artists$name == "Machines Dream",1, artists$gender)
artists$gender <- ifelse(artists$name == "Liebe Minou",0, artists$gender)
artists$gender <- ifelse(artists$name == "Eurooppa 3",1, artists$gender)
artists$gender <- ifelse(artists$name == "Machiavel",1, artists$gender)
artists$gender <- ifelse(artists$name == "Brennan Villines",1, artists$gender)
artists$gender <- ifelse(artists$name == "sqweez animal",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Digital Connection",1, artists$gender)
artists$gender <- ifelse(artists$name == "Freak Kitchen",1, artists$gender)
artists$gender <- ifelse(artists$name == "Rod Veldt",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Sonic Dawn",1, artists$gender)
artists$gender <- ifelse(artists$name == "Triosence",1, artists$gender)
artists$gender <- ifelse(artists$name == "Human Mastication",1, artists$gender)
artists$gender <- ifelse(artists$name == "Pain",1, artists$gender)
artists$gender <- ifelse(artists$name == "Nothing But Funk",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sightings",1, artists$gender)
artists$gender <- ifelse(artists$name == "the one and only PPL MVR",1, artists$gender)
artists$gender <- ifelse(artists$name == "Marah",1, artists$gender)
artists$gender <- ifelse(artists$name == "Rocío Quiroz",0, artists$gender)
artists$gender <- ifelse(artists$name == "Morphology",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Amazing Crowns",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mind Enterprises",1, artists$gender)
artists$gender <- ifelse(artists$name == "Excel Beats",1, artists$gender)
artists$gender <- ifelse(artists$name == "International Observer",1, artists$gender)
artists$gender <- ifelse(artists$name == "Volta Bureau",1, artists$gender)
artists$gender <- ifelse(artists$name == "Eliane Fernandes",0, artists$gender)
artists$gender <- ifelse(artists$name == "enkelson.",1, artists$gender)
artists$gender <- ifelse(artists$name == "Blind Mr. Jones",1, artists$gender)
artists$gender <- ifelse(artists$name == "The So So Glos",1, artists$gender)
artists$gender <- ifelse(artists$name == "WONDERFUL BOYS",1, artists$gender)
artists$gender <- ifelse(artists$name == "Spaghetti Western Orchestra",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Wagner Logic",1, artists$gender)
artists$gender <- ifelse(artists$name == "Vulkano",0, artists$gender)
artists$gender <- ifelse(artists$name == "Rare Essence",1, artists$gender)
artists$gender <- ifelse(artists$name == "35007",1, artists$gender)
artists$gender <- ifelse(artists$name == "Seas of Years",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dos Santos",1, artists$gender)
artists$gender <- ifelse(artists$name == "J-Phish",1, artists$gender)
artists$gender <- ifelse(artists$name == "Manuel Costa",1, artists$gender)
artists$gender <- ifelse(artists$name == "Edalam",1, artists$gender)
artists$gender <- ifelse(artists$name == "FO&O",1, artists$gender)
artists$gender <- ifelse(artists$name == "Kenneth Thomas",1, artists$gender)
artists$gender <- ifelse(artists$name == "Cordrazine",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lente",1, artists$gender)
artists$gender <- ifelse(artists$name == "Nachas",1, artists$gender)
artists$gender <- ifelse(artists$name == "Iba one",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bright Dog Red",1, artists$gender)
artists$gender <- ifelse(artists$name == "Taku",1, artists$gender)
artists$gender <- ifelse(artists$name == "9Gotti",1, artists$gender)
artists$gender <- ifelse(artists$name == "Horde",1, artists$gender)
artists$gender <- ifelse(artists$name == "Father Funk",1, artists$gender)
artists$gender <- ifelse(artists$name == "Beatriz",0, artists$gender)
artists$gender <- ifelse(artists$name == "Neuman",1, artists$gender)
artists$gender <- ifelse(artists$name == "Black Foxxes",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Wolf Banes",1, artists$gender)
artists$gender <- ifelse(artists$name == "Los Venturas",1, artists$gender)
artists$gender <- ifelse(artists$name == "Beady Eye",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Films",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sorghegard",1, artists$gender)
artists$gender <- ifelse(artists$name == "Les Nubians",0, artists$gender)
artists$gender <- ifelse(artists$name == "Zimbioziz",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sammy Bananas",1, artists$gender)
artists$gender <- ifelse(artists$name == "Marthe Wang",0, artists$gender)
artists$gender <- ifelse(artists$name == "Set and Setting",1, artists$gender)
artists$gender <- ifelse(artists$name == "Saturday Sun",1, artists$gender)
artists$gender <- ifelse(artists$name == "FLIGHTSCHOOL",1, artists$gender)
artists$gender <- ifelse(artists$name == "LeRiche",1, artists$gender)
artists$gender <- ifelse(artists$name == "Marcel Fengler",1, artists$gender)
artists$gender <- ifelse(artists$name == "Arab On Radar",1, artists$gender)
artists$gender <- ifelse(artists$name == "Smart Cops",1, artists$gender)
artists$gender <- ifelse(artists$name == "Young BB Young",1, artists$gender)
artists$gender <- ifelse(artists$name == "Paint The Sky Red",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sam Baker",1, artists$gender)
artists$gender <- ifelse(artists$name == "Toubab Krewe",1, artists$gender)
artists$gender <- ifelse(artists$name == "Wissam Hilal",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dan Patlansky",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Banner",1, artists$gender)
artists$gender <- ifelse(artists$name == "Swollen Members",1, artists$gender)
artists$gender <- ifelse(artists$name == "Schillah",1, artists$gender)
artists$gender <- ifelse(artists$name == "Whiplash",1, artists$gender)
artists$gender <- ifelse(artists$name == "zYnthetic",1, artists$gender)
artists$gender <- ifelse(artists$name == "Stokeswood",1, artists$gender)
artists$gender <- ifelse(artists$name == "Saimöö",1, artists$gender)
artists$gender <- ifelse(artists$name == "Niveau Zero",1, artists$gender)
artists$gender <- ifelse(artists$name == "AYA",1, artists$gender)
artists$gender <- ifelse(artists$name == "Näääk",1, artists$gender)
artists$gender <- ifelse(artists$name == "Antenna Happy",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lolabúm",1, artists$gender)
artists$gender <- ifelse(artists$name == "Os 3 do Nordeste",1, artists$gender)
artists$gender <- ifelse(artists$name == "Crowbar",1, artists$gender)
artists$gender <- ifelse(artists$name == "Chemicide",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mr. Kitty",1, artists$gender)
artists$gender <- ifelse(artists$name == "Trevor Green",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bear Vs. Shark",1, artists$gender)
artists$gender <- ifelse(artists$name == "Baby Brown",1, artists$gender)
artists$gender <- ifelse(artists$name == "Tortorum",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jaye Jayle",1, artists$gender)
artists$gender <- ifelse(artists$name == "Käärijä",1, artists$gender)
artists$gender <- ifelse(artists$name == "Harsimran",1, artists$gender)
artists$gender <- ifelse(artists$name == "Heavenwood",1, artists$gender)
artists$gender <- ifelse(artists$name == "Saccao",1, artists$gender)
artists$gender <- ifelse(artists$name == "Meludo",1, artists$gender)
artists$gender <- ifelse(artists$name == "Pheeno",1, artists$gender)
artists$gender <- ifelse(artists$name == "Static In Verona",1, artists$gender)
artists$gender <- ifelse(artists$name == "Brandhärd",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Mobbs",1, artists$gender)
artists$gender <- ifelse(artists$name == "Toxic Waltz",1, artists$gender)
artists$gender <- ifelse(artists$name == "Decibelios",1, artists$gender)
artists$gender <- ifelse(artists$name == "Naomi",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ceephax Acid Crew",1, artists$gender)
artists$gender <- ifelse(artists$name == "Redlight",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dub War",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sultan Mir",1, artists$gender)
artists$gender <- ifelse(artists$name == "Hellbringer",1, artists$gender)
artists$gender <- ifelse(artists$name == "No. 4",0, artists$gender)
artists$gender <- ifelse(artists$name == "Sopwith Camel",1, artists$gender)
artists$gender <- ifelse(artists$name == "Kinder",0, artists$gender)
artists$gender <- ifelse(artists$name == "Tigers on Trains",1, artists$gender)
artists$gender <- ifelse(artists$name == "Def",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Business",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mr. Chop",1, artists$gender)
artists$gender <- ifelse(artists$name == "Azaghal",1, artists$gender)
artists$gender <- ifelse(artists$name == "Los Benders",1, artists$gender)
artists$gender <- ifelse(artists$name == "Thermal And A Quarter",1, artists$gender)
artists$gender <- ifelse(artists$name == "Audiophysical",1, artists$gender)
artists$gender <- ifelse(artists$name == "Two Dark Birds",1, artists$gender)
artists$gender <- ifelse(artists$name == "Utopianisti",1, artists$gender)
artists$gender <- ifelse(artists$name == "Gusgri",1, artists$gender)
artists$gender <- ifelse(artists$name == "Osatia",1, artists$gender)
artists$gender <- ifelse(artists$name == "Hazen Street",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Beets",1, artists$gender)
artists$gender <- ifelse(artists$name == "Anni Lahe",0, artists$gender)
artists$gender <- ifelse(artists$name == "A & Z",1, artists$gender)
artists$gender <- ifelse(artists$name == "Limb",1, artists$gender)
artists$gender <- ifelse(artists$name == "Alex Germys",1, artists$gender)
artists$gender <- ifelse(artists$name == "Hadley Kennary",0, artists$gender)
artists$gender <- ifelse(artists$name == "Machine Woman",0, artists$gender)
artists$gender <- ifelse(artists$name == "Stulp Fiction",1, artists$gender)
artists$gender <- ifelse(artists$name == "Grand Magus",1, artists$gender)
artists$gender <- ifelse(artists$name == "AXEN",1, artists$gender)
artists$gender <- ifelse(artists$name == "Chente Barrera",1, artists$gender)
artists$gender <- ifelse(artists$name == "Speakman Sound",1, artists$gender)
artists$gender <- ifelse(artists$name == "Da Fresh",1, artists$gender)
artists$gender <- ifelse(artists$name == "Verse",1, artists$gender)
artists$gender <- ifelse(artists$name == "Smokey Bastard",1, artists$gender)
artists$gender <- ifelse(artists$name == "Thom Artway",1, artists$gender)
artists$gender <- ifelse(artists$name == "Michelly Cordova",0, artists$gender)
artists$gender <- ifelse(artists$name == "Alex Skolnick",1, artists$gender)
artists$gender <- ifelse(artists$name == "Red London",1, artists$gender)
artists$gender <- ifelse(artists$name == "Soly",1, artists$gender)
artists$gender <- ifelse(artists$name == "Golf Pichaya",1, artists$gender)
artists$gender <- ifelse(artists$name == "William's Orbit",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jared Anderson",1, artists$gender)
artists$gender <- ifelse(artists$name == "William Kiss",1, artists$gender)
artists$gender <- ifelse(artists$name == "Crown Magnetar",1, artists$gender)
artists$gender <- ifelse(artists$name == "A Pale Horse Named Death",1, artists$gender)
artists$gender <- ifelse(artists$name == "Volcano Choir",1, artists$gender)
artists$gender <- ifelse(artists$name == "Schooner Fare",1, artists$gender)
artists$gender <- ifelse(artists$name == "Death Threat",1, artists$gender)
artists$gender <- ifelse(artists$name == "Billion One",1, artists$gender)
artists$gender <- ifelse(artists$name == "Hijo de la Tormenta",1, artists$gender)
artists$gender <- ifelse(artists$name == "Everon",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jordi Rivera",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lujavo",1, artists$gender)
artists$gender <- ifelse(artists$name == "Evelyn Evelyn",0, artists$gender)
artists$gender <- ifelse(artists$name == "Sp3cial K",1, artists$gender)
artists$gender <- ifelse(artists$name == "Alzira E",0, artists$gender)
artists$gender <- ifelse(artists$name == "L. Spenser Smith",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mordbrand",1, artists$gender)
artists$gender <- ifelse(artists$name == "XLII",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dr Manhattan",1, artists$gender)
artists$gender <- ifelse(artists$name == "IAMBEAR",1, artists$gender)
artists$gender <- ifelse(artists$name == "Adele Harley",1, artists$gender)
artists$gender <- ifelse(artists$name == "Yoko-Zuna",1, artists$gender)
artists$gender <- ifelse(artists$name == "Isaac Kasule",1, artists$gender)
artists$gender <- ifelse(artists$name == "Enrique Guzmán & Teen Tops",1, artists$gender)
artists$gender <- ifelse(artists$name == "Simon Lynge",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mahotella Queens",0, artists$gender)
artists$gender <- ifelse(artists$name == "The March Divide",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sektemtum",1, artists$gender)
artists$gender <- ifelse(artists$name == "Hideous Sun Demon",1, artists$gender)
artists$gender <- ifelse(artists$name == "Egon Soda",1, artists$gender)
artists$gender <- ifelse(artists$name == "Smokey Fingers",1, artists$gender)
artists$gender <- ifelse(artists$name == "Memmaker",1, artists$gender)
artists$gender <- ifelse(artists$name == "Berri Txarrak",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lindsey Luff",0, artists$gender)
artists$gender <- ifelse(artists$name == "Stormtide",1, artists$gender)
artists$gender <- ifelse(artists$name == "Loopa Scava",1, artists$gender)
artists$gender <- ifelse(artists$name == "the Stupid Stupid Henchmen",1, artists$gender)
artists$gender <- ifelse(artists$name == "Maja Koman",0, artists$gender)
artists$gender <- ifelse(artists$name == "General Roots",1, artists$gender)
artists$gender <- ifelse(artists$name == "Horst Hansen Trio",1, artists$gender)
artists$gender <- ifelse(artists$name == "Arvas",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lazzo Matumbi",1, artists$gender)
artists$gender <- ifelse(artists$name == "Here I Come Falling",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jubël",1, artists$gender)
artists$gender <- ifelse(artists$name == "Midival Punditz",1, artists$gender)
artists$gender <- ifelse(artists$name == "Twisted Method",1, artists$gender)
artists$gender <- ifelse(artists$name == "Orphan Gypsy",1, artists$gender)
artists$gender <- ifelse(artists$name == "LNRipley",1, artists$gender)
artists$gender <- ifelse(artists$name == "Camper Van Beethoven",1, artists$gender)
artists$gender <- ifelse(artists$name == "Guillermo Anderson",1, artists$gender)
artists$gender <- ifelse(artists$name == "Manguala",1, artists$gender)
artists$gender <- ifelse(artists$name == "J. Karjalainen Yhtyeineen",1, artists$gender)
artists$gender <- ifelse(artists$name == "Cervello",1, artists$gender)
artists$gender <- ifelse(artists$name == "In Benny We Trust",1, artists$gender)
artists$gender <- ifelse(artists$name == "All Human",1, artists$gender)
artists$gender <- ifelse(artists$name == "surrenderdorothy",1, artists$gender)
artists$gender <- ifelse(artists$name == "Booggz",1, artists$gender)
artists$gender <- ifelse(artists$name == "MBP",1, artists$gender)
artists$gender <- ifelse(artists$name == "Super Mama Djombo",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ja, Panik",1, artists$gender)
artists$gender <- ifelse(artists$name == "Uzi Lvke",1, artists$gender)
artists$gender <- ifelse(artists$name == "Death Ray Vision",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dead Lakes",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jericho Woods",1, artists$gender)
artists$gender <- ifelse(artists$name == "Pangaea",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dewiq",0, artists$gender)
artists$gender <- ifelse(artists$name == "Bokito",1, artists$gender)
artists$gender <- ifelse(artists$name == "Age of Daze",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mike & The Melvins",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mark Vank",1, artists$gender)
artists$gender <- ifelse(artists$name == "Wildwood Kin",0, artists$gender)
artists$gender <- ifelse(artists$name == "The Dead Ships",1, artists$gender)
artists$gender <- ifelse(artists$name == "Body Count",1, artists$gender)
artists$gender <- ifelse(artists$name == "Beat Box",1, artists$gender)
artists$gender <- ifelse(artists$name == "Maye ",0, artists$gender)
artists$gender <- ifelse(artists$name == "Hugh Lee",1, artists$gender)
artists$gender <- ifelse(artists$name == "Grooms",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sali Kimi",0, artists$gender)
artists$gender <- ifelse(artists$name == "EXTREMA",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sølv",0, artists$gender)
artists$gender <- ifelse(artists$name == "Faela",1, artists$gender)
artists$gender <- ifelse(artists$name == "E.M.I.L.",1, artists$gender)
artists$gender <- ifelse(artists$name == "Herder",1, artists$gender)
artists$gender <- ifelse(artists$name == "Banda Eddie",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Francis Wolves",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ladotee",1, artists$gender)
artists$gender <- ifelse(artists$name == "Tejano Boys",1, artists$gender)
artists$gender <- ifelse(artists$name == "Natives",1, artists$gender)
artists$gender <- ifelse(artists$name == "Paper Lions",1, artists$gender)
artists$gender <- ifelse(artists$name == "Army of Freshmen",1, artists$gender)
artists$gender <- ifelse(artists$name == "Tetrameth",1, artists$gender)
artists$gender <- ifelse(artists$name == "KUMBIAMBEROS RS",1, artists$gender)
artists$gender <- ifelse(artists$name == "B.Eveready",1, artists$gender)
artists$gender <- ifelse(artists$name == "Trumans Water",1, artists$gender)
artists$gender <- ifelse(artists$name == "Miserable Faith",1, artists$gender)
artists$gender <- ifelse(artists$name == "Variety Lab",1, artists$gender)
artists$gender <- ifelse(artists$name == "Kamran & Hooman",1, artists$gender)
artists$gender <- ifelse(artists$name == "Durante",1, artists$gender)
artists$gender <- ifelse(artists$name == "Io e la tigre",0, artists$gender)
artists$gender <- ifelse(artists$name == "The South Austin Moonlighters",1, artists$gender)
artists$gender <- ifelse(artists$name == "Global Citizen",1, artists$gender)
artists$gender <- ifelse(artists$name == "Tren Loco",1, artists$gender)
artists$gender <- ifelse(artists$name == "Hyper Fenton",1, artists$gender)
artists$gender <- ifelse(artists$name == "Höröyá",1, artists$gender)
artists$gender <- ifelse(artists$name == "Michael Daves",1, artists$gender)
artists$gender <- ifelse(artists$name == "MDC",1, artists$gender)
artists$gender <- ifelse(artists$name == "Necrophile",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dj 4rain",1, artists$gender)
artists$gender <- ifelse(artists$name == "Wang Wen",1, artists$gender)
artists$gender <- ifelse(artists$name == "Myer Clarity",1, artists$gender)
artists$gender <- ifelse(artists$name == "Cory Marks",1, artists$gender)
artists$gender <- ifelse(artists$name == "White Rabbits",1, artists$gender)
artists$gender <- ifelse(artists$name == "Alex Lam",1, artists$gender)
artists$gender <- ifelse(artists$name == "Remember the Monsters",1, artists$gender)
artists$gender <- ifelse(artists$name == "maudlin of the Well",1, artists$gender)
artists$gender <- ifelse(artists$name == "C-Mob",1, artists$gender)
artists$gender <- ifelse(artists$name == "Las Kellies",0, artists$gender)
artists$gender <- ifelse(artists$name == "Itzik Dadya",1, artists$gender)
artists$gender <- ifelse(artists$name == "Montrose",1, artists$gender)
artists$gender <- ifelse(artists$name == "Darro",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Experimental Tropic Blues Band",1, artists$gender)
artists$gender <- ifelse(artists$name == "Tinne Oltmans",0, artists$gender)
artists$gender <- ifelse(artists$name == "The Ugly",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lee Coulter",1, artists$gender)
artists$gender <- ifelse(artists$name == "Reckless Anxiety",1, artists$gender)
artists$gender <- ifelse(artists$name == "SONTALK",1, artists$gender)
artists$gender <- ifelse(artists$name == "Forming the Void",1, artists$gender)
artists$gender <- ifelse(artists$name == "Smoke Blow",1, artists$gender)
artists$gender <- ifelse(artists$name == "Velvet Insane",1, artists$gender)
artists$gender <- ifelse(artists$name == "Loud Lary Ajust",1, artists$gender)
artists$gender <- ifelse(artists$name == "Seven Thorns",1, artists$gender)
artists$gender <- ifelse(artists$name == "Down In Ashes",1, artists$gender)
artists$gender <- ifelse(artists$name == "Tony Ramey",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bandman Kevo",1, artists$gender)
artists$gender <- ifelse(artists$name == "Andres Cabas",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bad Cop",0, artists$gender)
artists$gender <- ifelse(artists$name == "Wild Culture",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ye Vagabonds",1, artists$gender)
artists$gender <- ifelse(artists$name == "2Scratch",1, artists$gender)
artists$gender <- ifelse(artists$name == "Pretty Boy",1, artists$gender)
artists$gender <- ifelse(artists$name == "Shango V",1, artists$gender)
artists$gender <- ifelse(artists$name == "Notty Play",1, artists$gender)
artists$gender <- ifelse(artists$name == "Binary Star",1, artists$gender)
artists$gender <- ifelse(artists$name == "John Askew",1, artists$gender)
artists$gender <- ifelse(artists$name == "Balozu Pilni Pagalmi",1, artists$gender)
artists$gender <- ifelse(artists$name == "Firtan",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jéf",1, artists$gender)
artists$gender <- ifelse(artists$name == "fredo disco",1, artists$gender)
artists$gender <- ifelse(artists$name == "Artento Divini",1, artists$gender)
artists$gender <- ifelse(artists$name == "Pancho V",1, artists$gender)
artists$gender <- ifelse(artists$name == "Cold Reading",1, artists$gender)
artists$gender <- ifelse(artists$name == "Assassin",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dropbears",1, artists$gender)
artists$gender <- ifelse(artists$name == "Pulpo",1, artists$gender)
artists$gender <- ifelse(artists$name == "A-Lusion",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Lovelocks",0, artists$gender)
artists$gender <- ifelse(artists$name == "Masih",1, artists$gender)
artists$gender <- ifelse(artists$name == "Falling With Glory",1, artists$gender)
artists$gender <- ifelse(artists$name == "Kasket Club",1, artists$gender)
artists$gender <- ifelse(artists$name == "Billyclub",1, artists$gender)
artists$gender <- ifelse(artists$name == "George Pelham",1, artists$gender)
artists$gender <- ifelse(artists$name == "Beyond the Aftermath",1, artists$gender)
artists$gender <- ifelse(artists$name == "One Chance",1, artists$gender)
artists$gender <- ifelse(artists$name == "DJ Pepo",1, artists$gender)
artists$gender <- ifelse(artists$name == "Schwarz & Funk",1, artists$gender)
artists$gender <- ifelse(artists$name == "Hollow & Akimbo",1, artists$gender)
artists$gender <- ifelse(artists$name == "Clever",1, artists$gender)
artists$gender <- ifelse(artists$name == "Os Atuais",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mythic Sunship",1, artists$gender)
artists$gender <- ifelse(artists$name == "Melanie Pfirrman",0, artists$gender)
artists$gender <- ifelse(artists$name == "Deliri0us",1, artists$gender)
artists$gender <- ifelse(artists$name == "Norvis Junior",1, artists$gender)
artists$gender <- ifelse(artists$name == "Map.ache",1, artists$gender)
artists$gender <- ifelse(artists$name == "Daniel Dubb",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bosq",1, artists$gender)
artists$gender <- ifelse(artists$name == "U Sung Eun",0, artists$gender)
artists$gender <- ifelse(artists$name == "Anna Hertzman",0, artists$gender)
artists$gender <- ifelse(artists$name == "Umpire",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jae.T",1, artists$gender)
artists$gender <- ifelse(artists$name == "Sereda",1, artists$gender)
artists$gender <- ifelse(artists$name == "Crisis Era",1, artists$gender)
artists$gender <- ifelse(artists$name == "HUDBA Z MARSU",1, artists$gender)
artists$gender <- ifelse(artists$name == "Shiva In Exile",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ellen Krauss",0, artists$gender)
artists$gender <- ifelse(artists$name == "Jimmy James",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ingranaggi Della Valle",1, artists$gender)
artists$gender <- ifelse(artists$name == "Moerbeck",1, artists$gender)
artists$gender <- ifelse(artists$name == "InTechnicolour",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jeruji",1, artists$gender)
artists$gender <- ifelse(artists$name == "Seas of Conflict",1, artists$gender)
artists$gender <- ifelse(artists$name == "Amber Pacific",1, artists$gender)
artists$gender <- ifelse(artists$name == "Kito & Reija Lee",0, artists$gender)
artists$gender <- ifelse(artists$name == "DJ Polique",1, artists$gender)
artists$gender <- ifelse(artists$name == "Unearthly",1, artists$gender)
artists$gender <- ifelse(artists$name == "Freakulizer",1, artists$gender)
artists$gender <- ifelse(artists$name == "Party of the Sin",1, artists$gender)
artists$gender <- ifelse(artists$name == "Panzer",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ricky Tyler",1, artists$gender)
artists$gender <- ifelse(artists$name == "The American Boychoir",1, artists$gender)
artists$gender <- ifelse(artists$name == "Funky DL",1, artists$gender)
artists$gender <- ifelse(artists$name == "JAS CRW",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lobby Boxer",1, artists$gender)
artists$gender <- ifelse(artists$name == "Country Rockin' Rebels",1, artists$gender)
artists$gender <- ifelse(artists$name == "Trippy Wicked & The Cosmic Children Of The Knight",1, artists$gender)
artists$gender <- ifelse(artists$name == "Austin Allsup",1, artists$gender)
artists$gender <- ifelse(artists$name == "Wickeda",1, artists$gender)
artists$gender <- ifelse(artists$name == "Vector",1, artists$gender)
artists$gender <- ifelse(artists$name == "LUCK MUZIK",1, artists$gender)
artists$gender <- ifelse(artists$name == "Los Huaycos",1, artists$gender)
artists$gender <- ifelse(artists$name == "Neodisco",1, artists$gender)
artists$gender <- ifelse(artists$name == "Carinthia",1, artists$gender)
artists$gender <- ifelse(artists$name == "Tangorodrim",1, artists$gender)
artists$gender <- ifelse(artists$name == "Endstille",1, artists$gender)
artists$gender <- ifelse(artists$name == "Fizzy Blood",1, artists$gender)
artists$gender <- ifelse(artists$name == "Strong R.",1, artists$gender)
artists$gender <- ifelse(artists$name == "Marek Dyjak",1, artists$gender)
artists$gender <- ifelse(artists$name == "Algora",1, artists$gender)
artists$gender <- ifelse(artists$name == "Dalevuelta",1, artists$gender)
artists$gender <- ifelse(artists$name == "Kamufle",1, artists$gender)
artists$gender <- ifelse(artists$name == "Chachi",1, artists$gender)
artists$gender <- ifelse(artists$name == "Deals Death",1, artists$gender)
artists$gender <- ifelse(artists$name == "Slaughter & The Dogs",1, artists$gender)
artists$gender <- ifelse(artists$name == "Rob Heron & the Tea Pad Orchestra",1, artists$gender)
artists$gender <- ifelse(artists$name == "K's Choice",1, artists$gender)
artists$gender <- ifelse(artists$name == "Luiz Carlos Borges",1, artists$gender)
artists$gender <- ifelse(artists$name == "Lil Fizz",1, artists$gender)
artists$gender <- ifelse(artists$name == "Space Echo",1, artists$gender)
artists$gender <- ifelse(artists$name == "DJ Micro",1, artists$gender)
artists$gender <- ifelse(artists$name == "Timothy Seth Avett as Darling",1, artists$gender)
artists$gender <- ifelse(artists$name == "Kitrina Podilata",1, artists$gender)
artists$gender <- ifelse(artists$name == "Death Angel",1, artists$gender)
artists$gender <- ifelse(artists$name == "2Elements",0, artists$gender)
artists$gender <- ifelse(artists$name == "Super Magic Hats",1, artists$gender)
artists$gender <- ifelse(artists$name == "Copper Chief",1, artists$gender)
artists$gender <- ifelse(artists$name == "Laura Sauvage",0, artists$gender)
artists$gender <- ifelse(artists$name == "Shaded",1, artists$gender)
artists$gender <- ifelse(artists$name == "Moonalice",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bohren & Der Club Of Gore",1, artists$gender)
artists$gender <- ifelse(artists$name == "Animosity",1, artists$gender)
artists$gender <- ifelse(artists$name == "Hopetoun Brown",1, artists$gender)
artists$gender <- ifelse(artists$name == "Black Flower",1, artists$gender)
artists$gender <- ifelse(artists$name == "Memória De Peixe",1, artists$gender)
artists$gender <- ifelse(artists$name == "Tana Matz",0, artists$gender)
artists$gender <- ifelse(artists$name == "Woody Pines",1, artists$gender)
artists$gender <- ifelse(artists$name == "At Versaris",1, artists$gender)
artists$gender <- ifelse(artists$name == "Ad Bourke",1, artists$gender)
artists$gender <- ifelse(artists$name == "Chicharrica",1, artists$gender)
artists$gender <- ifelse(artists$name == "Grounders",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jason Gewalt",1, artists$gender)
artists$gender <- ifelse(artists$name == "Tibbah",1, artists$gender)
artists$gender <- ifelse(artists$name == "Stereoside",1, artists$gender)
artists$gender <- ifelse(artists$name == "Vandal",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Noise Figures",1, artists$gender)
artists$gender <- ifelse(artists$name == "Vassilis Tsabropoulos",1, artists$gender)
artists$gender <- ifelse(artists$name == "Julio y Agosto",1, artists$gender)
artists$gender <- ifelse(artists$name == "Horace Greene",1, artists$gender)
artists$gender <- ifelse(artists$name == "Brilha Som",1, artists$gender)
artists$gender <- ifelse(artists$name == "Moaning",1, artists$gender)
artists$gender <- ifelse(artists$name == "Billy Club Sandwich",1, artists$gender)
artists$gender <- ifelse(artists$name == "Johnny & The Hurricanes",1, artists$gender)
artists$gender <- ifelse(artists$name == "Tie Geresni",1, artists$gender)
artists$gender <- ifelse(artists$name == "Bushman'S Revenge",1, artists$gender)
artists$gender <- ifelse(artists$name == "Whores",1, artists$gender)
artists$gender <- ifelse(artists$name == "Minimono",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jack Pattern",1, artists$gender)
artists$gender <- ifelse(artists$name == "Track and Field",1, artists$gender)
artists$gender <- ifelse(artists$name == "Chiefland",1, artists$gender)
artists$gender <- ifelse(artists$name == "Spastic Ink",1, artists$gender)
artists$gender <- ifelse(artists$name == "Stian Fjelldal",1, artists$gender)
artists$gender <- ifelse(artists$name == "Jeppe Loftager",1, artists$gender)
artists$gender <- ifelse(artists$name == "Seeker Lover Keeper",0, artists$gender)
artists$gender <- ifelse(artists$name == "Fish From Japan",1, artists$gender)
artists$gender <- ifelse(artists$name == "MaWayy",1, artists$gender)
artists$gender <- ifelse(artists$name == "Seven Nations",1, artists$gender)
artists$gender <- ifelse(artists$name == "Childhood",1, artists$gender)
artists$gender <- ifelse(artists$name == "Skinny Days",1, artists$gender)
artists$gender <- ifelse(artists$name == "Giant Panda",1, artists$gender)
artists$gender <- ifelse(artists$name == "Nient'altro che macerie",1, artists$gender)
artists$gender <- ifelse(artists$name == "Erdbeerschnitzel",1, artists$gender)
artists$gender <- ifelse(artists$name == "Airship",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Philistines Jr.",1, artists$gender)
artists$gender <- ifelse(artists$name == "Twins Crew",1, artists$gender)
artists$gender <- ifelse(artists$name == "DJ Ken",1, artists$gender)
artists$gender <- ifelse(artists$name == "Young Wolf Hatchlings",1, artists$gender)
artists$gender <- ifelse(artists$name == "Haze",1, artists$gender)
artists$gender <- ifelse(artists$name == "Aleksandar Dimitrijevic",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Violent Years",1, artists$gender)
artists$gender <- ifelse(artists$name == "Mark Steiner & His Problems",1, artists$gender)
artists$gender <- ifelse(artists$name == "Procession",1, artists$gender)
artists$gender <- ifelse(artists$name == "Furyon",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Coathangers",0, artists$gender)
artists$gender <- ifelse(artists$name == "Grey Area",1, artists$gender)
artists$gender <- ifelse(artists$name == "Andi Almqvist",1, artists$gender)
artists$gender <- ifelse(artists$name == "Rua Direita",1, artists$gender)
artists$gender <- ifelse(artists$name == "Llajtaymanta",1, artists$gender)
artists$gender <- ifelse(artists$name == "The Wild",1, artists$gender)
artists$gender <- ifelse(artists$name == "",1, artists$gender)
artists$gender <- ifelse(artists$name == "",1, artists$gender)
artists$gender <- ifelse(artists$name == "",1, artists$gender)



View(artists)


#Predicting GENDER from NAME | citation: [![rOpenSci logo](http://ropensci.org/public_images/github_footer.png)](http://ropensci.org) 


#install.packages("gender")
#library(gender)
#gender(c("Madison", "Hillary"), years = 1930, method = "ssa")
#a <- gender("Mike", years=1990, method = 'ssa')
#a


artists <- artists[,c("name","country","country_classified", "gender", "record_label", "major_label", 
"press_contact","booking_agent",
"popularity_score", "monthly_listeners","followers", 
"popularity_score_rank", "monthly_listeners_rank","followers_rank", 
"listener_location", "playlist_count", "playlist_reach",
"deezer_fans_rank",
"youtube_subscribers_rank",
"youtube_views_rank",
"youtube_daily_views_rank",
"youtube_monthly_views_rank",
"pandora_monthly_listeners_rank",
"pandora_stations_rank",  
"pandora_streams_rank",
"twitch_followers_rank", 
"twitch_views_rank",
"twitch_monthly_viewers_rank",
"twitch_weekly_viewers_rank",
"tiktok_followers_rank",
"tiktok_likes_rank",
"instagram_followers_rank"
)] 


nrow(artists)

artists<-artists[(!is.na(artists$gender)),]

View(artists)
nrow(artists) # IN TOTAL 2258 datapoints








# create second dataset: social_stats -------------------------------------

social_stats <- artists
social_stats$deezer_fans_rank[artists$deezer_fans_rank == 0] <- NA
social_stats$youtube_subscribers_rank[artists$youtube_subscribers_rank == 0] <- NA
social_stats$youtube_views_rank[artists$youtube_views_rank == 0] <- NA
social_stats$pandora_monthly_listeners_rank[artists$pandora_monthly_listeners_rank == 0] <- NA
social_stats$twitch_monthly_viewers_rank[artists$twitch_monthly_viewers_rank == 0] <- NA

#remove Rows with 
social_stats<-social_stats[(!is.na(social_stats$deezer_fans_rank) & !is.na(social_stats$pandora_monthly_listeners_rank)),]
#social_stats<-social_stats[(!is.na(social_stats$gender) & !is.na(social_stats$country)),]
nrow(social_stats)
social_stats<-social_stats[(!is.na(social_stats$youtube_subscribers_rank)),]
nrow(social_stats)
social_stats<-social_stats[(!is.na(social_stats$twitch_monthly_viewers_rank)),]
nrow(social_stats)
social_stats<-social_stats[(!is.na(social_stats$youtube_views_rank)),]

social_stats1 <- social_stats[,c("name","country","country_classified", "gender", "record_label", "major_label", 
                                "press_contact","booking_agent",
                                "popularity_score", "monthly_listeners","followers", 
                                "popularity_score_rank", "monthly_listeners_rank","followers_rank", 
                                "playlist_count", "playlist_reach",
                                "deezer_fans_rank",
                                "youtube_subscribers_rank",
                                "youtube_views_rank",
                                "pandora_monthly_listeners_rank",
                                "twitch_monthly_viewers_rank",
                                "twitch_weekly_viewers_rank"
)] 

social_stats2 <- social_stats1[,c("name","popularity_score", "monthly_listeners","followers", 
                                     "popularity_score_rank", "monthly_listeners_rank","followers_rank", 
                                     "playlist_count", "playlist_reach",
                                     "deezer_fans_rank",
                                     "youtube_subscribers_rank",
                                     "youtube_views_rank",
                                     "pandora_monthly_listeners_rank",
                                     "twitch_monthly_viewers_rank",
                                     "twitch_weekly_viewers_rank"
)] 

nrow(social_stats1) # IN TOTAL 700 datapoints
View(social_stats1)
nrow(social_stats2) # IN TOTAL 700 datapoints
View(social_stats2)







# everything about missing data https://cran.r-project.org/web/packages/finalfit/vignettes/missing.html 

# ______Plots and General Data Preparation ______ --------



#Plots
#DV- Playlist Count = Logarithmic
plot(artists$popularity_score,artists$playlist_count)
#DV- Playlist Reach = Logarithmic
plot(artists$popularity_score,artists$playlist_reach)
#Playlist Count- Playlist Reach = Linear
plot(artists$playlist_count,artists$playlist_reach)
#DV- Gender 
boxplot(artists$popularity_score,artists$gender)
#DV- Press Contact 
plot(artists$popularity_score,artists$press_contact)
#DV- Booking Agent 
plot(artists$popularity_score,artists$booking_agent)

View(social_stats)
nrow(social_stats)

#DV _ Social Stats variables Cleaning
plot(artists$deezer_fans_rank, artists$popularity_score)  #----GOOD DATA----, 0 counts to 678 times
plot(artists$youtube_subscribers_rank, artists$popularity_score) #----GOOD DATA----, 0 counts to 1324 times
plot(artists$youtube_views_rank, artists$popularity_score) #----GOOD DATA----, 0 counts to 1238 times
plot(artists$youtube_daily_views_rank, artists$popularity_score) #few data, 0 counts to 2232 times
plot(artists$youtube_monthly_views_rank, artists$popularity_score) #few data, 0 counts to 2232 times 
plot(artists$pandora_stations_rank, artists$popularity_score) #few data, 0 counts to 2557 times
plot(artists$pandora_streams_rank, artists$popularity_score) #few data, 0 counts to 2557 times
plot(artists$pandora_monthly_listeners_rank, artists$popularity_score) #----GOOD DATA----, 0 counts to 828 times
plot(artists$twitch_followers_rank, artists$popularity_score) #few data, 0 counts to 2232 times
plot(artists$twitch_views_rank, artists$popularity_score) #few data, 0 counts to 2232 times
plot(artists$twitch_monthly_viewers_rank, artists$popularity_score) #----GOOD DATA----, 0 counts to 1238 times
plot(artists$twitch_weekly_viewers_rank, artists$popularity_score) #----GOOD DATA----, 0 counts to 846 times
plot(artists$tiktok_followers_rank, artists$popularity_score) #few data, 0 counts to 2560 times
plot(artists$tiktok_likes_rank, artists$popularity_score) #few data, 0 counts to 2563 times
plot(artists$instagram_followers_rank, artists$popularity_score) #few data, 0 counts to 2492 times

# Social Stats Variables that have a moderate amount of missing values, below:
hist(artists$deezer_fans_rank)
hist(artists$youtube_subscribers_rank)
hist(artists$youtube_views_rank)
hist(artists$pandora_monthly_listeners_rank)
hist(artists$twitch_monthly_viewers_rank)
hist(artists$twitch_weekly_viewers_rank)

artists <- artists[,c("name","country","country_classified", "gender", "record_label", "major_label", 
                      "press_contact","booking_agent",
                      "popularity_score", "monthly_listeners","followers", 
                      "popularity_score_rank", "monthly_listeners_rank","followers_rank", 
                      "playlist_count", "playlist_reach",
                     
)] 




# Descriptives ------------------------------------------------------------

summary(artists)
sd(artists$popularity_score, na.rm=TRUE)
sd(artists$playlist_count, na.rm=TRUE)
sd(artists$playlist_reach, na.rm=TRUE)
sd(artists$gender, na.rm=TRUE)
sd(artists$major_label, na.rm=TRUE)
summary(artists$major_label)

artists$press_contact <- as.integer(artists$press_contact)  
artists$booking_agent <- as.integer(artists$booking_agent)
sd(artists$press_contact, na.rm=TRUE)
sd(artists$booking_agent, na.rm=TRUE)

cor.test(artists$gender, artists$popularity_score, method = "pearson")
cor.test(artists$press_contact, artists$popularity_score, method = "pearson")
cor.test(artists$booking_agent, artists$popularity_score, method = "pearson")
cor.test(artists$playlist_count, artists$popularity_score, method = "pearson")
cor.test(artists$playlist_reach, artists$popularity_score, method = "pearson")
cor.test(artists$major_label, artists$popularity_score, method = "pearson")

sd(social_stats1$deezer_fans_rank, na.rm=TRUE)
sd(social_stats1$youtube_subscribers_rank, na.rm=TRUE)
sd(social_stats1$youtube_views_rank, na.rm=TRUE)
sd(social_stats1$pandora_monthly_listeners_rank, na.rm=TRUE)
sd(social_stats1$twitch_monthly_viewers_rank, na.rm=TRUE)
sd(social_stats1$twitch_weekly_viewers_rank, na.rm=TRUE)

cor.test(social_stats1$deezer_fans_rank, social_stats1$popularity_score, method = "pearson")
cor.test(social_stats1$youtube_subscribers_rank, social_stats1$popularity_score, method = "pearson")
cor.test(social_stats1$youtube_views_rank, social_stats1$popularity_score, method = "pearson")
cor.test(social_stats1$pandora_monthly_listeners_rank, social_stats1$popularity_score, method = "pearson")
cor.test(social_stats1$twitch_monthly_viewers_rank, social_stats1$popularity_score, method = "pearson")
cor.test(social_stats1$twitch_weekly_viewers_rank, social_stats1$popularity_score, method = "pearson")

#colSds(as.matrix(artists[sapply(artists, is.numeric)]))

#LOG Transform REACH & COUNT https://www.pluralsight.com/guides/normalizing-data-r
artists$playlist_reach_norm <- log(artists$playlist_reach)
hist(artists$playlist_reach_norm)
artists$playlist_count_norm <- log(artists$playlist_count)
hist(artists$playlist_count_norm)

#LOG Transform 
artists$deezer_fans_rank_norm <- log(artists$deezer_fans_rank)
hist(artists$deezer_fans_rank_norm)
artists$youtube_subscribers_rank_norm <- log(artists$youtube_subscribers_rank)
hist(artists$youtube_subscribers_rank_norm)
artists$pandora_monthly_listeners_rank_norm <- log(artists$pandora_monthly_listeners_rank)
hist(artists$pandora_monthly_listeners_rank_norm)
hist(artists$pandora_monthly_listeners_rank)

#Check Variable Normality, and normalize the Non-Normal Variables
skewness(artists$popularity_score, na.rm = TRUE) 
kyrtosis(artists$popularity_score, na.rm = TRUE) 
skewness(artists$followers, na.rm = TRUE)
skewness(artists$monthly_listeners, na.rm = TRUE) 

#CREATIVE PLOTS
library(ggplot2);
ggplot(artists, aes(as.factor(artists$country_classified), artists$popularity_score)) + 
  geom_boxplot() #popularity_score per country_classified boxplot

table(artists$country_classified)



# Creative Plots ----------------------------------------------------------

ggplot(artists, aes(as.factor(artists$booking_agent), artists$popularity_score)) + 
  geom_boxplot() #popularity_score per booking_agent boxplot
ggplot(artists, aes(as.factor(artists$press_contact), artists$popularity_score)) + 
  geom_boxplot() #popularity_score per press_contact boxplot
ggplot(artists, aes(as.factor(artists$major_label), artists$popularity_score)) + 
  geom_boxplot() #popularity_score per major_label boxplot
ggplot(artists, aes(as.factor(artists$gender), artists$popularity_score)) + 
  geom_boxplot() #popularity_score per gender boxplot
ggplot(artists, aes(as.factor(artists$country_classified), artists$popularity_score)) + 
  geom_boxplot() #popularity_score per gender boxplot




# Multiple Regressions ----------------------------------------------------


#Multiple Regression || https://www.statmethods.net/stats/regression.html




# Multiple Regression Model 1 ---------------------------------------


Model_Artists <- lm(artists$popularity_score ~ artists$gender+artists$country_classified+
                  artists$press_contact+artists$booking_agent+
                  artists$playlist_count+artists$playlist_reach, data=artists) 

summary(Model_Artists) # show results| p-value<0.05, r2=0.1725, adjusted r2=1647
print(Model_Artists)

#examining the multivariate regression http://www.sthda.com/english/articles/40-regression-analysis/168-multiple-linear-regression-in-r/ 

#RESIDUALS
residuals <- resid(Model_Artists)
hist(residuals) #Visually: not normal distribution
ggdensity(residuals, main = "Density plot of residuals", xlab = "residuals") 
ggqqplot(residuals)
shapiro.test(residuals) #shapiro test >0.05
skewness(residuals, na.rm = TRUE) #skewness
kyrtosis(residuals, na.rm = TRUE) #kyrtosis
sd(residuals, na.rm=TRUE) #standard deviation

sigma(Model_Artists) #residual standard error
sigma(Model_Artists)/mean(artists$popularity_score)

#OTHER REGRESSION CHECKS
head(fitted(Model_Artists)) # predicted values
vcov(Model_Artists) #not so useful
anova(Model_Artists) # anova table

# Other useful functions
coefficients(Model_Artists) # model coefficients
confint(Model_Artists, level=0.95) # CIs for model parameters
fitted(Model_Artists) # predicted values
residuals(Model_Artists) # residuals
anova(Model_Artists) # anova table
vcov(Model_Artists) # covariance matrix for model parameters
influence(Model_Artists) # regression diagnostics

#check for multicollinearity
vif(Model_Artists)



#Model 1'
k <- lm(artists$popularity_score ~artists$gender+artists$country_classified)
summary(k)
k <- lm(artists$playlist_count ~artists$popularity_score)
summary(k)





#Artists Model contra


# Multiple Regression Model 1 Reverse --------------------------------

nrow(artists)

Model_Artists_contra1 <- lm(artists$popularity_score ~ artists$gender+artists$country_classified, data=artists) 
summary(Model_Artists_contra1) # show results

Model_Artists_contra2 <- lm(artists$playlist_count ~ artists$popularity_score, data=artists) 
summary(Model_Artists_contra2) # show results

Model_Artists_contra3 <- lm(artists$playlist_reach ~ artists$popularity_score, data=artists) 
summary(Model_Artists_contra3) # show results

Model_Artists_contra4 <- lm(artists$press_contact ~ artists$popularity_score, data=artists) 
summary(Model_Artists_contra4) # show results

Model_Artists_contra5 <- lm(artists$booking_agent ~ artists$popularity_score, data=artists) 
summary(Model_Artists_contra5) # show results

Model_Artists_contra6 <- lm(artists$major_label ~ artists$popularity_score, data=artists) 
summary(Model_Artists_contra6) # show results





# Multiple Regression Model 2 ---------------------------------------


linearMod_Social_Stats1 <- lm(social_stats1$popularity_score ~ social_stats1$gender+social_stats1$country_classified+
                         social_stats1$press_contact+social_stats1$booking_agent+
                         social_stats1$playlist_count+social_stats1$playlist_reach+
                         social_stats1$deezer_fans_rank+social_stats1$youtube_subscribers_rank+
                         social_stats1$youtube_views_rank+social_stats1$pandora_monthly_listeners_rank+
                         social_stats1$twitch_monthly_viewers_rank+social_stats1$twitch_weekly_viewers_rank
                         , data=artists) 

summary(linearMod_Social_Stats1) # show results

#examining the multivariate regression http://www.sthda.com/english/articles/40-regression-analysis/168-multiple-linear-regression-in-r/ 

#RESIDUALS
residuals <- resid(linearMod_Social_Stats1)
hist(residuals) #Visually: not normal distribution
ggdensity(residuals, main = "Density plot of residuals", xlab = "residuals") 
ggqqplot(residuals)
shapiro.test(residuals) #Mathematically: not normal distribution
sigma(linearMod) #residual standard error
sigma(linearMod)/mean(artists$popularity_score)

head(fitted(linearMod)) # predicted values
vcov(linearMod)
anova(linearMod) # anova table

# Other useful functions
coefficients(linearMod) # model coefficients
confint(linearMod, level=0.95) # CIs for model parameters
fitted(linearMod) # predicted values
residuals(linearMod) # residuals
anova(linearMod) # anova table
vcov(linearMod) # covariance matrix for model parameters
influence(linearMod) # regression diagnostics

#check for multicollinearity
VIF(linearMod)



# Multiple Regression Model 2 Reverse ----------------------------------------------------

Model_Artists_contra1 <- lm(social_stats1$popularity_score ~ social_stats1$gender+social_stats1$country_classified, data=artists) 
summary(Model_Artists_contra1) # show results

Model_Artists_contra2 <- lm(social_stats1$playlist_count ~ social_stats1$popularity_score, data=artists) 
summary(Model_Artists_contra2) # show results

Model_Artists_contra3 <- lm(social_stats1$playlist_reach ~ social_stats1$popularity_score, data=artists) 
summary(Model_Artists_contra3) # show results

Model_Artists_contra4 <- lm(social_stats1$press_contact ~ social_stats1$popularity_score, data=artists) 
summary(Model_Artists_contra4) # show results

Model_Artists_contra5 <- lm(social_stats1$booking_agent ~ social_stats1$popularity_score, data=artists) 
summary(Model_Artists_contra5) # show results

Model_Artists_contra6 <- lm(social_stats1$major_label ~ social_stats1$popularity_score, data=artists) 
summary(Model_Artists_contra6) # show results

Model_Artists_contra7 <- lm(social_stats1$deezer_fans_rank ~ social_stats1$popularity_score, data=artists) 
summary(Model_Artists_contra7) # show results

Model_Artists_contra8 <- lm(social_stats1$youtube_subscribers_rank ~ social_stats1$popularity_score, data=artists) 
summary(Model_Artists_contra8) # show results

Model_Artists_contra9 <- lm(social_stats1$youtube_views_rank ~ social_stats1$popularity_score, data=artists) 
summary(Model_Artists_contra9) # show results

Model_Artists_contra10 <- lm(social_stats1$pandora_monthly_listeners_rank ~ social_stats1$popularity_score, data=artists) 
summary(Model_Artists_contra10) # show results

Model_Artists_contra11 <- lm(social_stats1$twitch_monthly_viewers_rank ~ social_stats1$popularity_score, data=artists) 
summary(Model_Artists_contra11) # show results

Model_Artists_contra12 <- lm(social_stats1$twitch_weekly_viewers_rank ~ social_stats1$popularity_score, data=artists) 
summary(Model_Artists_contra12) # show results





# _________________INTERPOLATION EXPERIMENTING_________________ -----------


hist(artists$deezer_fans_rank)
plot(artists$deezer_fans_rank)
artists$x <- as.numeric(na.approx(artists$deezer_fans_rank))
plot(x)
hist(as.numeric(x))
x
View(artists)
hist(artists$deezer_fans_rank_modified)

artists$youtube_subscribers_rank_new <- na.approx(artists$youtube_subscribers_rank)
hist(artists$youtube_subscribers_rank_new)

artists$deezer_fans_rank_new <- na.approx(artists$deezer_fans_rank)
artists$deezer_fans_rank_new <- na.approx(artists$deezer_fans_rank)
artists$deezer_fans_rank_new <- na.approx(artists$deezer_fans_rank)


#library(zoo)
#artists$deezer_fans_rank <- zoo(artists$deezer_fans_rank)
artists$deezer_fans_rank <- na.approx(artists$deezer_fans_rank, xout = artists$deezer_fans_rank, 
                                      na.rm = FALSE, maxgap = 3)


#artists$deezer1 <- approx(artists$deezer_fans_rank)
#artists$deezer2 <- approx(artists$deezer_fans_rank, xout = artists$deezer_fans_rank, 
# na.rm = FALSE, maxgap = 3)
#artists$deezer3 <- approx(artists$deezer_fans_rank, xout = artists$deezer_fans_rank, 
# na.rm = FALSE, maxgap = Inf)


#summary(artists$deezer1)
#summary(artists$deezer2)
#summary(artists$deezer3)

#Sorting columns
#artists[order(artists$deezer_fans_rank)]
artists$deezer_fans_rank_new <- artists$deezer_fans_rank
interp.dataset(artists, artists$deezer_fans_rank, xout = artists$deezer_fans_rank_new, method = c("linear"),
               rep.negt=TRUE, na.rm = FALSE)

View(artists)

index(Cz) <- Cz[,1]
Cz_approx <- na.approx(Cz)
Cz_approx
plot(Cz_approx,artists$popularity_score)



#approxfun(x = 22, y = 15, method = "linear",yleft, yright, rule = 1, f = 0, ties = mean)

#rule of thumb conjectured by Good and Hardin is {\displaystyle N=m^{n}}N=m^{n} 

## testing code
model <- lm(artists$popularity_score ~ artists$playlist_reach, data = artists)
model
par(mfrow=c(2,2)) # init 4 charts in 1 panel
plot(model)

corr.test(artists)

dataframe9 <- data.table(artists)
dataframe9[, major_label:=0]

#_________________INTERPOLATION EXPERIMENTING_________________




