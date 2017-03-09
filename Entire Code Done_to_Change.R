############################################################################################
####################################### Discogs Project ########################################
#########################        How valuable is your music?      ##########################
############################################################################################


##=========================================================================================
##------------------------- Table of contents ---------------------------------------------
##=========================================================================================
# 1. Data gathering
#   1.1 Loading R-packyears
#   1.2 Defining key links
#   1.3 Defining CSS Selectors
#   1.4 Bulding functions (link collection and scraping)
#   1.5 Applying functions to generate links
#   1.6 Applying functions to scrape the disc 2016
#   1.7 Importing table ranking for the disc 2016
#   1.8 Mergig data frames
# 2. Cleaning
#   2.1 Cleaning disc data and creating useful predictors
#   2.2 Adding a variable with the number of google hits on the disc
#   2.3 Cleaning club data and creating useful predictors
#   2.4 Merging disc and club data into one tidy data frame
# 3. Vizualization 
    # 3.1 Club map with transfer spending
    # 3.2 Club map with transfer paths 
    # 3.3 Transfer fee by year scatter plot
    # 3.4 Transfer fee by time left on contract scatter plot
    # 3.5 Averyear spending per club per league
    # 3.6 Averyear spending per disc per league 
    # 3.7 Averyear spending per disc per club status
# 4. Prediction Models
#   4.1 Dividing into a train and test sample
#   4.2 Create evaluation function
#   4.3 Baseline Model: Simple averyear from training sample
#   4.4 Ordinary least square model
#   4.5 Lasso model
#   4.6 Decision tree
#   4.7 Random forrest

##=========================================================================================
##------------------------- 1. Data Gathering--- ------------------------------------------
##=========================================================================================

##========================== 1.1 Loading R-packyears =======================================
library("rvest")
library("stringr")
library("purrr")
library("dplyr")
library("RCurl")
library("XML")
library("glmnet")
library("caret")
library("plotly")
library("ggplot2")
library("plotly")

# ##========================== 1.2 Defining key links =======================================
# Remove # for several lines by the shortcut ctrl/command + shift + c
#
# ## Links from discogs to overviews of transfers in season 14/15 in  the five major football leagues  
base.link = "https://www.discogs.com"
all.disclinks <- paste ("https://www.discogs.com/search/?layout=sm&pyear=",seq(0,50),"&decade=2010&year=2016", sep="")

# 
# ##========================== 1.3 Defining CSS selectors =======================================
# 
# ## Define CSS selectors from Discogs
css.selector.title = ".search_result_title"
# css.selector.transfer = ".table-header+ .responsive-table .rechts a"
# # (delete if no problem) css.selector.table = ".responsive-table .hauptlink .vereinprofil_tooltip"
# 

# 
# ##================= 1.4 Bulding functions (link collection and scraping) =====================
# 
## Function 1: creating collector-function that finds all the links to transfered discs in all major european football leagues
link.collector = function(vector){
  out = vector %>%
    read_html(encoding = "UTF-8") %>% #inddryearr special danish character
    html_nodes(css = css.selector.title) %>%
    html_attr(name = 'href') #tyearr den attribut med navnet hret
  Sys.sleep(sample(seq(10, 20, by=0.001), 1))
  return (out)
}

# 
## Function 3: Creating scraper-function to scrape all performance stats from detailed stat pyear
scrape_discstats = function(link){
  my.link = link %>%
    read_html(encoding = "UTF-8")
  name = my.link %>%
    html_nodes("#profile_title") %>%
    html_text()
  name = name[1]
  # Sys.sleep(sample(seq(1, 2, by=0.001), 1))

  artist = my.link %>%
    html_nodes("#profile_title a") %>%
    html_text()
  artist = artist[1]
  # Sys.sleep(sample(seq(1, 2, by=0.001), 1))

  year = my.link %>%
    html_nodes(".year.has_header") %>%
    html_text()
  year = year[1]
  # Sys.sleep(sample(seq(1, 2, by=0.001), 1))

  country = my.link %>%
    html_nodes(".country.has_header") %>%
    html_text()
  country = country[1]
  # Sys.sleep(sample(seq(1, 2, by=0.001), 1))

  label = my.link %>%
    html_nodes(".label.has_header") %>%
    html_text()
  label = label[1]
  # Sys.sleep(sample(seq(1, 2, by=0.001), 1))

  have = my.link %>%
    html_nodes(".coll_num") %>%
    html_text()
  have = have[1]
  # Sys.sleep(sample(seq(1, 2, by=0.001), 1))

  want = my.link %>%
    html_nodes(".want_num") %>%
    html_text()
  want = want[1]
  # Sys.sleep(sample(seq(1, 2, by=0.001), 1))

  genre = my.link %>%
    html_nodes(".content:nth-child(3) a") %>%
    html_text()
  genre = genre[1]
  # Sys.sleep(sample(seq(1, 2, by=0.001), 1))

  style = my.link %>%
    html_nodes(".content:nth-child(5) a") %>%
    html_text()
  style = style[1]
  # Sys.sleep(sample(seq(1, 2, by=0.001), 1))

  averageValue = my.link %>%
    html_nodes(".rating_value") %>%
    html_text()
  averageValue = averageValue[1]
  # Sys.sleep(sample(seq(1, 2, by=0.001), 1))

  format = my.link %>%
    html_nodes(".format") %>%
    html_text()
  format = format[1]
  # Sys.sleep(sample(seq(1, 2, by=0.001), 1))
  # Sys.sleep(sample(seq(1, 2, by=0.001), 1))
  #
   price = my.link %>%
    html_nodes(".price") %>%
    html_text()
  price = price[1]

  return(data.frame(name = name,
                    artists = artist,
                    year = year,
                    country = country,
                    label = label,
                    have = have,
                    total.want = want,
                    total.averageValue = averageValue,
                    format = format,
                    price = price,
                    genre=genre,
                    style=style))
 # Sys.sleep(sample(seq(3, 7, by=0.001), 1))
  }

# ##===================== 1.5 Applying functions to generate links =============================
# 
# applying function 1 and thereby creating a vector of all the links to transfered discs
# all.disclinks.partly = lapply(all.disclinks, link.collector)
# 
# all.profiles.partly = unlist(all.disclinks.partly) # transform from list to vector
# profile.links = paste(base.link,all.profiles.partly, sep ="") # creating full link
# profile.links[1:300] # showing the first 300 links

# ##=========== 1.6 Applying functions to scrape the performance and transfer stats ============
# 
## Create data frame with performance stats using function 3
disc.stats.year.rest = profile.links[500:100000]  %>%
  map_df(scrape_discstats)

??stringr

# 
# ## Create data frame with transfer stats using function 4
# transfer.stats = transfer.links  %>% 
#   map_df(scrape_transferstats)
#   
# 
# ##============ 1.7 Importing table ranking for the major leauges in season 14/15 ==============
# 
# pl.table14 = pl.table14.link %>%
#   read_html() %>% 
#   html_node(css.pl.table14) %>% 
#   html_table() %>%  # then convert the HTML table into a data frame
#   mutate(league = "Premier league") # adding a new column with the league name
# 
# bl.table14 = bl.table14.link %>%
#   read_html() %>% 
#   html_node(css.bl.table14) %>% 
#   html_table() %>%  
#   mutate(league = "Bundesliga")
#             
# ll.table14 = ll.table14.link %>%
#   read_html() %>% 
#   html_node(css.ll.table14) %>% 
#   html_table() %>%
#   mutate(league = "La Liga")
#             
# sa.table14 = sa.table14.link %>%
#   read_html() %>% 
#   html_node(css.sa.table14) %>% 
#   html_table() %>%  
#   mutate(league = "Serie A")
#             
# l1.table14 = l1.table14.link %>%
#   read_html() %>% 
#   html_node(css.l1.table14) %>% 
#   html_table() %>% 
#   mutate(league = "Ligue 1")
#             
# 
# 
# ##============================ 1.8 Merging data frames ====================================
# 
# ## merging the performance and transfer data frames into one disc data frame
# disc.data = left_join(transfer.stats, disc.stats.season)
# 
# ## saving uncleaned disc data as csv
# write.table(disc.data, file = "disc_data_unclean.csv",
#             sep = ",", col.names = NA, qmethod = "double")
# 
# ## merging the league specific data frames into one club data frame
# club.data = rbind(pl.table14, bl.table14, ll.table14, sa.table14, l1.table14)
# 
# ## saving uncleaned club data as csv
# write.table(club.data, file = "club_data_unclean.csv",
#              sep = ",", col.names = NA, qmethod = "double")


##=========================================================================================
##--------------------------------- 2. Cleaning -------------------------------------------
##=========================================================================================


##================ 2.1 Cleaning disc data and creating useful predictors ================
disc.data.cleaning = read.csv("disc_data_unclean.csv", encoding = "Latin1") # loading saved version of uncleaned disc data

## 2.1.1: Cleaning transfer fee variable
disc.data.cleaning$transfer.fee = str_replace(disc.data.cleaning$transfer.fee,"?","")
disc.data.cleaning$transfer.fee = str_replace(disc.data.cleaning$transfer.fee,"\\.","") #removing the dots
disc.data.cleaning$transfer.fee = str_replace(disc.data.cleaning$transfer.fee,"m","0000") #removing the m 
disc.data.cleaning$transfer.fee = str_replace(disc.data.cleaning$transfer.fee,"k","000") #removing the k
disc.data.cleaning$transfer.fee = str_replace(disc.data.cleaning$transfer.fee,"-", NA) #removing the - and turn into NA
disc.data.cleaning$transfer.fee = str_replace(disc.data.cleaning$transfer.fee,"\\?", NA) #removing ? and turn into NA
disc.data.cleaning$transfer.fee = str_replace(disc.data.cleaning$transfer.fee,"Free transfer","0") # Setting free transfer to be equal to 0

## 2.1.2: Cleaning want pr. minutes
disc.data.cleaning$minutes.pr.goal = str_sub(disc.data.cleaning$minutes.pr.goal, start=1, end=-2)
disc.data.cleaning$minutes.pr.goal = str_replace(disc.data.cleaning$minutes.pr.goal,"\\.","")

## 2.1.3: Cleaning total minutes played
disc.data.cleaning$total.minutes.played = str_sub(disc.data.cleaning$total.minutes.played, start=1, end=-2)
disc.data.cleaning$total.minutes.played = str_replace(disc.data.cleaning$total.minutes.played,"\\.","")

## 2.1.4: Cleaning variable for transferdate
disc.data.cleaning$transfer.year = sub(".*,", "", disc.data.cleaning$transfer.date) # subtract the year
disc.data.cleaning$transfer.month = str_sub(disc.data.cleaning$transfer.date, start=1, end=-9) #subtract month
disc.data.cleaning$transfer.month = str_replace(disc.data.cleaning$transfer.month, "Jan", 01) #month by number instead of character
disc.data.cleaning$transfer.month = str_replace(disc.data.cleaning$transfer.month, "Feb", 02)
disc.data.cleaning$transfer.month = str_replace(disc.data.cleaning$transfer.month, "Mar", 03)
disc.data.cleaning$transfer.month = str_replace(disc.data.cleaning$transfer.month, "Apr", 04)
disc.data.cleaning$transfer.month = str_replace(disc.data.cleaning$transfer.month, "May", 05)
disc.data.cleaning$transfer.month = str_replace(disc.data.cleaning$transfer.month, "Jun", 06)
disc.data.cleaning$transfer.month = str_replace(disc.data.cleaning$transfer.month, "Jul", 07)
disc.data.cleaning$transfer.month = str_replace(disc.data.cleaning$transfer.month, "Aug", 08)
disc.data.cleaning$transfer.month = str_replace(disc.data.cleaning$transfer.month, "Sep", 09)
disc.data.cleaning$transfer.month = str_replace(disc.data.cleaning$transfer.month, "Oct", 10)
disc.data.cleaning$transfer.month = str_replace(disc.data.cleaning$transfer.month, "Nov", 11)
disc.data.cleaning$transfer.month = str_replace(disc.data.cleaning$transfer.month, "Dec", 12)
disc.data.cleaning$transfer.day = str_sub(disc.data.cleaning$transfer.date, start=4, end=-7) #subtract day
disc.data.cleaning$transfer.date = paste(disc.data.cleaning$transfer.year,"/", 
                                           disc.data.cleaning$transfer.month,"/",
                                           disc.data.cleaning$transfer.day) ## full transferdate in right format

disc.data.cleaning$transfer.date = str_replace_all(disc.data.cleaning$transfer.date, " ","") 

## 2.1.5: Cleaning contract length left
disc.data.cleaning$contract.period.left = gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", disc.data.cleaning$contract.period.left, perl=T) #extract the date when the contract expires

disc.data.cleaning$end.year = sub(".*,", "", disc.data.cleaning$contract.period.left) # subtract the year
disc.data.cleaning$end.month = str_sub(disc.data.cleaning$contract.period.left, start=1, end=-9) #subtract month
disc.data.cleaning$end.month = str_replace(disc.data.cleaning$end.month, "Jan", 01) #month by number instead of character
disc.data.cleaning$end.month = str_replace(disc.data.cleaning$end.month, "Feb", 02)
disc.data.cleaning$end.month = str_replace(disc.data.cleaning$end.month, "Mar", 03)
disc.data.cleaning$end.month = str_replace(disc.data.cleaning$end.month, "Apr", 04)
disc.data.cleaning$end.month = str_replace(disc.data.cleaning$end.month, "May", 05)
disc.data.cleaning$end.month = str_replace(disc.data.cleaning$end.month, "Jun", 06)
disc.data.cleaning$end.month = str_replace(disc.data.cleaning$end.month, "Jul", 07)
disc.data.cleaning$end.month = str_replace(disc.data.cleaning$end.month, "Aug", 08)
disc.data.cleaning$end.month = str_replace(disc.data.cleaning$end.month, "Sep", 09)
disc.data.cleaning$end.month = str_replace(disc.data.cleaning$end.month, "Oct", 10)
disc.data.cleaning$end.month = str_replace(disc.data.cleaning$end.month, "Nov", 11)
disc.data.cleaning$end.month = str_replace(disc.data.cleaning$end.month, "Dec", 12)
disc.data.cleaning$end.day = str_sub(disc.data.cleaning$contract.period.left, start=4, end=-7) #subtract day
disc.data.cleaning$contract.end.date = paste(disc.data.cleaning$end.year,"/", 
                                               disc.data.cleaning$end.month,"/",
                                               disc.data.cleaning$end.day) ## full date in right format

disc.data.cleaning$contract.end.date = str_replace_all(disc.data.cleaning$contract.end.date, " ","") #remove whitespaces
disc.data.cleaning$contract.end.date = str_replace_all(disc.data.cleaning$contract.end.date, "//",NA) # label missing data NA
disc.data.cleaning$contract.end.date = str_replace_all(disc.data.cleaning$contract.end.date, "NA/NA/NA",NA) # label missing data NA

# calculating the number of days left of the contract 
disc.data.cleaning$contract.left = as.Date(as.character(disc.data.cleaning$contract.end.date), format = "%Y/%m/%d") -
  as.Date(as.character(disc.data.cleaning$transfer.date), format = "%Y/%m/%d")

disc.data.cleaning$contract.left.month = disc.data.cleaning$contract.left / 30.4375 ## rescaling from days to months

## mistakes at discogs.co.uk does that we recieve some observations with a negative contract length 
## we turn them into NA
disc.data.cleaning$contract.left.month[disc.data.cleaning$contract.left.month < 0] = NA 

## 2.1.6: Cleaning year variabel
disc.data.cleaning$birth.date = disc.data.cleaning$year
disc.data.cleaning$birth.date = str_replace_all(disc.data.cleaning$birth.date, " ","")
disc.data.cleaning$birth.date = str_sub(disc.data.cleaning$birth.date, start=1, end=-6) #birth date

disc.data.cleaning$birth.year = sub(".*,", "", disc.data.cleaning$birth.date) # subtract the year

disc.data.cleaning$birth.month = substring(disc.data.cleaning$birth.date, 1,4)
disc.data.cleaning$birth.month = str_replace(disc.data.cleaning$birth.month, "Jan", 01) #month by number instead of character
disc.data.cleaning$birth.month = str_replace(disc.data.cleaning$birth.month, "Feb", 02)
disc.data.cleaning$birth.month = str_replace(disc.data.cleaning$birth.month, "Mar", 03)
disc.data.cleaning$birth.month = str_replace(disc.data.cleaning$birth.month, "Apr", 04)
disc.data.cleaning$birth.month = str_replace(disc.data.cleaning$birth.month, "May", 05)
disc.data.cleaning$birth.month = str_replace(disc.data.cleaning$birth.month, "Jun", 06)
disc.data.cleaning$birth.month = str_replace(disc.data.cleaning$birth.month, "Jul", 07)
disc.data.cleaning$birth.month = str_replace(disc.data.cleaning$birth.month, "Aug", 08)
disc.data.cleaning$birth.month = str_replace(disc.data.cleaning$birth.month, "Sep", 09)
disc.data.cleaning$birth.month = str_replace(disc.data.cleaning$birth.month, "Oct", 10)
disc.data.cleaning$birth.month = str_replace(disc.data.cleaning$birth.month, "Nov", 11)
disc.data.cleaning$birth.month = str_replace(disc.data.cleaning$birth.month, "Dec", 12)

disc.data.cleaning$birth.day = substring(disc.data.cleaning$birth.date, 5,6) #subtract day
disc.data.cleaning$birth.day = str_replace(disc.data.cleaning$birth.day, ",","") #removing unneccesary

disc.data.cleaning$birth.date = paste(disc.data.cleaning$birth.year,"/",disc.data.cleaning$birth.month,"/",disc.data.cleaning$birth.day) ## full birthdate in right format


disc.data.cleaning$birth.date = str_replace_all(disc.data.cleaning$birth.date," ","") #remove whitespaces
disc.data.cleaning$birth.date = str_replace(disc.data.cleaning$birth.date," ","")
disc.data.cleaning$birth.date = str_replace(disc.data.cleaning$birth.date,"\n","")

## Calculating the year at the transferdate 
disc.data.cleaning$transferyear = as.Date(as.character(disc.data.cleaning$transfer.date), format = "%Y/%m/%d") -
  as.Date(as.character(disc.data.cleaning$birth.date), format = "%Y/%m/%d")

disc.data.cleaning$transferyear = disc.data.cleaning$transferyear / 365.25 #rescaling from days to years

### 2.1.7: Creating variable that group discs in defenders, midfielders and attackers 
#disc.data.cleaning$artists = str_replace(disc.data.cleaning$artists," Defender","Defender")
disc.data.cleaning$artists = str_replace(disc.data.cleaning$artists,"Right-Back","Defender")
disc.data.cleaning$artists = str_replace(disc.data.cleaning$artists,"Left-Back","Defender")
disc.data.cleaning$artists = str_replace(disc.data.cleaning$artists,"Centre Back","Defender")
#disc.data.cleaning$artists = str_replace(disc.data.cleaning$artists,"-","Defender") # one observation has a -, but is defender - AMER?
disc.data.cleaning$artists = str_replace(disc.data.cleaning$artists,"Attacking Midfield","Midfield")
disc.data.cleaning$artists = str_replace(disc.data.cleaning$artists,"Central Midfield","Midfield")
disc.data.cleaning$artists = str_replace(disc.data.cleaning$artists,"Defensive Midfield","Midfield")
disc.data.cleaning$artists = str_replace(disc.data.cleaning$artists,"Left Midfield","Midfield")
disc.data.cleaning$artists = str_replace(disc.data.cleaning$artists,"Right Midfield","Midfield")
disc.data.cleaning$artists = str_replace(disc.data.cleaning$artists,"Right Wing","Attacker")
disc.data.cleaning$artists = str_replace(disc.data.cleaning$artists,"Left Wing","Attacker")
disc.data.cleaning$artists = str_replace(disc.data.cleaning$artists,"Centre Forward","Attacker")
disc.data.cleaning$artists = str_replace(disc.data.cleaning$artists,"Secondary Striker","Attacker")

### 2.1.8: General cleaning 
## Setting "-" equal to 0 for the performance variables
perform.var = c("appearances", "total.want", "total.averageValue", 
                "substitutions_in", "substitutions_out", "price", "secondyellow",
                "redcards", "penaltywant", "minutes.pr.goal","total.minutes.played")
disc.data.cleaning[perform.var] = 
  sapply(disc.data.cleaning[perform.var], as.character) #  transforming performance var into character variables 

disc.data.cleaning[perform.var][disc.data.cleaning[perform.var] == "-"] = 0 #replacing - with 0 for performance variables

## Removing rows where artists are unknown.
disc.data.cleaning = disc.data.cleaning[-c(30, 67, 78, 83, 105, 178, 457, 659), ]

## Removing discs who appear double in the sample
disc.data.cleaning = subset(disc.data.cleaning, select=-c(X))
disc.data.cleaning = disc.data.cleaning[!duplicated(disc.data.cleaning), ] ## removing dublicated observations

## Removing rows which contain transfer.fee = NA
disc.data.cleaning = subset(disc.data.cleaning, !is.na(transfer.fee))

## Removing keepers
disc.data.cleaning = disc.data.cleaning[!grepl("Keeper", disc.data.cleaning$artists),]

## Putting NA in all blank cells 
disc.data.cleaning[disc.data.cleaning == ""] = NA 


### Transforming variables containing numbers into numeric variables
sapply(disc.data.cleaning, class) #inspecting the class of all variables

var.to.numeric = c("year", "transfer.fee", "appearances", "total.want", "total.averageValue", 
                   "substitutions_in", "substitutions_out", "price", "secondyellow",
                   "redcards", "penaltywant", "minutes.pr.goal","total.minutes.played") #creating a 
#vector with the names of the variables that we want numeric

disc.data.cleaning[var.to.numeric] = 
  sapply(disc.data.cleaning[var.to.numeric], as.numeric) #  transforming the selected var into numeric

## Rescaling transfer fees into million punds 
disc.data.cleaning$transfer.fee = disc.data.cleaning$transfer.fee / 1000000


## Removing column X.1 and X, which are not useful.
names(disc.data.cleaning)
disc.data.clean = subset(disc.data.cleaning, select=-c(contract.period.left, year, 
                                                           transfer.year, transfer.month, 
                                                           transfer.day, end.year, end.month,
                                                           end.day, contract.end.date, contract.left,
                                                           birth.year, birth.month, birth.day))


### 2.2: Adding a variable with the number of google hits on the disc

## Creating a function that find the 
GoogleHits <- function(input)  #Function that seach for the input specified
{
  input = gsub(" ", "+", input)
  #url <- paste("https://www.google.com/search?q=\"",
  #             input, "\"", sep = "")
  url = paste0("https://www.google.com/search?q=",
               input)
  
  CAINFO = paste(system.file(packyear="RCurl"), "/CurlSSL/ca-bundle.crt", sep = "")
  script <- getURL(url, followlocation = TRUE, cainfo = CAINFO)
  doc <- htmlParse(script)
  res <- xpathSApply(doc, '//*/div[@id="resultStats"]', xmlValue)
  cat(paste("\nYour Search URL:\n", url, "\n", sep = ""))
  #cat("\nNo. of Hits:\n")
  return(as.integer(gsub("[^0-9]", "", res)))
}

##
search.1=dQuote(disc.data.clean$name)  #Put quotation marks around name of the disc
search.2=paste(search.1,"footballer", sep=" ") #Paste name of footballer, the word footballer and country
search.2

# OBS!
# Google as maximum number of searches that you can make on one day. Therefore we splittet our
# search in three parts and did the searches from different IP-addresses. 

disc.data.clean.pt1=disc.data.clean[1:250,]
list.pt.1=lapply(search.2[1:250], GoogleHits)
disc.data.clean.pt1$searchresults=unlist(list.pt.1) #New column reporting number of search results

disc.data.clean.pt2=disc.data.clean[251:500,]
list.pt.2=lapply(search.2[251:500], GoogleHits)
disc.data.clean.pt2$searchresults=unlist(list.pt.2) #New column reporting number of search results

disc.data.clean.pt3=disc.data.clean[501:696,]
list.pt.3=lapply(search.2[501:696], GoogleHits)
disc.data.clean.pt3$searchresults=unlist(list.pt.3) #New column reporting number of search results

disc.data.clean = rbind(disc.data.clean.pt1, disc.data.clean.pt2, disc.data.clean.pt3)

## saving cleaned disc data as csv
write.table(disc.data.clean, file = "disc_data_clean.search.csv",
            sep = ",", col.names = NA, qmethod = "double", fileEncoding = "UTF-8")


##================ 2.3 Cleaning club data and creating useful predictors ================
club.data = read.csv("club_data_unclean.csv", encoding="latin1") # loading saved version of uncleaned club data
club.data.cleaning = club.data

## Giving different status to different clubs depending on which artist they ended at in the league.
attach(club.data.cleaning)
club.data.cleaning$Status[Pos <= 5] = "Top Club"
club.data.cleaning$Status[Pos <= 15 & Pos > 5] = "Middle Club"
club.data.cleaning$Status[Pos >= 16] = "Bottom Club"
detach(club.data.cleaning)

## Changing the name of the team name variable
names(club.data.cleaning)[names(club.data)=="Team..v.t.e"] <- "Team"
names(club.data.cleaning)

## Renaming clubs in Wikipedia-tabel first

club.data.cleaning$Team=recode(club.data.cleaning$Team,"Barcelona (C)"="FC Barcelona", "Valencia"="Valencia CF", "M?laga"="M?laga CF", "Elche[d](R)"="Elche CF", 
                               "Levante"="Levante UD", "Getafe"="Getafe CF", "Deportivo La Coru?a"="Dep. La Coru?a", "Granada"="Granada CF",
                               "Eibar"="SD Eibar", "Almer?a (R)"="UD Almer?a", "C?rdoba (R)"="C?rdoba CF", "Sevilla"="Sevilla FC",
                               "Villarreal" = "Villarreal CF", "Celta Vigo" = "Celta de Vigo","Juventus (C)"="Juventus", "Cargliari (R)"="Cagliari Calcio", "Parma[c](R)"="Parma", "Cesena (R)"="Cesena",
                               "Internazionale"="Inter", "Genoa[b]"="Genoa", "Roma"="AS Roma", "Napoli"="SSC Napoli", "Milan"="AC Milan",
                               "Palermo"="US Palermo", "Chievo"="Chievo Verona", "Empoli"="FC Empoli", "Udinese"="Udinese Calcio",
                               "Cagliari (R)"="Cagliari Calcio","Paris Saint-Germain (C)"="Paris SG", "Evian (R)"="Evian", "Metz (R)"="FC Metz", "Lyon"="Olympique Lyon",
                               "Bordeaux"="G. Bordeaux", "Lille"="LOSC Lille", "Nice"="OGC Nice", "Caen"="SM Caen", "Nantes"="FC Nantes",
                               "Lorient"="FC Lorient", "Bordeaux"="G. Bordeaux", "Lens[b](R)"="RC Lens", "Bastia"="SC Bastia","Bayern Munich (C)"="Bayern Munich ", "SC Freiburg (R)"="SC Freiburg", "SC Paderborn 07 (R)"="SC Paderborn",
                               "Hamburger SV (O)"="Hamburger SV", "Borussia M?nchengladbach"="Bor. M'gladbach", "Schalke 04"="FC Schalke 04",
                               "Bayer Leverkusen"="Bay. Leverkusen", "Eintracht Frankfurt"="E. Frankfurt", "Borussia Dortmund"="Bor. Dortmund",
                               "1899 Hoffenheim" = "TSG Hoffenheim", "FSV Mainz 05"="1.FSV Mainz 05","Chelsea (C)"="Chelsea", "Hull City (R)"="Hull City", "Burnley"="Burnley FC", "Queens Park Rangers (R)"="QPR",
                               "West Bromwich Albion"="West Brom", "Tottenham Hotspur"="Spurs","Swansea City"="Swansea", 
                               "Manchester United"="Manchester Utd.", "West Ham United"="West Ham", "Leicester City"="Leicester", 
                               "Newcastle United"="Newcastle")


# Selecting the useful clubvariables
names(club.data.cleaning)
club.data.clean = subset(club.data.cleaning, select=c(Team, league, Status))

## saving cleaned disc data as csv
write.table(club.data.clean, file = "club_data_clean.csv",
            sep = ",", col.names = NA, qmethod = "double", fileEncoding = "UTF-8")          

# ##================ 2.3 Merging disc and club data into one tidy data frame ================
# setwd("/Users/guillaumeslizewicz/Documents/SDS-group12/Exam_project")
# disc.data.clean= read.csv(file="disc_data_clean.csv", encoding = "latin1")
# iconv(disc.data.clean, from = "latin1", to = "UTF8", sub = NA, mark = TRUE, toRaw = FALSE)
# 
# club.data.clean=read.csv(file = "club.data.clean.csv", encoding="UTF8")

transferdata.tidy=merge(disc.data.clean,club.data.clean, by.x=c("club.to"),by.y=c("Team"), all.x=TRUE)

## Handeling promoted clubs
transferdata.tidy$Status[is.na(transferdata.tidy$Status)] = "Promoted"
transferdata.tidy$league[(transferdata.tidy$club.to == "Watford") | 
                           (transferdata.tidy$club.to == "Norwich")|
                           (transferdata.tidy$club.to == "Bournemouth")] = "Premier league"
transferdata.tidy$league[(transferdata.tidy$club.to == "FC Ingolstadt") | 
                           (transferdata.tidy$club.to == "SV Darmstadt 98")] = "Bundesliga"
transferdata.tidy$league[(transferdata.tidy$club.to == "UD Las Palmas")| 
                           (transferdata.tidy$club.to == "Sporting Gij?n")|
                           (transferdata.tidy$club.to == "Real Betis")] = "La Liga"
transferdata.tidy$league[(transferdata.tidy$club.to == "Bologna")| 
                           (transferdata.tidy$club.to == "Carpi")|
                           (transferdata.tidy$club.to == "Frosinone")] = "Serie A"
transferdata.tidy$league[(transferdata.tidy$club.to == "SCO Angers")| 
                           (transferdata.tidy$club.to == "Troyes")|
                           (transferdata.tidy$club.to == "G. Ajaccio")] = "Ligue 1"

## Saving the tidy final data set 
write.table(transferdata.tidy, file = "transferdata.final.csv",
            sep = ",", col.names = NA, qmethod = "double", fileEncoding = "UTF-8")

##=========================================================================================
##------------------------- 3. Visualisation --- ------------------------------------------
##=========================================================================================
##LIBRARIES
library(plotly)
#install.packyears("extrafont")
#install.packyears("Cairo")
library(extrafont)
library("ggmap")# getting maps and coordinates from google
library(maptools)# getting maps and coordinates from google
library(maps)# getting maps and coordinates from google
library("ggplot2")# plotting the data
library(dplyr)#tidying dataset
library(rworldmap)
library(stringr)#dealing with strings and replacing strings in observations
library(RCurl)


#creating dataset
df.viz<- read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/transferdata.final.csv", encoding = "UTF8", header = TRUE)

####remove disc with transfer fee= 0 et contract time left=0
df.viz<- filter(df.viz,transfer.fee>0)
####remove disc with transfer fee= 0 et contract time left=0
#df.stats<- filter(df.stats,transfer.fee>0 | is.na(contract.left.month)==FALSE)


##GETTING STARTING MAP
#Watercolor
myLocation <- "Zurich"
myMap <- get_map(location= myLocation,
                 source="stamen", maptype="watercolor", crop=FALSE,zoom=4)
ggmap(myMap)

#B&W
myLocation <- "Zurich"
myMap <- get_map(location= myLocation,
                 source="stamen",maptype="toner", crop=FALSE,zoom=4)
ggmap(myMap)

##================ 3.1 CLUB MAP WITH TRANSFER SPENDING ================

# #grouping by clubs
# df.spending.club = df.viz %>%
#   group_by(club.to,league)%>%
#   dplyr::summarise(transfer.fee.total = sum(transfer.fee))
# 
# #tidying data frame
# colnames(df.spending.club)[1] <- "team"#change "club to" to "team"
# 
# #TEAM
# 
# df.spending.club$team = str_replace(df.spending.club$team,"[1234567890]","")#removing the unwanted numbers*3 because it only take one out at a time
# df.spending.club$team = str_replace(df.spending.club$team,"[1234567890]","")
# df.spending.club$team = str_replace(df.spending.club$team,"[1234567890]","")
# df.spending.club$team = str_replace(df.spending.club$team,"[1234567890]","")
# df.spending.club$team = str_replace(df.spending.club$team,"*\\[.*?\\] *","")#removing the unwanted characters between brackets
# df.spending.club$team = str_replace(df.spending.club$team,"Borussia Mönchengladbach","Mönchengladbach Borussia")
# df.spending.club$team = str_replace(df.spending.club$team,"FC Augsburg","Augsburg FC")
# df.spending.club$team = str_replace(df.spending.club$team,"FC Köln","Cologne FC")
# df.spending.club$team = str_replace(df.spending.club$team,"VfB Stuttgart","Stuttgart VfB")
# df.spending.club$team = str_replace(df.spending.club$team,"Hellas Verona","Verona FC")
# df.spending.club$team = str_replace(df.spending.club$team,"BSC","Berlin")
# df.spending.club$team = str_replace(df.spending.club$team,"Juventus","Juventus Turin")
# df.spending.club$team = str_replace(df.spending.club$team,"Inter","Inter Milan")
# df.spending.club$team = str_replace(df.spending.club$team,"US","FC")
# df.spending.club$team = str_replace(df.spending.club$team,"\\.","")
# df.spending.club$team = str_replace(df.spending.club$team," *\\(.*?\\) *","") #remove (C) for champions
# 
# #class transforming to numeric value or character value
# df.spending.club$transfer.fee.total <- as.numeric(df.spending.club$transfer.fee.total)
# df.spending.club$team <- as.character(df.spending.club$team)
# 
# ###ADD COUNTRIES TO TEAM NAMES (in order to find them on gmap)
# 
# df.spending.club$team <- with(df.spending.club, ifelse(league=="Bundesliga", paste(team,"GERMANY", sep = " "),
#                                                        ifelse(league=="Ligue 1", paste(team,"FRANCE", sep = " "),
#                                                               ifelse(league=="Serie A", paste(team,"ITALY", sep = " "),
#                                                                      ifelse(league=="Premier league", paste(team,"UK", sep = " "),
#                                                                             ifelse(league=="La Liga", paste(team,"SPAIN", sep = " "),""))))))
# 
# 
# #geocode team
# # geocodes <- geocode(as.character(df.spending.club$team))
# # write_csv(geocodes,"geocodes.csv")
# read_csv("geocodes.csv")
# 
# #new dataframe with geocode
# df.spending.club <- data.frame(df.spending.club[1:3],geocodes)
# 
# out.of.europe<-filter(df.spending.club, lon < -10 |lat < 35)
# out.of.europe.2<- filter(df.spending.club, lon>20 |lat>60)
# out.full= rbind(out.of.europe.2, out.of.europe)
# 
# write_csv(df.spending.club,"df_spending_club_with_geo.csv")

#GETTING DATA
df.spending.club<-read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/df_spending_club_with_geo.csv", encoding = "UTF8", header = TRUE)

#WITH GGPLOT
map.clubs <- ggmap(myMap) +
  geom_point(aes(x = lon, y = lat, size=transfer.fee.total), data =df.spending.club,col="red", alpha=0.4)+
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks= element_line(color=NA),
        axis.line = element_line(color = NA),
        text=element_text(family="LM Roman 10"))+
  ggtitle("Total transfer spending for clubs in Europe")+
  labs(size="")+
  scale_size(breaks = c(5.0e+7,1.0e+8,1.5e+8),labels = c("50M£","100M£","150M£"))

map.clubs
#####with plotly
# m <- list(
#   colorbar = list(title = "Total transfer spending"),
#   size = 10, opacity = 0.8, symbol = 'circle'
# )
# 
# # geo styling
# g <- list(
#   scope = 'europe',
#   projection = list(type = 'mercator'),
#   showland = TRUE,
#   landcolor = toRGB("gray95"),
#   subunitcolor = toRGB("gray85"),
#   countrycolor = toRGB("gray85"),
#   countrywidth = 0.5,
#   subunitwidth = 0.5
# )
# g
# 
# plot_ly(df.spending.club, lat = lat, lon = lon,  color = transfer.fee.total,
#         #text = team,
#         hoverinfo = "text" ,
#         text=paste("Team = ", df.spending.club$team,"\n", "Total transfer = ", df.spending.club$transfer.fee.total),
#         type = 'scattergeo', locationmode = 'ISO-3', mode = 'markers', 
#         marker = m) %>%
#   layout(title = 'Football teams in Europe and transfer spending', geo = g)

#================ 3.2 CLUB MAP WITH TRANSFER PATHS ================

# #Getting data from df
# disc.data = read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/transferdata.final.csv", header=TRUE, stringsAsFactors=TRUE, fileEncoding="UTF8") # loading saved version of uncleaned disc data
# transfer.data = disc.data
# 
# # transfer.data <- transfer.data[sample(1:nrow(transfer.data), 50,# taking a random 50 sample
# #                           replace=FALSE),]
# 
# transfer.data.geo<-transfer.data %>% 
#   group_by(club.from) %>% 
#   dplyr::summarise()
# 
# 
# transfer.data.geo= completeFun(transfer.data.geo,"club.from") #function applied to transfer data to remove unknown origin
# 
# ###Looping for club coordinate
# 
# geocodes.club.from <- geocode(as.character(transfer.data.geo$club.from))
# #transfer.path.origin <- data.frame(transfer.data,geocodes.club.from)
# 
# #transfer.path.origin= data.frame(rep(i, nrow(transfer.data)), transfer.data$V1, transfer.data$lon, transfer.data$lat) #creating a dataset with destination coordinates
# transfer.path.origin= data.frame( disc.data.geo$club.from, geocodes.club.from$lon, geocodes.club.from$lat) #creating a dataset with destination coordinates
# colnames(transfer.path.origin)[1] <- "club.from"#change "club.from"
# 
# 
# ###CODE that takes the geocode from 1st visualisation
# #transfer path destination
# colnames(transfer.data)[2] <- "team"#change "club to" to "team"
# #colnames(transfer.data)[5] <- "club.from"#change "club to" to "team"
# 
# #TEAM
# 
# transfer.data$team = str_replace(transfer.data$team,"[1234567890]","")#removing the unwanted numbers*3 because it only take one out at a time
# transfer.data$team = str_replace(transfer.data$team,"[1234567890]","")
# transfer.data$team = str_replace(transfer.data$team,"[1234567890]","")
# transfer.data$team = str_replace(transfer.data$team,"[1234567890]","")
# transfer.data$team = str_replace(transfer.data$team,"*\\[.*?\\] *","")#removing the unwanted characters between brackets
# transfer.data$team = str_replace(transfer.data$team,"Borussia Mönchengladbach","Mönchengladbach Borussia")
# transfer.data$team = str_replace(transfer.data$team,"FC Augsburg","Augsburg FC")
# transfer.data$team = str_replace(transfer.data$team,"FC Köln","Cologne FC")
# transfer.data$team = str_replace(transfer.data$team,"VfB Stuttgart","Stuttgart VfB")
# transfer.data$team = str_replace(transfer.data$team,"Hellas Verona","Verona FC")
# transfer.data$team = str_replace(transfer.data$team,"BSC","Berlin")
# transfer.data$team = str_replace(transfer.data$team,"Juventus","Juventus Turin")
# transfer.data$team = str_replace(transfer.data$team,"Inter","Inter Milan")
# transfer.data$team = str_replace(transfer.data$team,"US","FC")
# transfer.data$team = str_replace(transfer.data$team,"\\.","")
# transfer.data$team = str_replace(transfer.data$team," *\\(.*?\\) *","") #remove (C) for champions
# 
# #class transforming to numeric value or character value
# transfer.data$transfer.fee <- as.numeric(transfer.data$transfer.fee)
# transfer.data$team <- as.character(transfer.data$team)
# 
# ###ADD COUNTRIES TO TEAM NAMES (in order to find them on gmap)
# 
# transfer.data$team <- with(transfer.data, ifelse(league=="Bundesliga", paste(team,"GERMANY", sep = " "),
#                                                        ifelse(league=="Ligue 1", paste(team,"FRANCE", sep = " "),
#                                                               ifelse(league=="Serie A", paste(team,"ITALY", sep = " "),
#                                                                      ifelse(league=="Premier league", paste(team,"UK", sep = " "),
#                                                                             ifelse(league=="La Liga", paste(team,"SPAIN", sep = " "),""))))))
# 
# 
# transfer.data= completeFun(transfer.data,"lon")#applying the function to remove NA/unidentified to transfer.path
# 
# #transfer path origin
# 
# transfer.path1<-dplyr::left_join(transfer.data, transfer.path.origin, by = "club.from")
# 
# 
# #read geocode and adding it to data frame
# geocodes<- read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/geocodes.csv", encoding = "UTF8", header = TRUE)
# transfer.data.destination <- data.frame(df.spending.club[1:3],geocodes)
# 
# #building destination list
# transfer.path2<-dplyr::left_join(transfer.data, transfer.data.destination, by = "team")
# 
# 
# 
# 
# #BINDING
# transfer.path1$index<-c(1:nrow(transfer.path1))
# colnames(transfer.path1)[27] <- "lon"#change "club to" to "team"
# colnames(transfer.path1)[28] <- "lat"#change "club to" to "team"
# 
# transfer.path2$index<-c(1:nrow(transfer.path2))
# transfer.path2
# 
# transfer.path.full<-bind_rows(transfer.path1,transfer.path2)
# 
# 
# transfer.path.full<-transfer.path.full %>%
#   select(lon,lat,name,team,club.from,transfer.fee, league,index)#selecting the columns
# 
# transfer.path.full= arrange(transfer.path.full, desc(index))# organising in descending order
#write.csv(transfer.path.full, "transfer_path_full.csv")

transfer.path.full = read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/transfer_path_full.csv", header=TRUE, stringsAsFactors=TRUE, fileEncoding="UTF8") # loading saved version of uncleaned disc data

##MAAPING 

path.map<-ggmap(myMap)+#calling map
  geom_path(aes(x = lon, y = lat, group = factor(name)), #putting paths on the map
            colour="red", data = transfer.path.full, alpha=0.4)+
  theme(axis.title.x=element_blank(),
        axis.ticks= element_line(color=NA),
        axis.text= element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour="#CACACA", size=0.2), #add grid
        axis.title.y=element_blank(),
        text=element_text(family="LM Roman 10"))+
  ggtitle("Transfer for season 2014/2015")

path.map


###BOTH MAPS SUPERPOSED
full.map<-ggmap(myMap)+#calling map
  geom_path(aes(x = lon, y = lat, group = factor(name)), #putting paths on the map
            colour="orange", data = transfer.path.full, alpha=0.4)+
  geom_point(aes(x = lon, y = lat, size=transfer.fee.total), data =df.spending.club,col="red", alpha=0.4)+
  theme(axis.title.x=element_blank(),
        axis.ticks= element_line(color=NA),
        axis.text= element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(colour="#CACACA", size=0.2), #add grid
        axis.title.y=element_blank(),
        text=element_text(family="LM Roman 10"))+
  ggtitle("Transfer for season 2014/2015")+
  labs(size="Transfer spending\n per club")+
  scale_size(breaks = c(5.0e+7,1.0e+8,1.5e+8),labels = c("50M£","100M£","150M£"))


full.map

#================ 3.3 TRANSFER FEE BY year SCATTER PLOT ================

p.year = ggplot(df.viz, aes(x = transferyear , y = transfer.fee))
p.year<-p.year + geom_point(stat = "identity",col="red",alpha=0.4,aes(text = paste("Name:",name)))+ #to use for ggplot
  geom_smooth(aes(colour = transferyear, fill = transferyear))+
  ggtitle("year repartition of transfers in European leagues")+
  labs(y="Transfer price\nin M£",x="year") +
  theme(axis.ticks.y= element_line(color=NA),
        axis.ticks.x=element_line(colour="#CACACA", size=0.2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.y =element_text(angle = 0,
                                   colour="#525252",
                                   vjust = 1,
                                   hjust = 0 ),
        axis.title.x =element_text(angle = 0,
                                   colour="#525252",
                                   vjust = -1,
                                   hjust = 1 ),
        panel.grid.major.y = element_line(colour="#CACACA", size=0.2), #add grid
        axis.title.y=element_blank(),
        text=element_text(family="LM Roman 10"))
p.year

#plotly.p.year<-ggplotly(p.year)
#plotly.p.year

#================ 3.4 TRANSFER FEE BY TIME LEFT ON CONTRACT SCATTER PLOT ================

p.time <- ggplot(data=df.viz, aes(x = contract.left.month , y = transfer.fee)) +
  geom_point(stat = "identity",col="red",alpha=0.4,aes(text = paste("Name:",name)))+
  #geom_point(aes(text = paste(name, " to ", club.to)), size = 4) +
  geom_smooth(aes(colour = contract.left.month, fill = contract.left.month))+
  ggtitle("Time left on contract seems to be positively correlated with transfer fees")+
  labs(y="Transfer price\nin M£",x="Time left on contract\nin months")+
  scale_x_continuous(breaks=seq(0,58,12))+
  theme(axis.ticks.y= element_line(color=NA),
        axis.ticks.x=element_line(colour="#CACACA", size=0.2),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.y =element_text(angle = 0,
                                   colour="#525252",
                                   vjust = 1,
                                   hjust = 0 ),
        axis.title.x =element_text(angle = 0,
                                   colour="#525252",
                                   vjust = -1,
                                   hjust = 1 ),
        panel.grid.major.y = element_line(colour="#CACACA", size=0.2),
        panel.grid.major.x =element_blank(), #add grid
        axis.title.y=element_blank(),
        text=element_text(family="LM Roman 10"))

p.time

# plotly.p.time <- ggplotly(p.time)
# plotly.p.time

#================ 3.5 AVERyear SPENDING PER CLUB PER LEAGUE  ================

#finding mean for every league
df.viz.ave<-df.viz %>% 
  group_by(league) %>% 
  dplyr::summarise(
    sum.transfer=sum(transfer.fee))

df.viz.ave<-df.viz.ave %>% 
  mutate(number.of.clubs = ifelse(league=="Bundesliga",18,20)) %>% 
  mutate(averyear.spending=sum.transfer/number.of.clubs)
#ordering
df.viz.ave <- transform(df.viz.ave, 
                        league = reorder(league, averyear.spending))
#plotting it
total.league = ggplot(df.viz.ave, aes( x =league, y=averyear.spending, fill=league))
total.league<-total.league + geom_bar(stat="identity",alpha=1)+
  theme(axis.title.x=element_blank(),
        axis.title.y =element_text(angle = 0,
                                   colour="#525252",
                                   vjust = 1,
                                   hjust = 0 ),
        panel.grid.major.y = element_line(colour="#CACACA", size=0.2), #add grid
        axis.ticks= element_line(color=NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.artist="none",
        panel.background = element_blank(),
        text=element_text(family="LM Roman 10"))+
  ggtitle("Premier League Clubs spend far more on averyear than any other leagues' clubs\n")+
  scale_fill_manual("National leagues",
                    values=c( "#F2E7DA", "#E89090","#92A0B0", "#C0CFAE", "#525252", "#DEE3DC"))+
  labs(y="Averyear transfer spending\nper league in M£")

total.league

#================ 3.6 AVERyear SPENDING PER disc PER LEAGUE  ================


#finding mean spending per disc for different leagues
df.viz.disc.league<-df.viz %>% 
  group_by(league) %>% 
  dplyr::summarise(averyear.spending.per.disc=mean(transfer.fee))

#ordering
df.viz.disc.league <- transform(df.viz.disc.league, 
                                  league = reorder(league, averyear.spending.per.disc))
#plotting it
ave.disc = ggplot(df.viz.disc.league, aes( x =league, y=averyear.spending.per.disc, fill=league))
ave.disc<-ave.disc + geom_bar(stat="identity",alpha=1)+
  theme(axis.title.x=element_blank(),
        axis.title.y =element_text(angle = 0,
                                   colour="#525252",
                                   vjust = 1),
        panel.grid.major.y = element_line(colour="#CACACA", size=0.2), #add grid
        axis.ticks= element_line(color=NA),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.artist="none",
        text=element_text(family="LM Roman 10"))+
  ggtitle("Premier League Clubs spend far more on averyear than any other leagues' clubs\n")+
  scale_fill_manual("National leagues",
                    values=c( "#E89090","#F2E7DA","#C0CFAE","#92A0B0",  "#525252", "#DEE3DC"))+
  labs(y="Averyear transfer price\nper disc in M£") 
ave.disc

#================ 3.7 AVERyear SPENDING PER disc PER CLUB STATUS  ================


df.viz.status<-df.viz %>% 
  group_by(Status) %>% 
  dplyr::summarise(mean.transfer=mean(transfer.fee))

#Ordering
df.viz.status <- transform(df.viz.status, 
                           Status = reorder(Status, mean.transfer))

#Plotting

p.club = ggplot(df.viz.status, aes(y= mean.transfer, x = Status, fill=Status))
p.club<- p.club + geom_bar(stat = "identity")+
  theme(axis.title.x=element_blank(),
        axis.text.x =element_text(size  = 9),
        axis.title.y =element_text(angle = 0,
                                   colour="#525252",
                                   vjust = 1),
        axis.ticks= element_line(color=NA),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour="#CACACA", size=0.2), #add grid
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.text=element_blank(),
        legend.title=element_blank(),
        legend.artist="none",
        text=element_text(family="LM Roman 10"))+
  ggtitle("Top Club spend far more\non averyear than other leagues' clubs\n")+
  scale_fill_manual(values=c( "#CFF09E", "#A8DBA8", "#79BD9A", "#3B8686"))+
  labs(y="Averyear transfer price\nper disc in M£") 

p.club



##=========================================================================================
##--------------------------------- 4. Prediction Models _---------------------------------
##=========================================================================================

## Loading the final data set
transfer.data = read.csv("https://raw.githubusercontent.com/basgpol/SDS-group12/master/Exam_project/transferdata.final.csv", 
                         encoding = "UTF8", header = TRUE)

## Due to the descriptive analysis we create a new variable where year is squared
transfer.data$transferyear_sq = transfer.data$transferyear^2

## creating a vector with selected predictors for transferfee
predicting.var = c("transfer.fee", "artists", "appearances", "total.want", "total.averageValue", 
                   "total.minutes.played", "contract.left.month","transferyear",
                   "league", "Status", "searchresults","transferyear_sq")

## Removing observations where contract lenght is unknown
transfer.data = filter(transfer.data, is.na(contract.left.month) == FALSE) 


##================ 4.1 Dividing into a train and test sample  ================

## Creating a vector with the count of 70 pct. of the sample size  
train_size = floor(0.70 * nrow(transfer.data)) # creates a vector 

## setting seed to enable reproductivity 
set.seed(123)

## creating a vector with random numbers (count = tran_size)
train.indicator = sample(seq_len(nrow(transfer.data)), size = train_size)

## Splitting the data frame into a train (70 pct.) and test sample (30 pct.)
train_sample = transfer.data[train.indicator,predicting.var] # selecting observations with a train indicator
test_sample = transfer.data[-train.indicator, predicting.var] # selecting observations without a train indicator


##================ 4.2 Create evaluation function  ================
## Creating a function that calculate the RMSE
get.rmse = function(real, estimate){
  return(sqrt(mean((real - estimate)^2)))
}

##================ 4.3 Baseline model: Simple averyear from training sample  ================
estimate_M1 = mean(train_sample$transfer.fee) #calculating estimate from model 1
get.rmse(test_sample$transfer.fee, estimate_M1) # calculating RMSE from estimate on test sample 

### 4.3.1: Illustrating our prediction model 1
## Create new data frame for the illustration
train_sample.1<- train_sample %>% 
  select(transfer.fee,league) 
train_sample.1<- train_sample.1%>% 
  mutate(index=1:258)

## Creating GGplot for visualisation
p = ggplot(train_sample.1, aes(x = index , y = transfer.fee))+
  geom_segment(aes(x= index, xend=index, y=transfer.fee, yend=estimate_M1), color="red") +
  geom_point(aes(x = index, y = transfer.fee, color = "black"))   +
  geom_line(aes(x = index, y = estimate_M1), color="green", size =1)+
  theme(axis.title.x=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks= element_line(color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        text=element_text(family="Goudy Old Style"))

gg <- ggplotly(p)  #using plotly to make it interactive
gg

##================ 4.4 Ordinary least square model  ================
Model_2 = lm(transfer.fee ~ ., data = (train_sample)) # generating linear model on training data
summary(Model_2)

estimate_M2 = predict(Model_2, test_sample) # calculating estimate from model 2

get.rmse(test_sample$transfer.fee, estimate_M2) # calculating RMSE from estimate on test sample 

##================ 4.5 Lasso model  ================
## Creating matrices with all regressors beacuse the glmnet function only works with matrices
RegressorMatrix_train=model.matrix(transfer.fee~ ., train_sample)
RegressorMatrix_test=model.matrix(transfer.fee~.,test_sample)



## Training Lasso
Model_3 = glmnet(x = RegressorMatrix_train, y = train_sample$transfer.fee)
Model_3

# Calculating RSME for each lambda
lambda_values = Model_3$lambda

performance_Lasso = data.frame()

for (lambda in lambda_values){
  performance_Lasso = rbind(performance_Lasso,
                            data.frame(lambda = lambda,
                                       RMSE = get.rmse(predict(Model_3, RegressorMatrix_test, s = lambda),
                                                       test_sample$transfer.fee)))
}
performance_Lasso

##Visualization of RSME as a function of lamda
ggplot(performance_Lasso, aes(x = lambda, y = RMSE))+
  geom_point() + 
  geom_line() + 
  theme_minimal()

## Identifying lambda with the lowest RMSE
best.lambda = performance_Lasso$lambda[performance_Lasso$RMSE == min(performance_Lasso$RMSE)]

## Coefficients for best models
coef(Model_3, s = best.lambda)

## RMSE for best model
Estimate_M3=predict(Model_3, RegressorMatrix_test, s=best.lambda)
get.rmse(Estimate_M3, test_sample$transfer.fee)

##================ 4.6 Decision tree  ================
set.seed(123)
Model_4=tree(transfer.fee~.,data=train_sample, method="anova")
Model_4$frame
plot(Model_4,niform=TRUE, 
     main="Regression Tree for Transfer fee ")
text(Model_4,pretty=0,use.n=TRUE, cex=.5)


##Estimating transfer fee for test data
Estimate_M4=predict(Model_4,test_sample)
Estimate_M4
##Calculating RMSE 
get.rmse(test_sample$transfer.fee,Estimate_M4)  #6.600


## Cross validation to find the optimal number of terminal nodes
cv.Model_4 = cv.tree(Model_4, FUN = prune.tree)
plot(cv.Model_4$size, cv.Model_4$dev, type = "b") #Optimal number with cross validation is 7
best.size=cv.Model_4$size[which.min(cv.Model_4$dev)]
prune.Model_4=prune.tree(Model_4,best = best.size)
plot(prune.Model_4);text(prune.Model_4)
## Calculating estimates with pruned model
pruned.estimate=predict(prune.Model_4,test_sample)
get.rmse(pruned.estimate,test_sample$transfer.fee)
## Higher RMSE with pruned model than the original


##================ 4.7 Random Forest  ================
library(randomForest)
set.seed(1)

Model_5 = randomForest(transfer.fee ~ ., data = train_sample, importance = TRUE)
print(Model_5)

estimate_M5 = predict(Model_5, test_sample) # calculating estimate from model 5
get.rmse(test_sample$transfer.fee, estimate_M5) # calculating the RMSE on test sample

var.list = importance(Model_5, type = 1) #calculate the variables influence
varImpPlot(Model_5) # plots the variables influence
var.list


