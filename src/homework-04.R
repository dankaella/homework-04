######################
# Házi feladat 4     #
# Programozás I.     #
# 2016/17. II. félév #
# Nagy Daniella      #
# 2017.05.01.        #
######################

#--- II. feladat ---------------------------------------------------------------

# 1.
# adatfájl behívása
# itt volt a kódolás miatt probléma, ezért hiányoznak az argumentumok, hogy 
# rendbe beolvassa
tweets <- read.csv2(file = "data/clinton_trump_tweets.csv")
head(tweets)

# 2.
# ellenőrzöm, hogy faktorváltozó-e a tweets$handle
class(tweets$handle)
# Hány tweet származik Hillary Clintontól és hány Donald Trumptól?
table(tweets$handle)
# ggplot2 telepítése
if (!("ggplot2" %in% installed.packages())) {
  install.packages("ggplot2", dependencies = TRUE)
}
# package behívása
library(ggplot2)
# long formátumra van szükségünk, ehhez package  telepítése és behívása
if (!("reshape2" %in% installed.packages())) {
  install.packages("reshape2", dependencies = TRUE)
}
library(reshape2)
# long formátumba átalakítom a data framet - minden változót meghagyok
melt(tweets)
# gyakoriságot ábrázoló oszlopdiagram készítése a tweetekről
ggplot(data = tweets, aes(x = as.factor(handle))) + 
  geom_bar(aes(fill=as.factor(handle))) +
  ggtitle("Candidate Tweets") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylab("Tweet frequency") +
  scale_fill_manual(name = "Candidate",
                    labels = c("Hillary Clinton","Donald Trump"),
                    values = c("blue", "red")) 
ggsave("fig/tweet1.png", width = 6, height = 4, dpi = 100)

# 3.
# Milyen nyelven írta a tweeteket a két jelölt külön-külön?
# - Kereszttáblaként íratom ki, hogy melyik jelölt hány tweetet írt az egyes
# - nyelveken
table(tweets$handle, tweets$lang)
# Dán nyelvű tweetek - itt link van, illetve hashtag - lehet angol
tweets$text[tweets$lang == "da"]
# átírom ezeknél a nyelvet angolra
tweets$lang[tweets$lang == "da"] <- "en"
# Trump spanyol tweetjei (3 db): angol szavak vannak benne, lehet inkább angol
tweets$text[tweets$lang == "es" & 
              tweets$handle == "realDonaldTrump"]
# Átírom angolra:
tweets$lang[tweets$lang == "es" & 
              tweets$handle == "realDonaldTrump"] <- "en"
# Észt tweet - hashtagek angolul vannak
tweets$text[tweets$lang == "et"]
# Átírom angolra:
tweets$lang[tweets$lang == "et"] <- "en"
# Finn tweet
tweets$text[tweets$lang == "fi"]
# Átírom angolra (szerintem nem volt benne egyértelműen más nyelvű, mint angol)
tweets$lang[tweets$lang == "fi"] <- "en"
# Finn tweet
tweets$text[tweets$lang == "fi"]
# Átírom angolra (szerintem nem volt benne egyértelműen más nyelvű, mint angol)
tweets$lang[tweets$lang == "fi"] <- "en"
# Francia tweetek
tweets$text[tweets$lang == "fr"]
# Átírom angolra (szerintem nem volt benne francia)
tweets$lang[tweets$lang == "fr"] <- "en"
# Tagalog tweetek
tweets$text[tweets$lang == "tl"]
# Átírom angolra (szerintem angol)
tweets$lang[tweets$lang == "tl"] <- "en"
# Ábrázolom a használt nyelvek gyakoriságát és mentem is:
# (az oszlopszélességet nem tudtam sajnos beállítani, hogy egyforma legyen, és
# ne töltse ki a rendelkezésre álló helyet Trumpnál)
ggplot(data = tweets, aes(x = as.factor(handle), 
                          y = ..count..)) + 
  geom_bar(aes(fill = as.factor(lang)), 
           position = position_dodge()) +
  ggtitle("Language Tweets") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylab("Tweet frequency") +
  scale_x_discrete(labels = c("Hillary Clinton", "Donald Trump")) +
  scale_fill_manual(name = "Language",
                    labels = c("English","Spanish"),
                    values = c("darkgrey", "cornflowerblue"))
ggsave("fig/tweet2.png", width = 6, height = 4, dpi = 100)

# 4.
# Függvények behívásának helye:
source("src/homework-04-functions.R")
# Lekérem függvénnyel Hillary Clinton első 10, valamint Donald Trump 
# első 15 legtöbbet retweetelt és kedvelt tweetjét.
tweets_candidate("Hillary Clinton", 10)
tweets_candidate("Donald Trump", 15)

#--- III. feladat --------------------------------------------------------------

# 1.
# fivethirtyeight package installálása (ha még nem volt)
if (!("fivethirtyeight" %in% installed.packages())) {
    install.packages("fivethirtyeight")
  }
library("fivethirtyeight")
# hiphop_cand_lyrics dataset megkeresése és beolvasása
data(package = "fivethirtyeight")
data(hiphop_cand_lyrics)
head(hiphop_cand_lyrics)
# az elemzés 1. ábrájának elkészítése
ggplot(data = hiphop_cand_lyrics, aes(x = as.factor(album_release_date),
                               fill = as.factor(candidate))) +
  geom_dotplot(binwidth = 0.5, method = "dotdensity", 
               stackgroups = TRUE, binpositions = "all", colour = NA) +
  ggtitle("Every mention of 2016 primary candidates in hip-hop songs") +
  scale_fill_manual(breaks = c("Donald Trump", "Hillary Clinton",
                               "Jeb Bush", "Chris Christie",
                               "Mike Huckabee", "Bernie Sanders",
                               "Ben Carson", "Ted Cruz"),
                    labels = c("Trump", "Clinton",
                               "Bush", "Christie",
                               "Huckabee", "Sanders",
                               "Carson", "Cruz"),
                    values = c("#A6D854","#66C2A5",
                               "#FFD92F","#FEC075",
                               "#94D7FA","#FF8974",
                               "#FCCDE5","#E78AC3")) +
theme(legend.title = element_blank(),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.background = element_blank(),
      plot.title = element_text(hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(colour= "darkgrey", size = 1),
      panel.grid.minor = element_blank()) +
scale_y_continuous(limits = c(0, 45),
                   breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40)) + 
scale_x_discrete(breaks = c(1990, 1995, 2000, 2005, 2010, 2015),
                 labels = c("1990", "'95", "2000", "'05", "'10", "'15")) +
guides(fill = guide_legend(nrow = 1,byrow = TRUE))
ggsave("fig/hiphop1.png", width = 6.8, height = 6.8, dpi = 100)
# az elemzés 2. ábrájának elkészítése
# ehhez egy új oszlopot hozunk létre a kis ábrák sorrendjéhez
hiphop_cand_lyrics$sentiment_f = factor(
  hiphop_cand_lyrics$sentiment,
  levels=c("positive","negative","neutral"))
# maga az ábra
ggplot(data = hiphop_cand_lyrics, aes(x = as.factor(album_release_date),
                                      fill = as.factor(candidate))) + 
    geom_dotplot(binwidth = 0.5, method = "dotdensity", 
               stackgroups = TRUE, 
               binpositions = "all", colour = NA) +
    ggtitle("Candidate mentions, by sentiment") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.direction = "horizontal",
          legend.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_line(colour= "darkgrey", size = 1),
          panel.grid.minor = element_blank()) + 
    scale_fill_manual(breaks = c("Donald Trump", "Hillary Clinton",
                               "Jeb Bush", "Chris Christie",
                               "Mike Huckabee", "Bernie Sanders",
                               "Ben Carson", "Ted Cruz"),
                    labels = c("Trump", "Clinton",
                               "Bush", "Christie",
                               "Huckabee", "Sanders",
                               "Carson", "Cruz"),
                    values = c("#A6D854","#66C2A5",
                               "#FFD92F","#FEC075",
                               "#94D7FA","#FF8974",
                               "#FCCDE5","#E78AC3")) +
    scale_y_continuous(limits = c(0, 20),
                       breaks=seq(0, 20, 5)) +
    scale_x_discrete(breaks = c(1990, 1995, 2000, 2005, 2010, 2015),
                   labels = c("1990", "'95", "2000", "'05", "'10", "'15")) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +  
    facet_grid(~ sentiment_f, scales = "fixed", space = "fixed")
ggsave("fig/hiphop2.png", width = 12, height = 3, dpi = 100)

# 2.
# általam értelmesnek tartott ábra
View(hiphop_cand_lyrics)
# melyik jelölt milyen téma kapcsán mennyit volt említve (NA is bennmarad): 
ggplot(data = hiphop_cand_lyrics , aes(x = as.factor(candidate),
                                       y = ..count..,
                                       fill = as.factor(theme))) + 
  geom_bar(aes(fill = as.factor(theme))) +
  ggtitle("Candidate mentions, by themes") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylab("Frequency of mentions") +
ggsave("fig/hiphop3.png", width = 12, height = 6, dpi = 100)