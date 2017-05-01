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
install.packages("ggplot2", dependencies = TRUE)
# package behívása
library(ggplot2)
# long formátumra van szükségünk, ehhez package  telepítése és behívása
install.packages("reshape2", dependencies = TRUE)
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