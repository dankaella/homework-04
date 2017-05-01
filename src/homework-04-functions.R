## tweetek függvény ############################################################
# Kiírja a jelolt argumentumba megadott jelölt esetében a szam argumentumba
# megadott darabszámú legtöbbet retweetelt és kedvelt tweetet.
tweets_jelolt <- function(jelolt = "Donald Trump", szam = 5) {
  # létrehozom a rangsoroláshoz azt a változót, 
  # ami a sorrendet majd meghatározza
  tweets$pop <- tweets$retweet_count + tweets$
  # első feltétel: ha Clintonról van szó, de több elemet akarunk lekérdezni,
  # mint amennyi tweetje van, akkor kiírja, hogy mennyi a maximális tweet-szám.
  if ((jelolt == "Hillary Clinton" & 
            szam > nrow(subset(tweets, 
                                 tweets$handle == "HillaryClinton")))){
    clinton <- nrow(subset(tweets, 
                         tweets$handle == "HillaryClinton"))
    print(paste0("A ", 
               jelolt, 
               "-hoz tartozó tweetek maximális száma: ",
               clinton))
  # második feltétel: ha Trumpról van szó, de több elemet akarunk lekérdezni,
  # mint amennyi tweetje van, akkor kiírja, hogy mennyi a maximális tweet-szám.
  } else if ((jelolt == "Donald Trump" & 
            szam > nrow(subset(tweets, 
                                 tweets$handle == "realDonaldTrump")))){
    trump <- nrow(subset(tweets, 
                       tweets$handle == "realDonaldTrump"))
    print(paste0("A ", 
               jelolt, 
               "-hoz tartozó tweetek maximális száma: ",
               trump))
  # harmadik feltétel: ha Clintonra vagyunk kiváncsiak, akkor kiírja, hogy
  # kinek és hány tweetjét fogjuk látni, majd ki is írja
  } else if (jelolt == "Hillary Clinton") {
    tweets_handle <- subset(tweets, tweets$handle == "HillaryClinton")
    print(paste0(jelolt, " ", 
                 szam, 
                 " legtobbet retweetelt es kedvelt tweetjet olvashatod!"))
    print(head(
      tweets_handle$text[order(tweets_handle$pop, decreasing = T)],
      szam))
  # negyedik feltétel: ha Trumpra vagyunk kiváncsiak, akkor kiírja, hogy
  # kinek és hány tweetjét fogjuk látni, majd ki is írja
  } else if (jelolt == "Donald Trump") {
    tweets_handle <- subset(tweets,tweets$handle == "realDonaldTrump")
    print(paste0(jelolt, " ", 
           szam, 
           " legtobbet retweetelt es kedvelt tweetjet olvashatod!"))
    print(head(
      tweets_handle$text[order(tweets_handle$pop, decreasing = T)],
      szam))
  # ötödik feltétel: ha nincs baj az elemszámmal, és nem is a két jelölt nevét
  # adtam ki, akkor kiírja, hogy rossz nevet adtam meg
  } else {
    print("Nem jó nevet adtál meg!")
  }
}
