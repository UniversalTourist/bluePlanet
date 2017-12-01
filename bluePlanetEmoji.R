# emojis
library(tidytext)
library(purrr)
library(fuzzyjoin)
library(rms)
library(gridSVG)
library(webshot)
library(gganimate)
library(ggimage)
library(magick)
library(knitr)

## load in the emoji dictionary
dico <- readr::read_csv2("https://raw.githubusercontent.com/today-is-a-good-day/emojis/master/emDict.csv")

# get emojis
emojis <- regex_left_join(tidyTweetsBlue, dico, by = c(text = "Native")) %>%
  group_by(Native) %>%
  filter(!is.na(Native)) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(15) %>%
  mutate(num = 1:15)

newEmojis <- data.frame(numberofEmojis = c(516,437,313,311,280,252,242,238,206,181,170,137,132,116,107),
                        orderofEmojis = c(1:15),
                        image = c("~/Desktop/bluePlanet/emojis/516.png",
                                  "~/Desktop/bluePlanet/emojis/437.png",
                                  "~/Desktop/bluePlanet/emojis/313.png",
                                  "~/Desktop/bluePlanet/emojis/311.png",
                                  "~/Desktop/bluePlanet/emojis/280.png",
                                  "~/Desktop/bluePlanet/emojis/252.png",
                                  "~/Desktop/bluePlanet/emojis/242.png",
                                  "~/Desktop/bluePlanet/emojis/238.png",
                                  "~/Desktop/bluePlanet/emojis/206.png",
                                  "~/Desktop/bluePlanet/emojis/181.png",
                                  "~/Desktop/bluePlanet/emojis/170.png",
                                  "~/Desktop/bluePlanet/emojis/137.png",
                                  "~/Desktop/bluePlanet/emojis/132.png",
                                  "~/Desktop/bluePlanet/emojis/116.png",
                                  "~/Desktop/bluePlanet/emojis/107.png"
                        ))


p <- ggplot(newEmojis, aes(y = numberofEmojis, x = orderofEmojis)) + 
  geom_image(aes(image = image), size = .06) +
  ylab("Number of Emojis") + xlab("Order of Emojis") + 
  theme_minimal()
p +  scale_x_continuous(breaks = c(2,4,6,8,10,12,14))








