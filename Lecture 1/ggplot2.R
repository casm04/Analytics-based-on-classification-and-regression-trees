library(gapminder)
library(dplyr)
library(ggplot2)

gapminder_2007 <- gapminder %>% 
  filter(year == 2007)
gapminder_2007
ggplot(gapminder_2007, aes(x = lifeExp)) +
  geom_histogram(binwidth = 2)

#by continent
by_continent <- gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(meanLifeExp = mean(lifeExp))
ggplot(by_continent, aes(x = continent, y = meanLifeExp, fill = continent)) +
  geom_col()