library(gapminder)
library(dplyr)
gapminder %>%
  filter(year == 2007) %>%
  summarize(meanLifeExp = mean(lifeExp))

# multiple columns
gapminder %>%
  filter(year == 2007) %>%
  summarize(meanLifeExp = mean(lifeExp), totalPop= sum(as.numeric(pop)))

gapminder %>%
  filter(year == 1957) %>%
  summarize(meanLifeExp = mean(lifeExp), maxGdp= max(gdpPercap))

# multiple columns
gapminder %>%
  group_by(year) %>%
  summarize(meanLifeExp = mean(lifeExp), totalPop= sum(as.numeric(pop)))

gapminder %>%
  group_by(year, continent) %>%
  summarize(meanLifeExp = mean(lifeExp), totalPop= sum(as.numeric(pop)))

#Ejercicios
# Find median life expectancy and maximum GDP per capita in each year
# Find median life expectancy and maximum GDP per capita in each continent in 1957
# Find median life expectancy and maximum GDP per
# capita in each continent/year combination
