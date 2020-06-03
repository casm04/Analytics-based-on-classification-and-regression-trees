library(gapminder)
library(dplyr)
gapminder %>%
  filter(year==1957)
gapminder %>%
  filter(country=='Austria')
gapminder %>%
  filter(country=='China')
gapminder %>%
  arrange(desc(pop))

gapminder %>%
  filter(year==1957) %>%
  arrange(desc(pop))

gapminder %>%
  mutate(pop=pop/1000000)

gapminder %>%
  mutate(gdp=gdpPercap*pop)

gapminder %>%
  mutate(lifeExpMonths=lifeExp*12)

gapminder %>%
  filter(year==2007) %>%
  mutate(lifeExpMonths=lifeExp*12) %>%
  arrange(desc(lifeExpMonths))
