library(gapminder)
library(dplyr)
library(ggplot2)

gapminder_2007 <- arrange(gapminder, desc(pop),by_group=lifeExp) %>% 
  filter(year == 2007)

top_pop <- gapminder_2007 %>% top_n(5, pop)
top_pop[,1]

p <- ggplot(gapminder_2007, aes(x= gdpPercap, y = lifeExp, color=continent, size=pop)) + 
    geom_point() + geom_text(data = top_pop,color="black", size=4.0,aes(label=country)) + 
  scale_x_log10() 
p

