library(gapminder)
library(tidyverse)
library(forcats)
library(ggthemes)

theme_dark <- theme_hc(bgcolor = "darkunica") +
  theme(axis.text.x = element_text(colour="white"),
        axis.text.y = element_text(colour="white"),
        plot.title = element_text(hjust = 0.5),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        legend.position = "none")


'%!in%' <- function(x,y)!('%in%'(x,y))

my_countries <- c("United States", "Brazil", "Mexico", "Canada", "Argentina", "China", "Japan", "India", "Korea, Rep.", "Iran", "Indonesia", "Germany", "United Kingdom", "France", "Italy", "Spain", "Netherlands", "Australia", "New Zealand", "Egypt")

q1 <- gapminder %>% filter(country %in% my_countries, year == 2007)%>% 
  mutate(gdp = pop * gdpPercap) %>%  select(continent, country, gdp)

q2 <- gapminder %>% filter(country %!in% my_countries, year == 2007)%>% 
  mutate(igdp = pop * gdpPercap) %>% group_by(continent) %>% summarise(gdp = sum(igdp), country = "Others")

country_colors["Others"] <- "#3f3f3f"

bind_rows(q2,q1) %>% ggplot(aes(x = continent, y = round(gdp/1000000000) , fill = fct_reorder(country,gdp, .desc = TRUE)))+ 
  geom_col(color = "black", size = 0.2) +
  guides(fill = FALSE) + 
  theme_dark+
  scale_fill_manual(values = country_colors) + 
  geom_label(aes(label = country ), color = "white",position =position_stack(vjust=1)) +
  labs(title = "Proportion of Continent GDP", x = "Continent" , y = "GDP in Trillions") + 
  scale_y_continuous(labels = scales::dollar_format()) + theme(plot.title = element_text(hjust = 0.5, size=15, face="bold"))

