pkgs <- c("tidyverse", "dplyr", "reshape", "ggplot2", "ggrepel", "jcolors")
installed_packages <- pkgs %in%
  rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}
invisible(lapply(pkgs, library, character.only = TRUE))

#the countries we want to look at
countries <- c('US','China','Italy','Spain','Iran','France','Korea, South','Japan','United Kingdom')


#get data and clean it up
d <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")) %>%
  select(-Lat, -Long) %>% melt(id = c("Province.State", "Country.Region")) %>%
  arrange(Country.Region, Province.State) %>%
  dplyr::rename(
    date = variable,
    country = Country.Region,
    state = Province.State
  ) %>%
  mutate(
    date = substring(date, 2),
    date = as.Date(date, format = "%m.%d.%y")
  )

recent = max(d$date)

#aggregate for the countries with dis-aggregated data
#and get days since 10th death
agg <- d %>% group_by(country, date) %>%
  summarize(deaths = sum(value)) %>%
  mutate(d10 = ifelse(deaths >= 10, 1, 0)) %>%
  filter(d10 == 1) %>% ungroup() %>%
  group_by(country) %>%
  mutate(min_date = min(date),
         days_10 = as.numeric(date - min_date))

#for labeling countries at the end
plot <- agg %>%
  filter(country %in% countries) %>%
  # filter(days_10 <= 35) %>% 
  group_by(country) %>%
  mutate(last = ifelse(row_number() == n(), 1, 0))

#make the graphic
plot %>% 
  ggplot(aes(x=days_10, y=deaths, group = country, color = country, label = country)) +
  geom_point(size=4, shape=16) +
  geom_line(size = 2) +
  theme_minimal() +
  labs(x = "Days since 10th death",
       y = "Total deaths",
       caption = "Figure: @benbbaldwin | Data: @JHU",
       title = "COVID-19 Tracker: Deaths Since 10th Death") +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x=element_text(hjust=0.5)
  )   +
  scale_color_jcolors(palette="pal8") +
  geom_text_repel(
    data = filter(plot, last == 1),
    color = "black",
    nudge_x = -2,
    nudge_y = 500,
    size = 5,
  ) +
  annotate("text",x=5, y= 8000, label = paste0("Most recent update:\n",month(recent),"-",day(recent)), color="red", size=5)


ggsave("deaths_since_10.png", dpi=800)

