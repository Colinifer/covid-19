#if you want to just use the version on github
#source("https://raw.githubusercontent.com/guga31bb/covid19/master/clean_data.R")

source("/guga_graphs/clean_data.R")
subDir <- "/guga_graphs/figures"
dir.create(file.path(getwd(), subDir), showWarnings = FALSE)

# argument is how many days to keep after 10th death
# subtracting max and min dates to get automatic updates
all <- get_data(as.integer(max(d1$date) - min(d1$date))) 

#keep the top X states/counties/countries with most deaths to show in plot. and also south korea for the country one
u <- all %>% filter(type == 'us') %>%
  keep_top("deaths", 10, c("Washington")) 
w <- all %>% filter(type == 'world') %>%
  keep_top("deaths", 10, c("Korea, South"))
c <- all %>% filter(type == 'county') %>%
  keep_top("deaths", 7, c("Seattle-Tacoma-Bellevue, WA")) 
c$state <- gsub( ",.*$", "", c$state )


#day of last nyt update
all %>% filter(type =='us') %>% select(date) %>% arrange(date) %>% tail(1)
#day of last jhu update
all %>% filter(type =='world') %>% select(date) %>% arrange(date) %>% tail(1)


## make figures: normal scale
u %>% make_figure('nyt')
ggsave("guga_graphs/figures/deaths_us_states.png", dpi=700, width = 16, height = 8)

c %>% make_figure('nyt')
ggsave("guga_graphs/figures/deaths_us_counties.png", dpi=700, width = 16, height = 8)

w %>% make_figure('jhu')
ggsave("guga_graphs/figures/deaths_world.png", dpi=700, width = 16, height = 8)

## make figures: log scale
u %>% make_figure_log('nyt', -4)
ggsave("guga_graphs/figures/deaths_log_us_states.png", dpi=700, width = 16, height = 8)

c %>% make_figure_log('nyt', -4)
ggsave("guga_graphs/figures/deaths_log_us_counties.png", dpi=700, width = 16, height = 8)

w %>% make_figure_log('jhu', -4)
ggsave("guga_graphs/figures/deaths_log_world.png", dpi=700, width = 16, height = 8)