# Packages & Initialize Setup ---------------------------------------------
proj_name <- "covid19"

pkgs <-
  c("tidyverse",
    "devtools",
    "ggplot2",
    "ggfortify",
    "ggmap", # mapping
    "gganimate",
    "magick",
    "RSocrata",
    "jsonlite",
    "sf",
    "reactable",
    "zoo", # moving averages
    "hrbrthemes", # themes for graphs
    "socviz", # %nin%
    "geofacet", # maps
    "usmap" # lat and long
    )

installed_packages <- pkgs %in%
  rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}
invisible(lapply(pkgs, library, character.only = TRUE))


# DB and various API keys ---------------------------------------------------

source("../../InitR/con.R")
cdc_app_token <- "kKUiVCQTC0UGduZxOSGGEhZhU"
cdc_secret_token <- "njWMqH5O2wxClDZLxsMzaP1fbOvaOl0ppFyN"

# Get Data ----------------------------------------------------------------

# Local
today <- Sys.Date()
today <- format(Sys.Date(),
                format("%m/%d/%y"))

states_lockdown <-
  read_csv("state lockdowns.csv")

# CSSEGIS
CSSEGIS_confirmed <-
  read_csv(
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  )
US_confirmed <- CSSEGISandData %>%
  filter(`Country/Region` == "US")

CSSEGIS_deaths <-
  read_csv(
    "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  )
US_deaths <- CSSEGISandData %>% 
  filter(`Country/Region` == "US")

# CDC
cdc_weekly_deaths_201418 <-
  read.socrata("https://data.cdc.gov/resource/3yf8-kanr.csv", app_token = cdc_app_token)

write_ %>% 

cdc_weekly_deaths_201920 <-
  read.socrata("https://data.cdc.gov/resource/muzy-jte6.csv", app_token = cdc_app_token)

# Covid Tracking Project
covid_tracking <- "https://covidtracking.com/api/v1/"

us_daily <-
  read_csv(paste0(covid_tracking,"us/daily.csv"))

states_daily <-
  read_csv(paste0(covid_tracking,"states/daily.csv"))

# Philly Open Data
phl_carto <- "https://phl.carto.com/api/v2/sql?q="
# phl_categories <- c("cases", "deaths", "hospitalizations")
# phl_subcategories <- c("date", "zip", "age", "sex", "race", "outcome")
# phl_endpoints <-
#   data.frame(matrix(
#     ncol = length(phl_categories),
#     nrow = length(phl_subcategories)
#   ))
# colnames(phl_endpoints) <- phl_categories
# rownames(phl_endpoints) <- phl_subcategories
# 
# fx.phl_endpoints <- function(x, y) {
#   phl_endpoints[x, y] <- paste("covid", x, "by", y, sep = "_")
# }
# 
# phl_endpoints <- mapply(fx.phl_endpoints, phl_categories, phl_subcategories, SIMPLIFY = FALSE)

phl_endpoints <- c("covid_cases_by_outcome",
                   "covid_cases_by_date",
                   "covid_cases_by_zip",
                   "covid_cases_by_age",
                   "covid_cases_by_sex",
                   "covid_cases_by_race",
                   "covid_hospitalizations_by_date",
                   "covid_hospitalizations_by_zip",
                   "covid_hospitalizations_by_age",
                   "covid_hospitalizations_by_sex",
                   "covid_hospitalizations_by_race",
                   "covid_deaths_by_date",
                   "covid_deaths_by_zip",
                   "covid_deaths_by_age",
                   "covid_deaths_by_race")


fx.phl_endpoints <- function(x) {
  phl_query <- paste(phl_carto, "SELECT", "*", "FROM", x, sep = "%20")
  query_rds <- paste0("data/","phl_", x, ".rds")
  df <- 
    phl_query %>%
      url() %>%
      fromJSON(
        simplifyVector = TRUE,
        simplifyDataFrame = TRUE,
        simplifyMatrix = TRUE,
        flatten = TRUE
      )
  df <- df$rows
  saveRDS(df, query_rds)
}

lapply(phl_endpoints, fx.phl_endpoints)


# Clean data --------------------------------------------------------------

cdc_recent_deaths <-
  cdc_weekly_deaths_201920 %>% filter(mmwryear == 2020, mmwrweek == 25)

cdc_recent_deaths <-
  cdc_weekly_deaths_201920 %>% filter(`MMWR Year` == 2020, `MMWR Week` == 25)

# Old code ----------------------------------------------------------------

## x.colnames <- seq(as.Date("3/1/20", format = "%m/%d/%Y"), today, by = 1)
# sum(US_confirmed$`4/5/20`) ## today's date
# 
# f.US_data <- "cases.csv"
# US_data <- read_csv(file = f.US_data) ## from google sheet
# 
# if (US_data$Date[1] == "3/1/2020") {
#   US_data$Date <- as.Date(US_data$Date, format = "%m/%d/%y")
#   write.csv(US_data, file = f.US_data, row.names = FALSE)
# }
# 
# US_data[,4] <- NULL
# view(US_data)
# US_data_rev <- US_data %>% filter(Date >= Sys.Date()-14 & Date <= Sys.Date()+7)
# view(US_data_rev)

# Chart -------------------------------------------------------------------

g1 <- US_data_rev %>% ggplot(
  aes(x = Date, y = Confirmed, color = Type)) +
  geom_point(aes(group = seq_along(Date))) + ## include line on chart
  geom_line(size = 1.5) + ## line width
  ylim(0, 500000) + ## y axis limit
  xlim(Sys.Date()-14, Sys.Date()+5) + ## x axis limit
  labs(title = "Confirmed vs Estimate 33% inc/day U.S. COVID-19 cases",
     caption = "Source: github.com/CSSEGISandData",
     y = "Confirmed Cases",
     color=NULL) +  # title and caption
  scale_color_manual(labels = c("Confirmed", "Estimate 1.30", "Estimate 1.33"), 
                   values = c("Confirmed"="#e06666", "Estimate 1.30"="#6d9eeb", "Estimate 1.33"="#93c47d")) +  # line color
  theme(axis.text.x = element_text(angle = 45, vjust=0.5, size = 12), # rotate x axis text
      axis.text.y = element_text(size = 12),
      panel.grid.minor = element_blank()) # turn off minor grid

print(g1)
## animate(g1)
g1 +
  transition_reveal(Date) +
  view_follow()

# Chart -------------------------------------------------------------------

g1 <- US_confirmed %>% ggplot(
  aes(x = Date, y = Confirmed, color = Type)) +
  geom_line(aes(y = Confirmed,
                col = Confirmed)) +
  geom_line(aes(y = Estimate,
                col = `Estimate (30% increase/day)`)) + 
  labs(title = "Confirmed vs Estimate COVID-19 cases",
       caption = "Source: github.com/CSSEGISandData",
       y = "Confirmed Cases",
       color=NULL) +  # title and caption
  scale_color_manual(labels = c("Confirmed", "Estimate (30% increase/day)"), 
                     values = c("Confirmed"="#00ba38", "Estimate (30% increase/day)"="#f8766d")) +  # line color
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid
