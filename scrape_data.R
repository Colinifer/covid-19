###############
## Packages
###############

pkgs <- c("tidyverse", "ggplot2", "ggfortify", "gganimate", "magick")
installed_packages <- pkgs %in%
  rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(pkgs[!installed_packages])
}
invisible(lapply(pkgs, library, character.only = TRUE))

###############
## Get Data
###############

confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

US_confirmed <- confirmed %>% filter(`Country/Region` == "US")

today <- Sys.Date()
today <- format(Sys.Date(), format("%m/%d/%y"))

## x.colnames <- seq(as.Date("3/1/20", format = "%m/%d/%Y"), today, by = 1)

sum(US_confirmed$`4/5/20`) ## today's date

f.US_data <- "cases.csv"
US_data <- read_csv(file = f.US_data) ## from google sheet

if (US_data$Date[1] == "3/1/2020") {
  US_data$Date <- as.Date(US_data$Date, format = "%m/%d/%y")
  write.csv(US_data, file = f.US_data, row.names = FALSE)
}

US_data[,4] <- NULL
view(US_data)
US_data_rev <- US_data %>% filter(Date >= Sys.Date()-14 & Date <= Sys.Date()+7)
view(US_data_rev)

###############
## Chart
###############

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

###############

###############

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

library(ggplot2)
library(ggfortify)
theme_set(theme_bw())
