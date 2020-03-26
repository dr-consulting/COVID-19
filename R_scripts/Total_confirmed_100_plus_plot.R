library(tidyverse)
# Pull in the data (forked JHU repo)
# Main source: https://github.com/CSSEGISandData/COVID-19
dat <- read.csv("~/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", 
                stringsAsFactors = FALSE)


# Clean up the data 
dat_country <- dat[,c(2,5:ncol(dat))] %>% 
  group_by(Country.Region) %>% 
  summarize_all(list(sum)) %>% 
  pivot_longer(cols=starts_with("X"), 
               names_to = "date") %>% 
  mutate(date = str_replace(date, "X", "")) %>%
  mutate(date = as.Date.character(date, format="%m.%d.%Y"))

# Selecting only countries with 100+ confirmed cases
countries_100_or_more <- dat_country %>% 
  group_by(Country.Region) %>%  
  summarize(max_cases = max(value, na.rm = TRUE)) %>% 
  filter(max_cases > 100)

# Stacked longitudinal data set
countries_100_or_more_long <- merge(countries_100_or_more, dat_country, by="Country.Region")
countries <- countries_100_or_more$Country.Region

# Centering time at first day with 100+ confirmed cases 
centered_countries_100_or_more_long <- data.frame()
for(c in 1:length(countries)){
  tmp_data <- countries_100_or_more_long[countries_100_or_more_long$ Country.Region == countries[c],]
  tmp_data <- tmp_data[order(tmp_data$date, decreasing = FALSE),]
  tmp_data <- tmp_data[tmp_data$value>=100,]
  tmp_data$day_centered <- 1:nrow(tmp_data)
  centered_countries_100_or_more_long <- rbind(centered_countries_100_or_more_long, 
                                               tmp_data, 
                                               stringsAsFactors=FALSE)
}

# Picking some countries to look at
countries_to_view <- c("US", "China", "Italy", "Spain", "United Kingdom", "Korea, South", "Germany", "Iran")
centered_countries_100_or_more_long$ctry_plot <- ifelse(centered_countries_100_or_more_long$Country.Region %in% countries_to_view, 
                                                        centered_countries_100_or_more_long$Country.Region, 
                                                        "Other")
# Lazily setting colors 
colors <- c("US"= "blue", "China"="red", "Italy"="green", "Spain"="yellow", "United Kingdom"="orange", 
            "Korea, South"="purple", "Germany"="black", "Iran"="lightgreen", "Other"="grey50")

# Creating graphics directory in local repo
dir.create("~/COVID-19/graphics/")

# Making and Saving my plot
png(paste0("~/COVID-19/graphics/Total_Cases_", Sys.Date(), ".png"), 
    res=900, units="in", height=6, width=9)
ggplot(data=centered_countries_100_or_more_long, 
       aes(x=day_centered, y=value, group=Country.Region, color=ctry_plot)) +
  geom_line() +
  scale_color_manual(values=colors) +
  labs(y="Total Confirmed COVID-19 Cases", 
       x="Number of Days since 100 Confirmed Cases", 
       color="Country",
       caption="Source:CSSE data maintained by Johns Hopkins University\nhttps://systems.jhu.edu/research/public-health/ncov/", 
       title = "Total Confirmed COVID-19 Cases by Country Starting on the First Day with 100+ Cases") + 
  theme_bw()
dev.off()