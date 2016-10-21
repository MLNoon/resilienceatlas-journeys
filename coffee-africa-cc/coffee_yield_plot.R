library(plotly)
library(dplyr)

d <- read.csv('FAOSTAT_data_10-20-2016.csv', encoding="UTF-8", stringsAsFactors=FALSE)

d$Country[d$Country == 'United Republic of Tanzania'] <- "Tanzania"

p <- filter(d, Element == 'Yield') %>%
    plot_ly(x=~Year, y=~Value / 1000)  %>%
    add_lines(color = ~ Country)
p <- layout(p, yaxis = list(title = "Yield (tons / ha)"),
       xaxis = list(title = "Year")) %>%
    config(showLink = F) %>%
    config(displayModeBar = F)
plotly_POST(p, filename = "african-coffee-yield")

p <- filter(d, Element == 'Production') %>%
    plot_ly(x=~Year, y=~Value)  %>%
    add_lines(color = ~ Country)
p <- layout(p, yaxis = list(title = "Production (tons)"),
       xaxis = list(title = "Year")) %>%
    config(showLink = F) %>%
    config(displayModeBar = F)
plotly_POST(p, filename = "african-coffee-production")
