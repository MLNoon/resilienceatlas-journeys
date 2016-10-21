library(ggplot2)
library(dplyr)
library(cdbr)
library(foreach)
library(plotly)
library(RColorBrewer)

# Extract minimum temperatures areas where more than xx ha of Arabica is grown, 
# and summarize trends by country

# Calculate mean change in minimum temperatures in 2050 in areas where coffee Arabica is grown, by country

# Show total value of this production
countries <- "'ETH', 'UGA', 'CIV', 'TZA', 'KEN', 'BRD', 'RWA'"

# Calculate historical temperature trends in each of these countries from this data
data_prefix <- 'global_cru_ts3_23_'
data_suffix <- '_19850101_20141231_trend_decadal'
datasets <- c('tmn', 'tmp', 'tmx')
hist_temps <- foreach (dataset=datasets, .combine=rbind) %do% {
    hist_q <- paste0('SELECT name_engli, min(val), avg(val), max(val)
    FROM (select * from gadm28_adm0 WHERE iso IN (', countries, ')) AS countries
    , (
      SELECT val, geom AS the_geom_webmercator
        FROM (
          SELECT
          (ST_DumpAsPolygons(the_raster_webmercator)).*
          FROM ', data_prefix, dataset, data_suffix, ') AS temps
    ) AS temps
    WHERE ST_Intersects(countries.the_geom_webmercator, temps.the_geom_webmercator) AND val != -9999
    GROUP BY name_engli, countries.the_geom_webmercator
    ORDER BY name_engli, countries.the_geom_webmercator')
    stats <- cdb_sql_call(hist_q)
    stats <- stats$rows
    stats$var <- dataset
    #gather(stats, var, stat, min:max)
    stats
}
hist_temps$period <- '1985-2015'

# Calculate future temperature trends in each of these countries from this data
fut_q <- paste0('SELECT name_engli, 
(ST_SummaryStats(ST_SetBandNoDataValue(ST_Union(ST_Clip(the_raster_webmercator, the_geom_webmercator, -9999)), -9999), TRUE)).*
FROM tasmin_rcp85_absdiff_1986_2005_vs_2040_2059_annual, (select * from gadm28_adm0 WHERE iso IN (', countries, ')) AS countries
GROUP BY name_engli
ORDER BY name_engli')
fut_temps <- cdb_sql_call(fut_q)
fut_temps <- fut_temps$row
# Convert from change over next 35 years to change per decade
fut_temps$min <-fut_temps$min / 3.5
fut_temps$mean <-fut_temps$mean / 3.5
fut_temps$max <-fut_temps$max / 3.5
fut_temps <- select(fut_temps, -sum, -stddev, -count, avg=mean)
fut_temps$var <- 'tmn'
fut_temps$period <- '2040-2060'

temps <- full_join(hist_temps, fut_temps)

temps$name_engli[temps$name_engli == 'Democratic Republic of the Congo'] <- 'DRC'

p <- plot_ly(filter(temps, var == 'tmn'),
                 x = ~name_engli, y = ~avg, color = ~period, colors='Accent') %>%
    add_markers(error_y = ~list(type="data", symmetric=FALSE, 
                                arrayminus=avg-min, array=max-avg)) %>%
    config(showLink = F) %>%
    config(displayModeBar = F)
p <- layout(p, xaxis=list(title=""), yaxis = list(title = "Change in minimum temperature (C)"))
plotly_POST(p, filename = "african-coffee-mintemp-trends")
