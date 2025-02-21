##***************************
## Assignment 1 - BOLD
##
## Nikki Smith
##
## 2024-10-04
##
##***************************

## _ Packages used -------
library(stats)
library(tidyverse)
library(viridis)

# map visualization
library(mapview)
library(sf)
library(countrycode)

#Histogram visualization
library(ggplot2)

# read in the BOLD dataframe
df_bold <- read_tsv(file = "http://www.boldsystems.org/index.php/API_Public/combined?taxon=Corvidae&format=tsv")

# Remove unknown latitude and longitude values
df_bold <- df_bold[!is.na(df_bold$lat & df_bold$lon),]
df_bold <- df_bold[!is.na(df_bold$country),]

# Map country names to continents using countrycode
df_bold$continent <- countrycode(sourcevar = df_bold$country, origin = "country.name", destination = "continent")

# Prepare the latitude and longitude coordinates in df_bold to be used by mapview
corvid_sites <- data.frame( longitude = df_bold$lon, latitude = df_bold$lat )

# Convert locations to an sf type. crs is a lat/long coordinate reference system
corvid_sites <- sf::st_as_sf(corvid_sites, coords = c("longitude","latitude"), crs = 4326) 

corvid_map <- mapview(corvid_sites, label=df_bold$species_name)

# Make a new dataframe that tallies the findings on each continent
df_continents <- data.frame(table(df_bold$continent))
df_continents <- rename(df_continents, "Continent"=Var1, "Frequency"=Freq)

# Histogram of corvid species by continent
corvid_histogram <- ggplot(df_continents, aes(x=Continent, y=Frequency, fill=Continent)) + geom_bar(stat="identity") + coord_flip() + ggtitle("Frequency of Corvidae Sightings by Continent") +  scale_fill_viridis_d()

# Is urbanization correlated with sample discovery?
# load in a  file from https://population.un.org/wup/Download/Files/WUP2018-F02-Proportion_Urban.xls
# Downloaded .xls file and removed header elements and unused column years in excel. Saved the result as a .csv file. Couldn't figure out how to do this in R :(

df_urbanization <- read.csv(file = ".../data/Proportion_Urban.csv")
df_urbanization <- rename(df_urbanization, "Country"=Region..subregion..country.or.area)
df_urbanization <- rename(df_urbanization, "Percent_Urban"=X2020)

# Made a new temporary dataframe so we can compare "Country" between the two dataframes to get urbanization values
# This dataframe is simply the number of records found in each country
new_df <- data.frame(table(df_bold$country))
new_df <- rename(new_df, "Country"=Var1)

# Merge these two dataframes to add in an urbanization column. Removed any NaN values in the percent urban column
urbanization_df <- merge(new_df, df_urbanization, by = "Country", all.x = TRUE)
urbanization_df <- urbanization_df[!is.na(urbanization_df$Percent_Urban),]

# Ran linear regression and made a scatterplot with the regression line added with abline
model <- lm(urbanization_df$Freq ~urbanization_df$Percent_Urban,data = urbanization_df)

corvid_plot <- plot(urbanization_df$Percent_Urban, urbanization_df$Freq, main = "Urbanization vs Record Frequency", abline(model), xlab = "Percent Urban", ylab = "Frequency")

# Model summary shows a low correlation between urbanization and record frequency (R2= 0.08033, p= 0.08915 > 0.05)
summary(model)
