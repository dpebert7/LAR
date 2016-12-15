# Ebert
# October 31 2016

# Determine if a point is in or out of LA county
library(ggmap)
library(rgeos)
library(sp)
library(rgdal)

ca.map <- readOGR("Zillow/ZillowNeighborhoods-CA.shp", layer="ZillowNeighborhoods-CA")
plot(ca.map) # shows distribution of neighborhoods in LA County

la.map = ca.map[ca.map$COUNTY == "Los Angeles",]
orange.map = ca.map[ca.map$COUNTY == "Orange",]
plot(la.map)
plot(orange.map)
combined.map = ca.map[ca.map$COUNTY == "Los Angeles" | ca.map$COUNTY == "Orange",]
plot(combined.map)


table(ca.map[[2]]) # Shows cities in ca.map with count of neighborhoods
# 163 neighborhoods in LA county (county, right?)
# 90 neighborhoods in Orange county (county, right?)

sum(table(ca.map[[4]], ca.map[[2]]=="Los Angeles")[,2]) # Shows neighborhoods in ca.map


hollywood <- ca.map[ca.map$CITY == "Los Angeles"  & ca.map$NAME == "Hollywood", ]


df <- data.frame(Latitude =c(34.101640, 34.134286,34.090819),
                 Longitude = c(-118.325692,-118.321584,-118.360900),
                 names = c("Hollywood Blvd", "Hollywood Sign", "West Hollywood"))
coordinates(df) <- ~ Latitude + Longitude


plot(hollywood)
points(df$Latitude ~ df$Longitude, col = "red", cex = 1)






ca.map <- readOGR("Zillow/ZillowNeighborhoods-CA.shp", layer="ZillowNeighborhoods-CA")
plot(ca.map) # shows distribution of neighborhoods in LA County


hollywood <- ca.map[ca.map$CITY == "Los Angeles"  & ca.map$NAME == "Hollywood", ]

# Don't use df as name, it is an R function
# Better to set longitudes as the first column and latitudes as the second
dat <- data.frame(Latitude =c(34.101640, 34.134286,34.090819),
                  Longitude = c(-118.325692,-118.321584,-118.360900),
                  names = c("Hollywood Blvd", "Hollywood Sign", "West Hollywood"))
# Assignment modified according
coordinates(dat) <- ~ Longitude + Latitude

# Set the projection of the SpatialPointsDataFrame using the projection of the shapefile
proj4string(dat) <- proj4string(hollywood)

over(dat, hollywood)
#  STATE COUNTY    CITY                NAME REGIONID
#1    WA   King Seattle Industrial District   271892
#2  <NA>   <NA>    <NA>                <NA>       NA
#3  <NA>   <NA>    <NA>                <NA>       NA

over(hollywood, dat)
#           names
#122 Safeco Field


dat$coordinates[over(hollywood,dat)]



