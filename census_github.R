#################
# Census tracts in R
# Citation: Goldstein ND. Working with census geographies in R. Epidemiology. 2015 Mar;26(2):e22-3.
# 10/14/14 -- Neal Goldstein
#################

#maptools provides the spatial recognition
library(maptools)
library(RColorBrewer)

##############################
# Function: tractLookup
# Use: returns the corresponding census tract(s) for the geographic coordinates
# Argument(s):
#    1) data frame of latitudes and longitudes (variable names must be "latitude" and "longitude")
#    2) TIGER/Line Shapefiles
#    3) shapefile year (2007 - present)
#    4) verbose boolean to print each coordinate during the mapping (optional, default=FALSE)
# Return: data frame containing census tract identifier (GEOID), state, county, census tract number, census tract name, internal point latitude and longitude
##############################
tractLookup <- function(coordinates, shapefile, year, verbose=FALSE)
{
  #read the shapefile
  cat("\nReading shapefile from path:",shapefile,"\n")
  census_tracts <- readShapePoly(shapefile)
  
  #resolve to census tracts
  cat("\nResolving census tracts, n =",nrow(coordinates),"\n")
  
  #create empty data frame to store results
  results <- NULL
  
  for (i in 1:nrow(coordinates))
  {
    if (verbose==TRUE) {
      cat("\nResolving census tract for coordinate #", i, "of", nrow(coordinates), ": Latitude =", coordinates$latitude[i], "Longitude =", coordinates$longitude[i],"\n")
    }
    
    if (!is.na(coordinates$longitude[i]) | !is.na(coordinates$latitude[i])) {
      
      #create a spatial point object from current coordinate
      spatial_pt <- SpatialPoints(data.frame(x = as.numeric(coordinates$longitude[i]), y = as.numeric(coordinates$latitude[i])))
      
      #map the point to the index
      map_index <- over(spatial_pt, census_tracts)
      
      #parse out variables
      if (year<=2009) {
        GEOID <- as.character(map_index$CTIDFP00)
        STATE <- as.character(map_index$STATEFP00)
        COUNTY <- as.character(map_index$COUNTYFP00)
        TRACT_NUM <- as.character(map_index$TRACTCE00)
        TRACT <- as.character(map_index$NAME00)
        INPUT_LAT <- coordinates$latitude[i]
        INPUT_LON <- coordinates$longitude[i]
      } else if (year==2010) {
        GEOID <- as.character(map_index$GEOID10)
        STATE <- as.character(map_index$STATEFP10)
        COUNTY <- as.character(map_index$COUNTYFP10)
        TRACT_NUM <- as.character(map_index$TRACTCE10)
        TRACT <- as.character(map_index$NAME10)
        INPUT_LAT <- coordinates$latitude[i]
        INPUT_LON <- coordinates$longitude[i]
      } else if (year>=2011) {
        GEOID <- as.character(map_index$GEOID)
        STATE <- as.character(map_index$STATEFP)
        COUNTY <- as.character(map_index$COUNTYFP)
        TRACT_NUM <- as.character(map_index$TRACTCE)
        TRACT <- as.character(map_index$NAME)
        INPUT_LAT <- coordinates$latitude[i]
        INPUT_LON <- coordinates$longitude[i]
      } else {
        cat("\nUnrecognized census year: ", year, "\n")
        stop()
      }
      
      #add record to results dataframe
      results = rbind(results, data.frame(GEOID, STATE, COUNTY, TRACT_NUM, TRACT, INPUT_LAT, INPUT_LON, stringsAsFactors=FALSE))      
      
      
    } else {
      
      #add NA to results dataframe
      results = rbind(results, data.frame("GEOID"=NA, "STATE"=NA, "COUNTY"=NA, "TRACT_NUM"=NA, "TRACT"=NA, "INPUT_LAT"=NA, "INPUT_LON"=NA, stringsAsFactors=FALSE))      
      
    }
    
  }
  
  return(results)
}


#Example mappings using cases.csv and Philadelphia, PA census tract shapefiles
#Census TIGER shapefiles available at: http://www.census.gov/geo/maps-data/data/tiger-line.html
#extract the TIGER files to a directory, and pass the path of the files and shapefile name (without extension)
#e.g., if 2010 shapefiles are in this directory "C:\2010 Shape Files\", pass "C:/2010 Shape Files/tl_2010_42101_tract10"

#read in the csv
cases = read.csv(file="cases.csv", as.is=T)

#map to census tract
results = tractLookup(cases, shapefile="tl_2010_42101_tract10", year=2010, verbose=TRUE)

#save census tract to data
cases$census_tract = results$TRACT

#plot a map using the census tracts
par(oma=c(0.5, 0.5, 0.5, 0.5))
par(mar=c(0, 0, 0, 0), xaxs='i', yaxs='i')
par(plt=c(0, 1, 0, 1))

#philly census tracts
philly_ct = readShapePoly("tl_2010_42101_tract10")

#test plot of Philly
plot(philly_ct)

#tally number of cases by census tract
cases_by_tract = data.frame("NAME10"=philly_ct@data$NAME10, stringsAsFactors=F)
cases_by_tract$ncases = NA
for (i in 1:nrow(cases_by_tract))
{
  cases_by_tract$ncases[i] = sum(na.omit(paste(cases_by_tract$NAME10[i]) == cases$census_tract))
}
rm(i)

#merge cases by census tract with census tract data
philly_ct = merge(x=philly_ct, y=cases_by_tract, by="NAME10", all.x=T)
rm(cases_by_tract)

#choropleth plots by 5-levels of shading
spplot(philly_ct, "ncases", cuts=4, col.regions=brewer.pal(5, "Reds"))
