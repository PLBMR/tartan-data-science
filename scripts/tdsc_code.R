#tdsc_code.R
#code meant to study the TDSC dataset

#imports
library(tree)
library(bla)
library(rpart)
require(ggplot2)
require(ggmap)
require(maps)
require(dplyr)

#standard constants
pchVal = 19 #for most plots

#load in dataset
datasetString = (
"http://www.stat.cmu.edu/tartandatasciencecup/20150708-citibike-tripdata.csv")
bikeFrame = read.csv(datasetString,header = TRUE)

#get rid of missing values for gender model
genderBikeFrame = bikeFrame[which(bikeFrame$gender != 0),]
#univariate analysis of quantitative data
par(mfrow = c(2,4))
hist(bikeFrame$tripduration,freq = FALSE, col = "Blue")
hist(bikeFrame$birth.year,freq = FALSE, col = "Blue")
hist(bikeFrame$gender,freq = FALSE, col = "Blue")
hist(bikeFrame$start.station.latitude,freq = FALSE, col = "Blue")
hist(bikeFrame$start.station.longitude, freq = FALSE, col = "Blue")
hist(bikeFrame$end.station.latitude, freq = FALSE, col = "Blue")
hist(bikeFrame$end.station.longitude, freq = FALSE, col = "Blue")
userTypeCounts = table(bikeFrame$usertype)
barplot(userTypeCounts, col = "Blue")
#results:
#   -way more males than females using service
#   -many more millenials using the service
#   -latitude, longitude variables are distributed in a relatively friendly
#   manner
#   -many more subscribers than one-off customers
#map out based on longitude and latitude
stationLevelFrame = data.frame(station = unique(bikeFrame$start.station.name),
                             latitude = rep(
                                0,length(unique(bikeFrame$start.station.name))),
                             longitude = rep(
                                0,length(unique(bikeFrame$start.station.name))))
mapper <- function(z, levels, ...) {
    # Which quantiles do we need?
    probs <- seq(from=0, to=1, length.out=(levels+1))
    # What are those quantiles?
    z.quantiles <- quantile(z, probs)
    # Assign each observation to its quantile
    z.categories <- cut(z, z.quantiles, include.lowest=TRUE)
    # Make up a color scale
    shades <- terrain.colors(levels)
    #want to plot based on start station
    plot(x=stationLevelFrame$longitude, 
             y=stationLevelFrame$latitude,col=shades[z.categories], ...)
    invisible(list(quantiles=z.quantiles, categories=z.categories))
}
#then plot each latitude-longitude map based on desired variables to consider
numLevels = 4 #just for friendliness for the visual of the map
#get longitude and latitude pairs for each station
for (i in 1:dim(stationLevelFrame)[1]){
    givenStation = stationLevelFrame$station[i]
    givenStationObs = bikeFrame[ #find station observations
                        which(bikeFrame$start.station.name == givenStation),]
    stationLevelFrame$latitude[i] = givenStationObs$start.station.latitude[1]
    stationLevelFrame$longitude[i] = givenStationObs$start.station.longitude[1]
}
#get num uses overall
stationLevelFrame$numUses = rep(0,length(stationLevelFrame$station))
for (i in 1:dim(stationLevelFrame)[1]){
    givenStation = stationLevelFrame$station[i]
    stationLevelFrame$numUses[i] = length(bikeFrame$start.station.name[which(
                        bikeFrame$start.station.name == givenStation)])
}
#then plot this using the mapper
par(mfrow=c(1,1))
numUses.map = mapper(stationLevelFrame$numUses,numLevels,pch = pchVal,
                     xlab= "Longitude", ylab = "Latitude",
                     main = "Num Uses on\nLongitude and Latitude")
legend("bottomleft", legend=levels(numUses.map$categories), pch=pchVal,
       col=terrain.colors(numLevels), cex=0.4)
#results: largest amount of usage is in midtown to UWS, UES manhattan
#get subscriber usage

#then plot each latitude-longitude map based on desired variables to consider

# Ian's code: the following code looks at the 
# start-stop frequencies, and calculates the distance between them
# data: [ start station-end station, startLat, startLong, endLat, endLong,
# distance, trip duration, timestart] 
# I added time start as depending on what time they commute it
# could give us an idea of what the traffic looks like

latOne = bikeFrame$start.station.latitude
longOne = bikeFrame$start.station.longitude 
latTwo = bikeFrame$end.station.latitude
longTwo = bikeFrame$end.station.longitude
latDiff = (latOne - latTwo)^2 
tripID = (bikeFrame$start.station.id * 1000) + bikeFrame$end.station.id
longDiff = (longOne - longTwo)^2 
l2Distance = sqrt(latDiff + longDiff)
timeStart = bikeFrame$starttime

stationFrame = data.frame(tripID, latOne, longOne,
                          latTwo, longTwo, l2Distance, 
                          bikeFrame$tripduration ,timeStart)

minDist = median(l2Distance) + (4 * sd(l2Distance))
toPlot = subset(stationFrame, l2Distance > minDist)

ggmap(get_map(location = 'new york', zoom = 13))  + 
  geom_segment(data = toPlot, mapping = aes(x = longOne, xend = longTwo, y = latOne, yend = latTwo))

# second option is duplicated/ combining the data
#naive modeling
#decision tree
#model latitude and longitude with gender
femaleObs = which(genderBikeFrame$gender == 2)
genderBikeFrame$genderString = rep("N",dim(genderBikeFrame)[1]) 
genderBikeFrame$genderString[femaleObs] = "F"
genderBikeFrame$genderString[!femaleObs] = "M"
naiveGenderMod.tree = rpart(genderString ~ start.station.latitude + 
                           start.station.longitude + end.station.latitude 
                           + end.station.longitude,
                           data = genderBikeFrame,method = "class")
plot(genderMod.tree)
text(genderMod.tree,pretty=1,cex=.75)
#consider customers
naiveCustomerMod.tree = rpart(usertype ~ start.station.latitude + 
                             start.station.longitude + end.station.latitude 
                             + end.station.longitude,
                             method = "class",data = bikeFrame)
plot(naiveCustomerMod.tree)
text(naiveCustomerMod.tree,pretty=1,cex=.25)
