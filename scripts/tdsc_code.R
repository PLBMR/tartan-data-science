#tdsc_code.R
#code meant to study the TDSC dataset

#imports

#standard constants
pchVal = 19 #for most plots

#load in dataset
datasetString = (
"http://www.stat.cmu.edu/tartandatasciencecup/20150708-citibike-tripdata.csv")
bikeFrame = read.csv(datasetString,header = TRUE)

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
numUses.map = mapper(stationLevelFrame$numUses,numLevels,pch = pchVal,
                     xlab= "Longitude", ylab = "Latitude",
                     main = "Num Uses on\nLongitude and Latitude")
legend("bottomleft", legend=levels(numUses.map$categories), pch=pchVal,
       col=terrain.colors(numLevels), cex=0.8)
#then plot each latitude-longitude map based on desired variables to consider
# Ian's code: the following code looks at the 
# start-stop frequencies, and calculates the distance between them
# data: [ start station, end station, distance, trip duration, timestart] 
# I added time start as depending on what time they commute it
# could give us an idea of what the traffic looks like
startLoc = bikeFrame$start.station.name
endLoc = bikeFrame$end.station.name
latDiff = (bikeFrame$start.station.latitude - 
             bikeFrame$end.station.latitude)^2 

longDiff = (bikeFrame$start.station.longitude - 
              bikeFrame$end.station.longitude)^2 
l2Distance = sqrt(latDiff + longDiff)
timeStart = bikeFrame$starttime

stationFrame = data.frame(startLoc, endLoc, l2Distance, 
                          bikeFrame$tripduration ,timeStart)