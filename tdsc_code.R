#tdsc_code.R
#code meant to study the TDSC dataset

#imports

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
#   -way more males than females using serviec
#   -many more millenials using the service
#   -latitude, longitude variables are distributed in a relatively friendly
#   manner
#   -many more subscribers than one-off customer
#map out based on longitude and latitude
mapper <- function(z, levels, isStartStation, ...) {
    # Which quantiles do we need?
    probs <- seq(from=0, to=1, length.out=(levels+1))
    # What are those quantiles?
    z.quantiles <- quantile(z, probs)
    # Assign each observation to its quantile
    z.categories <- cut(z, z.quantiles, include.lowest=TRUE)
    # Make up a color scale
    shades <- terrain.colors(levels)
    if (isStartStation){
        #want to plot based on start station
        plot(x=bikeFrame$start.station.longitude, 
             y=tractFrame$start.station.latitude,col=shades[z.categories], ...)
    }
    else { #plot based on end station
        #want to plot based on start station
        plot(x=bikeFrame$end.station.longitude, 
             y=tractFrame$end.station.latitude,col=shades[z.categories], ...)
    }
    invisible(list(quantiles=z.quantiles, categories=z.categories))
}
#then plot each latitude-longitude map based on desired variables to consider

# Ian's code: the following code looks at the 
# start-stop frequencies, and calculates the distance between them
# data is formatted as [ start station, end station, distance, trip duration, timestart] 
# I chose to add in time start as I think that depending on what time they commute that
# could give us a good idea of what the traffic in the area looks like
startLoc = bikeFrame$start.station.name
endLoc = bikeFrame$end.station.name
latDiff = (bikeFrame$start.station.latitude - bikeFrame$end.station.latitude)^2 
longDiff = (bikeFrame$start.station.longitude - bikeFrame$end.station.longitude)^2 
l2Distance = sqrt(latDiff + longDiff)
timeStart = bikeFrame$starttime

stationFrame = data.frame(startLoc, endLoc, l2Distance, bikeFrame$tripduration ,timeStart)