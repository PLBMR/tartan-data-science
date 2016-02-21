datasetString = (
  "http://www.stat.cmu.edu/tartandatasciencecup/20150708-citibike-tripdata.csv")
bikeFrame = read.csv(datasetString,header = TRUE)

# Num non-gendered
noGender <- length(which(bikeFrame$gender == 0))

# Pairs plots
smallds = subset(bikeFrame, select=c("starttime", "birth.year"), "birth.year" > 1960)
plot(smallds$starttime, smallds$birth.year)

# pairs(~birth.year + start.station.latitude + start.station.longitude, data = bikeFrame)
