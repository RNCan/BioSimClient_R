


library(BioSIM)

locations <- BioSIM::twoLocationsInSouthernQuebec
print(locations)

getModelOutput(2000, 2004, locations$Name, locations$Latitude, locations$Longitude, modelName = "DegreeDay_Annual")
generateModelOutput("DegreeDay_Annual", 2000, 2004, locations$Name, locations$Latitude, locations$Longitude)

rm(list = ls())
periods <- c("1951_1980", "1961_1990", "1971_2000", "1981_2010", "1991_2020", "2001_2030", "2011_2040", "2021_2050", "2031_2060", "2041_2070", "2051_2080", "2061_2090")
climModels <- c("GCM4", "RCM4", "Hadley")

output <- NULL
for (m in climModels) {
  for (p in periods) {
    normals <- getAnnualNormals(period = p, id = "Qu�bec", latDeg = 46.87, longDeg = -71.25, elevM = 114, climModel = m)
    normals$period <- p
    normals$model <- m
    output <- rbind(output, normals)
  }
}

output$per <- as.numeric(substr(output$period, 6, 9))
output$TM <- (output$TN + output$TX) * .5
getModelList()
obs1951_2020 <- getModelOutput(fromYr = 1951, toYr = 2040, id = "Qu�bec", latDeg = 46.87, longDeg = -71.25, elevM = 114, modelName = "DegreeDay_Annual")
obs1951_2020$TM <- (obs1951_2020$MeanTmin + obs1951_2020$MeanTmax) * .5

field <- "DD"
mean1951_1980 <- mean(obs1951_2020[which(obs1951_2020$Year >= 1951 & obs1951_2020$Year<= 1980), field])
mean1961_1990 <- mean(obs1951_2020[which(obs1951_2020$Year >= 1961 & obs1951_2020$Year<= 1990), field])
mean1971_2000 <- mean(obs1951_2020[which(obs1951_2020$Year >= 1971 & obs1951_2020$Year<= 2000), field])
mean1981_2010 <- mean(obs1951_2020[which(obs1951_2020$Year >= 1981 & obs1951_2020$Year<= 2010), field])
mean1991_2020 <- mean(obs1951_2020[which(obs1951_2020$Year >= 1991 & obs1951_2020$Year<= 2020), field])
mean2001_2030 <- mean(obs1951_2020[which(obs1951_2020$Year >= 2001 & obs1951_2020$Year<= 2030), field])
mean2011_2040 <- mean(obs1951_2020[which(obs1951_2020$Year >= 2011 & obs1951_2020$Year<= 2040), field])
y <- c(mean1951_1980, mean1961_1990, mean1971_2000, mean1981_2010, mean1991_2020, mean2001_2030, mean2011_2040)
x <- c(1980, 1990, 2000, 2010, 2020, 2030, 2040)
fromAnnualValues <- data.frame(x,y)

# require(ggplot2)
# ggplot() +
# #  geom_line(aes(x=per, y=TM, group=model, col=model), output) +
# #  geom_point(aes(x=per, y=TM), output) +
#   geom_point(aes(x=x, y=y), fromAnnualValues, col="blue")



#geom_point(aes(x=Year, y=TM), obs1951_2020, col="red")



#### Gardening in Aylmer, QC ####

dailyClimate <- getModelOutput(fromYr = 1991, toYr = 2020, id = "Aylmer", latDeg = 45.3912, longDeg = -75.8304, elevM=NA, modelName = "Climatic_Daily")
may15toJune6 <- dailyClimate[which((dailyClimate$Month == 5 & dailyClimate$Day >= 15) | (dailyClimate$Month == 6 & dailyClimate$Day <= 6)),]
may15toJune6$julianDate <- ifelse(may15toJune6$Month == 5, may15toJune6$Day + 120, may15toJune6$Day + 151)

min <- aggregate(Tmin ~ julianDate, may15toJune6, FUN="min")
colnames(min) <- c("julianDate", "min")
mean <- aggregate(Tmin ~ julianDate, may15toJune6, FUN="mean")
colnames(mean) <- c("julianDate", "mean")
max <- aggregate(Tmin ~ julianDate, may15toJune6, FUN="max")
colnames(max) <- c("julianDate", "max")
q10 <- aggregate(Tmin ~ julianDate, may15toJune6, FUN = 'quantile', probs=.1)
colnames(q10) <- c("julianDate", "q10")
q90 <- aggregate(Tmin ~ julianDate, may15toJune6, FUN = 'quantile', probs=.9)
colnames(q90) <- c("julianDate", "q90")

stats <- merge(merge(min, mean, by="julianDate"), max, by="julianDate")
stats <- merge(merge(stats, q10, by="julianDate"), q90, by="julianDate")
# require(ggplot2)
#
# ggplot() +
#   geom_ribbon(aes(x=julianDate, ymin=min, ymax=max), stats, alpha = .5) +
#   geom_line(aes(x=julianDate, y=mean), stats) +
#   geom_line(aes(x=julianDate, y=q10), stats, lty = 2) +
#   geom_line(aes(x=julianDate, y=q90), stats, lty = 2) +
#   scale_x_continuous(breaks = seq(135, 155, by = 5), labels = c("15/05", "20/05", "25/05", "30/05", "4/06")) +
#   ylab("Temperature") +
#   xlab("Julian date")


latDeg <- runif(n = 100, min=48, max=51)
longDeg <- runif(n = 100, min=-76, max=-66)
ids <- paste(rep("plot",100),1:100,sep="")
require(BioSIM)
getModelList()
system.time({
  test2 <- getModelOutput(fromYr = 1999, toYr = 2000, id = ids, latDeg, longDeg, modelName = "DegreeDay_Annual")
})
shutdownClient()


#### Mountain pine bark beetle in BC ####

require(BioSIM)
getModelHelp("MPB_SLR")
mpb_slr <- getModelOutput(fromYr = 2011, toYr = 2020, id = "SomewhereInBC", latDeg = 50, longDeg = -97, modelName = "MPB_SLR")
parms <- getModelDefaultParameters("MPB_SLR")
parms["RunLength"] <- 5
mpb_slr5YrPeriods <- getModelOutput(fromYr = 2011, toYr = 2020, id = "SomewhereInBC", latDeg = 50, longDeg = -97, modelName = "MPB_SLR", additionalParms = parms)
parms["RunLength"] <- 1
mpb_slr1YrPeriods <- getModelOutput(fromYr = 2011, toYr = 2020, id = "SomewhereInBC", latDeg = 50, longDeg = -97, modelName = "MPB_SLR", additionalParms = parms)


mpb_slrFuture <- getModelOutput(fromYr = 2025, toYr = 2030, id = "SomewhereInBC", latDeg = 50, longDeg = -97, modelName = "MPB_SLR")

#### Relative humidity in BC ####
require(BioSIM)
getModelHelp("Climatic_Monthly")
Weather <- generateWeather("Climatic_Monthly", fromYr = 1950, toYr = 2100, id = "Around_Kamloops", latDeg = 51, longDeg = -120)
Climatic_Monthly <- Weather$Climatic_Monthly
require(ggplot2)
ggplot() +
  geom_point(aes(y=MeanRelH, x=Year), Climatic_Monthly[which(Climatic_Monthly$Month==7),]) +
  geom_vline(xintercept = 2023) +
  ggtitle("Observed and predicted mean relative humidity in July") +
  ylab("Mean relative humidity (%)") +
  ylim(40,80)
