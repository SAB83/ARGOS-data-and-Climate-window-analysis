# ARGOS-data-and-Climate-window-analysis
#Modifying and cleaning data of ARGOS deployed on the migratory birds, calculating flight distance and running window climate analysis
library(tidyverse)
library(geosphere)
#citation("geosphere")


# Prepping data -----

#Download the most recent version of AMRO tracks from Movebank using:
# Download -> Download data -> 
#         Select the following options:
#            Available Sensor Types : GPS
#            CSV 
#            -should not have outliers or undeployed locations-
#         You can select other options but they are not important here
#

#Seek the tracking .csv file from movebank
AMRO <- read_csv(file.choose())

# Or specify which file you wish to select
#AMRO <- read_csv("ARGOS 10nov.csv")

# Select the only relevant variables
AMRO <- dplyr::select(AMRO, c("timestamp", 'individual-local-identifier', 
                              `tag-local-identifier`, "location-lat",`location-long`))

# Specify format and rename as needed
AMRO$`individual-local-identifier` <- as.character(AMRO$`individual-local-identifier`)
AMRO <- rename(AMRO, id.full = `individual-local-identifier`)
AMRO <- rename(AMRO, lat = `location-lat`)
AMRO <- rename(AMRO, long = `location-long`)

# Isolate type of tracking from the bird ID into new columns to simplify names 
# and ability to isolate them in the future
AMRO <- AMRO %>% separate(id.full,
                          c('prefix', 'ID', 'type', 'experiment.no'), remove = FALSE)
AMRO <- unique(AMRO) # Remove duplicate datapoints


# Start calculating distances ----
# Original distance calculation script and most comments for calculating distances
# from Aaron Skinner shared by Alicia Korpach, adapted by Christophe TurcottevdR

### Calculate mig distance for your birds

# Group and can apply any other filter if desired
xxx <-AMRO%>%group_by(id.full) # %>%filter()

# Select only relevant columns for dist calculation
# Make sure each bird/migration portion has a unique ID (here, use id.full)
zzz <- dplyr::select(xxx, c(id.full, lat, long))

# Check no. of detection for each birds (need at least 2 to calculate distance)
tba <- zzz %>% group_by(id.full) %>% summarize(n=n())
tba <- mutate(tba, bob = n < 2)
tba <- filter(tba, bob)
tba
# If tba is empty (tibble of 0 x 3), then everything is ok
# If tba not has rows of content, remove IDs with less than 2 data points:
#zzz2 <- anti_join(zzz, tba, by=id.full) # If you do this, make sure to also remove 
                #tba birds from the general AMRO df before binding them back together
# If empty:
zzz2 <- zzz # Don't run this if tba had contents

# The scrip below is in case that certain points needs to be removed due to lack of 
# spatial accuracy. This is most likely not needed for the ARGOS dataset as Movebank
# already flags and hides outliers. Would be interesting to know their process though.
# --
# df <- df[df$spatialerror <= 30, ] #You MAY need to subset points based on accuracy. In our data set, 
#                       points w/ HDOP > 30 were hundreds of km offbase at times. I am happy to discuss this threshold
#                       w/ folks, and please let me know if any data points were removed in this process 

# Loop to calculate distances between all consecutive points
whip.id.all <- unique(c(zzz2$id.full)) # Create list of IDs
ID.MD <- mig.pts <- dist <- vector("list", length = length(whip.id.all)) # Establish how many IDs there are
for(i in 1:length(whip.id.all)){ #Here some magic happens! All I know is [i] says it is going though every given ID in whip.id.all
ID.MD[[i]] <- subset(zzz2, id.full==whip.id.all[i])
dist[[i]] <- data.frame(rep(whip.id.all[i], nrow(ID.MD[[i]]) - 1), #Subtract 1 as distances are always between 2 points
             distHaversine(cbind(ID.MD[[i]]$long, ID.MD[[i]]$lat)))
}

dist.all <- dplyr::bind_rows(dist, .id = "column_label")[,c(2:3)] # Create dataframe with result of loop above
colnames(dist.all)[1:2] <- c("id.full2","dist.m") # Add column titles
dist.all <- mutate(dist.all, dist.km = dist.m / 1000) # Get distances in km
# This df lists all distances traveled between points for any given bird
head(dist.all)


# If want to calculate total distance traveled from start to finish of tag:
## Not exactly migratory distance as this distance includes small few meters 
## movements at Winnipeg and around stopovers
dist.total <- data.frame(dist.all %>% group_by(id.full2) %>% 
                          summarize(total.dist.traveled = sum(dist.m)/1000)) #Convert to km
dist.total$total.dist.traveled 


# Time analysis for locating missing data and night/day movements ----
library(lubridate)

AMRO2 <- AMRO

# Seek locations of previous column
AMRO2$startlat <- lag(AMRO2$lat)
AMRO2$startlong <- lag(AMRO2$long)

# Seek time of previous column
AMRO2$timestamp2 <- AMRO2$timestamp
AMRO2$departstamp <- lag(AMRO2$timestamp)
# Isolate time and date (may help later and with viewing in excel)
AMRO2$date.x <- as.Date(AMRO2$timestamp, format  = "%Y-%m-%d")
AMRO2$time.x <- strftime(AMRO2$timestamp, format="%H:%M:%S", tz= "GMT")
AMRO2$departdate.x <- lag(AMRO2$date.x)
AMRO2$departtime.x <- lag(AMRO2$time.x)

# gap since last datapoint

AMRO2$timegap2=as.numeric(difftime(AMRO2$timestamp,AMRO2$departstamp))

AMRO2 <- mutate(AMRO2, timegap2 = timestamp - departstamp)
#try converting seconds to hours for better visual
AMRO2$timegap <- hms::as_hms(AMRO2$timegap2)

# In anticipation of joining this data frame with the distances:
# Delete first row of each subset (bird) (because of the distance calculations)
AMRO3 <- mutate(AMRO2, samebird = id.full == lag(id.full))
AMRO3 <- filter(AMRO3, samebird)
AMRO3 <- drop_na(AMRO3, samebird) #remove rows with NA in column samebird

# Join distances to overall AMRO df -------

AMRO.final <- bind_cols(AMRO3, dist.all, .name_repair = "unique")

# Quick check if there are any issues with df
is.adjusted <- mutate(AMRO.final, same.ID = id.full != id.full2)
is.adjusted <- filter(is.adjusted, same.ID)
is.adjusted #should be empty

# Clean
AMRO.final <- dplyr::select(AMRO.final, -c(prefix,id.full2,samebird))

# Check locations with missing points in between (28800s = 8h, 97200s = 27h)
AMRO.final <- mutate(AMRO.final, missing.point = type == "Intensive" & timegap2 > 28800 |
                type != "Intensive" & timegap2 > 97200)

AMRO.final <- mutate(AMRO.final, missing.point =  timegap2 > 28800 |
                        timegap2 > 97200)
Rows.missing.point <- filter(AMRO.final, missing.point)
#you can view which lines have missing points and how big were those intervals:
Rows.missing.point
# Save overall file with birds of all types (intensives & experimentals)
write.csv(AMRO.final, "C:/Z-My Drive/postdoc/Robin/AMRO_ARGOS_int_08_3.csv", row.names = FALSE)


# Start intensive tag analysis --------
# Want to know:
#  1. Did the bird move.... lets say >5 kilometers
#  2. Did the bird start migrating during the day/night
#  3. In which quadrant of the day did the bird move
#  4.1 Did the bird have missing points
#  4.2 If there are, can we tell at what time of day movements were done 
#                              or are those overlapping day and night

# Either select appropriate csv file from computer using read_csv 
#            or continue from prior script

# AMRO.int <- read_csv(file.choose())
 AMRO.int <- AMRO.final

# Select intensive tags
AMRO.int <- subset(AMRO.int, type == "Intensive")
# Select rows with travels above certain cutoff
# Cutoff chosen here is 5km, can be changed depending on result desired
AMRO.int <- subset(AMRO.int, dist.km >= 5)

#only recorded in the city
AMRO.int <- subset(AMRO.int, dist.km < 12)

# Remove missing points for now to simplify operations
int.missing.point <- filter(AMRO.int, missing.point)
AMRO.int <- anti_join(AMRO.int,int.missing.point)

# Graphic below is just to keep track to time conversions and period of day

#  ------- #####-Night-######___________Day_________###-Night-### --
#  -- UTC :        6         13         18          23         6  --
#  ------- ##################                       ############# --
#  -- CST :        12        7          12          17         12 -- 
#  ------- ##################_______________________############# --
#  ------- Evening |  Night  | Morning  | Afternoon | Evening  |  --

# Remember the distance traveled was the one done prior to any time.x 
# So, a distance associated with a time.x of 13.xx.xx was done in the interval
# 6 to 13 UTC in other words the bird migrated at night between 12 am and 7 am CST

# In the script below, the cutoff times are chosen way out in the intervals 
# (eg. 15:00:00 for a 13:xx:xx time) to account for the slight differences between 
# tags for the time of location sampling. These could be closer to the time of sample,
# but I don't foresee any issue since all tags have the same program. 
# This script wouldn't work if the tags had more variability in sampling times.


# So if time sampled between 6 and 13 is night:
AMRO.int$day.night <- ifelse(AMRO.int$time.x <= "15:00:00", "Night", "Day")


# Selecting each quadrant of the day to associate:
# Morning -> Afternoon -> Evening -> Night
# Use the text graphic above for reference
AMRO.time <- AMRO.int %>% 
 rowwise() %>% 
 mutate(time.of.day = ifelse(time.x >= "08:00:00",  # If true, then continue, if not, write "Evening"
                             ifelse(time.x >= "15:00:00", # If true, then continue, if not, write "Night"
                                    ifelse(time.x >= "20:00:00",  # If true, write "Afternoon", if not, write "Morning"
                                           "Afternoon", "Morning")
                                    , "Night"), 
                             "Evening"))

# See if day/night can be determined for the missing points
int.missing.point <- int.missing.point %>% 
 rowwise() %>% 
 mutate(day.night = ifelse(timegap2 <= 48000,
                           ifelse(time.x <= "22:00:00",
                                  ifelse(time.x < "15:00:00" & time.x > "11:00:00",
                                         "Night", " "), "Day"), " "))

# Is approx 13h in seconds. If above this, there are more than 2 datapoints missing
# and then, it is impossible to determine if the travel was done in the night or day
 # Using the logic above, there are only two times where you can distinguish
 # the time of travel: if the time sample is the last of the night or the 
 # last of the day. If it is one or the other, there will be an overlap of d/n
                                    

#Join back to first subset
migr.time <- bind_rows(AMRO.time, int.missing.point)

# Clean
migr.time <- dplyr::select(migr.time, -c(experiment.no,timestamp2,timegap2,
                                         departstamp,`tag-local-identifier`,type))
head(migr.time)
# Save intensive file
# From this file, you can view the tracks in Excel and colour code them to view them
# However, saving them in excel will not preserve the 'timestamp' POSIX format
write.csv(migr.time, "C:/Z-My Drive/postdoc/Robin/Intensive AMRO all Movements 3Aug2023-NA.csv", row.names = FALSE)

# Quick graph to visualize things ----
library(ggplot2)

# Set order of variables in graph displays
migr.time$time.of.day <- factor(migr.time$time.of.day, 
                      levels= c("Morning", "Afternoon", "Evening", "Night"))
migr.time$day.night <- factor(migr.time$day.night, 
                                levels= c("Day", "Night"))

#"Day", "Night"
#"Morning", "Afternoon", "Evening", "Night"
#"#fa8202", "#ae00ff"

# Create graph
g1 <- ggplot(na.omit(migr.time), aes(x = time.of.day, y=dist.km, colour = time.of.day
                            ))
fig1 <- g1 + geom_boxplot(aes(colour=time.of.day)) + geom_point(
 aes(colour=time.of.day), position = position_dodge(width = 0.75), shape=1, size=3) + 
 theme_classic(base_size = 10) +
 scale_x_discrete(name = "",labels = c("Morning", "Afternoon", "Evening", "Night")) +
 scale_y_continuous(name="Distance traveled for day quadrant (km)",
                    breaks = c(25,50,100,200,300, 400)) +
 scale_color_manual(values=c("#fcba03","#fa8202", "#ae00ff", "#0024b5"),
                    name = "Day quadrant"#,           #change name of quadrants if/when needed
                    #breaks=c("a1", "b1", "c1", "d1"),#original name (set order)
                    #labels=c("a2", "b2", "c2", "d2") #desired name (in order above)
                             )+theme(axis.text.x=element_text(size=13),
                                     axis.text.y=element_text(size=13))+
  theme(text = element_text(size = 15))
fig1

#, "c1", "d1"
#, "c2", "d2"

############################################################
#climate window analysis

library(tidyverse)  
library(psych)
library(mgcv)
library(lmtest)
library(Hmisc)
library(RNCEP)
library(climwin)
library(lubridate)
library(slider)
library(lme4)
library(MuMIn)

install.packages("slider")

data <- read_csv("C:/Z-My Drive/postdoc/Robin/amro/data.csv")

mobu<-data 


mobu$fd<-as.integer(mobu$fd, length=1)

origin=paste(mobu$survey_year,"/1/1", sep="")

mobu$fd1<-as.Date(mobu$fd, origin=origin)
mobu$fd1<- format(mobu$fd1, format="%d/%m/%Y") 

cov<-read.csv("C:/Z-My Drive/postdoc/Robin/amro/ENVdata.csv") #see 00-WeatherDownload2.Rmd

cov$survey_year<-format(as.Date(cov$timestamp, format="%d/%m/%Y"), "%Y")

cov$timestamp1 <-as.Date(cov$timestamp, origin=origin)

cov$timestamp1 <- format(cov$timestamp1, format="%d/%m/%Y") 

#refday is the last day the all departure date (doy = 329) 
#range is the number of days before the refday we want to assess daily weather. 
#In this case it will be October 7 (doy = 281). 329-281
#Range 48

slid<-slidingwin(baseline=lm(doyf~1, data=mobu),
                 xvar=list(Temp=cov$temp1, 
                           Wind=cov$wind1,
                           Rain=cov$precipitation1), 
                 cdate=cov$timestamp1,
                 bdate=mobu$fd1,   type = "relative",
                 range=c(8, 0), func="lin", 
                 stat="mean", cinterval="day")


slid<-slidingwin(baseline=lm(doyf~1, data=mobu),
           xvar=list(Temp=cov$temp1, 
                     Wind=cov$wind1,
                     Rain=cov$precipitation1), 
           cdate=cov$timestamp1,
           bdate=mobu$fd1,   stat = c("mean", "max"), 
           type = "absolute", refday = c(25, 11),
           range=c(10, 0), func="lin", 
           cinterval="day")

head(slid[[1]]$Dataset)
b<-slid[[1]]$BestModel
slid[[2]]$BestModel
slid[[3]]$BestModel

plotbetas(dataset = b)


head(slid[[3]]$BestModelData)

slid$combos

summary(slid[[1]]$BestModel)
summary(slid[[2]]$BestModel)
summary(slid[[3]]$BestModel)
summary(slid[[4]]$BestModel)

AICc(slid[[3]]$BestModel)

ggplot(data, aes(x=temp, y=doyf)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_bw()

randomized1<-randwin(repeats = 10,     
                     baseline = lm(doyf~1, data = mobu),
                     xvar = list(Temp=cov$temp1),
                     type = "relative", 
                     range=c(8, 0),
                     stat = c("mean"), 
                     func = c("lin"),
                     cinterval = "day",
                     cdate=cov$timestamp1, bdate=mobu$fd1, 
                     window= "sliding")

 
randomized1[[1]]

pvalue(datasetrand = randomized1[[1]], dataset = slid[[1]]$Dataset, metric = "C", sample.size = 1)

Output1 <- slid[[1]]$Dataset
Rand1 <- randomized1[[1]]

plothist(dataset = Output1, datasetrand = Rand1)

randomized2<-randwin(repeats = 10,     
                     baseline = lm(doyf~1, data = mobu),
                     xvar = list(Wind=cov$wind1),
                     type = "relative", 
                     range=c(8, 0),
                     stat = c("mean"), 
                     func = c("lin"),
                     cinterval = "day",
                     cdate=cov$timestamp1, bdate=mobu$fd1,  
                     window= "sliding")
randomized2[[1]]

pvalue(datasetrand = randomized2[[1]], dataset = slid[[2]]$Dataset, metric = "C", sample.size = 1)

Output2 <- slid[[2]]$Dataset
Rand2 <- randomized2[[1]]

plothist(dataset = Output2, datasetrand = Rand2)

randomized3<-randwin(repeats = 10,     
                     baseline = lm(doyf~1, data = mobu),
                     xvar = list(Rain=cov$precipitation1),
                     type = "relative", 
                     range=c(8, 0),
                     stat = c("mean"), 
                     func = c("lin"),
                     cinterval = "day",
                     cdate=cov$timestamp1, bdate=mobu$fd1,
                     window= "sliding")
randomized3[[1]]

pvalue(datasetrand = randomized3[[1]], dataset = slid[[3]]$Dataset, metric = "C", sample.size = 1)

Output3 <- slid[[3]]$Dataset
Rand3 <- randomized3[[1]]

plothist(dataset = Output3, datasetrand = Rand3)

plotall(datasetrand = randomized1[[1]],
        dataset = slid[[1]]$Dataset, 
        bestmodel = slid[[1]]$BestModel,
        bestmodeldata = slid[[1]]$BestModelData,
        title=slid$combos[1,])

plotdelta(dataset = Output1)
plotweights(dataset = Output1)
plotbetas(dataset = Output1)
plotwin(dataset = Output1)
medwin(dataset = Output1, cw = 0.95)


Single <- singlewin(xvar=list(Temp=cov$temp1,Wind=cov$wind1,
                                  Rain=cov$precipitation1),
                        cdate = cov$timestamp1,
                        bdate = mobu$fd1,
                        baseline=lm(doyf~1, data=mobu),
                        cinterval = "day",range=c(7, 0),
                        type = "relative",
                        stat = "mean",func = "lin")



plotbest(dataset = Output1,
         bestmodel = Single$BestModel, 
         bestmodeldata =Single$BestModelData)

Win2 <- slidingwin(xvar=list(Temp=cov$temp1,Wind=cov$wind1,
                             Rain=cov$precipitation1),
                   cdate = cov$timestamp1,
                   bdate = mobu$fd1,
                   baseline=lm(doyf~1, data=mobu),
                   cinterval = "day",range=c(7, 0),
                   type = "relative",
                       stat = c("max", "mean"),
                       func = c("lin", "quad"))
Win2$combos

Win2[[1]]$BestModel
