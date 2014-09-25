
## ----getdata,cache=FALSE-------------------------------------------------
DATAFILE = 'StormData.csv.bz2'
CACHEFILE = 'stormdata.rds'
URL = 'http://d396qusza40orc.cloudfront.net/repdata/data/StormData.csv.bz2'
library(plyr)
library(ggplot2)

## change WORKDIR to the directory in which you want to download the data
WORKDIR = "./PA2"

# Because of the size of the data file we cache it in RDS format the first time and check for the cached 
# copy on subsequent runs.  If the neither the cache file nor the original data file are found, the data will
# will be downloaded from the URL above.

if (!file.exists(CACHEFILE)) {
  if (!file.exists(DATAFILE)) {
    download.file(URL,DATAFILE)
  }
  conn = bzfile(DATAFILE,open='r')
  cat("reading ",DATAFILE)
  sdf = read.csv(conn, stringsAsFactors=FALSE)
  saveRDS(sdf,file=CACHEFILE)
} else {
  sdf = readRDS(CACHEFILE)
}


## ------------------------------------------------------------------------
dim(sdf)


## ----subset_harm---------------------------------------------------------
sdf_harm = subset(sdf, FATALITIES > 0 | INJURIES > 0)
nrow(sdf_harm)


## ----checkeventcoding----------------------------------------------------
data.frame(table(sdf_harm$EVTYPE))


## ----clean_events--------------------------------------------------------
# first convert values in the sdf_harm subset
sdf_harm$EVTYPE = tolower(as.character(sdf_harm$EVTYPE))


## ----recodetable---------------------------------------------------------
recode = matrix(c(
"cold and snow","winter weather",
"excessive rainfall","heavy rain",
"extreme heat","excessive heat",
"extreme windchill","extreme cold/wind chill",
"flooding","flood",
"fog and cold temperatures","freezing fog",
"freezing drizzle","winter weather",
"freezing rain","winter weather",
"frost","frost/freeze",
"glaze","ice storm",
"glaze/ice storm","ice storm",
"gusty winds","strong wind",
"heat wave","excessive heat",
"heat wave drought","drought",
"heavy surf","high surf",
"heavy surf/high surf","high surf",
"high surf advisory","high surf",
"high wind and seas","marine high wind",
"high winds","high wind",
"high winds/snow","winter storm",
"hurricane","hurricane (typhoon)",
"hurricane/typhoon","hurricane (typhoon)",
"ice","winter weather",
"icy roads","winter weather",
"black ice","winter weather",
"landslide","debris flow",
"landslides","debris flow",
"marine tstm wind","marine thunderstorm wind",
"rain/snow","winter weather",
"record heat","excessive heat",
"record/excessive heat","excessive heat",
"rip currents","rip current",
"snow/high winds","winter storm",
"storm surge","storm surge/tide",
"strong winds","strong wind",
"thundersnow","winter weather",
"thunderstorm winds","thunderstorm wind",
"thunderstormw","thunderstorm wind",
"tornadoes, tstm wind, hail","tornado",
"tropical storm gordon","tropical storm",
"tstm wind","thunderstorm wind",
"tstm wind (g45)","thunderstorm wind",
"urban/sml stream fld","flood",
"unseasonably warm and dry","excessive heat",
"wild fires","wildfire",
"wild/forest fire","wildfire",
"winter storms","winter storm",
"winter storm high winds","winter storm",
"wintry mix","winter weather",
"winter weather mix","winter weather",
"winter weather/mix","winter weather"),
ncol=2,byrow=TRUE)

rcf = function(x) {
  n = match(x,recode[,1])
  if (!is.na(n)) {
    recode[n,2]
  } else {
    x
  }
}
sdf_harm$evRecode = sapply(as.character(sdf_harm$EVTYPE), rcf)
changed = round(mean(sdf_harm$EVTYPE != sdf_harm$evRecode) * 100,2)


## ----totalharmbyevent----------------------------------------------------
etab1 = ddply(sdf_harm, .(EVTYPE), summarize,fatalities=sum(FATALITIES),injuries=sum(INJURIES))
etab1$total = etab1$fatalities + etab1$injuries
etab1$percent = round(etab1$total / sum(etab1$total) * 100,2)
top=20
sorted_etab1 = etab1[order(etab1$percent,decreasing=TRUE),]
ggplot(sorted_etab1[1:top,], aes(x=factor(EVTYPE),y=percent)) + 
  geom_bar(stat='identity') + 
  coord_flip() +
  scale_x_discrete(limits=sorted_etab1[1:top,'EVTYPE']) + 
  ggtitle(paste0('Fatalities + Injuries Percent of Total by Event for Top ',top,' Events')) +
  labs(x = 'Event', y = 'Count')


## ----totalharmbyrecode---------------------------------------------------
etab2 = ddply(sdf_harm, .(evRecode), summarize,fatalities=sum(FATALITIES),injuries=sum(INJURIES))
etab2$total = etab2$fatalities + etab2$injuries
etab2$percent = round(etab2$total / sum(etab2$total) * 100,2)
top=20
sorted_etab2 = etab2[order(etab2$percent,decreasing=TRUE),]
ggplot(sorted_etab2[1:top,], aes(x=evRecode,y=percent)) + 
  geom_bar(stat='identity') + 
  coord_flip() +
  scale_x_discrete(limits=sorted_etab2[1:top,'evRecode']) + 
  ggtitle(paste0('Fatalities + Injuries Percent of Total by Event for Top ',top,' Events (Recoded)')) +
  labs(x = 'Event', y = 'Count')


## ----topfiveagg----------------------------------------------------------
sorted_etab2[1:5,]


## ----eventmeanmedian-----------------------------------------------------
# let's look at the event frequencies
emean = ddply(sdf_harm,.(evRecode),summarize,mean=mean(FATALITIES+INJURIES),median=median(FATALITIES+INJURIES))
sorted_emean= emean[order(emean$mean,decreasing=TRUE),]
head(sorted_emean,10)
topTen = subset(sdf_harm,evRecode %in% sorted_emean[1:10,'evRecode'])

ggplot(topTen, aes(x=reorder(evRecode, FATALITIES+INJURIES, FUN=median), y=log2(FATALITIES+INJURIES))) + 
  geom_boxplot() + 
  coord_flip() + 
  ggtitle('Top 10 Events by Median Fatalities + Injuries per Event') + 
  labs(x='Event',y='log2(Fatalities + Injuries)')


## ----econimpactsubset----------------------------------------------------
# select events that have greater-than-zero values for eiher PROPDMG or CROPDMG
sdf_econ = subset(sdf, PROPDMG > 0 | CROPDMG > 0)

# recode event type same as above - first to lower case and then using the event recode table
sdf_econ$EVTYPE = tolower(as.character(sdf_econ$EVTYPE))
sdf_econ$evRecode = sapply(as.character(sdf_econ$EVTYPE), rcf)


## ----totalvalue----------------------------------------------------------
# Since the exponent field is coded using both numbers and letters we use the following 
# function to translate it to a number.

exptbl = data.frame(
  name=c("h","H","k","K","m","M","b","B"),
  value=c(2,2,3,3,6,6,9,9))

exptr = function(x) {
  n = suppressWarnings(as.numeric(x))
  if (!is.na(n)) {
    n
  } else {
    i = match(x,exptbl[,"name"])
    if (!is.na(i)) {
      exptbl[i,"value"]
    } else {
      0
    }
  }
}

# translate property damage exponents
propExpVal = sapply(sdf_econ$PROPDMGEXP, exptr)
# translate crop damage exponents
cropExpVal = sapply(sdf_econ$CROPDMGEXP, exptr)

# calculate total damage value per record
sdf_econ$totalValue = (sdf_econ$PROPDMG * 10^propExpVal) + (sdf_econ$CROPDMG * 10^cropExpVal)

# do summary statistics
etab3 = ddply(sdf_econ, .(evRecode), summarize, total=sum(totalValue),median=median(totalValue),mean=mean(totalValue))


## ----showtoptotal--------------------------------------------------------
head(etab3[order(etab3$total, decreasing=TRUE),],10)


## ----showtopmean---------------------------------------------------------
head(etab3[order(etab3$mean, decreasing=TRUE),],10)

