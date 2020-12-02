#bull shark paper 
#### Load packages ####
library(devtools)
library(dplyr)
library(stringr)
library(glatos)
library(lubridate)
library(plotly)
library(yaml)
library(ggplot2)
library(plotrix)
library(reshape)
library(MASS)

#### Load data ####
detections_2015_MIA<-glatos::read_otn_detections("G:/My Drive/Acoustic Telemetry/acoustic_projects/Bull_Shark_Paper/Data/v2lurb_matched_detections_2015.csv")
detections_2016_MIA<-glatos::read_otn_detections("G:/My Drive/Acoustic Telemetry/acoustic_projects/Bull_Shark_Paper/Data/v2lurb_matched_detections_2016.csv")
detections_2017_MIA<-glatos::read_otn_detections("G:/My Drive/Acoustic Telemetry/acoustic_projects/Bull_Shark_Paper/Data/v2lurb_matched_detections_2017.csv")
detections_2018_MIA<-glatos::read_otn_detections("G:/My Drive/Acoustic Telemetry/acoustic_projects/Bull_Shark_Paper/Data/v2lurb_matched_detections_2018.csv")
detections_2019_MIA<-glatos::read_otn_detections("G:/My Drive/Acoustic Telemetry/acoustic_projects/Bull_Shark_Paper/Data/v2lurb_matched_detections_2019.csv")
detections_2020_OTN<-glatos::read_otn_detections("G:/My Drive/Acoustic Telemetry/acoustic_projects/Bull_Shark_Paper/Data/v2lurb_matched_detections_2020.csv")

#detections_fact_2015<-glatos::read_otn_detections("G:/My Drive/Acoustic Telemetry/acoustic_projects/Bull_Shark_Paper/Data/v2lurb_matched_detections_on_fact_2015.csv")
#detections_fact_2016<-glatos::read_otn_detections("G:/My Drive/Acoustic Telemetry/acoustic_projects/Bull_Shark_Paper/Data/v2lurb_matched_detections_on_fact_2016.csv")
#detections_fact_2017<-glatos::read_otn_detections("G:/My Drive/Acoustic Telemetry/acoustic_projects/Bull_Shark_Paper/Data/v2lurb_matched_detections_on_fact_2017.csv")
#detections_fact_2018<-glatos::read_otn_detections("G:/My Drive/Acoustic Telemetry/acoustic_projects/Bull_Shark_Paper/Data/v2lurb_matched_detections_on_fact_2018.csv")
#detections_fact_2019<-glatos::read_otn_detections("G:/My Drive/Acoustic Telemetry/acoustic_projects/Bull_Shark_Paper/Data/v2lurb_matched_detections_on_fact_2019.csv")

detects_non_otn<-glatos::read_otn_detections("G:/My Drive/Acoustic Telemetry/acoustic_projects/Bull_Shark_Paper/Data/master_outside_detects.csv")

#set up 2020 vue data
detections_2020_Mia <- read.csv("G:/My Drive/Acoustic Telemetry/acoustic_projects/sharks_tarpon_griffin/VUE_Export.csv") #load data
detections_2020_Mia$Receiver <- as.numeric(substring(detections_2020_Mia$Receiver, 6)) #remove the "VR2W-" from receiver row
detections_2020_Mia <- detections_2020_Mia[,1:3]

miami_sharks <- read.csv("G:/My Drive/Acoustic Telemetry/Networks/OTN/OTN_Tag/Miami/OTN_metadata_MIA_update_07.27.2020.csv")
miami_sharks$transmitter_id <- with(miami_sharks, paste0(TAG_CODE_SPACE, "-", TAG_ID_CODE)) #reformat transmitter code

miami_sharks <- miami_sharks[,c("COMMON_NAME_E","SCIENTIFIC_NAME", "transmitter_id", "TAG_OWNER_ORGANIZATION", "TAG_CODE_SPACE")]

otn_deployments <- read.csv("G:/My Drive/Acoustic Telemetry/Networks/OTN/OTN_receiver/Miami/V2LURB_deployments_plus_hobos_07.13.2020.csv")
otn_deployments <- otn_deployments %>% filter(year(as.Date(as.character(otn_deployments$RECOVER_DATE_TIME..yyyy.mm.ddThh.mm.ss.), "%m/%d/%Y %H:%M")) == 2020)
otn_deployments <- otn_deployments[,c("OTN_ARRAY", "OTN_NO", "DEPLOY_LAT", "DEPLOY_LONG", "INS_SERIAL_NO")]

names(detections_2020_Mia) <- c("detection_timestamp_utc", "receiver_sn", "transmitter_id")
names(miami_sharks) <- c("common_name_e","scientificname","transmitter_id","citation", 'transmitter_codespace')
names(otn_deployments)<- c("glatos_array", "station", "deploy_lat","deploy_long","receiver_sn")

detections_2020_MIA <- left_join(detections_2020_Mia, miami_sharks, by = "transmitter_id")
detections_2020_MIA <- left_join(detections_2020_MIA, otn_deployments, by = "receiver_sn")

detections_2020_MIA$detection_timestamp_utc <- as.POSIXct(detections_2020_MIA$detection_timestamp_utc, "%Y-%m-%d %H:%M:%S", tz="UTC")
detections_2020_MIA$receiver_sn <- as.character(detections_2020_MIA$receiver_sn)
detections_2020_MIA$detectedby <- "V2LURB" 

#### Set up data for analysis ####

#make sure to take out data from arendt et al. before adding on FACT 2019
#filter out 2017 deployment from the detections 2015-2017

#bind data together
detections <- bind_rows(detections_2015_MIA, detections_2016_MIA, detections_2017_MIA, 
                        detections_2018_MIA,detections_2019_MIA, detections_2020_MIA,
                        detections_2020_OTN,
                        detects_non_otn)


#filter out bull sharks
bull <- detections %>% filter(common_name_e=='bull shark') 

#write.csv(bull, file="C:/Users/mitch/Desktop/Outside_Detection_Project/bull.csv")
bull <- bull %>% filter(!detectedby %in% c("Angela Collins", "Michael Arendt","Mike McCallister", "Joy Young", "Eric Reyier"))

#example code for removing duplicates: bull[!duplicated(bull[c('detection_timestamp_utc','transmitter_id','station')]),]
bull<-bull[!duplicated(bull[c('detection_timestamp_utc','transmitter_id')]),]

#filter false detections
bull <- glatos::false_detections(bull, tf = 3600)
bull <- bull %>% filter(passed_filter != FALSE)

#before we move on to some analyses, we need to make sure that we are in the right timezone
bull <- mutate(bull, detection_timestamp_et = with_tz(detection_timestamp_utc,  tz = "America/New_York"))

#for detection event purposes
bull$animal_id <- bull$transmitter_id

#number of detections per individual
bull_number_detection <- bull %>% group_by(transmitter_id) %>% count(transmitter_id)

#number of days detected per individual
bull_detection_days <- bull %>% group_by(transmitter_id) %>% summarise(days_detected=n_distinct(as.Date(detection_timestamp_et)))

#filter out bulls detected less than 10 days
bull <- bull %>% filter(!transmitter_id %in% c("A69-1601-58392","A69-1601-58393","A69-1601-23347",
                                               "A69-9001-20562","A69-9001-18422"))

#saveRDS file for Laura
saveRDS(bull, "G:/My Drive/Acoustic Telemetry/acoustic_projects/Bull_Shark_Paper/Data/detection_data_full.rds")

#unique stations 
bull_stations <- unique(bull[c("station","deploy_lat","deploy_long","region")])
write.csv(bull_stations, file = "G:/My Drive/Acoustic Telemetry/acoustic_projects/Bull_Shark_Paper/Data/bull_stations.csv")

#### abacus plot with anamolies ####
anomaly<-readRDS("G:/My Drive/Acoustic Telemetry/acoustic_projects/Bull_Shark_Paper/Data/For Monthly BB Residency/BB_chla_anomalies_comparedtoallyears_20152020.rds")
str(anomaly)

anomaly$time<-as.Date(anomaly$time)
anomaly<-rename(anomaly, detection_timestamp_et=time)
bull$detection_timestamp_et<-as.Date(bull$detection_timestamp_et)
bull_bay<-bull%>% filter(detectedby == "V2LURB")
bull_anomaly<-left_join(anomaly,bull_bay[,c("transmitter_id","detection_timestamp_et")], by="detection_timestamp_et")

ggplot(bull_anomaly, aes(x=detection_timestamp_et, y=transmitter_id)) + geom_point() 

ggplot(bull_anomaly, aes(x=detection_timestamp_et, y=anom, color=transmitter_id)) +
  geom_point()


#### Detection Events ####
#now we can look at the detection events that happen at each station
#this adds the first detection time, last detection time, number of detections, and residence time in seconds
detection_events_bull <- glatos::detection_events(bull, location_col = 'region', time_sep =  9676800)

#we can create an interval column which we can use further to see if there is any overlap amongst individuals
detection_events_bull <- detection_events_bull %>% mutate(detection_interval = lubridate::interval(first_detection, last_detection))

write.csv(detection_events_bull, "G:/My Drive/Acoustic Telemetry/acoustic_projects/Bull_Shark_Paper/Data/connectivity.csv")

#going back now to the original detection events data sheet
#lets summarize the data and find the number of detections, number of unique individuals
#the total residence time, mean lat and long for each station
summary_data_bull <- detection_events_bull %>% group_by(location) %>% summarise(detection_count = sum(num_detections),
                                                                                num_unique_tags = n_distinct(animal_id),
                                                                                total_residence_time_in_seconds = sum(detection_interval),
                                                                                latitude = mean(mean_latitude),
                                                                                longitude=mean(mean_longitude))


#### Add maturity category ####
bull_mature <- bull %>% filter(transmitter_id %in% c("A69-1601-24655","A69-1601-24661", "A69-9001-20773", "A69-9001-20563",
                                                     "A69-9001-18421","A69-9001-18419", "A69-9001-18413", "A69-9001-16325"))
bull_mature$maturity <- "mature"

bull_immature <- bull %>% filter(transmitter_id %in% c("A69-1601-24660","A69-1601-58403","A69-1601-58396", "A69-9001-18401", 
                                                       "A69-9001-16328", "A69-9001-18415", "A69-9001-13487"))
bull_immature$maturity <- "immature"

#combine it all together
bull <- bind_rows(bull_mature, bull_immature)

#### Add region ####
#Now that we know everywhere the bull sharks are being detected, we would like to sort data out by region and then look at the monthly residence 
#index to see if the movement is truly seasonal
bull_north_gulf <- bull %>% filter(between(deploy_lat, 29.10, 33) & between(deploy_long, -89, -85.7)) 
bull_north_gulf$region <- "North Gulf"

bull_Tampa_gulf <- bull %>% filter(between(deploy_lat, 27.2, 28.6) & between(deploy_long, -84.8, -82.606)) 
bull_FortMyers_gulf <- bull %>% filter(between(deploy_lat, 26.50, 26.95) & between(deploy_long, -82.46, -82.22)) 

bull_lower_keys <- bull %>% filter(between(deploy_lat, 24, 24.96) & between(deploy_long, -82.3, -80.95))
bull_lower_keys$region<-'Lower Keys'

bull_upper_keys <- bull %>% filter(between(deploy_lat, 24.8, 25.2) & between(deploy_long, -80.78, -80)) 
bull_upper_keys$region<-'Upper Keys'

bull_biscayne_bay <- bull %>% filter(detectedby=='V2LURB') 
bull_biscayne_bay$region <- 'Biscayne Bay'

bull_centralFL_atlantic <- bull %>% filter(between(deploy_lat, 26.3, 28.5) & between(deploy_long, -80.8, -78)) 
bull_centralFL_atlantic$region <- 'Central FL Atlantic'

bull_NorthFL_GA_SC <- bull %>% filter(between(deploy_lat, 29.6, 33.6) & between(deploy_long, -81.5, -77)) 
bull_NorthFL_GA_SC$region <- 'North FL to SC'

bull_Chesapeake_North <- bull %>% filter(between(deploy_lat, 33.7, 50)) 
bull_Chesapeake_North$region <- 'Chesapeake Maryland'

#decided to combine tampa and fort myers regions into 'middle gulf'
bull_middle_gulf<-bind_rows(bull_Tampa_gulf, bull_FortMyers_gulf)
bull_middle_gulf$region<-"Middle Gulf"
bull_keys<-bind_rows(bull_upper_keys, bull_lower_keys)
bull_keys$region<-"Florida Keys"

bull<-bind_rows(bull_keys, bull_biscayne_bay,bull_north_gulf,bull_middle_gulf,
                bull_centralFL_atlantic,bull_NorthFL_GA_SC,bull_Chesapeake_North)

#count number of stations detected on by region and total 
bull %>% group_by(region) %>% summarise(n=n_distinct(station))
bull %>%  summarise(n=n_distinct(station))


#### Calculate Residency ####

#now we can look at the nubmer of days each individual is detected each month in each region
bull_detection_days_1 <- bull_north_gulf %>% 
  group_by(month=floor_date(detection_timestamp_et, "month"), transmitter_id) %>% 
  summarise(days_detected=n_distinct(as.Date(detection_timestamp_et)))
bull_detection_days_1$region <- "North Gulf"

bull_detection_days_2 <- bull_Tampa_gulf %>% 
  group_by(month=floor_date(detection_timestamp_et, "month"), transmitter_id) %>% 
  summarise(days_detected=n_distinct(as.Date(detection_timestamp_et)))
bull_detection_days_2$region <- "Tampa"

bull_detection_days_2 <- bull_middle_gulf %>% 
  group_by(month=floor_date(detection_timestamp_et, "month"), transmitter_id) %>% 
  summarise(days_detected=n_distinct(as.Date(detection_timestamp_et)))
bull_detection_days_2$region <- "Middle Gulf"

bull_detection_days_3 <- bull_FortMyers_gulf %>% 
  group_by(month=floor_date(detection_timestamp_et, "month"), transmitter_id) %>% 
  summarise(days_detected=n_distinct(as.Date(detection_timestamp_et)))
bull_detection_days_3$region <- "Fort Myers"

bull_detection_days_4 <- bull_lower_keys %>% 
  group_by(month=floor_date(detection_timestamp_et, "month"), transmitter_id) %>% 
  summarise(days_detected=n_distinct(as.Date(detection_timestamp_et)))
bull_detection_days_4$region <- "Lower Keys"

bull_detection_days_5 <- bull_upper_keys %>% 
  group_by(month=floor_date(detection_timestamp_et, "month"), transmitter_id) %>% 
  summarise(days_detected=n_distinct(as.Date(detection_timestamp_et)))
bull_detection_days_5$region <- "Upper Keys"

bull_detection_days_keys <- bull_keys %>% 
  group_by(month=floor_date(detection_timestamp_et, "month"), transmitter_id) %>% 
  summarise(days_detected=n_distinct(as.Date(detection_timestamp_et)))
bull_detection_days_keys$region <- "Florida Keys"

bull_detection_days_6 <- bull_biscayne_bay %>% 
  group_by(month=floor_date(detection_timestamp_et, "month"), transmitter_id) %>% 
  summarise(days_detected=n_distinct(as.Date(detection_timestamp_et)))
bull_detection_days_6$region <- "Biscayne Bay"

bull_detection_days_7 <- bull_centralFL_atlantic %>% 
  group_by(month=floor_date(detection_timestamp_et, "month"), transmitter_id) %>% 
  summarise(days_detected=n_distinct(as.Date(detection_timestamp_et)))
bull_detection_days_7$region <- "Central FL Atlantic"

bull_detection_days_8 <- bull_NorthFL_GA_SC %>% 
  group_by(month=floor_date(detection_timestamp_et, "month"), transmitter_id) %>% 
  summarise(days_detected=n_distinct(as.Date(detection_timestamp_et)))
bull_detection_days_8$region <- "North FL to SC"

bull_detection_days_9 <- bull_Chesapeake_North %>% 
  group_by(month=floor_date(detection_timestamp_et, "month"), transmitter_id) %>% 
  summarise(days_detected=n_distinct(as.Date(detection_timestamp_et)))
bull_detection_days_9$region <- "Chesapeake Bay and North"


#bind all these data frames together to make it easier to manage
bull_residency <- bind_rows(bull_detection_days_1, bull_detection_days_2, bull_detection_days_keys,
                            bull_detection_days_6, bull_detection_days_7, bull_detection_days_8, bull_detection_days_9)


#bull_residency <- bind_rows(bull_gulf_detection_days,bull_keys_detection_days, bull_atlantic_detection_days)
bull_residency$days_in_month <- days_in_month(bull_residency$month)
bull_residency$month_name <- month(bull_residency$month)

#find the number of days at liberty for each shark
miami_sharks <- read.csv("G:/My Drive/Acoustic Telemetry/Networks/OTN/OTN_Tag/Miami/OTN_metadata_MIA_update_07.27.2020.csv")
miami_sharks$transmitter_id <- with(miami_sharks, paste0(TAG_CODE_SPACE, "-", TAG_ID_CODE)) #reformat transmitter code
miami_sharks <- miami_sharks %>% filter(COMMON_NAME_E == "bull shark")
miami_sharks <- miami_sharks[,c("transmitter_id","EST_TAG_LIFE","UTC_RELEASE_DATE_TIME", "LENGTH..m.")]
miami_sharks <- miami_sharks %>% mutate(est_end_date = as.Date(UTC_RELEASE_DATE_TIME)+EST_TAG_LIFE, 
                        days_at_liberty = as.Date("2020-05-27")-as.Date(UTC_RELEASE_DATE_TIME))
miami_sharks <- miami_sharks %>% mutate(tag_end_date = if_else(est_end_date>as.Date("2020-05-27"), as.Date("2020-05-27"), est_end_date)) %>% 
  filter(!transmitter_id %in% c("A69-1602-56661","A69-1602-56664"))

#now create a list where each month at liberty is in a sequence
date_list <- data.frame()
for (i in miami_sharks$transmitter_id){
  month<-seq(as.Date(miami_sharks$UTC_RELEASE_DATE_TIME[miami_sharks$transmitter_id==i]), 
              as.Date(miami_sharks$tag_end_date[miami_sharks$transmitter_id==i]), by="months")
  transmitter_id<-i
  df <- data.frame(transmitter_id,month)
  date_list <- rbind(date_list,df)
}
#make sure to take out first and last month 
date_list <- date_list %>% group_by(transmitter_id) %>% filter(!month %in% c(min(month),max(month)))
#create a regions vector
region <- c("North Gulf", "Middle Gulf", "Florida Keys", "Biscayne Bay", "Central FL Atlantic",
             "North FL to SC","Chesapeake Bay and North")

#make all combos of transmitter, month at liberty, and region
tag_date_combos<-merge(date_list, as.data.frame(region))
tag_date_combos<-tag_date_combos %>% filter(!transmitter_id %in% c("A69-9001-20774","A69-9001-18418","A69-1601-58392",
                                                  "A69-1601-58393","A69-1601-58402","A69-1601-23347"))

#calculate residency as number of days detected divided by the number of days per month 
#make sure variables are formatted correctly
tag_date_combos$month<-as.POSIXct(tag_date_combos$month)
tag_date_combos$month<-floor_date(tag_date_combos$month, unit="month")
tag_date_combos$region <- as.character(tag_date_combos$region)
tag_date_combos$transmitter_id <- as.character(tag_date_combos$transmitter_id)

#doulbe check format
str(tag_date_combos)

#compute monthly and overall residencies for each shark in each month in each region 
bull_release <- detections_2018_MIA %>% filter(receiver_sn == 'release' & common_name_e == 'bull shark') %>% 
  mutate(days_at_liberty = as.Date("2020-05-27")-as.Date(detection_timestamp_utc))
bull_release <- bull_release[,c("transmitter_id","detection_timestamp_utc","days_at_liberty")]

bull_residency <- left_join(bull_residency, bull_release, by="transmitter_id")

bull_residency$days_liberty <- as.Date("2020-05-27")-as.Date(bull_residency$detection_timestamp_utc)

bull_residency$overall_ri<-bull_residency$days_detected/as.numeric(bull_residency$days_liberty)
bull_residency$montly_ri <- bull_residency$days_detected/bull_residency$days_in_month

#join template table and data frame on residency I created 
new_bull <- left_join(tag_date_combos, bull_residency, by=c("month", "transmitter_id", "region"))

#Make sure to fill NA's with zeros to be able to perform analyses
new_bull$days_detected[is.na(new_bull$days_detected)] <- 0
new_bull$overall_ri[is.na(new_bull$overall_ri)] <- 0
new_bull$montly_ri[is.na(new_bull$montly_ri)] <- 0

#fill in the blanks for days in month and month name
new_bull$days_in_month <- days_in_month(new_bull$month)
new_bull$month_name <- month(new_bull$month)
saveRDS(new_bull,"G:/My Drive/Acoustic Telemetry/acoustic_projects/Bull_Shark_Paper/Data/bull_residencies.rds")

#average the monthly RI's for each shark and then average them among all sharks 
new_bull_summary_monthly <- new_bull %>% group_by(transmitter_id, region, month_name) %>% 
  summarize(mean_ri=mean(montly_ri)) %>%
  group_by(region, month_name) %>%
  summarize(mean_shark_ri=mean(mean_ri), std_ri = std.error(mean_ri))

#Now we have to make sure the variables are in the right format before we can go ahead and ggplot these data
new_bull_summary_monthly$month_name <- as.factor(new_bull_summary_monthly$month_name)
new_bull_summary_monthly$region <- as.factor(new_bull_summary_monthly$region)

#now we plot!!!
neworder <- c("North Gulf", "Chesapeake Bay and North", "Middle Gulf",  "North FL to SC", "Florida Keys", 
              "Central FL Atlantic", "Biscayne Bay" )
new_bull_summary_monthly <- arrange(transform(new_bull_summary_monthly,
                           region=factor(region,levels=neworder)),region)

ggplot(new_bull_summary_monthly, aes(x=new_bull_summary_monthly$month_name, y=new_bull_summary_monthly$mean_shark_ri, fill=region)) +
  geom_bar(stat="identity") +
  facet_wrap(~region, ncol=2, scale="free_y") + labs(y="Residency Index", x="Month", title ="Monthly Residency") + 
  geom_errorbar(aes(ymax=mean_shark_ri+std_ri, ymin=mean_shark_ri)) + 
  scale_fill_manual(breaks = c("North Gulf", "Middle Gulf", "Florida Keys", "Biscayne Bay", "Central FL Atlantic Coast",
                     "North FL to SC", "Chesapeake Bay and North"),
                    values=c("purple", "black", "blue", "brown","green","red","yellow","orange"))



#### Environmental data ####
enviro_bulls<-readRDS("G:/My Drive/Acoustic Telemetry/acoustic_projects/Bull_Shark_Paper/Data/For Environmental Analysis/acousticdata_withSSTanddepthandchla_subset.rds")
enviro_bulls<-enviro_bulls %>% rename(deploy_long=Longitude,deploy_lat=Latitude)
library(mapview)
coordinates(enviro_bulls) <- c("deploy_long","deploy_lat")
proj4string(enviro_bulls) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
mapview(enviro_bulls)
#label by region
env_north_gulf <- enviro_bulls %>% filter(between(deploy_lat, 29.10, 33) & between(deploy_long, -89, -85.7)) 
env_north_gulf$region <- "North Gulf"

env_Tampa_gulf <- enviro_bulls %>% filter(between(deploy_lat, 27.2, 28.6) & between(deploy_long, -84.8, -82.606)) 
env_FortMyers_gulf <- enviro_bulls %>% filter(between(deploy_lat, 26.50, 26.95) & between(deploy_long, -82.46, -82.22)) 

env_lower_keys <- enviro_bulls %>% filter(between(deploy_lat, 24, 24.96) & between(deploy_long, -82.3, -80.95))
env_lower_keys$region<-'Lower Keys'

env_upper_keys <- enviro_bulls %>% filter(between(deploy_lat, 24.8, 25.2) & between(deploy_long, -80.78, -80)) 
env_upper_keys$region<-'Upper Keys'

env_biscayne_bay <- enviro_bulls %>% filter(DetectedBy=='V2LURB') 
env_biscayne_bay$region <- 'Biscayne Bay'

env_centralFL_atlantic <- enviro_bulls %>% filter(between(deploy_lat, 26.3, 28.5) & between(deploy_long, -80.8, -78)) 
env_centralFL_atlantic$region <- 'Central FL Atlantic'

env_NorthFL_GA_SC <- enviro_bulls %>% filter(between(deploy_lat, 29.6, 33.6) & between(deploy_long, -81.5, -77)) 
env_NorthFL_GA_SC$region <- 'North FL to SC'

env_Chesapeake_North <- enviro_bulls %>% filter(between(deploy_lat, 33.7, 50)) 
env_Chesapeake_North$region <- 'Chesapeake Maryland'

#decided to combine tampa and fort myers regions into 'middle gulf'
env_middle_gulf<-bind_rows(env_Tampa_gulf, env_FortMyers_gulf)
env_middle_gulf$region<-"Middle Gulf"
env_keys<-bind_rows(env_upper_keys, env_lower_keys)
env_keys$region<-"Florida Keys"

env<-bind_rows(env_keys, env_biscayne_bay,env_north_gulf,env_middle_gulf,
               env_centralFL_atlantic,env_NorthFL_GA_SC,env_Chesapeake_North)

#average SST and ChlA per month per region and link to residency data
env<-env %>% mutate(month_floor=floor_date(Time_UTC, unit="month")) %>% 
                 group_by(AnimalID, month_floor, region) %>% 
                 summarise(meanSST=mean(SST), meanCHLA=mean(chla), varSST=var(SST), varCHLA=var(chla))

#### GAMM Prep ####
str(new_bull)
new_bull$month_name<-as.factor(new_bull$month_name) #make month name a factor
new_bull$region<-as.factor(new_bull$region)
new_bull$sex<-as.factor(new_bull$sex)
new_bull$transmitter_id<-as.factor(new_bull$transmitter_id)

miami_sharks <- read.csv("G:/My Drive/Acoustic Telemetry/Networks/OTN/OTN_Tag/Miami/OTN_metadata_MIA_update_07.27.2020.csv")
miami_sharks$transmitter_id <- with(miami_sharks, paste0(TAG_CODE_SPACE, "-", TAG_ID_CODE)) #reformat transmitter code
miami_sharks <- miami_sharks %>% filter(COMMON_NAME_E == "bull shark")
tagged_bull_summary<-miami_sharks[,c("transmitter_id","SEX","LENGTH..m.")]
names(tagged_bull_summary)<-c("transmitter_id","sex","total_length")
new_bull<-left_join(new_bull, tagged_bull_summary, by="transmitter_id")
#save file 
saveRDS(new_bull,"G:/My Drive/Acoustic Telemetry/acoustic_projects/Bull_Shark_Paper/Data/bull_residencies.rds")

new_bull<-new_bull %>% filter(!montly_ri==0) %>% rename(AnimalID=transmitter_id, month_floor=month) 
new_bull$month_floor<-as.Date(new_bull$month_floor)
env$month_floor<-as.Date(env$month_floor)

enviro_bull_join <- left_join(new_bull, env, by=c("AnimalID","month_floor","region"))

enviro_bull_join$region<-as.factor(enviro_bull_join$region)
enviro_bull_join$season<-as.factor(enviro_bull_join$season)
enviro_bull_join$month_name<-as.factor(enviro_bull_join$month_name)

unique(enviro_bull_join$region)
enviro_bull_join<-enviro_bull_join %>% mutate(season=ifelse(month_name %in% c(11,12,1,2,3,4), "dry", "wet"))
enviro_bull_join$season<-as.factor(enviro_bull_join$season)

bull_biscayne <- enviro_bull_join %>% filter(region=="Biscayne Bay") #13
bull_CFL <- enviro_bull_join %>% filter(region=="Central FL Atlantic") #7
bull_NFL<- enviro_bull_join %>% filter(region=="North FL to SC") #4
bull_MG<- enviro_bull_join %>% filter(region=="Middle Gulf") #13
bull_NG <- enviro_bull_join %>% filter(region %in% c("North Gulf")) #6
bull_keys <- enviro_bull_join %>% filter(region=="Florida Keys") #14

#### Biscayne Bay GAMM####
bull_model_biscayne<-gamm(days_detected ~ s(meanSST,k=5) + s(meanCHLA, k=5), random=list(AnimalID=~1),
                  family=poisson, data=bull_biscayne, method="REML")

summary(bull_model_biscayne$gam)
par(mfrow=c(1,2))
draw(bull_model_biscayne$gam)
plot(bull_model_biscayne$gam, seWithMean=T, shift = coef(bull_model_biscayne$gam)[1], all.terms=T)
gam.check(bull_model_biscayne$gam)
concurvity(bull_model_biscayne$gam, full=T)
concurvity(bull_model_biscayne$gam, full=F)


#GLMM for biscayne Bay residencies
new_bull$month_name<-as.factor(new_bull$month_name)
bbb<-new_bull %>% filter(region=="Biscayne Bay")
bull_biscayne_glmm <- glmer(days_detected ~ month_name + offset(log(days_in_month)) +(1|transmitter_id),
                            family=poisson, data=bbb)
summary(bull_biscayne_glmm)
plot(bull_biscayne_glmm)
emmeans(bull_biscayne_glmm, pairwise ~ month_name)


#### Middle Gulf GAMM####
bull_model_MG<-gamm(days_detected ~ s(meanSST,k=5) + s(meanCHLA, k=5), random=list(AnimalID=~1),
                          family=poisson, data=bull_MG, method="REML")

summary(bull_model_MG$gam)
par(mfrow=c(1,1))
#plot(bull_model_MG$gam, residuals=T, seWithMean=T, shift = coef(bull_model_MG$gam)[1], shade=T)
draw(bull_model_MG$gam)
gam.check(bull_model_MG$gam)
concurvity(bull_model_MG$gam)
concurvity(bull_model_MG$gam, full=F)

#### Keys GAMM####
hist(bull_keys$days_detected)
bull_model_Keys<-gamm(days_detected ~ s(meanSST,k=5) + s(meanCHLA, k=10), random=list(AnimalID=~1),
                    family=poisson, data=bull_keys, method="REML")


summary(bull_model_Keys$gam)
#plot(bull_model_Keys$gam,residuals=T, seWithMean=T, shift = coef(bull_model_Keys$gam)[1], shade=T)
draw(bull_model_Keys$gam)
gam.check(bull_model_Keys$gam)
concurvity(bull_model_Keys$gam)

#### CFL GAMM####
hist(bull_CFL$days_detected)
bull_model_CFL<-gamm(days_detected ~ s(meanSST,k=5) + s(meanCHLA, k=5), random=list(AnimalID=~1),
                      family=poisson, data=bull_CFL, method="REML")


summary(bull_model_CFL$gam)
plot(bull_model_CFL$gam, residuals=T, seWithMean=T, shift = coef(bull_model_CFL$gam)[1], shade=T)
draw(bull_model_CFL$gam)
vis.gam(bull_model_CFL$gam, plot.type = "contour", color="heat")
gam.check(bull_model_CFL$gam)
concurvity(bull_model_CFL$gam)

#### NFL GAMM####
hist(bull_NG$days_detected)
bull_model_NFL<-gamm(days_detected ~ s(meanSST,k=5) + s(meanCHLA, k=5), random=list(AnimalID=~1),
                     family=poisson, data=bull_NFL)


summary(bull_model_NFL$gam)
#plot(bull_model_NFL$gam, seWithMean=T, shift = coef(bull_model_NFL$gam)[1], shade=T)
draw(bull_model_NFL$gam)
gam.check(bull_model_NFL$gam)
concurvity(bull_model_NFL$gam)
plot.gam(bull_model_NFL$gam)

#### Abacus plot  ####
neworder <- c("North Gulf", "Chesapeake Maryland", "Middle Gulf",  "North FL to SC", "Florida Keys", 
              "Central FL Atlantic", "Biscayne Bay" )
bull <- arrange(transform(bull,region=factor(region,levels=neworder)),region)
bull$region<-as.factor(bull$region)
ggplot(data=bull, aes(x=detection_timestamp_utc, y=transmitter_id, color=region)) + 
  geom_point(aes(color=region)) + xlab("Detection Timestamp UTC") +ylab("Transmitter ID") +
  scale_color_manual(breaks = c("North Gulf", "Chesapeake Maryland", "Middle Gulf",  "North FL to SC", "Florida Keys", 
              "Central FL Atlantic", "Biscayne Bay" ), 
                    values=c("pink", "green", "yellow", "brown","orange","blue","red")) +theme_bw() + labs(color="Region")




#### CPUE Analysis ####
#catch_data<-readxl::read_xls("G:/My Drive/Acoustic Telemetry/acoustic_projects/Bull_Shark_Paper/Data/MiamiLines.xls")
#str(catch_data)
#unique(catch_data$Species)
#unique(catch_data$Month)
catch_data<-read.csv("G:/My Drive/Acoustic Telemetry/acoustic_projects/Bull_Shark_Paper/Data/bullcatches_SSSandSST.csv")
View(catch_data)
catch_data$Month<-as.factor(catch_data$Month)
catch_data$TL..cm.<-as.numeric(catch_data$TL..cm.)
catch_data$Sex..M.F.<-as.factor(catch_data$Sex..M.F.)

levels(catch_data$Sex..M.F.)[levels(catch_data$Sex..M.F.)=="Female"] <- "F"
levels(catch_data$Sex..M.F.)[levels(catch_data$Sex..M.F.)=="Male"] <- "M"

catch_data %>% filter(Species %in% c("Bull","BULL"), !Sex..M.F.==".", Stand.Nonstand == "Standard") %>% 
  mutate(season=ifelse(Month %in% c(5,6,7,8,9,10), "wet","dry")) %>% 
  ggplot(., aes(x=season, y=TL..cm., fill=Sex..M.F.)) + geom_boxplot() +
  facet_wrap(~Sex..M.F.) +geom_jitter() + ylab("TL (cm)") + xlab("Season") +
  labs(fill="Sex")

#count number of lines or sets per month and use that as the cpue
#use the floor date to get month 
catch_data$date <- as.Date(with(catch_data, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
lines_per_month <- catch_data %>% filter(Stand.Nonstand == "Standard") %>% 
  mutate(month_year=floor_date(date, unit="month")) %>% count(month_year)
#classify lengths by size class 220 for males and 230 for females 
age_class <- catch_data %>% filter(Species %in% c("Bull","BULL"),!Sex..M.F.==".",Stand.Nonstand == "Standard") %>% 
  mutate(month_year=floor_date(date, unit="month")) %>% 
  mutate(life_stage = case_when((TL..cm.>210 & Sex..M.F.=="M" ~ "mature"),
                                (TL..cm.>225 & Sex..M.F.=="F" ~ "mature"), TRUE ~ "juvenile"))
age_class_counts <- age_class %>% group_by(month_year, Sex..M.F.) %>% count(life_stage)
mean_monthly_temp<-catch_data %>% mutate(month_year=floor_date(date, unit="month")) %>% group_by(month_year) %>% summarise(mean_sst=mean(SST))

bull_cpue<-left_join(lines_per_month,age_class_counts, by="month_year")
bull_cpue<-left_join(bull_cpue,mean_monthly_temp, by="month_year")
bull_cpue<-bull_cpue %>% 
  mutate(cpue=n.y/n.x, month = as.factor(month(month_year)), year=year(month_year)) %>% 
  #filter(year %in% c(2015,2016,2017,2018)) %>% 
  mutate(season=ifelse(month %in% c(6,7,8,9,10), "wet","dry")) 
names(bull_cpue)<-c("month_year","lines_per_month","sex","life_stage","caught","monthly_sst","cpue","month","year","season")
#plot it out by sex, life stage, and season
ggplot(bull_cpue,aes(x=season,y=log(cpue)))+geom_boxplot()+facet_wrap(~ sex)

#create model for testing
hist(log(bull_cpue$cpue))#looks like natural log of cpue is normal? 
bull_cpue$sex<-as.factor(bull_cpue$sex)
bull_cpue$season<-as.factor(bull_cpue$season)
bull_cpue$year<-as.factor(bull_cpue$year)
bull_cpue$life_stage<-as.factor(bull_cpue$life_stage)

bull_cpue$season<-relevel(bull_cpue$season,ref="wet")
bull_cpue$sex<-relevel(bull_cpue$sex,ref="M")
bull_cpue$life_stage<-relevel(bull_cpue$life_stage,ref="mature")

cpue2<-lm(log(cpue)~season*sex*life_stage,
            data=bull_cpue)

summary(cpue2)
plot(cpue)
emmeans_cpue<-emmeans(cpue2,pairwise ~ sex*season*life_stage)
plot(emmeans_cpue, comparisons=T)
pwpp(emmeans_cpue$contrasts, method="pairwise")

cpue.ref1<-data.frame(season="dry",sex="M",life_stage="mature")
cpue.ref2<-data.frame(season="dry",sex="M",life_stage="juvenile")

#you can calculate from the summary table and backtransform, or get predictions using predict, and I think we just need the main effect, i.e., % difference as a covid effect which can then be weighted using the df and t-statistic
pred1<-predict(cpue2,cpue.ref1,level=0)
pred2<-predict(cpue2,cpue.ref2,level=0)

#estimate how much more abundant fish were in covid year, as a % increase
round(((exp(pred2)-exp(pred1))/exp(pred1))*100)

#plot out fishing locations 
library(mapview)
mapview(breweries)  
coordinates(new_catch_data) <- c("Longitude","Latitude")
proj4string(new_catch_data) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

mapview(new_catch_data, zcol="Year")


bull_cpue %>% group_by(month, sex) %>% summarise(mean_cpue=mean(cpue), mean_sst=mean(monthly_sst)) %>% 
  ggplot(., aes(x=month, y=mean_cpue, color=sex, group=sex)) + geom_point()+ geom_point(aes(x=month, y=mean_sst/600))+
  geom_line(aes(x=month, y=mean_cpue, color=sex)) + 
  geom_line(aes(x=month, y=mean_sst/600), linetype="dashed") + 
  scale_y_continuous(sec.axis = sec_axis(~.*600, name = "Mean SST (C)"))

hist((bull_cpue$monthly_sst))
ggplot(bull_cpue, aes(x=monthly_sst, y=log(cpue), color=life_stage, group=life_stage))+
  geom_point()+geom_smooth()+facet_wrap(~sex)


#Looking for recaps to see how much they have grown etc.
recap_bulls<-catch_data %>% filter(Species %in% c("Bull","BULL"), New.or.Recap %in% c("R","Recap"))
recap_tags<-unique(recap_bulls$NOAA.RJD)
catch_data %>% filter(NOAA.RJD %in% recap_tags, New.or.Recap %in% c("N","New")) %>% View()


#### Round trip map####
library(tidyr)
bull_round_trip<-read.csv("G:/My Drive/Acoustic Telemetry/acoustic_projects/Bull_Shark_Paper/Data/bull_round_trip_condensed.csv")
str(bull_round_trip)
data_long <- gather(bull_round_trip, region, trips, 
                    Florida.Keys:FL.GoM.:North.GoM:Central.FL.Atlantic:North.FL.to.SC:Chesapeake.Maryland, factor_key=TRUE)
ggplot(data_long, aes(x=as.factor(Days.at.Liberty), y=trips, fill=region)) + geom_bar(stat="identity", position="dodge") 

