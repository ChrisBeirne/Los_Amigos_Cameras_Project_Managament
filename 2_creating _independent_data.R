#Load Packages
list.of.packages <- c("leaflet", "dplyr", "colortools", "kriging", "corrplot", "lubridate", "kableExtra", "rredlist","sf", "usedist", "ggplot2", "ggpubr", "googledrive", "purrr", "plotly", "googlesheets4")

# Check you have them and load them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)


# Create independent data

dat <- read.csv("data/processed-data/LosAmigos_all_image_data.csv", header=T)

# Interval for independence in minutes
independent <- 10

# Remove observations without animals detected
dat <- dat[dat$is_blank==0 & dat$species!="No CV Result",]

# Order the framed by deployment_id, date
dat <- dat[order(dat$deployment_id, dat$timestamp),]


### NEW WAY
dat <- dat %>%
  #filter(Species == i) %>%
  arrange(deployment_id) %>%
  group_by(deployment_id, wi_taxon_id) %>%
  mutate(duration = int_length(timestamp %--% lag(timestamp)))


# loop that assigns group ID
dat$event_id <- 9999
mins <- independent
seq <- as.numeric(paste0(nrow(dat),0))
seq <- round(seq,-(nchar(seq)))
for (i in 2:nrow(dat)) {
  dat$event_id[i-1]  <- paste0("E",format(seq, scientific = F))
  if(is.na(dat$duration[i]) | abs(dat$duration[i]) > (mins * 60)){
    seq <- seq + 1
  }
}

# Update the information for the last row
# group ID  for the last row
if(dat$duration[nrow(dat)] < (mins * 60)|
   is.na(dat$duration[nrow(dat)])){
  dat$event_id[nrow(dat)] <- dat$event_id[nrow(dat)-1]
} else{
  dat$event_id[nrow(dat)] <- paste0("E",format(seq+1, scientific = F))
}
# Calculate the event length and size

# find out the last and the first of the time in the group
top <- dat %>% group_by(event_id) %>% top_n(1,timestamp) %>% select(event_id, timestamp)
bot <- dat %>% group_by(event_id) %>% top_n(-1,timestamp) %>% select(event_id, timestamp)
names(bot)[2] <- c("timestamp_end")
dec_no <- dat %>% group_by(event_id) %>% summarise(n())
event_grp <- dat %>% group_by(event_id) %>% summarise(max(number_of_objects))

# calculate the duration
diff <-  top %>% left_join(bot, by="event_id") %>%
  mutate(duration=abs(int_length(timestamp %--% timestamp_end))) %>%
  left_join(event_grp, by="event_id")%>%
  left_join(dec_no, by="event_id")

# Remove duplicates
diff <- diff[duplicated(diff)==FALSE,]
head(diff)
names(diff) <- c("event_id","timestamp","timestamp_end","duration","min_count","event_observations")
diff$timestamp<-NULL;diff$timestamp_end<-NULL
dat$duration <-NULL

# Merge the data
dat_2 <-  dat %>%
  left_join(diff,by="event_id")

# Subset to the first observation in each event

# Subset to independent observations using your chosen threshold
ind_dat <- dat_2[!duplicated(dat_2$event_id),]
ind_dat <- as.data.frame(ind_dat)

tmp <- eff[is.na(eff$end_date)==F,]
daily.lookup <- list()
i <- 1
for(i in 1:nrow(tmp))
{
  if(as.Date(tmp$start_date[i])!=as.Date(tmp$end_date[i]))
  {
    daily.lookup[[i]] <- data.frame("Date"=seq(as.Date(tmp$start_date[i]), as.Date(tmp$end_date[i]), by="days"), "deployment_id"=tmp$deployment_id[i])
  }
}
row.lookup <- do.call(rbind, daily.lookup)

# Remove duplicates
row.lookup <- row.lookup[duplicated(row.lookup)==F,]


# Save it for a rainy day
write.csv(ind_dat, paste0("data/processed-data/LosAmigos_",independent ,"min_Independent.csv"), row.names = F)

# Also export the effort lookup
write.csv(row.lookup, paste0("data/processed-data/LosAmigos_daily_effort_lookup.csv"), row.names = F)

# Write to googledrive


library(googledrive)
drive_auth()
1
# and export to drive
folder_url <- "https://drive.google.com/drive/u/0/folders/1-is1Mbwqg-bZVVBYfDX8lRMYSnXoGAbd"

# Data
drive_upload(
  paste0("data/processed-data/LosAmigos_",independent ,"min_Independent.csv"),
  as_id(folder_url), overwrite=T
)

# Lookup
drive_upload(
  paste0("data/processed-data/LosAmigos_daily_effort_lookup.csv"),
  as_id(folder_url), overwrite=T
)

