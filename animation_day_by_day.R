# David Ebert
# 1 November 2016
# Updated 7 April 2017

# Make an animation of tweet times in LA county.
# Make a bunch of .png files, then use image magick to create .gif
# Idea from https://www.r-bloggers.com/animated-plots-with-r/

library(ggmap)
library(feather)
library(gridExtra)
library(ggthemes)


#############################################################
### Import 4 weeks of data with time set
#############################################################

all_august_pred_import = read_feather(path = "~/Desktop/Huang Research/LAR_Data/all_august_pred.feather")


# shorten to a random 1 million - optional
# all_august_pred_import = all_august_pred_import[sample(1:nrow(all_august_pred_import),1000000),]

# convert to POSIXct time and fix time zone (subtract 7 hours to get to PDT time zone)
head(all_august_pred_import$created_at)
all_august_pred_import$created_at = strptime(all_august_pred_import$created_at, "%a %b %d %H:%M:%S %z %Y", tz = "GMT")
                                                                                #"Mon Aug 01 05:09:35 +0000 2016"
head(all_august_pred_import$created_at)
all_august_pred_import$created_at = as.POSIXct(all_august_pred_import$created_at, tz = "GMT")
head(all_august_pred_import$created_at)
all_august_pred_import$created_at = format(all_august_pred_import$created_at, tz = "America/Los_Angeles", usetz = TRUE)

all_august_pred_import$created_at = strptime(all_august_pred_import$created_at, "%Y-%m-%d %H:%M:%S", tz = "PDT")
                                                                              #"2016-07-31 22:09:35 PDT"
head(all_august_pred_import$created_at) # Time should now start at 10:09 PM on the 31st, not 5 AM on the 1st
                                        # Note that this makes sense since we cut off tweet collection just after midnight CST = 5 AM GMT = 10 PM PDT 

# Use help(POSIXlt) to see subsetting types
table(all_august_pred_import$created_at$wday)
table(all_august_pred_import$created_at$mday)
table(all_august_pred_import$created_at$mon)

# Select exactly 4 weeks of data:
all_august_pred_import = all_august_pred_import[all_august_pred_import$created_at$mon==7,] # remove a few tweets from July
all_august_pred_import = all_august_pred_import[all_august_pred_import$created_at$mday>=2,] # remove tweets from the 1st of the month
all_august_pred_import = all_august_pred_import[all_august_pred_import$created_at$mday<=29,] # remove tweets from the 30th and 31st
length(table(all_august_pred_import$created_at$mday)) #28 days of month used

table(all_august_pred_import$created_at$mday)
table(all_august_pred_import$created_at$wday)
table(all_august_pred_import$created_at$hour)


#############################################################
### Initialize ggmap
#############################################################

# Initialize ggmap called basemap
la_county <- subset(map_data("county"), region == 'california' & subregion == 'los angeles')
orange_county <- subset(map_data("county"), region == 'california' & subregion == 'orange')
la_box = c(-119.2, 33.4, -116.6, 34.9) #This is zoomed out, zoom = 8
la_box = c(-119.3, 33.4, -116.6, 34.9) #This is zoomed in, zoom = 9
#LA_map = get_map(la_box, maptype = 'roadmap', color = 'bw', source = "osm") # original version
#LA_map = get_map(la_box, maptype = 'toner-lite', source = "google")
LA_map = get_map(la_box, maptype = 'roadmap', source = "google", zoom = 8) #zoom = 9 isn't working
#LA_map = get_map(la_box, maptype = 'terrain', source = "google")

basemap = ggmap(LA_map) +
  geom_polygon(data = la_county, aes(x=long, y=lat, group = group), fill = 'blue', alpha = 0.01, color = "black") +
  geom_polygon(data = orange_county, aes(x=long, y=lat, group = group), fill = 'blue', alpha = 0.01, color = "black") +
  xlab("Longitude") +
  ylab('Latitude') +
  theme(text = element_text(size=16), legend.position = 'right') +
  scale_x_continuous(limits = c(-119.15, -116.6), expand = c(0,0)) +
  scale_y_continuous(limits = c(33.4, 34.8), expand = c(0,0))
basemap


#############################################################
### Initialize timeline
#############################################################

# Initialize time frames...
time_frames = expand.grid(0:23,0:6,-1,-1)
colnames(time_frames) = c('hour', 'day','avg_sentiment', 'num_tweets')
time_frames$day_frac = time_frames$day + time_frames$hour/24

a = Sys.time() # about 2 mins
for(i in 1:nrow(time_frames)){
  # Find hour and day for plot title
  hour = paste(time_frames[i,1],':00',sep = "")
  day = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")[time_frames[i,2]+1]
  print(paste(hour, day))
  
  # Get hour data
  hour_data = all_august_pred_import[all_august_pred_import$created_at$wday == time_frames[i,2] 
                                     & all_august_pred_import$created_at$hour == time_frames[i,1],]
  hour_data = hour_data[sample(nrow(hour_data),nrow(hour_data)),] # Scramble data order for plotting
  
  # Collect information about average sentiment and number of tweets
  time_frames$avg_sentiment[i] = mean(hour_data$pred_polarity)
  time_frames$num_tweets[i] = nrow(hour_data)/4
}
time_frames$num_tweets_scaled = time_frames$num_tweets/max(time_frames$num_tweets)
Sys.time()-a

#save time_frames to file using feather
write_feather(x = time_frames, path = 'images/time_frames.feather')


# initialize ggplot timeline
timeline = ggplot(time_frames, aes(x = day_frac, y = num_tweets_scaled, color = "Number of \n tweets")) +
  geom_rect(aes(xmin=1, xmax=2, ymin=0, ymax=1), color = 'grey95', fill = 'grey95') +
  geom_rect(aes(xmin=3, xmax=4, ymin=0, ymax=1), color = 'grey95', fill = 'grey95') +
  geom_rect(aes(xmin=5, xmax=6, ymin=0, ymax=1), color = 'grey95', fill = 'grey95') +
  geom_line(size = 1) +
  geom_line(data = time_frames, aes(x = day_frac, y = avg_sentiment, color = "Average \n polarity"), size = 1) +
  theme_few() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = 'left',
        legend.key.size = unit(2, 'lines'),
        text = element_text(size = 16)) +
  scale_x_continuous(breaks = seq(0.5,6.5,0.5), 
                     labels = c("Sun","", "Mon","", "Tue","", "Wed","", "Thu","", "Fri","", "Sat"),
                     limits = c(0,7), expand = c(0,0)) +
  scale_color_manual(name = "", values = c("Number of \n tweets" = 'dark green', "Average \n polarity" = 'purple'))
timeline


#############################################################
### Use ggplot to plot each hour of the week
#############################################################


#  Read time_frames from feather
time_frames = read_feather(path = 'images/time_frames.feather')
time_frames = as.data.frame(time_frames)


# Initialize saturday night time_frame data frames
prev_prev_hour_data = all_august_pred_import[all_august_pred_import$created_at$wday == time_frames[nrow(time_frames)-1,2] 
                                        & all_august_pred_import$created_at$hour == time_frames[nrow(time_frames)-1,1], ]
prev_prev_hour_data = prev_prev_hour_data[sample(nrow(prev_prev_hour_data),nrow(prev_prev_hour_data)),] # Scramble data order for plotting

prev_hour_data = all_august_pred_import[all_august_pred_import$created_at$wday == time_frames[nrow(time_frames),2] 
                                        & all_august_pred_import$created_at$hour == time_frames[nrow(time_frames),1], ]
prev_hour_data = prev_hour_data[sample(nrow(prev_hour_data),nrow(prev_hour_data)),] # Scramble data order for plotting

#initialize junk tweets with polarity of 0 and 1 for purpose of ensuring color gradient
junk = all_august_pred_import[1:2,]
junk$pred_polarity = c(0,1)


# Big for loop creating plots!
a = Sys.time() # about 30-45 mins
for(i in 1:nrow(time_frames)){
  
  # creating a name for each plot file with leading zeros
  if (i < 10) {name = paste('img000',i,'.png',sep='')}
  if (i < 100 && i >= 10) {name = paste('img00',i,'.png', sep='')}
  if (i >= 100) {name = paste('img0', i,'.png', sep='')}
  
  # Find hour and day for plot title
  hour = paste(time_frames[i,1],':00',sep = "")
  day = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")[time_frames[i,2]+1]
  print(paste(hour, day))
  
  # Get hour data
  hour_data = all_august_pred_import[all_august_pred_import$created_at$wday == time_frames[i,2] 
                                     & all_august_pred_import$created_at$hour == time_frames[i,1],]
  hour_data = hour_data[sample(nrow(hour_data),nrow(hour_data)),] # Scramble data order for plotting
  
  # Collect information about average sentiment and number of tweets
  time_frames$avg_sentiment[i] = mean(hour_data$pred_polarity)
  time_frames$num_tweets[i] = nrow(hour_data)/4
  
  # Map plot
  plot.title = "Hourly Sentiment Polarity of Geotagged LA County Tweets"
  plot.subtitle = paste(day, " ", hour, " ~ August 2-29, 2016", sep = "")
  map_plot = basemap +
    geom_point(data = junk, aes(x = lon, y = lat, color = pred_polarity), alpha = 0) + #ensure color gradient
    geom_point(data = prev_prev_hour_data, aes(x = lon, y = lat, color = pred_polarity), alpha = 0.05) + #2 hours old
    geom_point(data = prev_hour_data, aes(x = lon, y = lat, color = pred_polarity), alpha = 0.1) + #1 hour old
    geom_point(data = hour_data, aes(x = lon, y = lat, color = pred_polarity), alpha = 1) + #current hour
    scale_colour_gradient(low = "blue", high = "red", breaks = c(0,.5,1),
                          labels = c("negative", "neutral", "positive"),
                          guide = guide_legend(title = NULL, reverse = TRUE)) +
    ggtitle(bquote(atop(.(plot.title), atop(italic(.(plot.subtitle)), ""))))

  # Timeline plot
  timeline_plot = timeline + geom_vline(xintercept = time_frames$day_frac[i], color = 'black', size = 2)

  # Combine and save plots
  combined_plot = grid.arrange(map_plot, timeline_plot, ncol = 1, heights = c(7,1))
  combined_plot
  ggsave(plot = combined_plot, name, path = './images', width = 14, height = 10, units = 'in')

  #Move data back an hour
  prev_prev_hour_data = prev_hour_data
  prev_hour_data = hour_data
}
Sys.time()-a




################################################
### Bash code for building animation with FFmpeg:
################################################

#ffmpeg -framerate 4 -i img%04d.png -c:v libx264 -r 30 -pix_fmt yuv420p out.mp4

