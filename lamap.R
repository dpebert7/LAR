# David Ebert
# 22 October

library(ggmap)
library(feather)
all_august_pred_import = read_feather(path = "~/Desktop/Huang Research/LAR_Data/feather_data/2016-08/all_august_pred.feather")
all_august_pred_import = all_august_pred_import[1:10000,]

#library(ggmap)
#Zoom 13: Downtown          ; Only 0.1% inbounds! 
#Zoom 11: Most of LA County ; 
#Zoom 8: San Diego          ; 
#Zoom 9:                    ;
#Zoom 7: Las Vegas          ; about 91% in bounds
LA_map = get_map("Los Angeles County", zoom = 8)

ggmap(LA_map) +
  geom_polygon(data = la_county, aes(x=long, y=lat, group = group), fill = 'blue', alpha = 0.2, color = "red") +
  scale_fill_gradient(low = "green", high = "red") + 
  geom_point(data = all_august_pred_import, aes(x = lon, y = lat, color = all_august_pred_import$pred_polarity+1), 
             alpha = 1) +
  geom_vline(xintercept = -117.6) +
  geom_vline(xintercept = -119) + 
  geom_hline(yintercept = 33.7) +
  geom_hline(yintercept = 34.9) +
  theme(text = element_text(size=20), legend.position = 'none')
#ggsave("la_map_points.png")


ggmap(LA_map) +
  geom_polygon(data = la_county, aes(x=long, y=lat, group = group), fill = 'blue', alpha = 0.2, color = "red") +
  scale_fill_gradient(low = "green", high = "red") + 
  geom_point(data = all_august_pred_import, aes(x = lon, y = lat, color = all_august_pred_import$pred_polarity+1), 
             alpha = 1) +
  geom_vline(xintercept = -117) +
  geom_vline(xintercept = -119) + 
  geom_hline(yintercept = 33) +
  geom_hline(yintercept = 35) +
  theme(text = element_text(size=20), legend.position = 'none')


# Why are there more than 2 colors?

# This takes much longer!
# ggmap(LA_map) +
#   scale_fill_gradient(low = "green", high = "red") + 
#   geom_tile(data = all_august_pred_import, aes(x = lon, y = lat, fill = (pred_polarity+1)), alpha = 0.7) +
#   labs(fill="") +
#   ggtitle("Happiness in Los Angeles County")
# ggsave("la_map.png")


la_county <- subset(map_data("county"), region == 'california' & subregion == 'los angeles')
LA_map = get_map("Los Angeles County", zoom = 8)

ggmap(LA_map) +
  geom_polygon(data = la_county, aes(x=long, y=lat, group = group), fill = NA, color = "red")



