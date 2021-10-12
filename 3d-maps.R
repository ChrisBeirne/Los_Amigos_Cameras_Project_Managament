# Interactive maps
library(raster)
library(elevatr)
library(sf)
#small version

all.locs <- read.csv("data/raw-data/Camera_locations_master.csv", header=T, sep=",")

tmp1 <- all.locs[all.locs$Group=="CM2",]

loc.df <- data.frame(x=c(min(tmp1$Longitude, na.rm=T), max(tmp1$Longitude, na.rm=T)),
                     y=c(min(tmp1$Latitude, na.rm=T), max(tmp1$Latitude, na.rm=T)))



tmp <- st_as_sf(tmp1[is.na(tmp1$Longitude)==F,],coords=c("Longitude", "Longitude"),crs=4326)

st_area(st_as_sfc(st_bbox(tmp)))/1e+6



# Window
elevation <- get_elev_raster(locations = loc.df, prj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", z=13)
image(elevation, asp=1)

points(tmp1$Longitude, tmp1$Latitude)


# Project it to meters
sr <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs"
projected_raster <- projectRaster(elevation, crs = sr)

library(rayshader)

elmat = raster_to_matrix(elevation)

#We use another one of rayshader's built-in textures:
elmat %>%
  sphere_shade(texture = "imhof1") %>% # makes the green colour
  plot_map()

# elmat %>%
#   sphere_shade(texture = "imhof1") %>% # makes the green colour
#   add_water(detect_water(elmat, min_area=length(elmat)/10000, cutoff=0.95), color = "desert") %>%
#   add_shadow(ray_shade(elmat, zscale = 3), 0.5) %>%
#   add_shadow(ambient_shade(elmat), 0) %>%
#   plot_3d(elmat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))



################

sunangle <- 270


library(geoviz)
mapbox_key <- "pk.eyJ1IjoiY3diZWlybmUiLCJhIjoiY2t1bmwyMmdhNDNqMDMybnowZXoxamE3NiJ9.gMl3Tx7e8_6TiwbPPNRtHw"
overlay_image <-
  slippy_overlay(
    elevation,
    image_source = "mapbox",
    image_type = "satellite",
    png_opacity = 0.9,
    api_key = mapbox_key
  )

#plot(overlay_image)


# Draw the scene
scene <- elmat %>%
  sphere_shade(sunangle = sunangle, texture = "bw") %>%
  add_overlay(overlay_image)

#rayshader::plot_3d(
#  scene,
#  elmat)
  


rayshader::plot_3d(
  scene,
  elmat,
  zscale = raster_zscale(elevation),
  solid = FALSE ,
  shadow = TRUE)# ,
#  shadowdepth = 0
)





