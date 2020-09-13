library(rnaturalearth)
library(sf)
library(ggplot2) # with support for geom_sf

# all countries at scale 10m
ctrys <- ne_countries(scale = 10, type = "countries", returnclass = "sf")

crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"
crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

# the bouding box polygon in long/lat projection, i.e. axis-aligned
studyarea <- st_sfc(
  st_polygon(list(cbind(
    c(-12, -5, -5, -12, -12), # x-coordinates (longitudes) of points A,B,C,D
    c(48, 48, 53.5, 53.5, 48)     # y-coordinates (latitudes) of points A,B,C,D
  ))),
  crs = crsLONGLAT)

bb <- st_sfc(
  st_polygon(list(cbind(
    c(-15, 0, 0, -15, -15), # x-coordinates (longitudes) of points A,B,C,D
    c(47, 47, 55.5, 55.5, 47)     # y-coordinates (latitudes) of points A,B,C,D
  ))),
  crs = crsLONGLAT)


# now in in LAEA projection
laeabb <- st_transform(bb, crs = crsLAEA)

# the extent of the bounding box in the new projection
b <- st_bbox(laeabb)
b

gg2 <- ggplot(data = ctrys) +
  geom_sf(fill = "grey", colour = "black") +
  geom_sf(data = studyarea, fill = "#12345678") +
  coord_sf(crs = crsLAEA, xlim = c(b["xmin"], b["xmax"]), ylim = c(b["ymin"], b["ymax"])) +
  theme_bw()

ggsave(gg2, filename = "www/studyAreaMap.png", pointsize = 30)
