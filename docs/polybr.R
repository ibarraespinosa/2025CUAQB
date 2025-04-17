library(ncdf4)
library(colorout)
library(ggplot2)
library(cptcity)
library(data.table)
library(sf)
library(geobr)
library(vein)
mi <- map_data('world')

sa <- readRDS("sa_sf.rds")
br <- read_country()
br <- st_transform(br, 4326)
brsa <- st_intersects(br, sa, sparse = F)

brsa <- as.vector(t(brsa))

g <- sa[brsa, "geometry"]

x <- readRDS("emisvein2020/brazil_08.rds")
g <- st_transform(g, 3857)
gx <- emis_grid(spobj = x, g = g)

gx <- st_transform(gx, 4326)

rc <- classInt::classIntervals(var = gx$CO_all_EXH_08,
                               n = 100, 
                               style = "sd")$brks

ggplot(gx) +
  geom_sf(aes(fill = as.numeric(CO_all_EXH_08)), lty = 0)+
  scale_fill_gradientn(expression(CO~kg/m^2),
                       colours = cpt(3846,
                                     rev = T), 
                       values = rc/max(rc)) +
  geom_polygon(data = mi[mi$long > -110 & mi$long < -10 &
                           mi$lat > -60 & mi$lat < 15, ],
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               colour = "black") +
  coord_sf() +
  labs(x = NULL, y = NULL, 
       title = "VEIN emissions over Brazil NCAR MUSICA South America O3 2020-08") +
  theme_minimal()+
  theme(text = element_text(size = 20),
        legend.key.height = unit(4, "lines"),
        plot.title = element_text(hjust = 0.5))


rc <- classInt::classIntervals(var = gx$NO_all_EXH_08,
                               n = 100, 
                               style = "sd")$brks

ggplot(gx) +
  geom_sf(aes(fill = as.numeric(NO_all_EXH_08)), lty = 0)+
  scale_fill_gradientn(expression(NO~kg/m^2),
                       colours = cpt(2025,
                                     rev = T), 
                       values = rc/max(rc)) +
  geom_polygon(data = mi[mi$long > -110 & mi$long < -10 &
                           mi$lat > -60 & mi$lat < 15, ],
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               colour = "black") +
  coord_sf() +
  labs(x = NULL, y = NULL, 
       title = "VEIN emissions over Brazil NCAR MUSICA South America O3 2020-08") +
  theme_minimal()+
  theme(text = element_text(size = 20),
        legend.key.height = unit(4, "lines"),
        plot.title = element_text(hjust = 0.5))
