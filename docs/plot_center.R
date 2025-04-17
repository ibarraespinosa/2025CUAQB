library(ncdf4)
library(colorout)
library(ggplot2)
library(cptcity)
library(data.table)
library(sf)

fc <- list.files(path = "/glade/scratch/sibarra/SAAP/run",
                 pattern = "SAAP.cam.h0.2014",
                 full.names = T)
nc <- list.files(path = "/glade/scratch/sibarra/SAAP/run",
                 pattern = "SAAP.cam.h0.2014")

dates <- as.Date(nc, format = "SAAP.cam.h0.%Y-%m-%d-00000.nc")

nd <- paste0(strftime(dates, "%Y%m%d"), ".nc")

mi <- map_data('world', wrap=c(0,360))

setDT(mi)

mi[group == 1]

########################################
nc <- nc_open("/glade/p/cgd/amp/patc/GRID_REPO/ne0np4.SAMwrf01.ne30x2/grids/SAMwrf01_ne30x2_np4_SCRIP.nc")
gp <- readRDS("globe_sf.rds")

ggplot(gp, 
       aes(x = clon, 
           y = clat,
           colour = O320140829.nc),
       alpha = 0.3) +
  geom_point()+
  scale_colour_gradientn(expression(O[3]~ppb),
                       colours = cpt(pal = 3715,
                                     rev = T)) +
  geom_polygon(data = mi,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               colour = "white") +
  coord_quickmap() +
  labs(x = NULL, y = NULL, 
       title = "NCAR MUSICA South America O3 2014-08-21 (avg)",
       subtitle = "SAMwrf01_ne30x2_EXODUS.nc. Period: 2014-07-01 - 2014-09-01") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal()+
  theme(text = element_text(size = 20),
        legend.key.height = unit(4, "lines"),
        plot.title = element_text(hjust = 0.5))

#####

lat <- ncdf4::ncvar_get(nc, "grid_corner_lat")
lat <- rbind(lat, lat[1, ])

lon <- ncdf4::ncvar_get(nc, "grid_corner_lon")
lon <- rbind(lon, lon[1, ])
