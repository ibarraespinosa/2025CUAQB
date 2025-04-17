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
mi2 <- map_data('world')

setDT(mi)

mi[group == 1]

########################################
nc <- nc_open("/glade/p/cgd/amp/patc/GRID_REPO/ne0np4.SAMwrf01.ne30x2/grids/SAMwrf01_ne30x2_np4_SCRIP.nc")
lat <- ncdf4::ncvar_get(nc, "grid_corner_lat")
lat <- rbind(lat, lat[1, ])

lon <- ncdf4::ncvar_get(nc, "grid_corner_lon")
lon <- rbind(lon, lon[1, ])

pb <- utils::txtProgressBar(min = 0,
                            max = ncol(lon),
                            style = 3)


rbindlist(lapply(1:ncol(lon), function(i) {
  utils::setTxtProgressBar(pb, i)
  
  data.frame(long = lon[, i],
             lat = lat[, i],
             group = i,
             order = 1:11)
})) -> dx

minsa <- fread("in_south_america.csv")

dx$minsa <- rep(minsa[[1]], each = 11)

# data
gp <- readRDS("globe_sf.rds")
dfgp <- st_set_geometry(gp, NULL)
setDT(dfgp)

for(i in 1:ncol(dfgp)) {
  dx[[names(dfgp)[i]]] <-  rep(dfgp[[i]], each = 11)
}


rc <- classInt::classIntervals(var = dx[["O320140821.nc"]],
                               n = 100, 
                               style = "sd")$brks



ggplot(dx, 
       aes(x = long, 
           y = lat, 
           group = group,
           fill = O320140821.nc),
       alpha = 0.3) +
  geom_polygon() +
  scale_fill_gradientn(expression(O[3]~mu*g*m^{-3}),
                       colours = cpt(pal = 3715,
                                     rev = T)) +
  geom_polygon(data = mi,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               colour = "grey50") +
  coord_quickmap() +
  labs(x = NULL, y = NULL, 
       title = "NCAR MUSICA South America O320140821.nc") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal()+
  theme(text = element_text(size = 20),
        legend.key.height = unit(4, "lines"),
        plot.title = element_text(hjust = 0.5))


rc <- classInt::classIntervals(var = dx[long > 250 & long < 350 &
                                          lat > -60 & lat < 15][["O320140821.nc"]],
                               n = 100, 
                               style = "sd")$brks
rc <- c(0.001, rc)

dxx <- dx[long > 250 & long < 350 &
            lat > -60 & lat < 15]
dxx$long <- ifelse(dxx$long > 180, -360 + dxx$long, dxx$long)


ggplot(dxx, 
       aes(x = long, 
           y = lat, 
           group = group,
           fill = O320140821.nc),
       alpha = 0.3) +
  geom_polygon() +
  scale_fill_gradientn(expression(O[3]~ppb),
                         colours = cpt(pal = 3715,
                                       rev = T),
                         values = rc/max(rc)) +
  coord_quickmap(xlim = ifelse(c(250, 350) > 180,
                               -360 + c(250, 350),
                               c(250, 350)),
                 ylim = c(-60, 15),
                 expand = c(0,0)) +  
  geom_polygon(data = mi2, 
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA, 
               colour = "white") +
  labs(x = NULL, y = NULL, 
       title = "NCAR MUSICA South America O320140821.nc") +
  # scale_x_continuous(expand = c(0, 0)) +
  # scale_y_continuous(expand = c(0, 0)) +
  theme_minimal()+
  theme(text = element_text(size = 20),
        legend.key.height = unit(4, "lines"),
        plot.title = element_text(hjust = 0.5))

# hacer video mostrando animacao e valores diarios

# comparar Santiago, Buenos Aires, Sao Paulo, Lima, Bogota, Ecuador
