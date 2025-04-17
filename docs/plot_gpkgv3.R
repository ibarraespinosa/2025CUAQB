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

mi <- map_data('world')

setDT(mi)

mi[group == 1]

########################################
nc <- nc_open("/glade/p/cgd/amp/patc/GRID_REPO/ne0np4.SAMwrf01.ne30x2/grids/SAMwrf01_ne30x2_np4_SCRIP.nc")
df <- readRDS("globe_sf.rds")
st_crs(df) <- 4326

ggplot(df) +
  geom_sf(aes(fill = O320140821.nc), lty = 0)+
  scale_fill_gradientn(expression(O[3]~ppb),
                       colours = cpt(pal = 3715,
                                     rev = T)) +
  geom_polygon(data = mi,
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               colour = "white") +
  coord_sf() +
  labs(x = NULL, y = NULL, 
       title = "NCAR MUSICA South America O3 2014-08-21 (avg)",
       subtitle = "SAMwrf01_ne30x2_EXODUS.nc. Period: 2014-07-01 - 2014-09-01") +
  theme_minimal()+
  theme(text = element_text(size = 20),
        legend.key.height = unit(4, "lines"),
        plot.title = element_text(hjust = 0.5))

#####
bb <- st_bbox(df)
# xmin      ymin      xmax      ymax 
# -85.45150 -59.03607 -85.05471 -58.79595

(ifelse(c(250, 350) > 180,
                -360 + c(250, 350),
                c(250, 350)))
# -110    -10
bb[1] <- -110
bb[2] <- -60
bb[3] <- -10
bb[4] <- 15

bb <- st_sf(id = 1, geometry = st_as_sfc(bb))

df2 <- st_make_valid(df)

dfsa <- st_crop(df2, bb)

saveRDS(dfsa, "sa_sf.rds")
st_write(dfsa, "sa_sf.gpkg")


ggplot(dfsa) +
  geom_sf(aes(fill = O320140818.nc), lty = 0)+
  scale_fill_gradientn(expression(O[3]~ppb),
                       colours = cpt(5251, rev = T)) +
  geom_polygon(data = mi[mi$long > -110 & mi$long < -10 &
                         mi$lat > -60 & mi$lat < 15, ],
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               colour = "white") +
  coord_sf() +
  labs(x = NULL, y = NULL, 
       title = "NCAR MUSICA South America O3 2014-08-18 (avg)",
       subtitle = "SAMwrf01_ne30x2_EXODUS.nc. Period: 2014-07-01 - 2014-09-01") +
  theme_minimal()+
  theme(text = element_text(size = 20),
        legend.key.height = unit(4, "lines"),
        plot.title = element_text(hjust = 0.5))

ggplot(dfsa) +
  geom_sf(aes(fill = PM2520140818.nc), lty = 0)+
  scale_fill_gradientn(expression(PM[2.5]~mu*g/m^3),
                       colours = cpt(5062, rev = T)) +
  geom_polygon(data = mi[mi$long > -110 & mi$long < -10 &
                           mi$lat > -60 & mi$lat < 15, ],
               aes(x = long,
                   y = lat,
                   group = group),
               fill = NA,
               colour = "white") +
  coord_sf() +
  labs(x = NULL, y = NULL, 
       title = "NCAR MUSICA South America O3 2014-08-18 (avg)",
       subtitle = "SAMwrf01_ne30x2_EXODUS.nc. Period: 2014-07-01 - 2014-09-01") +
  theme_minimal()+
  theme(text = element_text(size = 20),
        legend.key.height = unit(4, "lines"),
        plot.title = element_text(hjust = 0.5))


-34.59432733365671, -58.41304412033377
ci <- data.table::data.table(
           x = c(-46.655584189899,-70.6555874823907,
                 -77.0406595087482,-78.4717459708381,-43.3436798189408,
                 -74.0852253919585,
                 -58.41304412033377),
           y = c(-23.5456349059401,-33.4518007294943,
                 -12.0682377590533,-0.174920855176173,-22.8761162776745,
                 4.6893015701328,
                 -34.59432733365671),
        city = c("São Paulo","Santiago","Lima","Quito",
                 "Rio de Janeiro","Bogota",
                 "Buenos Aires"),
     country = c("Brazil", "Chile", "Peru", "Ecuador", 
                 "Brazil", "Colombia", "Argentina")
)

ci <- st_as_sf(ci, coords = c("x", "y"), crs = 4326)

cidfsa <- t(st_intersects(ci, dfsa, sparse = F))

x <- st_set_geometry(dfsa, NULL)

sao <- x[cidfsa[, 1], ]
san <- x[cidfsa[, 2], ]
lim <- x[cidfsa[, 3], ]
qui <- x[cidfsa[, 4], ]
rio <- x[cidfsa[, 5], ]
bog <- x[cidfsa[, 6], ]
bar <- x[cidfsa[, 7], ]


nr <- names(sao)
dfcit <- data.table(
  city = rep(ci$city, each = ncol(sao)),
  val = c(t(sao)[, 1], t(san)[, 1], t(lim)[, 1], 
          t(qui)[, 1], t(rio)[, 1], t(bog)[, 1],
          t(bar)[, 1]
          ),
  var = rep(nr, 7)
)

dfcit <- dfcit[!var %in% c("clat", "clon")]
dfcit[grepl(pattern = "PM", var), pol := "PM2.5"]
dfcit[grepl(pattern = "CO", var), pol := "CO"]
dfcit[grepl(pattern = "T", var), pol := "T"]
dfcit[grepl(pattern = "O3", var), pol := "O3"]
dfcit[, date := gsub(pattern = "PM25", replacement = "", x = var)]
dfcit[, date := gsub(pattern = "T", replacement = "", x = date)]
dfcit[, date := gsub(pattern = "CO", replacement = "", x = date)]
dfcit[, date := gsub(pattern = "O3", replacement = "", x = date)]
dfcit[, date := as.Date(date, format = "%Y%m%d.nc")]

dfcit[pol == "CO", val := val/1000]
dfcit[pol == "T", val := val-274.15*2]

ggplot(dfcit, 
       aes(x = date, 
           y = val, 
           colour = city,
           shape = city)) +
  geom_point(size = 3) +
  geom_line() +
  facet_wrap(~pol, scales = "free") +
  labs(x = NULL, y = NULL, 
       title = "NCAR MUSICA South America O3 2014-08 (avg)",
       subtitle = "SAMwrf01_ne30x2_EXODUS.nc. Period: 2014-07-01 - 2014-09-01") +
  theme_minimal()+
  theme(text = element_text(size = 20),
        legend.key.height = unit(4, "lines"),
        plot.title = element_text(hjust = 0.5))

