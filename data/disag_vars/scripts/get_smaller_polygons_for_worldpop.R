library(tidyverse)
library(sf)
library(ProjectTemplate)

load.project()

sp <- read_sf('data/GDL Shapefiles V4 0.005', 'GDL Shapefiles V4')
d <- read.csv('data/disag_vars/rawdata/age_gender_missing.csv')

#Get polygons that ARE in FIES but NOT in results so far
need <- sp$GDLcode[!sp$GDLcode %in% d$GDLcode & substr(sp$GDLcode, 1, 3) %in% fies_raw$ISO3]

sel <- sp[sp$GDLcode %in% need, ]

#Make global grid of polygons
makepoly <- function(x, y, res){
  list(matrix(c(x, y, x + res, y, x + res, y + res, x, y + res, x, y), ncol=2, byrow=TRUE))
}

res <- 2.5

df <- expand.grid(list(x=seq(-180, 180 - res, res), y=seq(-90, 90 - res, res)))

grid <- mapply(FUN=makepoly, x=df$x, y=df$y, MoreArgs=list(res=res), SIMPLIFY=FALSE) %>%
  lapply(st_polygon) %>%
  st_sfc(crs=4326) %>%
  st_sf

#Intersect grid with GDL polygons
join <- st_intersection(grid, st_make_valid(sel))

#Make sure it is < 100000 sqkm
max(join %>% st_area)/1000000

write_sf(st_collection_extract(join, 'POLYGON'), 'data/gridded_gdlpolys', 'gridded_gdlpolys', driver='ESRI Shapefile')


