
# V2
# 1. Use the HydroRIVERS as a stream network (use Strahler order > 5)
# 2. Identify merging points in the network (will work as discharge points)
# 3. For each discharge point, draw a catchment using the ldd map
# 4. Merge all catchments




library(tidyverse)
library(stars)
library(mapview)
library(qgisprocess)
# options(qgisprocess.path = '/usr/bin/qgis_process.bin') # run this before loading library
# qgis_enable_plugins("grassprovider")


d <- "/mnt/pers_disk/miss_wsheds/"
# fs::dir_create(d)



aoi <- 
  "/mnt/bucket_cmip6/CWatM/Mississippi/basin.tif" %>% 
  read_stars()

aoi_ext <- 
  st_bbox(aoi)

ldd <- 
  read_ncdf("/mnt/bucket_cmip6/CWatM/global_files/ldd.nc") %>% 
  setNames("ldd")

ldd <- 
  ldd %>%
  st_set_crs(4326) %>% 
  st_crop(aoi_ext)


land <- 
  "/mnt/bucket_mine/misc_data/ne_50m_land/ne_50m_land.shp" %>%
  st_read()

land_r <- 
  land %>% 
  mutate(a = 1) %>% 
  select(a) %>% 
  st_rasterize(ldd %>% mutate(a = NA) %>% select(a))

lakes <- 
  "/mnt/bucket_mine/misc_data/ne_50m_lakes/ne_50m_lakes.shp" %>% 
  st_read()

# sf_use_s2(F)
lakes <- 
  lakes %>% 
  filter(scalerank == 0,
         featurecla != "Reservoir") %>% 
  select(1:2) #%>% 
# mutate(ar = st_area(.) %>% units::set_units(km^2)) %>% 
# select(ar) %>% 
# filter(ar > units::set_units(4000, km^2))

lakes_r <- 
  lakes %>% 
  mutate(b = 1) %>% 
  select(b) %>% 
  st_rasterize(ldd %>% mutate(a = NA) %>% select(a))

land_non_land <-
  c(land_r, lakes_r) %>% 
  mutate(a = if_else(is.na(a), 0, 1),
         b = if_else(is.na(b), 1, 0),
         a = if_else(a == 0 | b == 0, 0, 1)) %>% 
  select(a) %>%
  st_warp(ldd)


ldd <- 
  ldd %>% 
  mutate(ldd = case_when(ldd == 9 ~ 1,
                         ldd == 8 ~ 2,
                         ldd == 7 ~ 3,
                         ldd == 6 ~ 8,
                         ldd == 5 ~ -1,
                         ldd == 4 ~ 4,
                         ldd == 3 ~ 7,
                         ldd == 2 ~ 6,
                         ldd == 1 ~ 5,
                         ldd == 0 ~ -1))

ldd <- 
  ldd %>% 
  c(land_non_land) %>% 
  mutate(ldd = if_else(a == 1, ldd, -1)) %>% 
  select(ldd)

write_stars(ldd, paste0(d, "/ldd.tif"), type = "Int16")#, NA_value = -9999L)



# FIRST PASS ******************************************************************


network_h <- 
  "/mnt/bucket_mine/misc_data/HydroRIVERS_v10_na_shp/HydroRIVERS_v10_na_shp/HydroRIVERS_v10_na.shp" %>% 
  st_read() %>% 
  st_crop(ldd)

# Connect line segments
# (right now consisting of many small fragments)

network_h_f <- 
  network_h %>% 
  filter(ORD_STRA >= 6) %>% 
  group_by(MAIN_RIV) %>% 
  summarise()

# Break lines based on intersections/nodes
network_h_ff <- qgis_run_algorithm("grass7:v.clean",
                                   input = network_h_f,
                                   layer = "MAIN_RIV",
                                   type = "line",
                                   tool = "break",
                                   `-c` = TRUE)

network <- st_as_sf(network_h_ff)


# Obtain start + end points for each line segment
nodes <- 
  network %>% 
  st_transform(3857) %>% 
  st_line_sample(sample = c(0,1)) %>% # start + end
  st_sf() %>% 
  st_transform(4326) %>% 
  mutate(r = row_number()) %>% 
  st_cast("POINT") # from multipoint (pair of points)

# remove duplicates
pts_1 <- 
  nodes %>%
  distinct(geometry)

pts_1 <- 
  pts_1 %>% 
  st_crop(ldd)

pts_1 %>% 
  st_bbox() %>%
  st_as_sfc() %>%
  st_sf() %>% 
  st_buffer(-0.1) -> b

pts_1 <- 
  pts_1 %>% 
  st_crop(b)

# mapview(pts_1) + mapview(a, alpha.regions = 0) + mapview(network)


# st_write(pts_1, paste0(d, "pts_1.gpkg"), delete_layer = T)
# pts_1 <- st_read(paste0(d, "pts_1.gpkg"))


# Draw a catchment for each point
catchments_1 <- 
  map(seq_len(nrow(pts_1)), function(i) {
    
    print(paste0(i, " / ", nrow(pts_1)))
    
    outlet <- 
      pts_1 %>% 
      slice(i)
    
    catch <- 
      qgis_run_algorithm("grass7:r.water.outlet",
                         input = paste0(d, "ldd.tif"),
                         # input = "/mnt/bucket_cmip6/CWatM/global_files/ldd.nc",
                         coordinates = outlet %>% st_coordinates() %>% as.vector() %>% str_flatten(","))
    
    
    a <- 
      catch$output %>% 
      read_stars() %>% 
      st_as_sf(as_points = F, merge = T) %>% 
      rename("pol" = 1) %>% 
      mutate(pol = i)
    
    if (nrow(a) < 1) {
      
    }
    
    # merge polygons if there are more than 1
    if (nrow(a) > 1) {
      a <- 
        a %>% 
        summarize
    }
    
    return(a)
    
  })


# Merge all catchments
sf_use_s2(F)

catchments_1_all <-
  bind_rows(catchments_1) %>%
  # filter(!is.na(pol)) %>% 
  # st_set_precision(100) %>% # if GEOS error
  st_intersection() %>% 
  st_collection_extract("POLYGON")


# catchments_1_all <- 
#   catchments_1_all %>% 
#   st_make_valid() %>% 
#   filter(!st_is_empty(.))
# 
# sf_use_s2(F)
# 
# catchments_1_all <- 
#   catchments_1_all %>% 
#   mutate(a = st_area(.)) %>% 
#   units::drop_units() %>% 
#   filter(a > 200000) %>%
#   mutate(r = row_number()) %>%
#   select(r)
# 
# sf_use_s2(T)

"/mnt/bucket_mine/misc_data/hybas_na_lev06_v1c/" %>% st_read() -> a
mapview(catchments_1_all %>% mutate(a = sample(20,471, replace = T)) %>% select(a), alpha = 0, col.regions = hcl.colors(palette = "Dark3", 20)) + mapview(a %>% select(1) %>% st_crop(ldd), alpha.regions = 0, color = "red")


