
# V1
# 1. Use the ups map to derive the stream network (apply a threshold)
# 2. Identify merging points in the network (will work as discharge points)
# 3. For each discharge point, draw a catchment using the ldd map
# 4. Merge all catchments

# This script has pieces of another used for Chiapas. It includes a 
# second pass to draw catchments in areas that the first pass missed.



library(tidyverse)
library(stars)
library(igraph)
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

ups <- 
  read_ncdf("/mnt/bucket_cmip6/CWatM/global_files/ups.nc") %>% 
  setNames("ups")

ups <- 
  ups %>%
  st_set_crs(4326) %>% 
  st_crop(aoi_ext)


land <- 
  "/mnt/bucket_mine/misc_data/ne_50m_land/ne_50m_land.shp" %>%
  st_read()

land_r <- 
  land %>% 
  mutate(a = 1) %>% 
  select(a) %>% 
  st_rasterize(ups %>% mutate(a = NA) %>% select(a))

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
  st_rasterize(ups %>% mutate(a = NA) %>% select(a))

land_non_land <-
  c(land_r, lakes_r) %>% 
  mutate(a = if_else(is.na(a), 0, 1),
         b = if_else(is.na(b), 1, 0),
         a = if_else(a == 0 | b == 0, 0, 1)) %>% 
  select(a) %>%
  st_warp(ups)

ups <- 
  ups %>% 
  c(land_non_land) %>% 
  mutate(ups = if_else(a == 1, ups, NA)) %>% 
  select(ups)


ldd <- 
  read_ncdf("/mnt/bucket_cmip6/CWatM/global_files/ldd.nc") %>% 
  setNames("ldd")

ldd <- 
  ldd %>%
  st_set_crs(4326) %>% 
  st_crop(aoi_ext)

ldd <- 
  ldd %>% 
  mutate(ldd = case_when(ldd == 9 ~ 1,
                         ldd == 8 ~ 2,
                         ldd == 7 ~ 3,
                         ldd == 6 ~ 8,
                         ldd == 5 ~ 0,
                         ldd == 4 ~ 4,
                         ldd == 3 ~ 7,
                         ldd == 2 ~ 6,
                         ldd == 1 ~ 5,
                         ldd == 0 ~ 0))

ldd <- 
  ldd %>% 
  c(land_non_land) %>% 
  mutate(ldd = if_else(a == 1, ldd, 0)) %>% 
  select(ldd)

write_stars(ldd, paste0(d, "/ldd.tif"), type = "Int16")#, NA_value = -9999L)



# FIRST PASS ******************************************************************

th <- 6000

# Create drainage network
ups_th <- 
  ups %>% 
  units::drop_units() %>% 
  mutate(ups = if_else(ups > th, 1L, -9999L))

write_stars(ups_th, paste0(d, "/ups_th.tif"), type = "Int16", NA_value = -9999L)

# Thin network
ups_thin <- 
  qgis_run_algorithm("grass7:r.thin",
                     input = paste0(d, "ups_th.tif"),
                     output = paste0(d, "ups_th_thined.tif"))


# Convert to vector
network <- 
  qgis_run_algorithm("grass7:r.to.vect",
                     input = paste0(d, "ups_th_thined.tif"),
                     type = "line",
                     output = paste0(d, "stream_network_ups.gpkg"),
                     `-s` = T)

network <- st_read(paste0(d, "stream_network_ups.gpkg"))

mapview(network %>% select(cat))




# Connect line segments
# (right now consisting of many small fragments)
# https://stackoverflow.com/questions/69175360/is-there-a-way-to-merge-contiguous-multilinestrings-sf-object-in-r
my_idx_touches <- st_touches(network)
my_igraph <- graph_from_adj_list(my_idx_touches)
my_components <- components(my_igraph)$membership

network <- 
  network %>% 
  group_by(section = as.character({{my_components}})) %>% 
  summarise()

# Break lines based on intersections/nodes
network <- qgis_run_algorithm("grass7:v.clean",
                              input = network,
                              layer = "section",
                              type = "line",
                              tool = "break",
                              `-c` = TRUE)

network <- st_as_sf(network)


# Obtain start + end points for each line segment
nodes <- 
  network %>% 
  st_transform(3857) %>% 
  st_line_sample(sample = c(0,1)) %>% # start + end
  st_sf() %>% 
  st_transform(4326) %>% 
  mutate(r = row_number()) %>% 
  st_cast("POINT") # from multipoint (pair of points)

# Extract upstream flow values
nodes <- 
  bind_cols(nodes, st_drop_geometry(st_extract(ups, nodes)))

# Retain only points with max flow vals between pairs
pts_1 <- 
  nodes %>%
  group_by(r) %>% 
  filter(ups == max(ups)) %>% 
  ungroup() %>% 
  
  # remove duplicates
  # 2 initial streams that merge (a Y) will have the same max 
  group_by(ups) %>%  
  slice(1) %>% 
  ungroup()

# st_write(pts_1, paste0(d, "pts_1.gpkg"), delete_layer = T)
pts_1 <- st_read(paste0(d, "pts_1.gpkg"))


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
mapview(catchments_1_all %>% mutate(a = sample(20,1918, replace = T)) %>% select(a), alpha = 0, col.regions = hcl.colors(palette = "Dark2", 20)) + mapview(a %>% select(1) %>% st_crop(ups), alpha.regions = 0, color = "red")


# SECOND PASS *****************************************************************

mask_catchments_1 <- 
  catchments_1_all %>%
  mutate(r = 1) %>% 
  select(r) %>% 
  st_rasterize(basins %>% mutate(a = if_else(is.na(basin.tif), NA,-9999)) %>% select(a))

ups_masked <- 
  ups %>% 
  units::drop_units() %>%
  c(mask_catchments_1) %>% 
  mutate(ups = if_else(r == -9999, ups, NA)) %>%
  select(ups)

ups_th <- 
  ups_masked %>% 
  units::drop_units() %>% 
  mutate(ups = if_else(ups > 750, 1L, -9999L))

write_stars(ups_th, "/mnt/pers_disk/chiapas/ups_th.tif", type = "Int16", NA_value = -9999L)

ups_thin <- qgis_run_algorithm("grass7:r.thin",
                               input = "/mnt/pers_disk/chiapas/ups_th.tif",
                               output = "/mnt/pers_disk/chiapas/ups_th_thined.tif")

network <- qgis_run_algorithm("grass7:r.to.vect",
                              input = "/mnt/pers_disk/chiapas/ups_th_thined.tif",
                              type = "line",
                              output = "/mnt/pers_disk/chiapas/stream_network_ups.gpkg",
                              `-s` = T)

network <- st_read("/mnt/pers_disk/chiapas/stream_network_ups.gpkg")

# https://stackoverflow.com/questions/69175360/is-there-a-way-to-merge-contiguous-multilinestrings-sf-object-in-r
my_idx_touches <- st_touches(network)
my_igraph <- igraph::graph_from_adj_list(my_idx_touches)
my_components <- igraph::components(my_igraph)$membership

network <- 
  network %>% 
  group_by(section = as.character({{my_components}})) %>% 
  summarise()

network <- qgis_run_algorithm("grass7:v.clean",
                              input = network,
                              layer = "section",
                              type = "line",
                              tool = "break",
                              `-c` = TRUE)

network <- st_as_sf(network)

nodes <- 
  network %>% 
  st_transform(3857) %>% 
  st_line_sample(sample = c(0,1)) %>%
  st_sf() %>% 
  st_transform(4326) %>% 
  mutate(r = row_number()) %>% 
  st_cast("POINT")

nodes <- 
  bind_cols(nodes, st_drop_geometry(st_extract(ups, nodes)))

pts_2 <- 
  nodes %>%
  group_by(r) %>% 
  filter(ups == max(ups)) %>% 
  ungroup() %>% 
  group_by(ups) %>% 
  slice(1) %>% 
  ungroup()

catchments_2 <- 
  map(seq_len(nrow(pts_2)), function(i) {
    
    print(i)
    
    outlet <- 
      pts_2 %>% 
      slice(i)
    
    catch <- 
      qgis_run_algorithm("grass7:r.water.outlet",
                         input = "/mnt/pers_disk/chiapas/ldd.tif",
                         coordinates = outlet %>% st_coordinates() %>% as.vector() %>% str_flatten(","))
    
    a <- 
      catch$output %>% 
      read_stars() %>% 
      st_as_sf(as_points = F, merge = T) %>% 
      rename("pol" = 1) %>% 
      mutate(pol = i+length(catchments_1))
    
    if (nrow(a) > 1) {
      a <- 
        a %>% 
        summarize
    }
    
    return(a)
    
  })


# *****************************



# big_pol <- 
#   basins %>% 
#   mutate(a = if_else(is.na(basin.tif), NA,-9999)) %>% 
#   select(a) %>% 
#   st_as_sf(as_points = F, merge = T) %>% 
#   mutate(pol = 1) %>% 
#   select(pol)


bind_rows(catchments_1, catchments_2) %>% 
  st_intersection() %>% 
  st_collection_extract("POLYGON") -> foo

foo %>% 
  mutate(r = 1) %>% 
  select(r) %>%
  st_rasterize(basins %>% 
                 mutate(a = if_else(is.na(basin.tif), NA,-9999)) %>%
                 select(a)) %>% 
  st_as_sf(as_points = F, merge = T) %>% 
  filter(r == -9999) %>% 
  mutate(r = row_number()) %>% 
  select(r) -> remainders


bind_rows(catchments_1, catchments_2, remainders) %>% 
  st_intersection() %>% 
  st_collection_extract("POLYGON") -> foo

foo %>% 
  st_make_valid() %>% 
  filter(!st_is_empty(.)) -> foo

sf_use_s2(F)
foo %>% 
  mutate(a = st_area(.)) %>% 
  units::drop_units() %>% 
  filter(a > 200000) -> foo

# foo <- 
#   foo %>% 
#   mutate(r = row_number()) %>% 
#   select(r)

foo %>% as_tibble(rownames = "id") %>% st_as_sf() %>% select(-pol, -r, -a) %>% mutate(id = as.integer(id)) -> bar


st_write(bar %>% select(-origins), "/mnt/bucket_mine/misc_data/temporary/chiapas_catchments.gpkg", append = F)

bar %>% 
  st_drop_geometry() %>% 
  unnest(origins) %>% 
  write_csv("/mnt/bucket_mine/misc_data/temporary/chiapas_catchments_order.csv")

bind_rows(pts_1, pts_2) %>% st_write("/mnt/bucket_mine/misc_data/temporary/chiapas_outflow_points.gpkg")
bind_rows(network, network2) %>% st_write("/mnt/bucket_mine/misc_data/temporary/chiapas_stream_network.gpkg")
