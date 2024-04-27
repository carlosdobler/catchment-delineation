
# V3.1
# 1. Instead of using a stream network to obtain discharge points, identify the
#    location within each HydroBASINS catchment with the highest accumulation (ups).
# 2. For each discharge point, draw a catchment using the ldd map
# 3. Merge all catchments
# 4. Repeat steps above for areas not covered in the first pass



# run this the first time the {qgisprocess} package is used (before anything else): 
# options(qgisprocess.path = '/usr/bin/qgis_process.bin')


library(tidyverse)
library(stars)
library(furrr)
library(qgisprocess)

# qgis_enable_plugins("grassprovider")


plan(multisession)

sf_use_s2(F) # turn off spherical config


d <- "/mnt/pers_disk/miss_wsheds/" # results and intermediary files
# fs::dir_create(d)

d_cwatm <- "/mnt/bucket_cmip6/CWatM/" # cwatm input layers/data

d_misc <- "/mnt/bucket_mine/misc_data/" # ancillary data





# STEP 1: PREPARE BASE LAYERS -------------------------------------------------

aoi_ext <- 
  paste0(d_cwatm, "Mississippi/basin.tif") %>% 
  read_stars() %>% 
  st_bbox()

# local drainage direction
ldd <- 
  paste0(d_cwatm, "global_files/ldd.nc") %>% 
  read_ncdf() %>% 
  st_crop(aoi_ext)

# reclassify for GRASS
ldd <- 
  ldd %>% 
  mutate(ldd = case_when(ldd == 9 ~ 1,
                         ldd == 8 ~ 2,
                         ldd == 7 ~ 3,
                         ldd == 6 ~ 8,
                         ldd == 5 ~ -1, # 
                         ldd == 4 ~ 4,
                         ldd == 3 ~ 7,
                         ldd == 2 ~ 6,
                         ldd == 1 ~ 5,
                         ldd == 0 ~ -1))

# save for GRASS
# write_stars(ldd, paste0(d, "/ldd.tif"), type = "Int16")

# upstream accumulated flow
ups <- 
  paste0(d_cwatm, "global_files/ups.nc") %>% 
  read_ncdf() %>% 
  st_crop(aoi_ext)

# dem (not needed in this version)
# dem <-
#   paste0(d, "global_files/dem.nc") %>% 
#   read_ncdf() %>%
#   st_crop(aoi_ext)




# STEP 1b: LAND MASK

# land <-
#   paste0(d_misc, "ne_50m_land/") %>% 
#   st_read() %>% 
#   mutate(a = 1) %>% 
#   select(a) %>% 
#   st_rasterize(ldd %>% mutate(a = NA) %>% select(a)) %>% 
#   st_warp(ldd)
# 
# lakes <-
#   paste0(d_misc, "ne_50m_lakes/") %>% 
#   st_read() %>% 
#   filter(scalerank == 0,
#          featurecla != "Reservoir") %>% 
#   mutate(b = 1) %>% 
#   select(b) #%>% 
#   # st_rasterize(ldd %>% mutate(a = NA) %>% select(a)) %>% 
#   # st_warp(ldd)
#   
# land_mask <-
#   c(land, lakes) %>%
#   mutate(a = if_else(is.na(a), 0, 1),
#          b = if_else(is.na(b), 1, 0),
#          a = if_else(a == 0 | b == 0, NA, 1)) %>%
#   select(a)






# STEP 2: PREPARE STREAM NETWORK -----------------------------------------------

# HydroRIVERS stream network is used to force discharge 
# points to align with the river

# import stream network (for North Am)
network_all <- 
  paste0(d_misc, "HydroRIVERS_v10_na_shp/HydroRIVERS_v10_na_shp") %>% 
  st_read() %>% 
  st_crop(ldd)

# subset based on Strahler order
network_stra <- 
  network_all %>% 
  filter(ORD_STRA >= 4) %>% 
  group_by(MAIN_RIV) %>% 
  summarise() %>% 
  mutate(r = 1) %>% 
  select(r)

# # rasterize and extract ups values
# ups_network <- 
#   network_stra  %>% 
#   st_rasterize(ldd %>% mutate(a = NA) %>% select(a)) %>% 
#   st_warp(ldd) %>% 
#   c(ups) %>% 
#   mutate(ups = if_else(is.na(r), NA, ups)) %>% 
#   select(ups)




# STEP 3: PREPARE HYDROBASINS -------------------------------------------------

# level 6
basins <- 
  paste0(d_misc, "hybas_na_lev06_v1c/") %>% 
  st_read() %>% 
  st_crop(ldd) %>% 
  mutate(HYBAS_ID = factor(HYBAS_ID),
         area = st_area(.) %>% units::drop_units()) %>% 
  select(HYBAS_ID, area)

# remove very small basins
basins <- 
  basins %>% 
  filter(area >= 1e9)
  
# rasterize  
basins_r <-
  basins %>%
  select(HYBAS_ID) %>%
  st_rasterize(ldd %>% mutate(a = NA) %>% select(a)) %>%
  st_warp(ldd) 


# is the number of rasterized basins the same 
# as HydroBASINS basins?
identical(basins$HYBAS_ID %>% 
            na.omit() %>% 
            unique() %>% 
            length(),
          
          basins_r$HYBAS_ID %>% 
            as.vector() %>% 
            na.omit() %>% 
            unique() %>% 
            length())



# STEP 4: OBTAIN DISCHARGE POINTS ---------------------------------------------

# tb_0 <- 
#   c(basins_r, 
#     ups_network %>% setNames("ups_network"),
#     ups %>% setNames("ups_whole")) %>% 
#   as_tibble() %>% 
#   filter(!is.na(id)) %>% 
#   
#   # identify basins without stream network in them
#   group_by(id) %>% 
#   mutate(flag = all(is.na(ups_network))) %>% 
#   ungroup()
# 
# 
# # process basins WITH stream network
# tb_1 <-
#   tb_0 %>%
#   filter(flag == F) %>% 
#   group_by(id) %>% # 700
#   mutate(ups_max = max(ups_network, na.rm = T)) %>% 
#   filter(ups_network == ups_max) %>%
#   
#   # remove duplicates (> 1 point sharing the same max ups)
#   mutate(d = duplicated(ups_max)) %>% 
#   ungroup() %>% 
#   filter(d == F) # 700
# 
# # process basins WITHOUT stream network
# # (use the original (whole) ups map to obtain max ups) 
# tb_2 <-
#   tb_0 %>%
#   filter(flag == T) %>% 
#   group_by(id) %>% # 23
#   mutate(ups_max = max(ups_whole, na.rm = T)) %>% 
#   filter(ups_whole == ups_max) %>%
#   mutate(d = duplicated(ups_max)) %>% # remove duplicates 
#   ungroup() %>% 
#   filter(d == F) # 23
# 
# # generate discharge points (sf obj)
# disch_pts <- 
#   bind_rows(tb_1, tb_2) %>% 
#   select(lon, lat, id) %>% 
#   st_as_sf(coords = c("lon", "lat")) %>% 
#   st_set_crs(4326)



# ALTERNATIVE APPROACH (simpler)
# Use the original ups map (not constrained by the stream network)

disch_pts <-
  c(basins_r,
    ups %>% setNames("ups_whole")) %>%
  as_tibble() %>%
  filter(!is.na(HYBAS_ID)) %>%
  group_by(HYBAS_ID) %>%
  # mutate(ups_max = max(ups_whole, na.rm = T)) %>% 
  mutate(ups_max = quantile(ups_whole, 0.99, na.rm = T, type = 1)) %>%
  filter(ups_whole == ups_max) %>%
  mutate(d = duplicated(ups_max)) %>% 
  filter(d != T) %>%
  ungroup() %>%
  select(lon, lat) %>%
  mutate(id = row_number()) %>% 
  as.data.frame() %>% 
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(4326)

# This approach would keep everything more consistent.


# is the number of discharge points the same  
# as HydroBASINS basins?
identical(basins$HYBAS_ID %>% 
            na.omit() %>% 
            unique() %>% 
            length(),
          
          disch_pts$id %>%  
            na.omit() %>% 
            unique() %>% 
            length())



# STEP 5: DRAW CATCHMENTS ------------------------------------------------------

# draw a catchment for each point

catchments <- 
  future_map(disch_pts$id, function(i) { # run in parallel
    
    # select discharge pt
    outlet <- 
      disch_pts %>% 
      filter(id == i)
    
    # draw catchment
    catch <- 
      qgis_run_algorithm("grass7:r.water.outlet",
                         input = paste0(d, "ldd.tif"), # ldd map previously saved
                         coordinates = 
                           outlet %>% 
                           st_coordinates() %>% 
                           as.vector() %>% 
                           str_flatten(",")) %>% 
      suppressMessages()
    
    # catchment polygon
    a <- 
      catch$output %>% 
      read_stars() %>% 
      st_as_sf(as_points = F, merge = F) %>% 
      summarize()
    
    # assign id
    a <- 
      a %>%
      mutate(id = i) %>% 
      select(id)
    
    return(a)
    
  })


# merge all catchments

# sf_use_s2(T) 
catchments_f <-
  catchments %>% 
  bind_rows() %>%
  st_set_precision(1e6) %>% 
  st_intersection() %>%
  st_collection_extract("POLYGON")

# fix id (messed up when intersecting)
catchments_ff <- 
  catchments_f %>% 
  select(-id) %>% 
  st_join(disch_pts) %>% 
  select(id, everything()) %>% 
  filter(!is.na(id))



catchments_ff <- 
  catchments_f %>% 
  mutate(id = row_number())


 catchments_ff <- 
  catchments_f %>%
  st_make_valid() %>%
  filter(!st_is_empty(.))


sf_use_s2(F)
catchments_fff <- 
  catchments_f[which(map_dbl(st_contains(catchments_f, disch_pts), length) > 0), ] # 723






sf_use_s2(F)
st_within(disch_pts, st_make_valid(catchments_ff))
which(map_dbl(st_contains(catchments_f, disch_pts), length) > 0)
  

catchments_ff %>% 
  mutate(a = st_contains(catchments_ff, disch_pts))



sf_use_s2(F)
catchments_ff %>%
  mutate(a = st_area(.) %>% units::drop_units()) %>% 
  filter(a > 1) %>%
  arrange(a) %>% 
  .[1:4,] %>% select(2) -> foo

catchments_ff <- 
  catchments_f %>% 
  mutate(id2 = row_number())


# save everything for report

miss_catch <- 
  catchments_ff %>%
  slice_max(n.overlaps) %>% 
  pull(origins) %>% 
  unlist()


catchments_ff %>%
  filter(id2 %in% miss_catch) %>% 
  select(id2) %>% 
  mapview::mapview()

write_rds(list(catchments_f,
               disch_pts,
               network_all %>%
                 filter(ORD_STRA >= 4) %>% 
                 select(ORD_STRA),
               basins),
          paste0(d, "report_data.rds"))




catchments_ff %>%
  mutate(a = st_area(.) %>% units::drop_units()) %>% 
  # arrange(a)
  filter(a > 50000) -> foo

foo %>% 
  select(-id) %>% 
  as_tibble(rownames = "id") %>% 
  st_as_sf() %>% 
  select(-a) %>% #pull(id)
  mutate(id = as.numeric(id)) %>% pull(id)
