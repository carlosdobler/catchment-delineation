
# V3.2
# 1. Instead of using a stream network to obtain discharge points, identify the
#    location within each HydroBASINS catchment with the highest accumulation (ups).
# 2. For each discharge point, draw a catchment using the ldd map
# 3. Merge all catchments
# 4. Repeat steps above for areas not covered in the first pass


# Why 2 passes? Since we are forcing discharge points to be inside HydroBASINS
# basins, there are some areas that flow towards a distant discharge point, but they
# don't have their own (nearby) discharge point (probably due to HydroBASINS being 
# delineated with a different ldd map). These areas thus remain "orphan". The 
# solution is to find a discharge point within them and re-run the process. At 
# the end, both sets of catchments are merged, and the flow hierarchy correctly mapped.



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
write_stars(ldd, paste0(d, "/ldd.tif"), type = "Int16")

# upstream accumulated flow
ups <- 
  paste0(d_cwatm, "global_files/ups.nc") %>% 
  read_ncdf() %>% 
  st_crop(aoi_ext)




# STEP 2: PREPARE HYDROBASINS -------------------------------------------------

# level 6
basins <- 
  paste0(d_misc, "hybas_na_lev06_v1c/") %>% 
  st_read() %>% 
  st_crop(ldd) %>% 
  mutate(HYBAS_ID = factor(HYBAS_ID),
         area = st_area(.) %>% units::drop_units()) %>% 
  select(HYBAS_ID, area)

# remove small basins
# (areas removed will be covered by 2nd pass)
basins <- 
  basins %>% 
  filter(area > 1e9)
  
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





# STEP 3: OBTAIN DISCHARGE POINTS: 1ST PASS ------------------------------------

# Use the original ups map (not constrained by the stream network)

disch_pts_1 <-
  c(basins_r,
    ups %>% setNames("ups_whole")) %>%
  as_tibble() %>%
  filter(!is.na(HYBAS_ID)) %>%
  group_by(HYBAS_ID) %>%
  mutate(ups_max = max(ups_whole, na.rm = T)) %>%
  # mutate(ups_max = quantile(ups_whole, 0.99, na.rm = T, type = 1)) %>%
  filter(ups_whole == ups_max) %>%
  mutate(d = duplicated(ups_max)) %>% 
  filter(d != T) %>%
  ungroup() %>%
  select(lon, lat) %>%
  mutate(id = row_number()) %>% 
  as.data.frame() %>% 
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(4326)


# is the number of discharge points the same  
# as HydroBASINS basins?
identical(basins$HYBAS_ID %>% 
            na.omit() %>% 
            unique() %>% 
            length(),
          
          disch_pts_1$id %>%  
            na.omit() %>% 
            unique() %>% 
            length())




# STEP 4: DRAW CATCHMENTS: 1ST PASS -------------------------------------------

# draw a catchment for each point

catchments_1 <- 
  future_map(disch_pts_1$id, function(i) { # run in parallel
    
    # select discharge pt
    outlet <- 
      disch_pts_1 %>% 
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
    p <- 
      catch$output %>% 
      read_stars() %>% 
      st_as_sf(as_points = F, merge = F) %>% 
      summarize() %>%
      mutate(idx = i) %>% # assign id
      select(idx)
    
    return(p)
    
  })




# STEP 5: IDENTIFY UNCOVERED REGIONS ------------------------------------------

catchments_1_m <-
  catchments_1 %>% 
  bind_rows() %>%
  st_set_precision(1e6) %>% # avoid GEOS error
  st_intersection() %>% # merge pols
  st_collection_extract("POLYGON") %>% 
  st_join(disch_pts_1) %>% # overlay with disch pts
  filter(!is.na(id)) # remove orphan regions

uncov_reg <- 
  catchments_1_m %>% 
  mutate(a = 0) %>% 
  select(a) %>% 
  st_rasterize(ldd %>% mutate(a = 1) %>% select(a)) %>% 
  st_warp(ldd) %>% 
  mutate(a = if_else(a == 0, NA, 1))

# assign unique id to each polygon
uncov_reg <-
  uncov_reg %>% 
  st_as_sf(as_points = F, merge = T, connect8 = T) %>% 
  mutate(id = row_number()) %>% 
  select(id) %>%
  st_rasterize(ldd %>% mutate(a = NA) %>% select(a)) %>% 
  st_warp(ldd)


# STEP 6: OBTAIN DISCHARGE POINTS: 2ND PASS ------------------------------------

disch_pts_2 <-
  c(uncov_reg,
    ups %>% setNames("ups_whole")) %>%
  as_tibble() %>%
  filter(!is.na(id)) %>%
  group_by(id) %>%
  mutate(ups_max = max(ups_whole, na.rm = T)) %>%
  filter(ups_whole == ups_max) %>%
  mutate(d = duplicated(ups_max)) %>% 
  filter(d != T) %>%
  ungroup() %>%
  select(lon, lat) %>%
  
  # id numbers to continue from disch_pts_1
  mutate(id = row_number() + nrow(disch_pts_1)) %>% 
  as.data.frame() %>% 
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(4326)




# STEP 7: DRAW CATCHMENTS: 2ND PASS -------------------------------------------

# draw a catchment for each point

catchments_2 <- 
  future_map(disch_pts_2$id, function(i) { # run in parallel
    
    # select discharge pt
    outlet <- 
      disch_pts_2 %>% 
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
    p <- 
      catch$output %>% 
      read_stars() %>% 
      st_as_sf(as_points = F, merge = F) %>% 
      summarize() %>%
      mutate(idx = i) %>% # assign id
      select(idx)
    
    return(p)
    
  })




# STEP 8: MERGE EVERYTHING ----------------------------------------------------

catchments_f <- 
  bind_rows(catchments_1, catchments_2) %>%
  st_set_precision(1e6) %>% # avoid GEOS error
  st_intersection() %>% # merge pols
  st_collection_extract("POLYGON")

disch_pts_f <- 
  bind_rows(disch_pts_1, disch_pts_2)


# fix id (messed up when intersecting)
catchments_ff <- 
  catchments_f %>%
  st_join(disch_pts_f) %>% 
  select(id, everything(), -idx) %>% 
  filter(!is.na(id))




# save everything for report


write_rds(list(catchments_ff,
               disch_pts_f,
               network_all %>%
                 filter(ORD_STRA >= 4) %>% 
                 select(ORD_STRA),
               basins),
          paste0(d, "report_data.rds"))





