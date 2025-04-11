library(parallel)
library(doSNOW)
library(tidyverse)
library(ncdf4)

terraclim_path <- "/nfs/turbo/seas-zhukai/datasets/climate/TerraClimate/annual_2024_update/"
scratch_path <- "/scratch/zhukai_root/zhukai0/kcdobson/"
var_list <- c("tmax", "tmin", "ppt")

cl <- makeCluster(20, outfile = "")
doSNOW::registerDoSNOW(cl)
for (var in var_list) {
  foreach(year = seq(1998, 2024)) %dopar% {
    url <- paste0("http://thredds.northwestknowledge.net:8080/thredds/fileServer/TERRACLIMATE_ALL/data/TerraClimate_", var, "_", year, ".nc")
    system(paste0("wget ", url, " -P ", terraclim_path, " -nv"))
  }
}


# ppt
foreach(
  year = 2014:2017,
  .packages = c("raster", "ncdf4")
) %do% {
  rasterOptions(tmpdir = paste0(scratch_path, "tmpraster")) # important, otherwise might run out of memory
  ppt_file <- paste0(terraclim_path, "TerraClimate_ppt_", year, ".nc")
  
  if (!file.exists(ppt_file)) {
    stop(paste("Missing file:", ppt_file))
  }
  
  monthly_ppt <- stack(ppt_file, varname = "ppt")
  map <- mean(monthly_ppt)
  
  raster::writeRaster(map, paste0(terraclim_path, "metric/MAP_1_24degree_", year, ".tif"), overwrite = TRUE, format = "GTiff")
}

# temp
foreach(
  year = 1998:2024,
  .packages = c("raster", "ncdf4")
) %dopar% {
  rasterOptions(tmpdir = paste0(scratch_path, "tmpraster")) # important, otherwise might run out of memory
  tmax_file <- paste0(terraclim_path, "TerraClimate_tmax_", year, ".nc")
  monthly_tmax <- stack(tmax_file, varname = "tmax")
  tmin_file <- paste0(terraclim_path, "TerraClimate_tmin_", year, ".nc")
  monthly_tmin <- stack(tmin_file, varname = "tmin")
  monthly_mean <- (monthly_tmax + monthly_tmin) / 2
  mat <- mean(monthly_mean)
  
  dir.create(paste0(terraclim_path, "metric/"))
  raster::writeRaster(mat, paste0(terraclim_path, "metric/MAT_1_24degree_", year, ".tif"), overwrite = TRUE, format = "GTiff")
}


unlink(paste0(scratch_path, "tmpraster"))

stopCluster(cl)
