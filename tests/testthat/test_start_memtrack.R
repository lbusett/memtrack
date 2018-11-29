context("start_perftrack")


test_that("start_perftrack works as expected", {
  start_perftrack(trackdisk = TRUE, trackCPU = TRUE)
                  # , add_processes = c("gdalwarp"))
  rr <- raster::raster(ncol = 10000, nrow = 1000)
  rr <- raster::init(rr, runif)
  rr2 <- raster::init(rr, runif)
  rr3 <- raster::init(rr, runif)
  rr5 <- raster::init(rr, runif)
  temp_file <- tempfile(fileext = ".tif")
  raster::writeRaster(rr2, filename = temp_file,
                      options = c("COMPRESS = DEFLATE"))
#   raster::writeRaster(rr2, filename = tempfile(fileext = ".tif"),
#                       options = c("COMPRESS = DEFLATE"))
#   raster::writeRaster(rr2, filename = tempfile(fileext = ".tif"),
#                       options = c("COMPRESS = DEFLATE"))
  # gdalUtils::gdalwarp(temp_file, tempfile(fileext = ".tif"),
  #                     t_srs='+proj=utm +zone=11 +datum=WGS84')
# rm(rr2)
# rr3 <- 1

# Sys.sleep(5)
mem_track <- stop_perftrack()

})


