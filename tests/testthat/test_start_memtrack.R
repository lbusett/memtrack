context("start_memtrack")


test_that("start_memtrack works as expected", {
  start_memtrack()
  rr <- raster::raster(ncol = 10000, nrow = 10000)
  rr <- raster::init(rr, runif)
  rr2 <- rr * 5
  rr3 <- rr * 6
  rr3 <- rr * 6
  rr4 <- rr * 6
  rr5 <- rr * 6
  raster::writeRaster(rr2, filename = tempfile(fileext = ".tif"),
                      options = c("COMPRESS = DEFLATE"))
  rm(rr2)
  rr3 <- 1

  # Sys.sleep(5)
  mem_track <- stop_memtrack()

})
