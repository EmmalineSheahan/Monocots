# generating presence absence and coordinate matrices per region

library(sf)
library(raster)
library(rgdal)
library(dplyr)
library(rnaturalearth)
library(doParallel)

load('./data/aus_stack.Rdata')
load('./data/cal_stack.Rdata')
load('./data/chi_stack.Rdata')
load('./data/med_stack.Rdata')
load('./data/saf_stack.Rdata')

# presence absence matrix function
pam_maker <- function(wanted_stack, wanted_region) {
  mat <- matrix(NA, ncol = dim(wanted_stack)[3], 
                nrow = ncell(wanted_stack[[1]]) + 1)
  for (i in 1:dim(wanted_stack)[3]) {
    fname <- wanted_stack[[i]]@file@name
    sname <- gsub(paste0(getwd(), '/regions/', wanted_region, '/'), '', fname)
    sname <- gsub('.tif', '', sname)
    mat[1,i] <- sname
    mat[2:nrow(mat),i] <- as.numeric(values(wanted_stack[[i]]))
  }
  mat <- data.frame(mat)
  names(mat) <- lapply(mat[1, ], as.character)
  mat <- mat[-1,]
  return(mat)
}

# cordinates matrix function
coord_maker <- function(wanted_stack) {
  mat <- matrix(NA, ncol = 2, 
               nrow = ncell(wanted_stack[[1]]) + 1)
    lay <- wanted_stack[[1]]
    rclmat <- matrix(c(NA, 0), ncol = 2, byrow = T)
    lay <- reclassify(lay, rclmat)
    pts <- rasterToPoints(lay, spatial = T)
    dim(pts@coords)
    mat[2:nrow(mat),1] <- as.numeric(pts@coords[,1])
    mat[2:nrow(mat),2] <- as.numeric(pts@coords[,2])
    mat[1,1] <- "lon"
    mat[1,2] <- "lat"
    mat <- data.frame(mat)
    names(mat) <- lapply(mat[1, ], as.character)
    mat <- mat[-1,]
  return(mat)
}

# pams per region
aus_pam <- pam_maker(aus_stack, "Australia")
save(aus_pam, file = './data/aus_pam.Rdata')

cal_pam <- pam_maker(cal_stack, "California")
save(cal_pam, file = './data/cal_pam.Rdata')

med_pam <- pam_maker(med_stack, "Med")
save(med_pam, file = './data/med_pam.Rdata')

chi_pam <- pam_maker(chi_stack, "Chile")
save(chi_pam, file = './data/chi_pam.Rdata')

saf_pam <- pam_maker(saf_stack, "South_Africa")
save(saf_pam, file = './data/saf_pam.Rdata')

# coord matrices per region
aus_coords <- coord_maker(aus_stack)
save(aus_coords, file = './data/aus_coords.Rdata')

cal_coords <- coord_maker(cal_stack)
save(cal_coords, file = './data/cal_coords.Rdata')

med_coords <- coord_maker(med_stack)
save(med_coords, file = './data/med_coords.Rdata')

chi_coords <- coord_maker(chi_stack)
save(chi_coords, file = './data/chi_coords.Rdata')

saf_coords <- coord_maker(saf_stack)
save(saf_coords, file = './data/saf_coords.Rdata')