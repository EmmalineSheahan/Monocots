library(dplyr)
library(raster)
library(sp)
library(rnaturalearth)

land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")[1]

load('./data/aus_richness.Rdata')
load('./data/cal_richness.Rdata')
load('./data/chi_richness.Rdata')
load('./data/med_richness.Rdata')
load('./data/saf_richness.Rdata')

# reading in geophyte scored csv
geophyte <- read.csv('./data/wcvp_names_accepted_subset_geophytescored.csv')
wanted_name_match <- vector(length = nrow(geophyte))
for (i in 1:nrow(geophyte)) {
  wanted_name_match[i] <- paste0(geophyte$genus[i], '_', geophyte$species[i],
                                 '.tif')
}

geophyte <- cbind(geophyte, wanted_name_match)
remove <- which(is.na(geophyte$geophyte))
geophyte <- geophyte[-remove,]

# creating function to write geophyte specific rasters
geo_fun <- function(wanted_reg) {
  want_list <- list.files(paste0('./regions/', wanted_reg))
  dir.create(paste0('./regions/', wanted_reg, '/geophyte_', wanted_reg))
  for (i in seq_along(geophyte$wanted_name_match)) {
    if (geophyte$wanted_name_match[i] %in% want_list == T) {
      tmp <- raster(paste0('./regions/', wanted_reg, '/', 
                           geophyte$wanted_name_match[i]))
      if (geophyte$geophyte[i] == 0) {
        rclmat <- matrix(c(0,1,0), ncol = 3, byrow = T)
        tmp <- reclassify(tmp, rclmat)
        writeRaster(tmp, filename = paste0('./regions/', wanted_reg, 
        '/geophyte_', wanted_reg, '/', geophyte$wanted_name_match[i]), 
        overwrite = T)
      } else {
        writeRaster(tmp, filename = paste0('./regions/', wanted_reg, 
                                       '/geophyte_', wanted_reg, '/', 
                                       geophyte$wanted_name_match[i]), 
                    overwrite = T)
      }
    } else {
      print("not in region")
    }
  }
}


geo_fun("Australia")
geo_fun("California")
geo_fun("Chile")
geo_fun("Med")
geo_fun("South_Africa")

# creating lists of filenames to stack
species_aus_geo <- list.files('./regions/Australia/geophyte_Australia')
for (i in 1:length(species_aus_geo)) {
  species_aus_geo[i] <- paste0('./regions/Australia/geophyte_Australia/', 
                           species_aus_geo[i])
}
aus_list_geo <- as.list(species_aus_geo)

species_cal_geo <- list.files('./regions/California/geophyte_California')
for (i in 1:length(species_cal_geo)) {
  species_cal_geo[i] <- paste0('./regions/California/geophyte_California/', 
                           species_cal_geo[i])
}
cal_list_geo <- as.list(species_cal_geo)

species_chi_geo <- list.files('./regions/Chile/geophyte_Chile')
for (i in 1:length(species_chi_geo)) {
  species_chi_geo[i] <- paste0('./regions/Chile/geophyte_Chile/', 
                               species_chi_geo[i])
}
chi_list_geo <- as.list(species_chi_geo)

species_med_geo <- list.files('./regions/Med/geophyte_Med')
for (i in 1:length(species_med_geo)) {
  species_med_geo[i] <- paste0('./regions/Med/geophyte_Med/', 
                               species_med_geo[i])
}
med_list_geo <- as.list(species_med_geo)

species_saf_geo <- list.files('./regions/South_Africa/geophyte_South_Africa')
for (i in 1:length(species_saf_geo)) {
  species_saf_geo[i] <- paste0('./regions/South_Africa/geophyte_South_Africa/',
                               species_saf_geo[i])
}
saf_list_geo <- as.list(species_saf_geo)

# creating stacks per region
aus_stack_geo <- stack(aus_list_geo)
save(aus_stack_geo, file = './data/aus_stack_geo.Rdata')

cal_stack_geo <- stack(cal_list_geo)
save(cal_stack_geo, file = './data/cal_stack_geo.Rdata')

chi_stack_geo <- stack(chi_list_geo)
save(chi_stack_geo, file = './data/chi_stack_geo.Rdata')

med_stack_geo <- stack(med_list_geo)
save(med_stack_geo, file = './data/med_stack_geo.Rdata')

saf_stack_geo <- stack(saf_list_geo)
save(saf_stack_geo, file = './data/saf_stack_geo.Rdata')

land_mask <- as_Spatial(land)

# richness plots
aus_richness_geo <- calc(aus_stack_geo, fun = sum, na.rm = T)
aus_richness_geo <- mask(aus_richness_geo, land_mask)
save(aus_richness_geo, file = './data/aus_richness_geo.Rdata')

cal_richness_geo <- calc(cal_stack_geo, fun = sum, na.rm = T)
cal_richness_geo <- mask(cal_richness_geo, land_mask)
save(cal_richness_geo, file = './data/cal_richness_geo.Rdata')

chi_richness_geo <- calc(chi_stack_geo, fun = sum, na.rm = T)
chi_richness_geo <- mask(chi_richness_geo, land_mask)
save(chi_richness_geo, file = './data/chi_richness_geo.Rdata')

med_richness_geo <- calc(med_stack_geo, fun = sum, na.rm = T)
med_richness_geo <- mask(med_richness_geo, land_mask)
save(med_richness_geo, file = './data/med_richness_geo.Rdata')

saf_richness_geo <- calc(saf_stack_geo, fun = sum, na.rm = T)
saf_richness_geo <- mask(saf_richness_geo, land_mask)
save(saf_richness_geo, file = './data/saf_richness_geo.Rdata')

# dividing geophyte richness by total richness and plotting
aus_geo_prop <- aus_richness_geo/aus_richness
pdf('./plots/geophyte_proportion_Australia.pdf')
plot(aus_geo_prop, main = "Proportion of geophytes in Australia")
plot(land, add = T, col = NA)
dev.off()
save(aus_geo_prop, file = './data/aus_geo_prop.rdata')

cal_geo_prop <- cal_richness_geo/cal_richness
pdf('./plots/geophyte_proportion_California.pdf')
plot(cal_geo_prop, main = "Proportion of geophytes in California")
plot(land, add = T, col = NA)
dev.off()
save(cal_geo_prop, file = './data/cal_geo_prop.rdata')

chi_geo_prop <- chi_richness_geo/chi_richness
pdf('./plots/geophyte_proportion_Chile.pdf')
plot(chi_geo_prop, main = "Proportion of geophytes in Chile")
plot(land, add = T, col = NA)
dev.off()
save(chi_geo_prop, file = './data/chi_geo_prop.rdata')

med_geo_prop <- med_richness_geo/med_richness
pdf('./plots/geophyte_proportion_Med.pdf')
plot(med_geo_prop, main = "Proportion of geophytes in the Mediterranean")
plot(land, add = T, col = NA)
dev.off()
save(med_geo_prop, file = './data/med_geo_prop.rdata')

saf_geo_prop <- saf_richness_geo/saf_richness
pdf('./plots/geophyte_proportion_South_Africa.pdf')
plot(saf_geo_prop, main = "Proportion of geophytes in South Africa")
plot(land, add = T, col = NA)
dev.off()
save(saf_geo_prop, file = './data/saf_geo_prop.rdata')

