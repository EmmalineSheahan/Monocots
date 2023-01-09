# species richness and PAMs

library(sf)
library(raster)
library(rgdal)
library(dplyr)
library(rnaturalearth)
library(doParallel)

land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")[1]

species_list <- list.files('./rasters')
species_list <- gsub('.tif.aux.xml', '', species_list)
species_list <- gsub('.tif', '', species_list)
species_list <- unique(species_list)
species_listT <- gsub('_', ' ', species_list)

# reading in region shape files
Australia <- shapefile('./masterShpFiles/AU_Buffer.shp')
California <- shapefile('./masterShpFiles/CA_Buffer.shp')
Chile <- shapefile('./masterShpFiles/CL_Buffer.shp')
Med <- shapefile('./masterShpFiles/ME_Buffer.shp')
South_Africa <- shapefile('./masterShpFiles/SA_Buffer.shp')

# create separate directories per region
dir.create('./regions/Australia')
dir.create('./regions/California')
dir.create('./regions/Chile')
dir.create('./regions/Med')
dir.create('./regions/South_Africa')

# finding species with weird list files error
flag <- integer()
for (i in 1:length(species_list)) {
  tryCatch(
    {
      test <- raster(paste0('./rasters/', species_list[i], '.tif'))
    },
    error=function(err){
      message('On iteration ',i, ' there was an error: ',err)
      flag <<-c(flag,i)
    }
  )
}
missed <- species_list[flag]
missedT <- gsub("_", " ", missed)
write.table(missedT, './data/missed_species.txt', row.names = F, col.names = F)

species_list_use <- species_list[-flag]

# dir_sep function to separate the rasters into directories by region
# and align their extents
dir_sep <- function(wanted_dir, wanted_dir_name, no_of_features) {
  for (i in 1:length(species_list_use)) {
    test <- raster(paste0('./rasters/', species_list_use[i], '.tif'))
    test_mat <- extract(test, wanted_dir)
    test_mat[[1]][test_mat[[1]] == "NaN"] <- NA
    if (no_of_features == 2) {
    test_mat_sum1 <- sum(test_mat[[1]], na.rm = T)
    test_mat_sum2 <- sum(test_mat[[2]], na.rm = T)
    test_mat_sum <- sum(test_mat_sum1, test_mat_sum2)
    } else {
      test_mat_sum <- sum(test_mat[[1]], na.rm = T)
    }
      if (test_mat_sum > 0) {
        if (extent(test) > extent(wanted_dir)) {
          wanted_ras1 <- crop(test, wanted_dir)
          wanted_ras <- extend(wanted_ras1, wanted_dir)
          wanted_ras <- setExtent(wanted_ras, ext = extent(wanted_dir))
          wanted_ras2 <- raster(ext = extent(wanted_dir), 
                                resolution = res(test))
          wanted_ras <- resample(wanted_ras, wanted_ras2, method = 'ngb')
          wanted_ras <- mask(wanted_ras, wanted_dir)
        } else {
          wanted_ras <- extend(test, wanted_dir)
          wanted_ras <- setExtent(wanted_ras, ext = extent(wanted_dir))
          wanted_ras2 <- raster(ext = extent(wanted_dir), 
                                resolution = res(test))
          wanted_ras <- resample(wanted_ras, wanted_ras2, method = 'ngb')
          wanted_ras <- mask(wanted_ras, wanted_dir)
        }
        writeRaster(wanted_ras, filename = paste0('./regions/', 
                                                  wanted_dir_name, '/',
                                                  species_list_use[i], '.tif'),
                    format = "GTiff", overwrite = T)
    }
  }
}

no_cores <- 10  
registerDoParallel(cores=no_cores)  
cl <- makeCluster(no_cores)

dir_sep(Australia, "Australia")
dir_sep(California, "California")
dir_sep(Chile, "Chile")
dir_sep(Med, "Med")
dir_sep(South_Africa, "South_Africa")

stopCluster(cl)

# creating lists of filenames to stack
species_aus <- list.files('./regions/Australia')
for (i in 1:length(species_aus)) {
  species_aus[i] <- paste0('./regions/Australia/', species_aus[i])
}
aus_list <- as.list(species_aus)

species_cal <- list.files('./regions/California')
for (i in 1:length(species_cal)) {
  species_cal[i] <- paste0('./regions/California/', species_cal[i])
}
cal_list <- as.list(species_cal)

species_chi <- list.files('./regions/Chile')
for (i in 1:length(species_chi)) {
  species_chi[i] <- paste0('./regions/Chile/', species_chi[i])
}
chi_list <- as.list(species_chi)

species_med <- list.files('./regions/Med')
for (i in 1:length(species_med)) {
  species_med[i] <- paste0('./regions/Med/', species_med[i])
}
med_list <- as.list(species_med)

species_saf <- list.files('./regions/South_Africa')
for (i in 1:length(species_saf)) {
  species_saf[i] <- paste0('./regions/South_Africa/', species_saf[i])
}
saf_list <- as.list(species_saf)

# creating stacks per region
aus_stack <- stack(aus_list)
save(aus_stack, file = './data/aus_stack.Rdata')

cal_stack <- stack(cal_list)
save(cal_stack, file = './data/cal_stack.Rdata')

chi_stack <- stack(chi_list)
save(chi_stack, file = './data/chi_stack.Rdata')

med_stack <- stack(med_list)
save(med_stack, file = './data/med_stack.Rdata')

saf_stack <- stack(saf_list)
save(saf_stack, file = './data/saf_stack.Rdata')

land_mask <- as_Spatial(land)

# richness plots
aus_richness <- calc(aus_stack, fun = sum, na.rm = T)
aus_richness <- mask(aus_richness, land_mask)

cal_richness <- calc(cal_stack, fun = sum, na.rm = T)
cal_richness <- mask(cal_richness, land_mask)

chi_richness <- calc(chi_stack, fun = sum, na.rm = T)
chi_richness <- mask(chi_richness, land_mask)

med_richness <- calc(med_stack, fun = sum, na.rm = T)
med_richness <- mask(med_richness, land_mask)

saf_richness <- calc(saf_stack, fun = sum, na.rm = T)
saf_richness <- mask(saf_richness, land_mask)

pdf('./plots/richness_Australia.pdf')
plot(aus_richness, main = "Australia")
plot(land, add = T, col = NA)
dev.off()

pdf('./plots/richness_California.pdf')
plot(cal_richness, main = "California")
plot(land, add = T, col = NA)
dev.off()

pdf('./plots/richness_Chile.pdf')
plot(chi_richness, main = "Chile")
plot(land, add = T, col = NA)
dev.off()

pdf('./plots/richness_Mediterranean.pdf')
plot(med_richness, main = "Mediterranean")
plot(land, add = T, col = NA)
dev.off()

pdf('./plots/richness_South_Africa.pdf')
plot(saf_richness, main = "South Africa")
plot(land, add = T, col = NA)
dev.off()

# write region lists
aus_species <- list.files('./regions/Australia')
aus_species <- gsub('.tif', '', aus_species)
aus_species <- gsub('_', ' ', aus_species)
write.table(aus_species, './data/aus_species.txt', row.names = F, 
            col.names = F)

cal_species <- list.files('./regions/California')
cal_species <- gsub('.tif', '', cal_species)
cal_species <- gsub('_', ' ', cal_species)
write.table(cal_species, './data/cal_species.txt', row.names = F, 
            col.names = F)

med_species <- list.files('./regions/Med')
med_species <- gsub('.tif', '', med_species)
med_species <- gsub('_', ' ', med_species)
write.table(med_species, './data/med_species.txt', row.names = F, 
            col.names = F)

chi_species <- list.files('./regions/Chile')
chi_species <- gsub('.tif', '', chi_species)
chi_species <- gsub('_', ' ', chi_species)
write.table(aus_species, './data/chi_species.txt', row.names = F, 
            col.names = F)

saf_species <- list.files('./regions/South_Africa')
saf_species <- gsub('.tif', '', saf_species)
saf_species <- gsub('_', ' ', saf_species)
write.table(saf_species, './data/saf_species.txt', row.names = F, 
            col.names = F)

species_list_useT <- gsub('_', ' ', species_list_use)
write.table(species_list_useT, './data/species_list_use.txt', row.names = F, 
            col.names = F)

# investigating Australia
Australia_continent <- rnaturalearth::ne_countries(country = "Australia", 
                                                   scale = "medium", 
                                                   returnclass = "sf")[1]
Australia_continent <- as_Spatial(Australia_continent)

dir.create('./regions/Australia_continent')

no_cores <- 10  
registerDoParallel(cores=no_cores)  
cl <- makeCluster(no_cores)

dir_sep(Australia_continent, "Australia_continent")

stopCluster(cl)

aus_species_cont <- list.files('./regions/Australia_continent')
aus_species_cont <- gsub('.tif', '', aus_species_cont)
aus_species_cont <- gsub('_', ' ', aus_species_cont)

length(aus_species)
length(aus_species_cont)

wanted <- (which(!(aus_species_cont %in% aus_species)))
aus_species_missing <- aus_species_cont[wanted]

write.table(aus_species_missing, './data/aus_species_missing.txt', 
            row.names = F, 
            col.names = F)

test_sp <- raster('./rasters/Agrostocrinum_hirsutum.tif')
