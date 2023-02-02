# taxonomic endemism

library(dplyr)
library(raster)
library(sp)
library(rnaturalearth)
library(terra)

load('./data/aus_richness.Rdata')
load('./data/cal_richness.Rdata')
load('./data/chi_richness.Rdata')
load('./data/med_richness.Rdata')
load('./data/saf_richness.Rdata')

land <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")[1]
land_mask <- as_Spatial(land)

calc_endemism <- function(wanted_region) {
  species <- list.files(paste0('./regions/', wanted_region))
  endemism_ras_list <- vector("list", length = length(species))
  for (i in 1:length(species)) {
    ras <- raster(paste0('./regions/', wanted_region, '/', species[i]))
    ras <- ras %>% rast()
    end_ras <- terra::focal(ras, w=3, fun = sum,
                            na.rm = T)
    end_ras <- end_ras/sum(values(ras), na.rm = T)
    end_ras <- raster(end_ras)
    endemism_ras_list[[i]] <- end_ras
  }
  end_stack <- stack(endemism_ras_list)
  endimism <- calc(end_stack, fun = sum, na.rm = T)
  endimism <- mask(endimism, land_mask)
  return(endimism)
}

aus_endemism <- calc_endemism("Australia")
aus_weighted_endemism <- aus_endemism/aus_richness
save(aus_weighted_endemism, file = './data/aus_weighted_endemism.Rdata')
pdf('./plots/aus_weighted_endemism.pdf')
plot(log(aus_weighted_endemism), 
     main = "Australia Corrected Weighted Endemism")
plot(land, add = T, col = NA)
dev.off()

cal_endemism <- calc_endemism("California")
cal_weighted_endemism <- cal_endemism/cal_richness
save(cal_weighted_endemism, file = './data/cal_weighted_endemism.Rdata')
pdf('./plots/cal_weighted_endemism.pdf')
plot(log(cal_weighted_endemism), 
     main = "California Corrected Weighted Endemism")
plot(land, add = T, col = NA)
dev.off()

med_endemism <- calc_endemism("Med")
med_weighted_endemism <- med_endemism/med_richness
save(med_weighted_endemism, file = './data/med_weighted_endemism.Rdata')
pdf('./plots/med_weighted_endemism.pdf')
plot(log(med_weighted_endemism), 
     main = "Mediterranean Corrected Weighted Endemism")
plot(land, add = T, col = NA)
dev.off()

chi_endemism <- calc_endemism("Chile")
chi_weighted_endemism <- chi_endemism/chi_richness
save(chi_weighted_endemism, file = './data/chi_weighted_endemism.Rdata')
pdf('./plots/chi_weighted_endemism.pdf')
plot(log(chi_weighted_endemism), 
     main = "Chile Corrected Weighted Endemism")
plot(land, add = T, col = NA)
dev.off()

saf_endemism <- calc_endemism("South_Africa")
saf_weighted_endemism <- saf_endemism/saf_richness
save(saf_weighted_endemism, file = './data/saf_weighted_endemism.Rdata')
pdf('./plots/saf_weighted_endemism.pdf')
plot(log(saf_weighted_endemism), 
     main = "South Africa Corrected Weighted Endemism")
plot(land, add = T, col = NA)
dev.off()
