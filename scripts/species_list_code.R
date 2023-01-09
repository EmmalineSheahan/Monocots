# pulling all rasters into one place

library(dplyr)
library(doParallel)
library(sf)
library(raster)
library(rgdal)

# figuring out which file we want
test <- raster('./moose_results/resultDirMoose/Achnatherum_bromoides/Achnatherum_bromoides_SDM_PA.tif')
plot(test)
species <- list.files('./moose_results/resultDirMoose')
for (i in 1:length(species)) {
     file.copy(from = paste0('./moose_results/resultDirMoose/', 
                             species[i], '/', species[i], '_SDM_PA.tif'),
          to = paste0('./rasters/', species[i], '.tif'))
     file.remove(from = paste0('./moose_results/resultDirMoose/', 
                               species[i], '/', species[i], '_SDM_PA.tif'))
}

# creating species list
species_list <- list.files('./rasters')
species_list <- gsub('.tif.aux.xml', '', species_list)
species_list <- gsub('.tif', '', species_list)
species_list <- unique(species_list)
species_listT <- gsub('_', ' ', species_list)
write.table(species_listT, './data/species_list.txt', row.names = F, 
            col.names = F)
