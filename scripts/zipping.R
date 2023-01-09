library(dplyr)

pfdf <- read.csv('./data/Hull_passfail.csv')
colnames(pfdf) <- c("species", "status", "notes")
fail_df <- pfdf %>% filter(status == 'F')
fail_list <- fail_df$species
fails <- vector(length = length(fail_list))
for (i in 1:length(fail_list)) {
  fails[i] <- paste0('./species/forBuffer/', fail_list[i], '.csv')
}
zip(zipfile = './species/failed_species.zip', files = fails)


plants <- list.files('./species/forBuffer')

