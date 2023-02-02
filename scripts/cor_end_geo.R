# checking for correlation between endemism and proportion of geophytes

library(raster)
library(dplyr)
library(ggplot2)

load('./data/aus_geo_prop.rdata')
load('./data/cal_geo_prop.rdata')
load('./data/chi_geo_prop.rdata')
load('./data/med_geo_prop.rdata')
load('./data/saf_geo_prop.rdata')
load('./data/aus_weighted_endemism.Rdata')
load('./data/cal_weighted_endemism.Rdata')
load('./data/chi_weighted_endemism.Rdata')
load('./data/med_weighted_endemism.Rdata')
load('./data/saf_weighted_endemism.Rdata')

# creating function to run lm and plot
geo_end <- function(geo, end, wanted_region) {
x_val <- values(geo)
y_val <- values(end)
y_val[y_val == "Inf"] <- NA
x_val <- na.omit(x_val)
y_val <- na.omit(y_val)

pdf(paste0('./plots/end_v_geo_', wanted_region, '.pdf'))
Q <- ggplot(mapping = aes(x_val, log(y_val))) +
  geom_point() +
  xlab("Proportion of Geophytes") +
  ylab("Corrected Weighted Endemism") +
  ggtitle(wanted_region)
print(Q)
dev.off()

ex <- lm(y_val ~ x_val)
return(summary(ex))
}

# running function across all regions
aus_geo_end <- geo_end(aus_geo_prop, aus_weighted_endemism, "Australia")
cal_geo_end <- geo_end(cal_geo_prop, cal_weighted_endemism, "California")
med_geo_end <- geo_end(med_geo_prop, med_weighted_endemism, "Mediterranean")
chi_geo_end <- geo_end(chi_geo_prop, chi_weighted_endemism, "Chile")
saf_geo_end <- geo_end(saf_geo_prop, saf_weighted_endemism, "South_Africa")
