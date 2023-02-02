# tree cleaning and matching

library(ape)
library(dplyr)

load('./data/chi_pam.Rdata')

# reading in the whole tree
whole_tree <- read.tree('./data/v2_undated_monocot_renamed.tre')
clean_names <- gsub("'", '', whole_tree$tip.label)
clean_names <- gsub(' ', '_', clean_names)
whole_tree$tip.label <- clean_names
length(whole_tree$tip.label)

# getting names for chile
chi_names <- list.files('./regions/Chile')
chi_names <- gsub('.tif', '', chi_names)

wanted_chi <- which(chi_names %in% whole_tree$tip.label)
wanted_chi <- chi_names[wanted_chi]

drop <- which(!(whole_tree$tip.label %in% wanted_chi))

# dropping non Chile species
chi_tree <- drop.tip(whole_tree, drop)
length(chi_tree$tip.label)

plot.phylo(chi_tree, use.edge.length = F, cex = 0.3)

# matching pam to tree
wanted_pam <- chi_pam %>% select(chi_tree$tip.label)
