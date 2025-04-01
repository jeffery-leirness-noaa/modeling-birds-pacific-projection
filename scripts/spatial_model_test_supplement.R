# create objects to run spatial models
#must install fields

# Markov random field
install.packages(c('fields', 'spdep'))
library(fields)
library(spdep)

# get data coords


#calculate distances (fields package handles very large matrices)
dist_mat <- fields::rdist(coords_scaled)
#get median for d2 parameter
d2 <- median(dist_mat[upper.tri(dist_mat)])

#create neighborhood matrix
nb <- spdep::dnearneigh(as.matrix(coords_scaled), d1 = 0, d2 = d2)
nb_mat <- spdep::nb2mat(nb, style = "B")

#'[Note: This sequence takes a lot of memory. It was maxing out at about 52 GB. It also
#'[leaves a few large intermediate products that are unnecessary in the rest of the
#'[model workflow once nb_mat has been created.

#k-nearest neighbors approach for MRF
# Use k-nearest neighbors approach
k <- 4  # number of neighbors to consider
knn <- spdep::knearneigh(coords_scaled, k = k)
nb <- spdep::knn2nb(knn)
nb_mat <- spdep::nb2mat(nb, style = "B")


























