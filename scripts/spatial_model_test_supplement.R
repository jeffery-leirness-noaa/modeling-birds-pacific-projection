# create objects to run spatial models


# Markov random field


# Create neighborhood matrix
coords <- data[, c("x", "y")]
nb <- spdep::dnearneigh(as.matrix(coords), d1 = 0, d2 = your_distance)
nb_mat <- spdep::nb2mat(nb, style = "B")































