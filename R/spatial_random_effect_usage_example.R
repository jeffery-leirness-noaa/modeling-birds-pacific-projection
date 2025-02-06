# Spatial Random Effect with Markov random Field Usage Example -----------------

# From the mgcv documentation:

# Markov random fields as a spatial random effect are implemented using smooth
# term via mrf smooth class (bs = 'mrf'). Defining the smooth class is the basis
# used to represent the smooth function and quadratic penalty (or multiple penalties)
# used to penalize the basis coefficients in order to control the degree of smoothness
# The 'mrf' smooth class is invoked directly by the s term in the model formula.

# Markov random field is a single penalty built in smooth class (bs = 'mrf') that
# is popular when space is split up into discrte contiguous geographic units.
# The simple smoothing penalty is constructed based on the neighborhood structure
# of the geographic units (in our case these are the observation points of species
# counts)

# Create neighborhood matrix
coords <- data[, c("x", "y")]
nb <- spdep::dnearneigh(as.matrix(coords), d1 = 0, d2 = your_distance)
nb_mat <- spdep::nb2mat(nb, style = "B")

# Create model formula with MRF
formula <- create_model_formula_mgcv(
  lhs = "abundance",
  type = "hindcast",
  spatial_random_effect = TRUE,
  mrf = TRUE
)

# Create workflow with MRF
workflow <- define_model_workflow(
  model_formula = formula,
  data = your_data,
  species_size_class = "your_class",
  nb_mat = nb_mat
)

# Additional notes on implementation -------------------------------------------

# The neighborhood matrix defines the spatial relationships between observation
# points. The process works like this:

  # Each row in your data represents a sampling location with:
    # x,y coordinates
    # Species count
    # Environmental variables

# The dnearneigh() function creates connections between points based on distance criteria:
  # d1: minimum distance (typically 0)
  # d2: maximum distance within which points are considered neighbors
  # This matrix then informs the MRF smooth about spatial autocorrelation structure in your data.

# The choice of d2 is important - too small means insufficient spatial
# smoothing, too large means over-smoothing. Consider your study system's
# spatial scale when selecting this parameter.

#Example:

# Example with sample points
coords <- cbind(
  x = c(1, 1.5, 2, 3, 2.5),
  y = c(1, 1.2, 1, 2, 1.8)
)

# Points within 1 unit distance are neighbors
nb <- spdep::dnearneigh(coords, d1 = 0, d2 = 1)

# Convert to binary matrix (1 = neighbors, 0 = not neighbors)
nb_mat <- spdep::nb2mat(nb, style = "B")

# Proper evaluation the study system's spatial scale ---------------------------

# Domain knowledge criteria:
  # Movement range of study species
  # Known environmental correlation distances
  # Sampling design spacing Management-relevant spatial scales

# Consider multiple approaches and validate choice through model
# diagnostics and residual spatial autocorrelation tests.

# Systematic approaches to selecting the d2 parameter for the spatial neighborhood

# Empirical variogram analysis ----

library(gstat)
library(sf)

# Convert points to sf object
points_sf <- st_as_sf(data, coords = c("x", "y"))

# Calculate empirical variogram
v <- variogram(abundance ~ 1, data = points_sf)
plot(v)

# Find range parameter
v_fit <- fit.variogram(v, model = vgm("Sph"))
range <- v_fit$range[2]  # Use this as guidance for d2

# Average nearest neighbor distance ----

library(spatstat)

# Calculate nearest neighbor distances
nn_dist <- nndist(ppp(x = data$x, y = data$y,
                      window = owin(range(data$x), range(data$y))))

# Summary statistics
d2_suggestions <- c(
  mean = mean(nn_dist),
  median = median(nn_dist),
  q75 = quantile(nn_dist, 0.75)
)

# Cross-validation approach ----

test_d2 <- function(data, d2_values) {
  cv_scores <- numeric(length(d2_values))

  for(i in seq_along(d2_values)) {
    nb <- spdep::dnearneigh(as.matrix(data[,c("x","y")]),
                            d1 = 0,
                            d2 = d2_values[i])
    # Fit model and get CV score
    # ... model fitting code ...
  }
  return(cv_scores)
}

d2_values <- seq(min_dist, max_dist, length.out = 10)
scores <- test_d2(data, d2_values)

# For spatial random effects in species distribution modeling, you should base
# the neighborhood distance (d2) on the observation points rather than the
# underlying environmental raster resolution. Here's why:

# Statistical Considerations
# The MRF models spatial autocorrelation in your response variable (species counts)
# This autocorrelation exists between sampling locations, not raster cells
# The 10m grid resolution is relevant for environmental covariates but not necessarily for species spatial patterns

library(spatstat)

# Examine distribution of inter-point distances
points_ppp <- ppp(x = data$x, y = data$y,
                  window = owin(range(data$x), range(data$y)))

# Get distance matrix between all points
dist_mat <- pairdist(points_ppp)

# Summary of distances
summary(as.vector(dist_mat[upper.tri(dist_mat)]))

# Plot distance distribution
hist(dist_mat[upper.tri(dist_mat)],
     breaks = 50,
     main = "Distribution of Inter-point Distances")


# Selection Strategy
  # Start with median nearest-neighbor distance
  # Test multiple distances using cross-validation
  # Check residual spatial autocorrelation at different distances
  # Consider biological relevance (e.g., species movement patterns)
  # The 10m grid resolution is too fine for defining neighborhoods unless your
    # sampling points are that close together, which is unlikely in most ecological surveys.

# So, we should probably evaluate this for each species independently, if all are
# about the same we could use the same d2 parameter for the mrf, but we may need to
# develop a function that evaluates for optimal species d2 in order to implement this
# efficiently


# Other spatial random effect options available in mgcv ------------------------

# Key considerations:
  # MRF works best with irregular sampling
  # Gaussian process better for continuous surfaces
  # Soap film superior near complex boundaries
  # Tensor products handle anisotropic patterns (spatial correlation varies by
    # direction rather than being uniform in all directions)

# Choice depends on:
  # Sampling design
  # Study area geometry
  # Computational resources
  # Expected spatial patterns

# Soap film smoothing - for complex boundaries/coastlines ----
gam_model <- gam(
  abundance ~ s(x, y, bs = "so", xt = list(bnd = boundary)),
  family = nb()
)

# Gaussian process with Matern correlation ----
gam_model <- gam(
abundance ~ s(x, y, bs = "gp", m = c(2, 3/2)),
family = nb()
)

# In species distribution modeling, a continuous surface represents smoothly varying spatial patterns.
# Examples include:

# Environmental gradients:
  # Sea surface temperature that changes gradually across an ocean basin
  # Depth profiles that smoothly transition across a study area
  # Elevation gradients in terrestrial systems

# Gaussian processes work well when you expect the underlying process to vary
# smoothly in space, without sharp discontinuities.

# Example implementation with simulated data:
library(mgcv)
library(ggplot2)

# Create smooth environmental surface
n <- 50
x <- seq(0, 1, length = n)
y <- seq(0, 1, length = n)
grid <- expand.grid(x = x, y = y)

# Simulate smooth surface with correlation
sigma <- 0.3  # correlation length scale
D <- as.matrix(dist(grid))
Sigma <- exp(-D/sigma)
grid$z <- mvrnorm(1, mu = rep(0, n*n), Sigma = Sigma)

# Fit GAM with Gaussian process
m <- gam(z ~ s(x, y, bs = "gp", m = c(2, 3/2)), data = grid)

# Visualize
ggplot(grid, aes(x, y, fill = z)) +
  geom_raster() +
  scale_fill_viridis_c() +
  theme_minimal()

# Tensor product of 1d smooths - allows different smoothing in x/y -----
gam_model <- gam(
  abundance ~ te(x, y, bs = "cr"),
  family = nb()
)

# Examples in species distribution modeling:
#
  # Fish abundance varying more strongly along-shore vs cross-shore
  # Bird distributions following river valleys
  # Plant species following elevation gradients
  # Visual example of anisotropic vs isotropic patterns:

# 2d spline + spatially varying coefficients ----
gam_model <- gam(
  abundance ~ s(x, y) + s(x, y, by = environmental_var),
  family = nb()
)

