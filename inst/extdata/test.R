hack_size <- function(x, ...) {
    UseMethod("hack_size")
}

hack_size.stanfit <- function(x) {
    x@stanmodel <- structure(numeric(0), class="stanmodel")
    x@.MISC <- new.env()
    return(x)
}

hack_size.brmsfit <- function(x) {
    x$fit <- hack_size(x$fit)
    return(x)
}

library(rstan)
rstan_options(auto_write = TRUE)
model_file <- system.file("extdata", "8schools.stan", package = "broom.mixed")
schools_dat <- list(
    J = 8,
    y = c(28, 8, -3, 7, -1, 1, 18, 12),
    sigma = c(15, 10, 16, 11, 9, 11, 10, 18)
)
set.seed(2015)
rstan_example <- stan(
    file = model_file, data = schools_dat,
    iter = 1000, chains = 2, save_dso = FALSE
)

rstan_example <- hack_size(rstan_example)

library(brms)
## ggeffects::efc, "Sample dataset from the EUROFAMCARE project"
efc <- readRDS(system.file("extdata","efc.rds",package="broom.mixed"))
print(sessionInfo())    
brms_crossedRE <- brm(mpg ~ wt + (1 | cyl) + (1 + wt | gear),
                      data = mtcars,
                      iter = 100, chains = 2,
                      family=gaussian
                      )
