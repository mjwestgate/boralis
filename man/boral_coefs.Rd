\name{boral_coefs}
\alias{boral_coefs}
\title{Extract regression coefficients from a boral model}
\description{Extracts the data necessary for drawing a caterpillar plot of regression coefficients from a boral model. Intended as an internal function called by \code{ggboral_coefs}, but included here in for users interested in exploring the data themselves.}
\usage{
ggboral_coefs(x, covname, labely, est)
}
\arguments{
  \item{x}{A boral model}
  \item{covname}{A covariate for which coefficients should be extracted. If missing or NULL, then values are extracted for all covariates in the model.}
  \item{labely}{Optional vector of strings giving alternate y axis labels.}
  \item{est}{A choice of either the posterior median (est = "median") or posterior mean (est = "mean"), which are then used as the point estimates in the lines. Default is posterior median.}
}
\details{

}
\value{
A data.frame of predictions from the boral model
}
\seealso{
	\code{\link{ggboral_coefs}} for the related plotting function.
}
\examples{

# use example from boral:
library(mvabund) ## Load a dataset from the mvabund package
data(spider)
y <- spider$abun
X <- scale(spider$x)

  ## NOTE: The values below MUST NOT be used in a real application;
## they are only used here to make the examples run quick!!!
example_mcmc_control <- list(
  n.burnin = 10,
  n.iteration = 100,
  n.thin = 1)

spiderfit_nb <- boral(
  y,
  X = X,
  family = "negative.binomial",
  lv.control = list(num.lv = 2),
  row.eff = "fixed",
  mcmc.control = example_mcmc_control
)

# generate a data.frame of predictions
df <- boral_coefs("bare.sand", x = spiderfit_nb)

# ditto, but for all variables
df <- boral_coefs(x = spiderfit_nb)

}