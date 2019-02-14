\name{boral_coefsplot}
\alias{boral_coefsplot}
\title{Caterpillar plots of regression coefficients from a boral model using ggplot2}
\description{Constructs horizontal line plot (point estimate and HPD intervals), otherwise known as "caterpillar plots", for the column-specific regression coefficients corresponding to a covariate in X fitted in the boral model.}
\usage{
boral_coefsplot(x, covname, est)
}
\arguments{
  \item{x}{either a boral model, or a data.frame with same column names as returned by \code{boral_coefs}.}
  \item{covname}{A covariate for which coefficients should be extracted. If missing or NULL, then values are extracted for all covariates in the model.}
  \item{est}{A choice of either the posterior median (est = "median") or posterior mean (est = "mean"), which are then used as the point estimates in the lines. Default is posterior median.}
}
\details{
This function duplicates \code{boral::coefsplot}, but draws the figure using \code{ggplot2} rather than \code{graphics}. It differs from \code{coefsplot} in two ways. First, the order of arguments has been altered to place the argument \code{x} (i.e. the boral model) first, for consistency with \code{lvsplot}. Second, alterations to the plot must now be accomplished by adding parameters to the returned \code{ggplot} object, not using \code{...}; see examples.
}
\value{
Invisibly returns a ggplot2 object
}
\seealso{
\code{\link{boral_coefs}} for the underlying data extraction function.
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

# build model
spiderfit_nb <- boral(
  y,
  X = X,
  family = "negative.binomial",
  lv.control = list(num.lv = 2),
  row.eff = "fixed",
  mcmc.control = example_mcmc_control
)

# generate some plots of the coefficients from a single variable
coefsplot(spiderfit_nb, "bare.sand") # traditional method
ggboral_coefs(spiderfit_nb, "bare.sand") # new method

# to alter the appearance of the plot, add extra ggplot2 functions
boral_coefsplot(spiderfit_nb, "bare.sand") + theme_void()

# new feature: plot all variables from the model at once
boral_coefs(spiderfit_nb)

}