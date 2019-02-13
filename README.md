# boralis
useful add-ons to work with for multivariate models from <a href="https://cran.r-project.org/package=boral">boral</a>. Mainly focussed on functions to extract useful data.frames of coefficients etc. Besides being incomplete, this package also doesn't do anything especially new, and draws heavily on code already provided in boral.

<a href="https://github.com/mbedward/ggboral">ggboral</a> provides an alternative means of plotting  boral outputs using ggplot.

Currently available functions are:
- <code>boral_coefs</code> to export coefficients from a boral model
- <code>boral_coefsplot</code> as a ggplot2 replacement for <code>coefsplot</code>

Coming soon:
- <code>boral_lvsplot</code> as a replacement for <code>lvsplot</code>