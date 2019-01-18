# testing
# covname <- "weight"
# x <- model_percent
# library(ggplot2)

# necessary attributes of ggboral package
  # functions return a ggplot object so the user can add extra args
  # should allow no variables to be specified, returning plots for all variables
  # note also need replacement for lvsplot
  # might be useful to have functions to plot residual correlations as well


# function to return coefficients for a specified predictor
calc_coefs <- function(covname, x, labely = NULL, est = "median", ...){

  if (!is.null(labely))
      if (!(length(labely) == nrow(x$X.coefs.median) || length(labely) ==
          1))
          stop("If labely is not NULL, then it must be either of length one or a vector as long as the number of rows in x$X.coefs.median (number of species). Thanks!")
  if (!(covname %in% colnames(x$X.coefs.mean)))
      stop("covname not found among the covariates in the boral object x")

  # new code
  lci <- x$hpdintervals$X.coefs[, covname, "lower"]
  uci <- x$hpdintervals$X.coefs[, covname, "upper"]

  data <- data.frame(
    labels = NA,
    x = x[[paste0("X.coefs.", est)]][, covname],
    x0 = lci,
    x1 = uci,
    y = rev(seq_len(nrow(x$X.coefs.median))),
    color = "black",
    stringsAsFactors = FALSE
  )
  rownames(data) <- NULL
  data$color[lci < 0 & uci > 0] <- "grey"
  if(is.null(labely)) {
    data$labels <- rownames(x$X.coefs.mean)
  }else{
    data$labels <- labely
  }

  return(data)
}


# function to draw ggplot versions of the caterpillar plots returned by boral::coefsplot
ggcoefs <- function (covname, x, labely = NULL, est = "median", ...)
{
  if(missing(covname)){
    covname <- colnames(x$X.coefs.mean)
  }

  if(length(covname) > 1){
    lapply(covname, calc_coefs)
    # rbind
  }else{
    calc_coefs
  }

    # draw plot
    if(length(unique(data$labels)) == 1){

    }else{
      ggplot(data, aes(x = x, y = y, color = color)) +
        scale_y_continuous(
          breaks = data$y,
          labels = data$labels,
          name = ""
        ) +
        xlab(covname) +
        geom_vline(xintercept = 0, linetype = "dashed") +
        geom_errorbarh(aes(xmin = x0, xmax = x1)) +
        geom_point() +
        theme_bw()
    }

    # original code
    col.seq <- rep("black", length(x$hpdintervals$X.coefs[, covname,
        "lower"]))
    col.seq[x$hpdintervals$X.coefs[, covname, "lower"] < 0 &
        x$hpdintervals$X.coefs[, covname, "upper"] > 0] <- "grey"
    At.y <- rev(seq_len(nrow(x$X.coefs.median)))
    if (est == "median")
        plot(x = x$X.coefs.median[, covname], y = At.y, yaxt = "n",
            ylab = "", col = col.seq, xlab = covname, xlim = c(min(x$hpdintervals$X.coefs[,
                covname, "lower"]), max(x$hpdintervals$X.coefs[,
                covname, "upper"])), pch = "x", ...)
    if (est == "mean")
        plot(x = x$X.coefs.mean[, covname], y = At.y, yaxt = "n",
            ylab = "", col = col.seq, xlab = covname, xlim = c(min(x$hpdintervals$X.coefs[,
                covname, "lower"]), max(x$hpdintervals$X.coefs[,
                covname, "upper"])), pch = "x", ...)
    segments(x0 = x$hpdintervals$X.coefs[, covname, "lower"],
        y0 = At.y, x1 = x$hpdintervals$X.coefs[, covname, "upper"],
        y1 = At.y, col = col.seq, ...)
    abline(v = 0, lty = 3)
    if (is.null(labely)) {
        axis(2, at = At.y, labels = rownames(x$X.coefs.mean),
            las = 1, ...)
    }
    if (!is.null(labely)) {
        if (length(labely) == nrow(x$X.coefs.mean))
            axis(2, at = At.y, labels = labely, las = 1, ...)
        if (length(labely) == 1)
            mtext(text = labely, side = 2, line = 3, las = 3,
                ...)
    }
    if (exists("ssvs.indcoefs.mean", x)) {
        message("Posterior probabilities of inclusion for ",
            covname, ":")
        print(round(x$ssvs.indcoefs.mean[, covname], 3))
        message()
    }
}