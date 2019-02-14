# function to return coefficients etc for a (single) specified predictor
calc_coefs <- function(
  x,
  covname,
  est = "median"
){

  if(!(covname %in% colnames(x$X.coefs.mean))){
    stop("covname not found among the covariates in the boral object x")
  }

  lci <- x$hpdintervals$X.coefs[, covname, "lower"]
  uci <- x$hpdintervals$X.coefs[, covname, "upper"]

  data <- data.frame(
    covname = covname,
    labels = NA,
    x = x[[paste0("X.coefs.", est)]][, covname],
    x0 = lci,
    x1 = uci,
    y = seq_len(nrow(x$X.coefs.median)),
    overlaps_zero = FALSE,
    stringsAsFactors = FALSE
  )
  rownames(data) <- NULL
  data$overlaps_zero[lci < 0 & uci > 0] <- TRUE
  data$labels <- rownames(x$X.coefs.mean)

  return(data)
}


# a user-called function to create a data.frame of predictions for 1+ variables
# this can then be passed to ggcoefs
boral_coefs <- function(
  x,
  covname,
  est = "median"
){

  if(missing(covname)){
    covname <- colnames(x$X.coefs.mean)
  }else{
    if(is.null(covname)){
      covname <- colnames(x$X.coefs.mean)
    }
  }

  if(length(covname) > 1){
    result_list <- lapply(covname, function(a){
      calc_coefs(x, a, est)
    })
    result <- data.frame(
      do.call(rbind, result_list),
      stringsAsFactors = FALSE
    )
  }else{
    result <- calc_coefs(x, covname, est)
  }
  result$labels <- factor(
    result$y,
    levels = unique(result$y),
    labels = unique(result$labels)
  )
  return(result)
}


# function to draw ggplot versions of the caterpillar plots returned by boral::coefsplot
# note that ggplot is returned, so extra ggplot2 functions can be added
boral_coefsplot <- function (
  x,
  covname,
  # labely = NULL, # add in again later
  est = "median"
){

  if(missing(x)){
    stop("x is missing, with no default")
  }

  if(any(c("boral", "data.frame") %in% class(x))){
    if(class(x) == "data.frame"){
      if(missing(covname)){
        data <- x
        covname <- unique(data$covname)
      }else{
        if(!(covname %in% x$covname)){
          stop("covname not found in x$covname")
        }
        data <- x[which(x$covname == covname), ]
      }
    }else{
      if(missing(covname)){
        covname <- colnames(x$X.coefs.mean)
      }else{
        if(is.null(covname)){
          covname <- colnames(x$X.coefs.mean)
        }
      }
      data <- boral_coefs(x, covname, est)
    }
  }else{
    stop("boral_coefsplot only accepts objects of class 'boral' or 'data.frame'.")
  }

  # draw plot
  object <- ggplot(data,
    aes(x = x, y = labels, color = overlaps_zero)
  ) +
    scale_y_discrete() +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_errorbarh(aes(xmin = x0, xmax = x1)) +
    geom_point() +
    scale_colour_manual(values = c("black", "grey70")) +
    theme_bw() +
    guides(color = FALSE) +
    ylab("Species")

  if(length(covname) > 1){
    return(object +
      facet_wrap( ~ covname, scales = "free_x") +
      xlab("Coefficient")
    )
  }else{
    return(object + xlab(covname))
  }
}