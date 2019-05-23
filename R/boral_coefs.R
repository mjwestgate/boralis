# low-level function to extract estimates for a single variable
extract_boral_estimates <- function(
  x, # model
  covname,
  type = "coefs",
  est = "median"
){

  # work out what to extract
  if(missing(type)){type = "coefs"}
  if(!(any(c("coefs", "traits") == type))){
    stop(paste0(type, " is not a valid input to argument 'type'; please specify 'coefs' or 'traits'"))
  }
  extract_what <- switch(type,
    "coefs" = "X.coefs",
    "traits" = "traits.coefs"
  )
  if((type == "traits") & !any(names(x$hpdintervals) == extract_what)){
    stop("type 'traits' has been specified, but no traits are present in x")
  }
  est_central <- x[[paste(extract_what, est, sep = ".")]]
  est_range <- x$hpdintervals[[extract_what]]

  # check if present
  if(!(covname %in% colnames(est_central))){
    stop("covname not found among the covariates in the boral object x")
  }

  # extract into data.frame
  lci <- est_range[, covname, "lower"]
  uci <- est_range[, covname, "upper"]
  data <- data.frame(
    covname = covname,
    statistic = est,
    labels = NA,
    x = est_central[, covname],
    x0 = lci,
    x1 = uci,
    y = seq_len(nrow(est_central)),
    overlaps_zero = FALSE,
    stringsAsFactors = FALSE
  )
  rownames(data) <- NULL
  data$overlaps_zero[lci < 0 & uci > 0] <- TRUE
  data$labels <- rownames(est_central)

  return(data)
}


# a user-called function to create a data.frame of predictions for 1+ variables
# this can then be passed to ggcoefs
boral_coefs <- function(
  x,
  covname,
  type = "coefs",
  est = "median"
){

  if(missing(type)){type = "coefs"}
  if(!(any(c("coefs", "traits") == type))){
    stop(paste0(type, " is not a valid input to argument 'type'; please specify 'coefs' or 'traits'"))
  }
  extract_what <- switch(type,
    "coefs" = "X.coefs",
    "traits" = "traits.coefs"
  )
  if((type == "traits") & !any(names(x$hpdintervals) == extract_what)){
    stop("type 'traits' has been specified, but no traits are present in x")
  }
  variable <- x[[paste(extract_what, est, sep = ".")]]

  if(missing(covname)){
    covname <- colnames(variable)
  }else{
    if(is.null(covname)){
      covname <- colnames(variable)
    }
  }

  if(length(covname) > 1){
    result_list <- lapply(covname, function(a){
      extract_boral_estimates(x, a, type, est)
    })
    result <- data.frame(
      do.call(rbind, result_list),
      stringsAsFactors = FALSE
    )
  }else{
    result <- extract_boral_estimates(x, covname, type, est)
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
  type = "coefs",
  # labely = NULL, # add in again later
  est = "median"
){

  if(missing(x)){
    stop("x is missing, with no default")
  }
  if(missing(type)){type = "coefs"}
  if(!(any(c("coefs", "traits") == type))){
    stop(paste0(type, " is not a valid input to argument 'type'; please specify 'coefs' or 'traits'"))
  }
  extract_what <- switch(type,
    "coefs" = "X.coefs",
    "traits" = "traits.coefs"
  )
  if((type == "traits") & !any(names(x$hpdintervals) == extract_what)){
    stop("type 'traits' has been specified, but no traits are present in x")
  }
  variable <- x[[paste(extract_what, est, sep = ".")]]


  if(any(c("boral", "data.frame") %in% class(x))){
    if(class(x) == "data.frame"){
      if(missing(covname)){
        data <- x
        covname <- unique(data$covname)
      }else{
        if(!(covname %in%  x$covname)){
          stop("covname not found in x$covname")
        }
        data <- x[which(x$covname == covname), ]
      }
    }else{
      if(missing(covname)){
        covname <- colnames(variable)
      }else{
        if(is.null(covname)){
          covname <- colnames(variable)
        }
      }
      data <- boral_coefs(x, covname, type, est)
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