plot_list <- function(names) {
  .pl <- list()
  for (name in names) {
    .pl[[as.character(name)]] <- ggplot()
  }
  return(.pl)
}

plot_c <- function(.pl, data, x, y, group, country, ...) {
  group = rlang::enquo(group) 
  x = rlang::enquo(x) 
  y = rlang::enquo(y) 
  country = rlang::enquo(country)
  
  .pl[["global"]] <- ggplot(data=data, aes_string(x=rlang::quo_text(x), y=rlang::quo_text(y))) +
                                        geom_point(aes()) +
                                        geom_smooth(method = "loess", size=.75, lty=1, colour="black") + 
                                        theme(legend.position = "none")
  
  data <- split(data, dplyr::select(data, !!group) %>% unlist)
  for (name in names(.pl)) {
    i <- as.character(name)
    if (is.null(data[[i]]))
      next
    .pl[[i]] <- .pl[[i]] + geom_line(data = data[[i]],
                                     size = .75,
                                     aes_string(x=rlang::quo_text(x), 
                                                y=rlang::quo_text(y), 
                                                group=rlang::quo_text(country),
                                                color=rlang::quo_text(country))) +
                                      xlab(x) + 
                                      ylab(y) + 
                                      ggtitle(as.character(name))# + 
                                      #theme(legend.position = "none")
  }
  for (name in names(.pl)) {
    i <- as.character(name)
    for (obj in list(...)) .pl[[i]] <- .pl[[i]] + obj
  }
  
  return(.pl)
}


plot_lm <- function(.pl, data, x, y, group, country, ...) {
  group = rlang::enquo(group) 
  x = rlang::enquo(x) 
  y = rlang::enquo(y) 
  country = rlang::enquo(country)
  
  .pl[["global"]] <- ggplot(data=data, aes_string(x=rlang::quo_text(x), y=rlang::quo_text(y))) +
    geom_point(aes()) +
    geom_smooth(method = "loess", size=.75, lty=1, colour="black") + 
    theme(legend.position = "none")
  
  data <- split(data, dplyr::select(data, !!group) %>% unlist)
  for (name in names(.pl)) {
    i <- as.character(name)
    if (is.null(data[[i]]))
      next
    .pl[[i]] <- .pl[[i]] + geom_smooth(method="lm", 
                                       se=FALSE, 
                                       size = .75,
                                       data = data[[i]],
                                       aes_string(x=rlang::quo_text(x), 
                                                y=rlang::quo_text(y), 
                                                group=rlang::quo_text(country),
                                                color=rlang::quo_text(country))) +
      xlab(x) + 
      ylab(y) + 
      ggtitle(as.character(name)) + 
      theme(legend.position = "none")
  }
  for (name in names(.pl)) {
    i <- as.character(name)
    for (obj in list(...)) .pl[[i]] <- .pl[[i]] + obj
  }
  
  return(.pl)
}



plot_general_r <- function(.pl, data, x, y, group, country, ...) {
  group = rlang::enquo(group) 
  x = rlang::enquo(x) 
  y = rlang::enquo(y) 
  country = rlang::enquo(country)
  
  .pl[["global"]] <- ggplot(data=data, aes_string(x=rlang::quo_text(x), y=rlang::quo_text(y), color=rlang::quo_text(country))) +
    geom_point(aes()) +
    geom_smooth(method = "loess", size=1, lty=2, colour="black") + 
    theme(legend.position = "none")
  
  data <- split(data, dplyr::select(data, !!group) %>% unlist)
  for (name in names(.pl)) {
    i <- as.character(name)
    if (is.null(data[[i]]))
      next
    .pl[[i]] <- .pl[[i]] + geom_smooth(method="loess", size = 1, data = data[[i]], aes_string(x=rlang::quo_text(x), y=rlang::quo_text(y))) +
      xlab(x) + 
      ylab(y) + 
      ggtitle(as.character(name)) + 
      theme(legend.position = "none")
  }
  for (name in names(.pl)) {
    i <- as.character(name)
    for (obj in list(...)) .pl[[i]] <- .pl[[i]] + obj
  }
  
  return(.pl)
}





plot_general <- function(data, x, y, country, region, ...) {
  x = rlang::enquo(x) 
  y = rlang::enquo(y) 
  region = rlang::enquo(region)
  country = rlang::enquo(country)
  .out <- list()
  .out[[1]] <- ggplot(data=data, aes_string(x=rlang::quo_text(x), y=rlang::quo_text(y), color=rlang::quo_text(country))) +
    geom_point(aes()) +
    geom_smooth(method = "loess", size=1, lty=2, colour="black") 
  .out[[2]] <- ggplot(data=data, aes_string(x=rlang::quo_text(x), y=rlang::quo_text(y), color=rlang::quo_text(region))) +
    geom_point(aes()) +
    geom_smooth(method = "loess", size=1, lty=2, colour="black")
  return(.out)
}


