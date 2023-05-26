lat_lon_prep <- function(lat, lon_min, lon_max) {
  #Create a sequence of longitudes for a given latitude using the provided maximum and minimum
  lon <- seq(from = lon_min, to = lon_max , by = 0.5)
  return(lon)
}

create_plot_df <-
  function(year,
           reg_latitudes,
           reg_long_min,
           reg_long_max,
           lats,
           lons,
           SPEI = 0,
           SMoist = 0,
           NDVI = 0,
           y = 0,
           index_pairs = 0,
           test_df = 0) {
    # Create the latitude and longitude ranges for the map
    lat_port <- 0
    lon_port <- 0
    for (i in 1:length(reg_latitudes)) {
      value <-
        lat_lon_prep(reg_latitudes[i], reg_long_min[i], reg_long_max[i])
      lat_port <- c(lat_port, rep(reg_latitudes[i], length(value)))
      lon_port <- c(lon_port, value)
    }
    
    #Exclude the first 0-values
    lat_port <- lat_port[2:length(lat_port)]
    lon_port <- lon_port[2:length(lon_port)]
    
    # Create a data frame to store the plot data
    plot_df <- data.frame(
      lat = lat_port,
      lon = lon_port,
      lat_ind = 0,
      lon_ind = 0
    )
    
    # Sort the dataframe
    plot_df <-
      plot_df %>%
      arrange(lat, lon)
    
    
    # If 'y' is numeric, assume the environmental data is in arrays and fill the
    # plot data frame with the corresponding values
    if (class(y)[1] == "numeric") {
      for (i in 1:dim(plot_df)[1]) {
        plot_df$lat_ind[i] <- which(plot_df$lat[i] == lats)
        plot_df$lon_ind[i] <- which(plot_df$lon[i] == lons)
        plot_df$SPEI[i] <-
          SPEI[plot_df$lon_ind[i], plot_df$lat_ind[i], year]
        plot_df$SMoist[i] <-
          SMoist[plot_df$lon_ind[i], plot_df$lat_ind[i], year]
        plot_df$NDVI[i] <-
          NDVI[plot_df$lon_ind[i], plot_df$lat_ind[i], year]
      }
    }
    # Otherwise, assume the environmental data is in a matrix and fill the plot
    # data frame with the corresponding values
    else {
      for (i in 1:dim(plot_df)[1]) {
        for (c in 1:139) {
          if (paste(paste(plot_df$lat[i], plot_df$lon[i], sep = ",") == paste(lats[as.numeric(strsplit(index_pairs[c], ",")[[1]][1])], lons[as.numeric(strsplit(index_pairs[c], ",")[[1]][2])], sep = ","))) {
            plot_df$reg_ind[i] <- c
          }
        }
        plot_df$SPEI[i] <-
          y[year, (plot_df$reg_ind[i])]
        plot_df$SMoist[i] <-
          y[year, ((2 - 1) * 139 + plot_df$reg_ind[i])]
        plot_df$NDVI[i] <-
          y[year, ((3 - 1) * 139 + plot_df$reg_ind[i])]
      }
    }
    
    # Add columns to the data frame with the boundaries of each latitude and longitude point
    plot_df$lat_min <- plot_df$lat - 0.25
    plot_df$lat_max <- plot_df$lat + 0.25
    plot_df$lon_min <- plot_df$lon - 0.25
    plot_df$lon_max <- plot_df$lon + 0.25
    plot_df$lat_plot[1:1] <- min(plot_df$lat) - 0.2
    plot_df$lat_plot[2:dim(plot_df)[1]] <- max(plot_df$lat) + 0.2
    plot_df$lon_plot[1:1] <- min(plot_df$lon) - 0.2
    plot_df$lon_plot[2:dim(plot_df)[1]] <- max(plot_df$lon) + 0.2
    
    return(plot_df)
  }

create_plot_df_weights <- function(weights) {
  # Create the latitude and longitude ranges for the map
  lat_port <- 0
  lon_port <- 0
  for (i in 1:length(reg_latitudes)) {
    value <-
      lat_lon_prep(reg_latitudes[i], reg_long_min[i], reg_long_max[i])
    lat_port <- c(lat_port, rep(reg_latitudes[i], length(value)))
    lon_port <- c(lon_port, value)
  }
  
  #Exclude the first 0-values
  lat_port <- lat_port[2:length(lat_port)]
  lon_port <- lon_port[2:length(lon_port)]
  
  # Create a data frame to store the plot data
  plot_df <- data.frame(
    lat = lat_port,
    lon = lon_port,
    lat_ind = 0,
    lon_ind = 0
  )
  
  # Sort the dataframe
  plot_df <-
    plot_df %>%
    arrange(lat, lon)
  
  # Fill the dataframe with the weights
  for (i in 1:dim(plot_df)[1]) {
    plot_df$weights[i] <- weights[i]
  }
  
  # Add columns to the data frame with the boundaries of each latitude and longitude point
  plot_df$lat_min <- plot_df$lat - 0.25
  plot_df$lat_max <- plot_df$lat + 0.25
  plot_df$lon_min <- plot_df$lon - 0.25
  plot_df$lon_max <- plot_df$lon + 0.25
  plot_df$lat_plot[1:1] <- min(plot_df$lat) - 0.4
  plot_df$lat_plot[2:dim(plot_df)[1]] <- max(plot_df$lat) + 0.4
  plot_df$lon_plot[1:1] <- min(plot_df$lon) - 0.4
  plot_df$lon_plot[2:dim(plot_df)[1]] <- max(plot_df$lon) + 0.4
  
  return(plot_df)
}


create_plot <-
  function(plot_df,
           var,
           scale_fixed = FALSE,
           max = 0,
           min = 0,
           tit = 0,
           no_lab = FALSE,
           color_0 = FALSE,
           reg_highlight = 0) {
    # Set color scales for each variable
    if (var == "SPEI") {
      up_color <- "darkblue"   # Color for the top value
      low_color <- "red"       # Color for the bottom value
      variable <- plot_df$SPEI # Variable
    } else if (var == "SMoist") {
      up_color <- "darkblue"   # Color for the top value
      low_color <- "orange"    # Color for the bottom value
      variable <- plot_df$SMoist # Variable
    } else if (var == "NDVI") {
      up_color <- "darkgreen"  # Color for the top value
      low_color <- "brown"     # Color for the bottom value
      variable <- plot_df$NDVI # Variable
    }
    else if (var == "Mean PRE") {
      up_color <- "darkblue"             # Color for the top value
      low_color <- "red"       # Color for the bottom value
      variable <- plot_df$weights   # Variable
      
    }
    else{
      up_color <- "red"             # Color for the top value
      low_color <- "darkblue"       # Color for the bottom value
      variable <- plot_df$weights   # Variable
    }
    # if scale_fixed is False we don't set the upper and lower limit for the color
    # scale, letting them be data driven
    if (scale_fixed == FALSE) {
      # Create the plot
      plot <-
        qmplot(
          y = lat_plot,
          x = lon_plot,
          data = plot_df,
          geom = "blank",
          maptype = "toner-background",
          zoom = 7
        ) +
        geom_rect(
          aes(
            ymin = lat_min,
            ymax = lat_max,
            xmin = lon_min,
            xmax = lon_max,
            color = variable,
            fill = variable,
            alpha = 0.1
          )
        ) + scale_alpha_continuous(guide = "none") + scale_color_gradient2(
          name = var,
          low = low_color,
          mid = "grey",
          high = up_color
        ) + scale_fill_gradient2(
          name = var,
          low = low_color,
          mid = "grey",
          high = up_color
        )
    }
    # if scale_fixed is not False we set the upper and lower limits for the color
    # scale based on the variable being plotted
    else {
      if (var == "SPEI") {
        if (max != 0) {
          up_lim <- max
          low_lim <- min
          mid <- 0
        }
        else{
          up_lim <- 2.7
          low_lim <- -2.7
          mid <- 0
        }
      } else if (var == "SMoist") {
        up_lim <- 740
        low_lim <- 0
        mid <- 200
      } else if (var == "NDVI") {
        up_lim <- 1
        low_lim <- 0
        mid <- 0.2
      } else if (var == "Mean PRE") {
        up_lim <- 190
        low_lim <- 40
        mid <- 90
      }
      else if (var == "Mean PET") {
        up_lim <- 3.5
        low_lim <- 1
        mid <- 2
      }
      else{
        up_lim <- max
        low_lim <- min
        mid <- 17
      }
      # Create the plot
      plot <-
        qmplot(
          y = lat_plot,
          x = lon_plot,
          data = plot_df,
          geom = "blank",
          maptype = "toner-background",
          zoom = 7
        ) +
        geom_rect(
          aes(
            ymin = lat_min,
            ymax = lat_max,
            xmin = lon_min,
            xmax = lon_max,
            color = variable,
            fill = variable,
            alpha = 0.1
          )
        ) + scale_alpha_continuous(guide = "none") + scale_color_gradient2(
          name = var,
          low = low_color,
          mid = "grey",
          high = up_color,
          midpoint = mid,
          limits = c(low_lim, up_lim)
        ) +
        scale_fill_gradient2(
          name = var,
          low = low_color,
          mid = "grey",
          high = up_color,
          midpoint = mid,
          limits = c(low_lim, up_lim)
        )
      
    }
    # Remove the legend and adjust the plot margin if no_lab is TRUE
    if (no_lab == TRUE) {
      plot <-
        plot + theme(legend.position = "none")
    }
    # Add a plot title and adjust the size of the title text if tit is not 0
    if (tit != 0) {
      plot <- plot + ggtitle(tit) +
        theme(plot.title = element_text(size = 12))
    }
    # add an "R" label to the location specified by reg_highlight
    if (reg_highlight != 0) {
      plot <-
        plot + annotate(
          "text",
          x = lons[as.numeric(strsplit(index_pairs[reg_highlight], ",")[[1]][2])],
          y = lats[as.numeric(strsplit(index_pairs[reg_highlight], ",")[[1]][1])],
          label = "R",
          size = 4,
          color = "black"
        )
    }
    return(plot)
  }

create_test_df <- function(reg_latitudes,
                           reg_long_min,
                           reg_long_max,
                           lats,
                           lons,
                           SPEI,
                           SMoist,
                           NDVI) {
  # Create the latitude and longitude ranges for the map
  lat_port <- 0
  lon_port <- 0
  for (i in 1:length(reg_latitudes)) {
    value <-
      lat_lon_prep(reg_latitudes[i], reg_long_min[i], reg_long_max[i])
    lat_port <- c(lat_port, rep(reg_latitudes[i], length(value)))
    lon_port <- c(lon_port, value)
  }
  
  #Exclude the first 0-values
  lat_port <- lat_port[2:length(lat_port)]
  lon_port <- lon_port[2:length(lon_port)]
  
  # Create sequence of months
  months <- 1:402
  
  # Create data frame with columns for latitude, longitude, month, latitude index,
  # longitude index, SPEI, SMoist, and NDVI
  test_df <- data.frame(
    # Fill lat column with repeated latitude values
    lat = rep(lat_port, times = 402),
    # Fill lon column with repeated longitude values
    lon = rep(lon_port, times = 402),
    # Fill month column with repeated month values
    month = rep(months, each = length(lat_port)),
    # Initialize lat_ind column with zeros
    lat_ind = 0,
    # Initialize lon_ind column with zeros
    lon_ind = 0
  )
  
  # Loop through all rows of test_df and update lat_ind and lon_ind columns with
  # corresponding indices in another data frame
  for (i in 1:length(lat_port)) {
    test_df$lat_ind[i] <-
      which(test_df$lat[i] == lats) # Update lat_ind column with index of
    #corresponding latitude value in lats
    test_df$lon_ind[i] <-
      which(test_df$lon[i] == lons) # Update lon_ind column with index of
    #corresponding longitude value in lons
  }
  
  # Repeat the indices for each year
  test_df$lat_ind[(i + 1):dim(test_df)[1]] <-
    rep(test_df$lat_ind[1:i], times = 401)
  test_df$lon_ind[(i + 1):dim(test_df)[1]] <-
    rep(test_df$lon_ind[1:i], times = 401)
  
  
  # Get the unique values of lat_ind and lon_ind
  lat_ind <- test_df$lat_ind[1:i]
  lon_ind <- test_df$lon_ind[1:i]
  
  # Initialize SPEI, SMoist, and NDVI columns with zeros
  test_df$SPEI <- 0
  test_df$SMoist <- 0
  test_df$NDVI <- 0
  
  # Loop through all time steps (i.e., all months) and populate SPEI, SMoist, and
  #NDVI columns with corresponding values
  for (reg in 1:139) {
    # Populate SPEI, SMoist, and NDVI columns with corresponding values based
    #on lat_ind and lon_ind indices
    test_df$SPEI[test_df$lat_ind == lat_ind[reg] &
                   test_df$lon_ind == lon_ind[reg]] <-
      SPEI[lon_ind[reg], lat_ind[reg],]
    test_df$SMoist[test_df$lat_ind == lat_ind[reg] &
                     test_df$lon_ind == lon_ind[reg]] <-
      SMoist[lon_ind[reg], lat_ind[reg],]
    test_df$NDVI[test_df$lat_ind == lat_ind[reg] &
                   test_df$lon_ind == lon_ind[reg]] <-
      NDVI[lon_ind[reg], lat_ind[reg],]
  }
  
  # Create a unique index for each region and order the resulting dataframe
  test_df <-
    test_df %>%
    mutate(index = paste(lat_ind, lon_ind, sep = ",")) %>%
    arrange(month, lat_ind, lon_ind)
  
  return(test_df)
}

prepare_weights <- function(index_pairs, lats, lons) {
  # Create a matrix to store weights between each pair of indexes
  weights <-
    matrix(nrow = length(index_pairs),
           # Number of rows equal to the number of index pairs
           ncol = length(index_pairs)) # Number of columns equal to the number of index pairs
  
  # Loop through each index pair to calculate the corresponding weight
  for (i in 1:length(index_pairs)) {
    # Split the index pair into latitude and longitude values
    indexes <- strsplit(index_pairs[i], ",")
    lat <- as.numeric(indexes[[1]][1])
    lon <- as.numeric(indexes[[1]][2])
    weights[i, i] <-
      0 # Set the weight between the same index pair to 0
    
    # Loop through all other index pairs to calculate their weight relative to the current index pair
    for (j in 1:length(index_pairs)) {
      indexes <- strsplit(index_pairs[j], ",")
      lat_2 <- as.numeric(indexes[[1]][1])
      lon_2 <- as.numeric(indexes[[1]][2])
      # Calculate the weight based on the distance between the latitude and longitude
      # values of the two index pairs
      if (abs(lats[lat] - lats[lat_2]) + abs(lons[lon] - lons[lon_2]) == 0) {
        weights[i, j] <- 0 # Set the weight to 0 if the distance is 0
      } else {
        weights[i, j] <-
          1 / (35 * (abs(lats[lat] - lats[lat_2]) + abs(lons[lon] - lons[lon_2])) ^
                 2) # Calculate the weight based on the formula explained in the text
      }
    }
  }
  
  return(weights)
}


compute_lambda <- function(var, weights) {
  T <- dim(var)[1] # Number of time steps
  R <- dim(var)[2] # Number of spatial locations
  
  lambda <-
    matrix(nrow = R) # Create an empty matrix to store the results
  
  # Loop through each spatial location
  for (i in 1:R) {
    # Fit a linear regression model to the variable at the i-th location, using lagged values of the variable
    # and a linear combination of the neighboring locations, weighted by the spatial weights
    reg <-
      lm(var[2:T, i] ~ var[1:T - 1, i] + as.numeric(weights[i, ] %*% t(var[1:T - 1, ])))
    # Compute lambda as the sum of the coefficients of the lagged variable and the linear combination of neighboring locations
    lambda[i] <- reg$coefficients[2] + reg$coefficients[3]
  }
  return(mean(lambda))
}

mc_test_values <- function(S, R, T, phi_mean, weights, verbose) {
  # Initialize phi and pi matrices with random values within a small range around the mean
  phi <-
    matrix(
      runif(
        min = phi_mean - 0.05,
        max = phi_mean + 0.05,
        n = R * S
      ),
      nrow = S,
      ncol = R
    )
  pi <-
    matrix(
      runif(
        min = -phi_mean + 0.95,
        max = -phi_mean + 1.05,
        n = R * S
      ),
      nrow = S,
      ncol = R
    )
  
  # Initialize y array with zeros
  y <- array(0, dim = c(T, R, S))
  
  # Compute y values using the DGP described in the text
  for (s in 1:S) {
    A <- diag(R) - (diag(R) * phi[s, ]) %*% weights
    A_inv <- solve(A)
    A_pi <- A_inv %*% (diag(R) * pi[s, ])
    for (t in 2:T) {
      y[t, , s] <- A_pi %*% y[t - 1, , s] + A_inv %*% rnorm(sd = 1, n = R)
    }
  }
  
  # Exclude the first 98 time periods of y values and compute lambda for each simulation
  y <- y[99:T, , ]
  lambda <- matrix(0, nrow = S)
  for (s in 1:S) {
    # Print progress message if verbose is TRUE and s is a multiple of 1000
    if (verbose == TRUE & s %% 1000 == 0) {
      print(paste("Processing s", s, "out of", S))
    }
    for (i in 1:R) {
      # Fit a linear regression model to the variable at the i-th location, using lagged values
      # of the variable and a linear combination of the neighboring locations, weighted by the spatial
      # weights
      reg <-
        lm(y[2:(T - 98), i, s] ~ y[1:(T - 99), i, s] + as.numeric(weights[i, ] %*% t(y[2:(T - 98), , s])))
      # Compute lambda as the sum of the coefficients of the lagged variable and the linear combination of neighboring locations
      lambda[s] <-
        lambda[s] + reg$coefficients[2] + reg$coefficients[3]
    }
    # Take the mean of the lambdas
    lambda[s] <- lambda[s] / R
  }
  return(lambda)
}


mc_critical_values <-
  function(S,
           R,
           T,
           weights,
           seed = 123,
           verbose = TRUE) {
    # Set seed for reproducibility
    set.seed(seed)
    
    # Create a matrix to store lambda values
    lambda <- matrix(nrow = S * 10)
    
    # Initialize index
    i <- 0
    
    # Loop over different phi_mean values
    for (phi_mean in seq(0.05, 0.95, 0.1)) {
      # Print progress message if verbose is TRUE
      if (verbose == TRUE) {
        print(paste("Now setting phi_mean", phi_mean))
      }
      
      # Increment index
      i <- i + 1
      
      # Compute lambda values for current phi_mean value using the mc_test_values function
      lambda[(((i - 1) * S) + 1):(i * S), ] <-
        mc_test_values(S, R, T, phi_mean, weights, verbose)
    }
    
    # Loop over the 10 tests and print rejection condition
    for (i in 1:10) {
      print(paste(
        "We reject in the test",
        i,
        "if lambda <",
        quantile(lambda[(((i - 1) * S) + 1):(i * S), ], probs = 0.05, na.rm = TRUE)
      ))
    }
    
    return(lambda)
  }


prepare_spvar_df <-
  function(test_df,
           test_df_spei,
           test_df_smoist,
           test_df_ndvi,
           T = 402,
           lags = 1) {
    # Initialize matrices to store spatial lags
    sp_spei <- matrix(0, nrow = T - lags, ncol = 139)
    sp_smoist <- matrix(0, nrow = T - lags, ncol = 139)
    sp_ndvi <- matrix(0, nrow = T - lags, ncol = 139)
    
    # Calculate spatial lags using weights and store in matrices
    for (i in 1:(T - lags)) {
      sp_spei[i, ] <- weights %*% as.numeric(test_df_spei[i, ])
      sp_smoist[i, ] <- weights %*% as.numeric(test_df_smoist[i, ])
      sp_ndvi[i, ] <- weights %*% as.numeric(test_df_ndvi[i, ])
    }
    
    # Convert matrices to vectors
    sp_spei <- as.numeric(t(sp_spei))
    sp_smoist <- as.numeric(t(sp_smoist))
    sp_ndvi <- as.numeric(t(sp_ndvi))
    
    # Create a factor variable with levels 1 to 139 to represent fixed effects
    fe <- rep(1:139, T - lags)
    
    # If two lags are requested add the second lag
    if (lags == 2) {
      # Initialize matrices to store the second spatial lag
      sp_spei_2 <- matrix(0, nrow = T - lags, ncol = 139)
      sp_smoist_2 <- matrix(0, nrow = T - lags, ncol = 139)
      sp_ndvi_2 <- matrix(0, nrow = T - lags, ncol = 139)
      
      # Calculate the second spatial lags using weights and store in matrices
      for (i in 1:(T - lags)) {
        sp_spei_2[i, ] <- weights %*% as.numeric(test_df_spei[i + 1, ])
        sp_smoist_2[i, ] <-
          weights %*% as.numeric(test_df_smoist[i + 1, ])
        sp_ndvi_2[i, ] <-
          weights %*% as.numeric(test_df_ndvi[i + 1, ])
      }
      
      # Convert matrices to vectors
      sp_spei_2 <- as.numeric(t(sp_spei_2))
      sp_smoist_2 <- as.numeric(t(sp_smoist_2))
      sp_ndvi_2 <- as.numeric(t(sp_ndvi_2))
      
      # Extract time series data and lagged values for SPEI, SMoist, and NDVI
      spei <-
        test_df$SPEI[test_df$month > lags & test_df$month < T + 1]
      spei_lag <-
        test_df$SPEI[test_df$month < T & test_df$month > 1]
      spei_lag_2 <- test_df$SPEI[test_df$month < T - 1]
      smoist <-
        test_df$SMoist[test_df$month > lags & test_df$month < T + 1]
      smoist_lag <-
        test_df$SMoist[test_df$month < T & test_df$month > 1]
      smoist_lag_2 <- test_df$SMoist[test_df$month < T - 1]
      ndvi <-
        test_df$NDVI[test_df$month > lags & test_df$month < T + 1]
      ndvi_lag <-
        test_df$NDVI[test_df$month < T & test_df$month > 1]
      ndvi_lag_2 <- test_df$NDVI[test_df$month < T - 1]
      
      # Combine all variables into a data frame
      data <-
        data.frame(
          spei,
          smoist,
          ndvi,
          fe,
          spei_lag,
          ndvi_lag,
          smoist_lag,
          spei_lag_2,
          ndvi_lag_2,
          smoist_lag_2,
          sp_spei,
          sp_ndvi,
          sp_smoist,
          sp_spei_2,
          sp_ndvi_2,
          sp_smoist_2
        )
      
    }
    
    else{
      # Extract time series data and lagged values for SPEI, SMoist, and NDVI
      spei <-
        test_df$SPEI[test_df$month > 1 & test_df$month < T + 1]
      spei_lag <- test_df$SPEI[test_df$month < T]
      smoist <-
        test_df$SMoist[test_df$month > 1 & test_df$month < T + 1]
      smoist_lag <- test_df$SMoist[test_df$month < T]
      ndvi <-
        test_df$NDVI[test_df$month > 1 & test_df$month < T + 1]
      ndvi_lag <- test_df$NDVI[test_df$month < T]
      
      # Combine all variables into a data frame
      data <-
        data.frame(spei,
                   ndvi,
                   smoist,
                   fe,
                   spei_lag,
                   ndvi_lag,
                   smoist_lag,
                   sp_spei,
                   sp_ndvi,
                   sp_smoist)
    }
    
    # Convert the fixed effects to a factor variable
    data$fe <- as.factor(data$fe)
    
    # Create dummy variables for all variables in the data frame
    dmy <- dummyVars(" ~ .", data = data)
    
    # Create dummies in the data frame
    data <- data.frame(predict(dmy, newdata = data))
    
    return(data)
  }


prepare_coeff_matrixes <- function(spvar_fit) {
  #Create a block diagonal matrix weight_otimes using the connectivity matrix
  weight_otimes <- matrix(0, nrow = 139 * 3, ncol = 139 * 3)
  
  for (v in 1:139) {
    for (i in 1:139) {
      weight_otimes[((v - 1) * 3 + 1):((v) * 3), ((i - 1) * 3 + 1):((i) * 3)] <-
        diag(3) * weights[v, i]
    }
  }
  # Create the matrices containing the coefficients of the model
  theta <- matrix(0, nrow = 3, ncol = 3)
  upsilon <- matrix(0, nrow = 3, ncol = 3)
  for (v in 1:3) {
    theta[v, 1:3] <-
      as.numeric(spvar_fit$coefficients[(145 * (v - 1) + 140):(145 * (v - 1) + 142)])
    upsilon[v, 1:3] <-
      as.numeric(spvar_fit$coefficients[(145 * (v - 1) + 143):(145 * (v - 1) + 145)])
  }
  
  # Initialize theta_otimes and upsilon_otimes matrices as all zeros
  theta_otimes <- matrix(0, nrow = 139 * 3, ncol = 139 * 3)
  upsilon_otimes <- matrix(0, nrow = 139 * 3, ncol = 139 * 3)
  
  # Loop over i and j to fill in theta_otimes and upsilon_otimes matrices
  for (i in 1:3) {
    for (j in 1:3) {
      # Fill in the diagonal block for theta_otimes corresponding to (i,j)
      theta_otimes[((i - 1) * 139 + 1):((i) * 139), ((j - 1) * 139 + 1):((j) * 139)] <-
        diag(139) * theta[i, j]
      # Fill in the diagonal block for upsilon_otimes corresponding to (i,j)
      upsilon_otimes[((i - 1) * 139 + 1):((i) * 139), ((j - 1) * 139 + 1):((j) * 139)] <-
        diag(139) * upsilon[i, j]
    }
  }
  
  return(list(weight_otimes, theta_otimes, upsilon_otimes))
}


irf <- function(t_irf,
                reg,
                var,
                sur_fit_reduced) {
  # Prepare the coefficients block diagonal matrices using the prepare_coeff_matrixes function
  coeffs <- prepare_coeff_matrixes(sur_fit_reduced)
  
  # Create a block diagonal matrix weight_otimes using the connectivity matrix
  weight_otimes <- coeffs[[1]]
  
  # Prepare the Cholesky identification matrix
  chol <- chol(sur_fit_reduced$residCovEst)
  chol_m <- matrix(0, nrow = 139 * 3, ncol = 139 * 3)
  for (i in 1:3) {
    for (j in 1:3) {
      chol_m[((i - 1) * 139 + 1):((i) * 139), ((j - 1) * 139 + 1):((j) * 139)] <-
        diag(139) * chol[i, j]
    }
  }
  
  #Create a vector shock with a value of 1 at the specified region and variable position.
  shock <- matrix(0, nrow = 139 * 3)
  shock[((var - 1) * 139) + reg] <- 1
  
  # Transform the structural shock in reduced form using the Cholesky identification
  red_shock <- chol_m %*% shock
  
  # Initialize the output matrix y with the transformed shock vector as the first row.
  y <- matrix(nrow = t_irf, ncol = 139 * 3)
  y[1, ] <- red_shock
  
  # Store the theta_otimes and upsilon_otimes matrices computed before
  theta_otimes <- coeffs[[2]]
  upsilon_otimes <- coeffs[[3]]
  
  # Loop over time steps to compute IRFs
  for (t in 2:t_irf) {
    # Compute IRF at time t using previous IRF and weight matrix
    y[t, ] <-
      (theta_otimes + upsilon_otimes %*% weight_otimes) %*% y[t - 1, ]
  }
  return(y)
}


irf_plot <- function(y,
                     var,
                     reg_data,
                     reg = 0,
                     no_lab = FALSE,
                     tit = 0,
                     quantiles_up = 0,
                     quantiles_down = 0) {
  # Set variable name and scaling factor based on var
  if (var == 2) {
    var_name <- "NDVI"
    scale = scale_NDVI
    
  } else if (var == 3) {
    var_name <- "SMoist"
    scale = scale_SMoist
  } else {
    var_name <- "SPEI"
    scale = scale_SPEI
  }
  # If reg=0 plot all regions' irf
  if (reg == 0) {
    # Extract data for selected variable and rescale using scaling factor
    plot_data <- y[, ((var - 1) * 139 + 1):((var) * 139)]
    for (i in 1:139) {
      plot_data[, i] <- plot_data[, i] * scale
    }
    # Convert to data frame and set column names
    plot_data <- data.frame(plot_data)
    for (i in 1:139) {
      colnames(plot_data)[i] <- toString(i)
    }
    
    # Remove same-region data
    plot_data <- plot_data[, -reg_data]
    
    # Reshape data to long format
    plot_data <- plot_data %>%
      pivot_longer(
        .,
        cols = colnames(plot_data),
        names_to = "Var",
        values_to = "Val"
      )
    
    # Color lines
    plot_data$color <- "black"
    # Generate plot with labeled lines and legend
    plot <- ggplot(data = plot_data) +
      geom_line(aes(
        y = Val,
        x = rep(1:dim(y)[1], each = 138),
        color = color,
        group = Var
      )) +
      labs(x = "Months", y = var_name, color = "Regions (Lat and Long)") +
      geom_hline(aes(yintercept = 0), color = "red") +
      scale_color_identity(guide = "none")
  }
  # Plot specified region's irf
  else {
    # Extract data for selected variable and region
    plot_data <- data.frame(y[, ((var - 1) * 139 + reg)])
    plot_data$quantile = "no"
    colnames(plot_data)[1] <- "plot_data"
    
    # Extract the upper quantile's irf for the selected variable and region
    plot_data_up <-
      data.frame(quantiles_up[, ((var - 1) * 139 + reg)])
    plot_data_up$quantile = "up"
    colnames(plot_data_up)[1] <- "plot_data"
    
    # Extract the lower quantile's irf for the selected variable and region
    plot_data_down <-
      data.frame(quantiles_down[, ((var - 1) * 139 + reg)])
    plot_data_down$quantile = "down"
    colnames(plot_data_down)[1] <- "plot_data"
    plot_data <- rbind(plot_data, plot_data_up, plot_data_down)
    
    # Rescale using scaling factor
    plot_data$plot_data <- plot_data$plot_data * scale
    
    # Generate plot without or with labels based on no_lab
    if (no_lab == TRUE) {
      plot <- ggplot(data = plot_data) +
        geom_line(aes(
          y = plot_data,
          x = rep(1:dim(plot_data_up)[1], times = 3),
          group = quantile,
          linetype = quantile
        )) +
        scale_linetype_manual(
          values = c(
            "no" = "solid",
            "up" = "dotted",
            "down" = "dotted"
          ),
          guide = "none"
        ) +
        geom_hline(aes(yintercept = 0), color = "red") + labs(x = element_blank(), y = element_blank())
    } else {
      plot <- ggplot(data = plot_data) +
        geom_line(aes(
          y = plot_data,
          x = rep(1:dim(plot_data_up)[1], times = 3),
          group = quantile,
          linetype = quantile
        )) +
        scale_linetype_manual(
          values = c(
            "no" = "solid",
            "up" = "dotted",
            "down" = "dotted"
          ),
          guide = "none"
        ) +
        geom_hline(aes(yintercept = 0), color = "red") + labs(x = element_blank(), y = var_name)
    }
  }
  
  # If tit is not 0, it adds a title to the plot.
  if (tit != 0) {
    plot <- plot + ggtitle(tit) +
      theme(plot.title = element_text(size = 12))
  }
  
  return(plot)
}

compute_pred_steps <-
  function(start_t,
           t_pred,
           fit_short,
           df_short,
           check = 0,
           value = "metric") {
    # Initialize the metrics
    smape <- 0
    rmse <- 0
    for (t in 1:start_t) {
      # Initialize the results' matrix
      pred <- matrix(0, nrow = 139 * (t_pred + 1), ncol = 149)
      
      # Add fixed effects to the matrix
      for (reg_i in 1:139) {
        fe <- rep(0, reg_i - 1)
        fe <- c(fe, 1)
        fe <- c(fe, rep(0, 139 - reg_i))
        fe <- rep(fe, (t_pred + 1))
        pred[, reg_i + 3] <- fe
      }
      
      
      # Turn the matrix in a dataframe and rename the columns
      pred <- data.frame(pred)
      colnames(pred) <- colnames(df_short)
      
      # Initialize the matrix with the first lagged values
      pred[1:139, 143:148] <-
        df_short[((t - 1) * 139 + 1):((t) * 139), 143:148]
      
      
      for (i in 1:t_pred) {
        #Compute the predicted values and store them
        prediction_sar <-
          predict(fit_short, pred[(((i - 1) * 139) + 1):((i) * 139), ])
        
        pred[(i * 139 + 1):((i + 1) * 139), 1] <-
          prediction_sar$eqspei.pred
        pred[(i * 139 + 1):((i + 1) * 139), 2] <-
          prediction_sar$eqndvi.pred
        pred[(i * 139 + 1):((i + 1) * 139), 3] <-
          prediction_sar$eqsmoist.pred
        
        pred[(i * 139 + 1):((i + 1) * 139), 146] <-
          weights %*% pred[(i * 139 + 1):((i + 1) * 139), 1]
        pred[(i * 139 + 1):((i + 1) * 139), 147] <-
          weights %*% pred[(i * 139 + 1):((i + 1) * 139), 2]
        pred[(i * 139 + 1):((i + 1) * 139), 148] <-
          weights %*% pred[(i * 139 + 1):((i + 1) * 139), 3]
        pred[(i * 139 + 1):((i + 1) * 139), 143:145] <-
          pred[(i * 139 + 1):((i + 1) * 139), 1:3]
      }
      
      # Take the correct values
      true_val <-
        df_short[((t + t_pred - 2) * 139 + 1):((t + t_pred  - 1) * 139), 1]
      
      # Take the predicted values
      predicted <-
        pred[(((t_pred) * 139) + 1):((t_pred + 1) * 139), 1]
      
      # Update the metrics if requested
      if (value == "metric") {
        smape <-
          smape + sum(abs((true_val - predicted)) / ((abs(true_val) + abs(predicted)) /
                                                       2))
        rmse <- rmse + sum((true_val - predicted) ** 2)
        
      }
      
      # Else update a vector of residuals squared
      else{
        rmse <- c(rmse, (true_val - predicted) ** 2)
      }
    }
    if (value == "metric") {
      return(c(sqrt(rmse / (start_t * 139)), (smape) / (start_t * 139)))
    }
    else{
      return(rmse)
    }
  }

multiplot_irf_sameregion <-
  function(spvar_fit, spvar_df, reg, t, quantiles) {
    # Calculate the IRF for the SMoist shock (var = 3)
    y <- irf(t_irf = t,
             reg = reg,
             var = 3,
             spvar_fit)
    
    # We take the confidence bands stored in the quantiles variable
    quantiles_up <- quantiles[[5]]
    quantiles_down <- quantiles[[6]]
    
    # Plot the IRF for the SMoist shock on SPEI (var = 1)
    irf_1 <-
      irf_plot(
        y,
        var = 1,
        reg = reg,
        tit = "SMoist shock",
        quantiles_up = quantiles_up,
        quantiles_down = quantiles_down
      )
    
    # Plot the IRF for the NDVI shock on SMoist (var = 2)
    irf_1_2 <-
      irf_plot(
        y,
        var = 2,
        reg = reg,
        quantiles_up = quantiles_up,
        quantiles_down = quantiles_down
      )
    
    # Plot the IRF for the NDVI shock on NDVI (var = 3)
    irf_1_3 <-
      irf_plot(
        y,
        var = 3,
        reg = reg,
        quantiles_up = quantiles_up,
        quantiles_down = quantiles_down
      )
    
    # Calculate the IRF for the SMoist shock (var = 2)
    y <- irf(t_irf = t,
             reg = reg,
             var = 2,
             spvar_fit)
    
    # We take the confidence bands stored in the quantiles variable
    quantiles_up <- quantiles[[3]]
    quantiles_down <- quantiles[[4]]
    
    # Plot the IRF for the SMoist shock on SPEI (var = 1)
    irf_2 <- irf_plot(
      y,
      var = 1,
      reg = reg,
      no_lab = TRUE,
      tit = "NDVI shock",
      quantiles_up = quantiles_up,
      quantiles_down = quantiles_down
    )
    
    # Plot the IRF for the SMoist shock on SMoist (var = 2)
    irf_2_2 <- irf_plot(
      y,
      var = 2,
      reg = reg,
      no_lab = TRUE,
      quantiles_up = quantiles_up,
      quantiles_down = quantiles_down
    )
    
    # Plot the IRF for the SMoist shock on NDVI (var = 3)
    irf_2_3 <- irf_plot(
      y,
      var = 3,
      reg = reg,
      no_lab = TRUE,
      quantiles_up = quantiles_up,
      quantiles_down = quantiles_down
    )
    
    # Calculate the IRF for the SPEI shock (var = 1)
    y <- irf(t_irf = t,
             reg = reg,
             var = 1,
             spvar_fit)
    
    # We take the confidence bands stored in the quantiles variable
    quantiles_up <- quantiles[[1]]
    quantiles_down <- quantiles[[2]]
    
    # Plot the IRF the SPEI shock on SPEI (var = 1)
    irf_3 <- irf_plot(
      y,
      var = 1,
      reg = reg,
      no_lab = TRUE,
      tit = "SPEI shock",
      quantiles_up = quantiles_up,
      quantiles_down = quantiles_down
    )
    
    # Plot the IRF for the SPEI shock on SMoist (var = 2)
    irf_3_2 <- irf_plot(
      y,
      var = 2,
      reg = reg,
      no_lab = TRUE,
      quantiles_up = quantiles_up,
      quantiles_down = quantiles_down
    )
    
    # Plot the IRF for the SPEI shock on NDVI (var = 3)
    irf_3_3 <- irf_plot(
      y,
      var = 3,
      reg = reg,
      no_lab = TRUE,
      quantiles_up = quantiles_up,
      quantiles_down = quantiles_down
    )
    
    # Return a grid of nine plots arranged in three rows and three columns
    return(
      grid.arrange(
        irf_1,
        irf_2,
        irf_3,
        irf_1_2,
        irf_2_2,
        irf_3_2,
        irf_1_3,
        irf_2_3,
        irf_3_3,
        ncol = 3,
        heights = c(2.4, 2, 2),
        widths = c(2.2, 2, 2)
      )
    )
    
  }

bootstrap <- function(t_irf, reg, spvar_fit, spvar_df, B) {
  # Set any missing value to 0
  spvar_df[is.na(spvar_df)] <- 0
  
  # Compute predicted values from the model
  y_pred <- predict(spvar_fit, spvar_df)
  
  # Compute the residuals implied by those predicted values
  res_spei <- y_pred$eqspei.pred - spvar_df$spei
  res_smoist <- y_pred$eqsmoist.pred - spvar_df$smoist
  res_ndvi <- y_pred$eqndvi.pred - spvar_df$ndvi
  
  # Prepare the block diagonal matrices with the coefficients
  coeffs <- prepare_coeff_matrixes(spvar_fit)
  weight_otimes <- coeffs[[1]]
  theta_otimes <- coeffs[[2]]
  upsilon_otimes <- coeffs[[3]]
  
  # Create an empty array to store results
  irf_fin <- array(0, dim = c(t_irf, 139 * 3, B, 3))
  
  for (b in 1:B) {
    # Print progress every 100 iterations
    if (b %% 100 == 0) {
      print(b)
    }
    
    # Create empty matrices to store results
    y <- matrix(0, nrow = 403, ncol = 139 * 3)
    y[1, ] <-
      c(spvar_df[1:139, 1], spvar_df[1:139, 2], spvar_df[1:139, 3])
    y_hor <- matrix(0, nrow = 402 * 139, ncol = 148)
    
    # Add the fixed effects to the y_hor matrix
    for (reg_i in 1:139) {
      fe <- rep(0, reg_i - 1)
      fe <- c(fe, 1)
      fe <- c(fe, rep(0, 139 - reg_i))
      fe <- rep(fe, 402)
      y_hor[, reg_i + 3] <- fe
      
    }
    
    for (t in 2:403) {
      #Sample from the set of residuals
      res_ind <- (sample.int(n = 401, size = 1))
      res_1 <-
        res_spei[(((res_ind - 1) * 139) + 1):((res_ind) * 139)]
      res_2 <-
        res_ndvi[(((res_ind - 1) * 139) + 1):((res_ind) * 139)]
      res_3 <-
        res_smoist[(((res_ind - 1) * 139) + 1):((res_ind) * 139)]
      
      # Compute the current period y using the lags and its sampled resiudals
      y[t, ] <-
        (theta_otimes + upsilon_otimes %*% weight_otimes) %*% y[t - 1, ] + c(res_1, res_2, res_3)
      for (v in 1:3) {
        # Store the values in y_hor
        y_hor[((t - 2) * 139 + 1):((t - 1) * 139), v] <-
          y[t, ((v - 1) * 139 + 1):((v) * 139)]
      }
    }
    
    # Add spatial and temporal lags to y_hor
    for (i in 1:401) {
      y_hor[(i * 139 + 1):((i + 1) * 139), 146] <-
        weights %*% y_hor[((i - 1) * 139 + 1):((i) * 139), 1]
      y_hor[(i * 139 + 1):((i + 1) * 139), 147] <-
        weights %*% y_hor[((i - 1) * 139 + 1):((i) * 139), 2]
      y_hor[(i * 139 + 1):((i + 1) * 139), 148] <-
        weights %*% y_hor[((i - 1) * 139 + 1):((i) * 139), 3]
      y_hor[(i * 139 + 1):((i + 1) * 139), 143:145] <-
        y_hor[((i - 1) * 139 + 1):((i) * 139), 1:3]
    }
    
    # Transform y_hor in a dataframe
    y_hor <- data.frame(y_hor)
    colnames(y_hor) <- colnames(spvar_df)
    
    # Fit an SpVAR on the bootstrapped data
    boots_fit <-
      systemfit(
        c(
          eqspei = spei ~ 0 + . - spei - ndvi - smoist,
          eqndvi = ndvi ~ 0 + . - spei - ndvi - smoist,
          eqsmoist = smoist ~ 0 + . - spei - ndvi - smoist
        ),
        data = y_hor,
        method = "SUR"
      )
    
    # Compute the IRFs resulting from the fitted SpVAR
    for (var in 1:3) {
      irf_fin[, , b, var] <- irf(t_irf, reg, var, boots_fit)
    }
  }
  
  # Prepare the quantiles matrices
  quantiles_up_spei <- matrix(nrow = t_irf, ncol = 139 * 3)
  quantiles_down_spei <- matrix(nrow = t_irf, ncol = 139 * 3)
  quantiles_up_ndvi <- matrix(nrow = t_irf, ncol = 139 * 3)
  quantiles_down_ndvi <- matrix(nrow = t_irf, ncol = 139 * 3)
  quantiles_up_smoist <- matrix(nrow = t_irf, ncol = 139 * 3)
  quantiles_down_smoist <- matrix(nrow = t_irf, ncol = 139 * 3)
  
  #Store values in the quantiles matrices
  for (t in 1:t_irf) {
    for (m in 1:417) {
      quantiles_up_spei[t, m] <- quantile(irf_fin[t, m, , 1], 0.975)
      quantiles_down_spei[t, m] <-
        quantile(irf_fin[t, m, , 1], 0.025)
      quantiles_up_ndvi[t, m] <- quantile(irf_fin[t, m, , 2], 0.975)
      quantiles_down_ndvi[t, m] <-
        quantile(irf_fin[t, m, , 2], 0.025)
      quantiles_up_smoist[t, m] <-
        quantile(irf_fin[t, m, , 3], 0.975)
      quantiles_down_smoist[t, m] <-
        quantile(irf_fin[t, m, , 3], 0.025)
    }
    
  }
  
  return(
    list(
      quantiles_up_spei,
      quantiles_down_spei,
      quantiles_up_ndvi,
      quantiles_down_ndvi,
      quantiles_up_smoist,
      quantiles_down_smoist
    )
  )
}


multiplot_irf_map <-
  function(spvar_fit,
           index_pairs,
           test_df,
           t,
           reg,
           t_plot) {
    
    # Calculate the IRF for the SMoist shock (var = 3)
    y <- irf(t_irf = t,
             reg = reg,
             var = 3,
             spvar_fit)
    
    # Create a data frame for plotting SMoist shocks's IRF with latitude, longitude and SPEI values
    plot_df_1 <-
      create_plot_df(
        t_plot,
        reg_latitudes,
        reg_long_min,
        reg_long_max,
        lats,
        lons,
        y = y,
        index_pairs = index_pairs,
        test_df = test_df
      )
    
    # Calculate the IRF for the NDVI shock (var = 2)
    y <- irf(t_irf = t,
             reg = reg,
             var = 2,
             spvar_fit)
    
    # Create a data frame for plotting NDVI shocks's IRF with latitude, longitude and SPEI values
    plot_df_2 <-
      create_plot_df(
        t_plot,
        reg_latitudes,
        reg_long_min,
        reg_long_max,
        lats,
        lons,
        y = y,
        index_pairs = index_pairs,
        test_df = test_df
      )
    
    # Calculate the IRF for the SPEI shock (var = 1)
    y <- irf(t_irf = t,
             reg = reg,
             var = 1,
             spvar_fit)
    
    # Create a data frame for plotting SPEI shocks's IRF with latitude, longitude and SPEI values
    plot_df_3 <-
      create_plot_df(
        t_plot,
        reg_latitudes,
        reg_long_min,
        reg_long_max,
        lats,
        lons,
        y = y,
        index_pairs = index_pairs,
        test_df = test_df
      )
    
    # Find the maximum absolute value of SPEI across all three data frames
    max = max(abs(c(
      plot_df_1$SPEI[plot_df_1$reg_ind != reg], plot_df_2$SPEI[plot_df_2$reg_ind !=
                                                                 reg], plot_df_3$SPEI[plot_df_3$reg_ind != reg]
    )))
    
    # Create a map for the SMoist shock
    plot_1 <-
      create_plot(
        plot_df_1[plot_df_1$reg_ind != reg, ],
        "SPEI",
        scale_fixed = "Yes",
        max = max,
        min = -max,
        reg_highlight = reg
      )
    
    
    # Create a map for the NDVI shock
    plot_2 <-
      create_plot(
        plot_df_2[plot_df_2$reg_ind != reg, ],
        "SPEI",
        scale_fixed = "Yes",
        max = max,
        min = -max,
        reg_highlight = reg
      )
    
    
    # Create a map for the SPEI shock
    plot_3 <-
      create_plot(
        plot_df_3[plot_df_3$reg_ind != reg, ],
        "SPEI",
        scale_fixed = "Yes",
        max = max,
        min = -max,
        reg_highlight = reg
      )
    
    # Return a grid of three maps
    return(list(plot_1,
                plot_2,
                plot_3))
  }

multiplot_irf_all <- function(spvar_fit, t, reg, plot_all = TRUE) {
  # Calculate the IRF for SMoist shock (var = 3)
  y <- irf(t_irf = t,
           reg = reg,
           var = 3,
           spvar_fit)
  
  # Plot the IRF for the SMoist shock
  irf_1 <- irf_plot(y, var = 1, reg, tit = "SMoist shock")
  
  # Calculate the IRF for NDVI shock (var = 2)
  y <- irf(t_irf = t,
           reg = reg,
           var = 2,
           spvar_fit)
  
  # Plot the IRF for the NDVI shock
  irf_2 <- irf_plot(y, var = 1, reg, tit = "NDVI shock")
  
  # Calculate the IRF for SPEI shock (var = 1)
  y <- irf(t_irf = t,
           reg = reg,
           var = 1,
           spvar_fit)
  
  # Plot the IRF for the SPEI shock
  irf_3 <- irf_plot(y, var = 1, reg, tit = "SPEI shock")
  
  if (plot_all == TRUE) {
    # Return a grid of three plots arranged vertically
    return(grid.arrange(irf_1, irf_2, irf_3, nrow = 3))
  }
  else{
    return(list(irf_1, irf_2, irf_3))
  }
}

long_term_prediction_plot <-
  function(test_df_spei,
           test_df_last_year = NA,
           test_df_last_year_spei = NA,
           test_df_last_year_smoist = NA,
           test_df_last_year_ndvi = NA,
           t_pred = 402,
           plot = "comparison",
           spvar_fit = spvar_fit) {
    # Prepare the plotting df
    plot_df_initial <-
      create_plot_df(1,
                     reg_latitudes ,
                     reg_long_min,
                     reg_long_max,
                     lats,
                     lons,
                     SPEI,
                     NDVI,
                     SMoist)
    
    # Store in it the number of droughts on the real data based on the previous categories
    for (r in 1:139) {
      lon_ind <- as.numeric(strsplit(index_pairs[r], ",")[[1]][2])
      lat_ind <- as.numeric(strsplit(index_pairs[r], ",")[[1]][1])
      plot_df_initial[plot_df_initial$lat_ind == lat_ind &
                        plot_df_initial$lon_ind == lon_ind, 5:7] <-
        c(
          sum(test_df_spei[, r] < -2),
          sum(test_df_spei[, r] > -2 &
                test_df_spei[, r] < -1.5),
          sum(test_df_spei[, r] > -1.5 & test_df_spei[, r] < -1)
        )
      
    }
    
    # Store the maximum and minimum number of droughts
    max_n <- max(plot_df_initial[, 5:7])
    min_n <- min(plot_df_initial[, 5:7])
    
    
    # Create the plot for the actual number of severe droughts
    severe_plot_initial <- plot_df_initial
    names(severe_plot_initial)[5] <- "weights"
    severe_plot_initial <-
      create_plot(
        severe_plot_initial,
        var = "Months",
        tit = "Severe Droughts",
        scale_fixed = TRUE,
        max = max_n,
        min = min_n
      )
    
    # Create the plot for the actual number of moderate droughts
    moderate_plot_initial <- plot_df_initial
    names(moderate_plot_initial)[6] <- "weights"
    moderate_plot_initial <-
      create_plot(
        moderate_plot_initial,
        var = "Moderate Droughts",
        no_lab = TRUE ,
        tit = "Moderate Droughts",
        scale_fixed = TRUE,
        max = max_n,
        min = min_n
      )
    
    # Create the plot for the actual number of mild droughts
    mild_plot_initial <- plot_df_initial
    names(mild_plot_initial)[7] <- "weights"
    mild_plot_initial <-
      create_plot(
        mild_plot_initial,
        var = "Mild Droughts",
        no_lab = TRUE,
        tit = "Mild Droughts",
        scale_fixed = TRUE,
        max = max_n,
        min = min_n
      )
    
    # If plot is initial only the actual data plot is returned
    if (plot == "initial") {
      return(
        grid.arrange(
          mild_plot_initial,
          moderate_plot_initial,
          severe_plot_initial,
          ncol = 3,
          widths = c(1, 1, 1.3372)
        )
      )
    }
    
    # Start by creating a dataframe using last year's data
    long_term_df <-
      prepare_spvar_df(
        test_df_last_year,
        test_df_last_year_spei,
        test_df_last_year_smoist,
        test_df_last_year_ndvi,
        T = 2,
        lags = 1
      )
    
    # Prepare a matrix to store results in
    pred <- matrix(0, nrow = 139 * (t_pred + 1), ncol = 148)
    
    # Add fixed effects dummies to the matrix
    for (reg in 1:139) {
      fe <- rep(0, reg - 1)
      fe <- c(fe, 1)
      fe <- c(fe, rep(0, 139 - reg))
      fe <- rep(fe, (t_pred + 1))
      pred[, reg + 3] <- fe
    }
    
    # Turn the matrix in a df and have it share the column names with the original df
    pred <- data.frame(pred)
    colnames(pred) <- colnames(long_term_df)
    
    
    # Start by filling the (spatial and temporal) lags in the matrix
    pred[1:139, 143:145] <- long_term_df[, 1:3]
    pred[1:139, 146] <- weights %*% long_term_df[, 1]
    pred[1:139, 147] <- weights %*% long_term_df[, 2]
    pred[1:139, 148] <- weights %*% long_term_df[, 3]
    
    for (i in 1:t_pred) {
      # Predict the next time period values
      prediction_sar <-
        predict(spvar_fit, pred[(((i - 1) * 139) + 1):((i) * 139), ])
      
      # Store the obtained values
      pred[(i * 139 + 1):((i + 1) * 139), 1] <-
        prediction_sar$eqspei.pred
      pred[(i * 139 + 1):((i + 1) * 139), 2] <-
        prediction_sar$eqndvi.pred
      pred[(i * 139 + 1):((i + 1) * 139), 3] <-
        prediction_sar$eqsmoist.pred
      
      pred[(i * 139 + 1):((i + 1) * 139), 146] <-
        weights %*% pred[(i * 139 + 1):((i + 1) * 139), 1]
      pred[(i * 139 + 1):((i + 1) * 139), 147] <-
        weights %*% pred[(i * 139 + 1):((i + 1) * 139), 2]
      pred[(i * 139 + 1):((i + 1) * 139), 148] <-
        weights %*% pred[(i * 139 + 1):((i + 1) * 139), 3]
      pred[(i * 139 + 1):((i + 1) * 139), 143:145] <-
        pred[(i * 139 + 1):((i + 1) * 139), 1:3]
      
    }
    
    # Filter out the first empty month
    pred <- pred[-(1:139), ]
    
    # Prepare a plotting df
    plot_df <-
      create_plot_df(1,
                     reg_latitudes ,
                     reg_long_min,
                     reg_long_max,
                     lats,
                     lons,
                     SPEI,
                     NDVI,
                     SMoist)
    
    # Compute the number of droughts predicted in each category and store them in the plot df
    for (r in 1:139) {
      lon_ind <- as.numeric(strsplit(index_pairs[r], ",")[[1]][2])
      lat_ind <- as.numeric(strsplit(index_pairs[r], ",")[[1]][1])
      plot_df[plot_df$lat_ind == lat_ind &
                plot_df$lon_ind == lon_ind, 5:7] <-
        c(sum(pred[seq(r, 139 * t_pred, 139), 1] < -2),
          sum(pred[seq(r, 139 * t_pred, 139), 1] > -2 &
                pred[seq(r, 139 * t_pred, 139), 1] < -1.5),
          sum(pred[seq(r, 139 * t_pred, 139), 1] > -1.5 &
                pred[seq(r, 139 * t_pred, 139), 1] < -1))
      
    }
    
    # Store the maximum and minimum number of droughts
    max_n <- max(plot_df[, 5:7])
    min_n <- min(plot_df[, 5:7])
    
    # Create the plot for the number of severe droughts
    severe_plot <- plot_df
    names(severe_plot)[5] <- "weights"
    severe_plot <-
      create_plot(
        severe_plot,
        var = "Months",
        tit = "Severe Droughts",
        scale_fixed = TRUE,
        max = max_n,
        min = min_n
      )
    
    # Create the plot for the number of moderate droughts
    moderate_plot <- plot_df
    names(moderate_plot)[6] <- "weights"
    moderate_plot <-
      create_plot(
        moderate_plot,
        var = "Moderate Droughts",
        no_lab = TRUE ,
        tit = "Moderate Droughts",
        scale_fixed = TRUE,
        max = max_n,
        min = min_n
      )
    
    # Create the plot for the number of mild droughts
    mild_plot <- plot_df
    names(mild_plot)[7] <- "weights"
    mild_plot <-
      create_plot(
        mild_plot,
        var = "Mild Droughts",
        no_lab = TRUE,
        tit = "Mild Droughts",
        scale_fixed = TRUE,
        max = max_n,
        min = min_n
      )
    
    # Return only predictions plot if plot is future
    if (plot == "future") {
      return(grid.arrange(
        mild_plot,
        moderate_plot,
        severe_plot,
        ncol = 3,
        widths = c(1, 1, 1.344)
      ))
    }
    
    # Return both actual data and predictions plot otherwise
    else{
      return(
        grid.arrange(
          mild_plot_initial,
          moderate_plot_initial,
          severe_plot_initial,
          mild_plot,
          moderate_plot,
          severe_plot,
          ncol = 3,
          widths = c(1, 1, 1.337)
        )
      )
    }
    
  }
