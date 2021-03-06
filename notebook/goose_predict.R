
logit <- function(p){
    size <- length(p);
    resv <- rep(x = NA, length = size)
    for(i in 1:size){
        if(p[i] >= 0 & p[i] <= 1){
            resv[i] <- log(p[i] / (1 - p[i]));   
        } 
    }
    return(resv);
}


goose_rescale_AIG <- function(data, years = 22){
  
    AIGs   <- data$AIG[1:years];         # Take the years before the change
    tii    <- 1:years;                   # Corresponding time variable
    DATg   <- data.frame(AIGs,tii);      # Create dataframe with grass and time
    get_cf <- coef(object = lm(logit(AIGs/8000)~tii, data = DATg)); # Vals
    cf_p2  <- as.numeric(get_cf[1]);
    cf_p3  <- as.numeric(get_cf[2]);
    lmodg  <- nls(formula = AIGs~phi1/(1+exp(-(phi2+phi3*tii))),
                  data    = DATg, trace = FALSE,
                  start   = list(phi1 = 7000, phi2 = cf_p2, phi3 = cf_p3));
    newx   <- data.frame(tii = years + 1);             # Predict one year ahead
    pg     <- predict(object = lmodg, newdata = newx); 
    dif    <- data$AIG[(years+1):dim(data)[1]][1]-pg;  
   
    data$AIG[(years+1):dim(data)[1]] <- data$AIG[(years+1):dim(data)[1]] - dif;
    return(data);
}

goose_clean_data <- function(file){
  
    data   <- read.csv(file);                # Load dataset
    data$y <- data$Count+data$IslayCull;     # Count data + culled
    data   <- goose_rescale_AIG(data = data, years = 22);
  
    data$AugTemp   <- as.numeric( scale(data$AugTemp) );
    data$IslayTemp <- as.numeric( scale(data$IslayTemp) );
    data$AugRain   <- as.numeric( scale(data$AugRain) );
    data$AIG.sc    <- as.numeric( scale(data$AIG) );
    data$HB        <- data$IcelandCull+data$GreenlandCull;
  
    return(data);
}  

goose_growth <- function(para, data){
  
    data_rows <- dim(data)[1];
    N_pred <- goose_pred(para = para, data = data);
  
    DEV    <- N_pred[3:data_rows] - data$y[3:data_rows];
    sq_Dev <- DEV * DEV;
    pr_sum <- sum( sq_Dev / N_pred[3:data_rows] );
    SS_tot <- (1 / pr_sum) * 1000;
    return(SS_tot);
}

goose_pred <- function(para, data){
  r_val        <- para[1]; # Maximum growth rate
  K_val        <- para[2]; # Carrying capacity
  G_rain_coeff <- para[3]; # Effect of precipitation on Greenland in August
  G_temp_coeff <- para[4]; # Effect of temperature on Greenland in August
  I_temp_coeff <- para[5]; # Effect of temperature on Islay the previous winter
  AIG_2_yrs    <- para[6]; # Effect of area of improved grassland 2 years prior
  hunting_bag  <- para[7]; # Effect of hunting bag
  
  data_rows <- dim(data)[1];
  N_pred    <- rep(x = NA, times = data_rows);
  for(time in 3:data_rows){
      goose_repr   <- r_val * data$y[time - 1];
      goose_dens   <- 1 - (data$y[time -1] / (K_val * data$AIG[time - 1]));
      goose_now    <- data$y[time - 1];
      G_rain_adj   <- G_rain_coeff * data$AugRain[time - 1];
      G_temp_adj   <- G_temp_coeff * data$AugTemp[time - 1];
      I_temp_adj   <- I_temp_coeff * data$IslayTemp[time - 1];
      AIG_2_adj    <- AIG_2_yrs    * data$AIG.sc[time - 2];
      adjusted     <- G_rain_adj + G_temp_adj + I_temp_adj + AIG_2_adj
      hunted       <- hunting_bag  * goose_now;
      N_pred[time] <- goose_repr * (goose_dens + adjusted) + goose_now - 
                      goose_now * hunting_bag;
  }
  
  return(N_pred);
}

get_goose_paras <- function(data, init_params = NULL){
    if( is.null(init_params) == TRUE ){
        init_params    <- c(0.1,6,0,0,0,0, 0);
    }
    contr_paras    <- list(trace = 1, fnscale = -1, maxit = 1000, factr = 1e-8,
                           pgtol = 0);
    get_parameters <- optim(par = init_params, fn = goose_growth, data = data,
                            method = "BFGS", control = contr_paras, 
                            hessian = TRUE);
    return(get_parameters);
}

goose_plot_pred <- function(data, year_start = 1987, ylim = c(10000, 60000),
                            plot = TRUE){
    params <- get_goose_paras(data = data);
    Npred  <- goose_pred(para = params$par, data = data);
    yrs    <- year_start:(year_start + length(data$y) - 1);
    if(plot == TRUE){
        par(mar = c(5, 5, 1, 1));
        plot(x =  yrs, y = data$y, pch = 1, ylim = ylim, cex.lab = 1.5,
             xlab="Year", ylab="Population size")         # Observed time series
        points(x = yrs, y = Npred, pch = 19, col = "red") # Predict time series
        oend <- length(data$y);
        points(x = yrs[3:oend], y = data$y[2:(oend - 1)], pch = 19, 
               col = "blue");
    }
    return(Npred);
}

goose_predict_and_plot <- function(file, plot = TRUE){
    dat    <- read.csv(file);
    data   <- goose_clean_data(file);
    goosep <- goose_plot_pred(data = data, plot = plot);
    return(goosep);
}

goose_gmse_popmod <- function(goose_data){
    N_pred <- goose_plot_pred(data = goose_data, plot = FALSE);
    N_last <- length(N_pred);
    New_N  <- as.numeric(N_pred[N_last]);
    New_N  <- New_N - (0.03 * New_N);
    if(New_N < 1){
        New_N <- 1;
        warning("Extinction has occurred");
    }
    return(New_N);
}

goose_gmse_obsmod <- function(resource_vector, obs_error, use_est){
    obs_err    <- rnorm(n = 1, mean = 0, sd = obs_error);
    obs_vector <- resource_vector + obs_err;
    if(use_est == -1){
        obs_vector <- obs_vector - abs(obs_error * 1.96);
    }
    if(use_est == 1){
        obs_vector <- obs_vector + abs(obs_error * 1.96);
    }
    return(obs_vector);
}

goose_gmse_manmod <- function(observation_vector, manage_target){
    manager_vector <- observation_vector - manage_target;
    if(manager_vector < 0){
        manager_vector <- 0;
    }
    return(manager_vector);
}

goose_gmse_usrmod <- function(manager_vector, max_HB){
    user_vector <- manager_vector;
    if(user_vector > max_HB){
        user_vector <- max_HB;
    }
    return(user_vector);
}

goose_sim_paras <- function(goose_data){
    last_row <- dim(goose_data)[1];
    for(col in 1:dim(goose_data)[2]){
        if( is.na(goose_data[last_row, col]) == TRUE ){
            if(col < 6){
                goose_data[last_row, col] <- 0;
            }else{
                all_dat   <- goose_data[,col];
                avail_dat <- all_dat[!is.na(all_dat)];
                rand_val  <- sample(x = avail_dat, size = 1);
                goose_data[last_row, col] <- rand_val;
            }
        }
    }
    return(goose_data);
}

sim_goose_data <- function(gmse_results, goose_data){
    gmse_pop   <- gmse_results$resource_results;
    gmse_obs   <- gmse_results$observation_results;
    if(length(gmse_results$manager_results) > 1){
        gmse_man   <- gmse_results$manager_results[3];
    }else{
        gmse_man   <- as.numeric(gmse_results$manager_results);
    }
    if(length(gmse_results$user_results) > 1){
        gmse_cul   <- sum(gmse_results$user_results[,3]);
    }else{
        gmse_cul   <- as.numeric(gmse_results$user_results);
    }
    I_G_cul_pr <- (goose_data[,3] + goose_data[,5]) / goose_data[,10];
    I_G_cul_pr <- mean(I_G_cul_pr[-length(I_G_cul_pr)]);
    goose_data <- goose_sim_paras(goose_data);
    rows       <- dim(goose_data)[1];
    cols       <- dim(goose_data)[2];
    goose_data[rows, 3]    <- gmse_obs * I_G_cul_pr;
    goose_data[rows, 4]    <- gmse_cul;
    goose_data[rows, 5]    <- 0;
    goose_data[rows, cols] <- gmse_cul;
    new_r     <- rep(x = 0, times = cols);
    new_r[1]  <- goose_data[rows, 1] + 1;
    new_r[2]  <- gmse_pop - gmse_cul;
    new_r[3]  <- 0; 
    new_r[4]  <- 0;
    new_r[5]  <- 0;
    new_r[6]  <- sample(x = goose_data[,6], size = 1);
    new_r[7]  <- sample(x = goose_data[,7], size = 1);
    new_r[8]  <- sample(x = goose_data[,8], size = 1);
    new_r[9]  <- sample(x = goose_data[,9], size = 1);
    new_r[10] <- gmse_pop - gmse_cul;
    new_r[11] <- sample(x = goose_data[,11], size = 1);
    new_r[12] <- 0;
    new_dat   <- rbind(goose_data, new_r);
    return(new_dat);
}

gmse_goose <- function(data_file = "toy_data.csv", manage_target, max_HB, 
                       obs_error = 1438.614, years = 10, use_est = "normal",
                       plot = TRUE){
    # -- Initialise ------------------------------------------------------------
    proj_yrs   <- years;
    goose_data <- goose_clean_data(file = data_file);
    last_year  <- goose_data[dim(goose_data)[1], 1];
    use_est    <- 0;
    if(use_est == "cautious"){
        use_est <- -1;
    }
    if(use_est == "aggressive"){
        use_est <- 1;
    }
    assign("goose_data", goose_data, envir = globalenv() );
    assign("target", manage_target, envir = globalenv() );
    assign("max_HB", max_HB, envir = globalenv() );
    assign("obs_error", obs_error, envir = globalenv() );
    assign("use_est", use_est, envir = globalenv() );
    gmse_res   <- gmse_apply(res_mod = goose_gmse_popmod, 
                             obs_mod = goose_gmse_obsmod,
                             man_mod = goose_gmse_manmod,
                             use_mod = goose_gmse_usrmod,
                             goose_data = goose_data, obs_error = obs_error,
                             manage_target = target, max_HB = max_HB,
                             use_est = use_est, stakeholders = 1, 
                             get_res = "full");
    goose_data <- sim_goose_data(gmse_results = gmse_res$basic, 
                                 goose_data = goose_data);
    assign("goose_data", goose_data, envir = globalenv() );
    assign("target", manage_target, envir = globalenv() );
    assign("max_HB", max_HB, envir = globalenv() );
    assign("obs_error", obs_error, envir = globalenv() );
    assign("use_est", use_est, envir = globalenv() );
    assign("gmse_res", gmse_res, envir = globalenv() );
    # -- Simulate --------------------------------------------------------------
    while(years > 0){
        gmse_res_new   <- gmse_apply(res_mod = goose_gmse_popmod, 
                                     obs_mod = goose_gmse_obsmod,
                                     man_mod = goose_gmse_manmod,
                                     use_mod = goose_gmse_usrmod,
                                     goose_data = goose_data,
                                     manage_target = target, use_est = use_est,
                                     max_HB = max_HB, obs_error = obs_error,
                                     stakeholders = 1, get_res = "full");
       if(as.numeric(gmse_res_new$basic[1]) == 1){
           break;      
       }
       assign("gmse_res_new", gmse_res_new, envir = globalenv() );
       gmse_res   <- gmse_res_new;
       assign("gmse_res", gmse_res, envir = globalenv() );
       goose_data <- sim_goose_data(gmse_results = gmse_res$basic, 
                                    goose_data = goose_data);
       assign("goose_data", goose_data, envir = globalenv() );
       assign("target", manage_target, envir = globalenv() );
       assign("max_HB", max_HB, envir = globalenv() );
       assign("obs_error", obs_error, envir = globalenv() );
       assign("use_est", use_est, envir = globalenv() );
       years <- years - 1;
    }
    if(plot == TRUE){
        dat <- goose_data[-1,];
        yrs <- dat[,1];
        NN  <- dat[,10];
        HB  <- dat[,3];
        pry <- (last_year):(yrs[length(yrs)]-2+20);
        par(mar = c(5, 5, 1, 1));
        plot(x = yrs, y = NN, xlab = "Year", ylab = "Population size",
             cex = 1.25, pch = 20, type = "b", ylim = c(0, max(NN)), 
             cex.lab = 1.5, cex.axis = 1.5, lwd = 2);
        polygon(x = c(pry, rev(pry)), 
                y = c(rep(x = -10000, times = proj_yrs + 20), 
                      rep(x = 2*max(NN), times = proj_yrs + 20)), 
                col = "grey", border = NA);
        box();
        points(x = yrs, y = NN, cex = 1.25, pch = 20, type = "b");
        points(x = yrs, y = HB, type = "b", cex = 1.25, col = "red", 
               pch = 20, lwd = 2);
        abline(h = manage_target, lwd = 0.8, lty = "dotted");
        text(x = dat[5,1], y = max(NN), labels = "Observed", cex = 2.5);
        text(x = pry[5] + 1, y = max(NN), labels = "Projected", cex = 2.5);
    }
    return(goose_data);
}


gmse_goose_multiplot <- function(data_file = "toy_data.csv", proj_yrs = 10, 
                                 obs_error = 1438.614, manage_target = 26000, 
                                 max_HB = 1200, iterations = 10, 
                                 use_est = "normal"){
    goose_multidata <- NULL;
    for(i in 1:iterations){
        goose_multidata[[i]] <- gmse_goose(data_file = data_file,
                                           obs_error = obs_error,
                                           manage_target = manage_target, 
                                           max_HB = max_HB, plot = FALSE,
                                           use_est = use_est);
        print(paste("Simulating ---------------------------------------> ",i));
    }
    goose_data <- goose_multidata[[1]];
    dat        <- goose_data[-1,];
    last_year  <- dat[dim(dat)[1], 1];
    yrs        <- dat[,1];
    NN         <- dat[,10];
    HB         <- dat[,3];
    pry        <- (last_year - proj_yrs):last_year;
    obsrvd     <- 1:(dim(dat)[1] - proj_yrs - 1);
    par(mar = c(5, 5, 1, 1));
    plot(x = yrs, y = NN, xlab = "Year", ylab = "Population size",
         cex = 1.25, pch = 20, type = "n", ylim = c(0, max(NN)), 
         cex.lab = 1.5, cex.axis = 1.5, lwd = 2);
    polygon(x = c(pry, 2*last_year, 2*last_year, rev(pry)), 
            y = c(rep(x = -10000, times = length(pry) + 1), 
                  rep(x = 2*max(NN), times = length(pry) + 1)), 
            col = "grey", border = NA);
    box();
    points(x = yrs[obsrvd], y = NN[obsrvd], cex = 1.25, pch = 20, type = "b");
    abline(h = manage_target, lwd = 0.8, lty = "dotted");
    text(x = dat[5,1], y = max(NN), labels = "Observed", cex = 2.5);
    text(x = pry[5], y = max(NN), labels = "Projected", cex = 2.5);
    for(i in 1:length(goose_multidata)){
        goose_data <- goose_multidata[[i]];
        dat <- goose_data[-1,];
        yrs <- dat[,1];
        NN  <- dat[,10];
        HB  <- dat[,3];
        pry <- (last_year):(yrs[length(yrs)]-2+20);
        points(x = yrs, y = NN, pch = 20, type = "l", lwd = 0.6);
    }
    return(goose_multidata);
}


gmse_print_multiplot <- function(goose_multidata, manage_target, proj_yrs,
                                 type = 0){
    iters      <- length(goose_multidata);
    rows       <- dim(goose_multidata[[1]])[1];
    goose_data <- goose_multidata[[1]];
    dat        <- goose_data;
    last_year  <- dat[dim(dat)[1], 1];
    yrs        <- dat[,1];
    NN         <- dat[,10];
    HB         <- dat[,3];
    pry        <- (last_year - proj_yrs):last_year;
    obsrvd     <- 1:(dim(dat)[1] - proj_yrs);
    par(mar = c(5, 5, 1, 1));
    plot(x = yrs, y = NN, xlab = "Year", ylab = "Population size",
         cex = 1.25, pch = 20, type = "n", ylim = c(0, max(NN)), 
         cex.lab = 1.5, cex.axis = 1.5, lwd = 2);
    polygon(x = c(pry, 2*last_year, 2*last_year, rev(pry)), 
            y = c(rep(x = -10000, times = length(pry) + 1), 
                  rep(x = 2*max(NN), times = length(pry) + 1)), 
            col = "grey", border = NA);
    box();
    points(x = yrs[obsrvd], y = NN[obsrvd], cex = 1.25, pch = 20, type = "b");
    abline(h = manage_target, lwd = 0.8, lty = "dotted");
    text(x = dat[5,1], y = max(NN), labels = "Observed", cex = 2.5);
    text(x = pry[5], y = max(NN), labels = "Projected", cex = 2.5);
    if(type == 0){
        for(i in 1:iters){
            goose_data <- goose_multidata[[i]];
            dat <- goose_data;
            yrs <- dat[,1];
            NN  <- dat[,10];
            HB  <- dat[,3];
            pry <- (last_year+1):(yrs[length(yrs)]-1+20);
            points(x = yrs, y = NN, pch = 20, type = "l", lwd = 0.6);
        }
    }
    if(type == 1){
        NN  <- matrix(data = 0, nrow = rows, ncol = iters);
        yrs <- goose_multidata[[1]][,1];
        for(i in 1:iters){
            goose_data <- goose_multidata[[i]];
            dat        <- goose_data;
            NN[,i]     <- dat[,2];
        }
        py <- (rows - proj_yrs + 1):(rows-1); 
        NN_qu <- apply(X = NN, MARGIN = 1, FUN = quantile);
        points(x = yrs[py], y = NN_qu[3,py], cex = 1.25, pch = 20, lwd = 2, 
               type = "b");
        arrows(x0 = yrs[py], x1 = yrs[py], y0 = NN_qu[3,py], y1 = NN_qu[2,py], 
               angle = 90, length = 0.1);
        arrows(x0 = yrs[py], x1 = yrs[py], y0 = NN_qu[3,py], y1 = NN_qu[4,py], 
               angle = 90, length = 0.1);
        points(x = yrs[py], y = NN_qu[1,py], cex = 0.8, pch = 25, bg = "red");
        points(x = yrs[py], y = NN_qu[5,py], cex = 0.8, pch = 24, bg = "red");
    }
}




