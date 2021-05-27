# Adrian Bach
# University of Stirling

t <- rep(NA, 20)
for (i in 1:20) {t[i] <- paste("t",i, sep = "")}
flw.names <- c("rep", "pred", "Extinct", t)

#### functions needed ####

boot_sd_ci <- function(x, confidence = 95, itr = 1000) {
  
  # init iterations and sample
  i <- itr
  bt_avg <- NULL
  
  # loop over iterations
  while (i > 0) {
    # sample randomly from x
    spl <- sample(x, length(x), replace = TRUE)
    
    # store mean
    bt_avg <- c(bt_avg, sum(spl)/length(spl))
    
    # decrement i
    i <- i-1
  }
  
  # mean over the bootstrapped samples
  bt_est <- sum(bt_avg)/itr
  
  # compute standard deviation
  bt_sd <- sqrt((1/(length(x)-1)) * sum((bt_avg-bt_est)^2))
  
  # compute confidence interval
  # sorting bt_avg numerically
  st_avg <- sort(bt_avg)
  
  # get the first value after the 2.5 first centiles
  bt_95ci_inf <- st_avg[floor(0.5*(1-0.01*confidence)*itr)+1]
  
  # get the last value before the 2.5 last centiles
  bt_95ci_sup <- st_avg[floor((0.01*confidence+0.5*(1-0.01*confidence))*itr)-1]
  
  res <- c(bt_sd, bt_95ci_inf, bt_95ci_sup)
  return(res) 
  
}

predVSreg_stats <- function(df, ts, omit.extinction = FALSE) {
  
  df <- as.data.frame(df)
  
  # levels of pred options
  prd <- levels(as.factor(df$pred))
  
  # number of samples for bootstrap 
  nbs <- 1000
  
  # create empty tab
  res_tab <- NULL
  
  # # initiate a count for later
  # if (omit.extinction == TRUE) {
  #   zz <- 1
  # }
  
  ## loop over the prediction options
  # for each prd value
  for (i in 1:length(prd)) {
    
    if (omit.extinction == "TRUE") { 
      
      # a loop to calculate extinction freq
      sub <- subset(df, pred == prd[i])
      
      # NULL tab
      ext_freq <- NULL
      sd_ci <- boot_sd_ci(sub$extinct, itr = nbs)
      res <- c(sum(sub$extinct)/dim(sub)[1], sd_ci[1], sd_ci[2],sd_ci[3])
      ext_freq <- rbind(ext_freq, res)
      
      print("Ommiting replicates that resulted in Resource extinction")
      df <- subset(df, extinct == 0)
       
      # # increment count 
      # zz <- zz + 1
      
    } # end if loop on omit.extinction 
      
    # initiate a string
    res_str <- NULL
    
    # subset
    sub <- subset(df, pred == prd[i])
    
    # number of replicates
    nbrep <- dim(sub)[1]
    
    # extinction frequency
    if (omit.extinction == TRUE) {
      ext <- ext_freq
    } else {
      sd_ci <- boot_sd_ci(sub$extinct, itr = nbs)
      ext <- c(sum(sub$extinct)/dim(sub)[1], sd_ci[1], sd_ci[2], sd_ci[3])
    }
      
      # start filling the string in
      res_str <- c(prd[i], nbrep, ext)
      
      # avoid problems if there is only one replicate
      if (nbrep >= 2) {
        
      ## mean, sd and 95ci for each proxy
      
      # Actual Resource population deviation from Manager's target at ts before last
      sd_ci <- boot_sd_ci(sub$act_dev_bflast, itr = nbs)
      res_str <- c(res_str, mean(sub$act_dev_bflast), sd_ci[1], sd_ci[2], sd_ci[3])
      
      # Actual Resource population deviation from Manager's target at last ts
      sd_ci <- boot_sd_ci(sub$act_dev, itr = nbs)
      res_str <- c(res_str, mean(sub$act_dev), sd_ci[1], sd_ci[2], sd_ci[3])
      
      # Users' total final yield at ts before last
      # sd_ci <- boot_sd_ci(sub$fin_yield, itr = nbs)
      # res_str <- c(res_str, mean(sub$fin_yield), sd_ci[1], sd_ci[2], sd_ci[3])
      sd_ci <- boot_sd_ci(sub$fin_yield_bflast/40000, itr = nbs)
      res_str <- c(res_str, mean(sub$fin_yield_bflast/40000), sd_ci[1], sd_ci[2], sd_ci[3])
        
      # Users' total final yield at last ts
      # sd_ci <- boot_sd_ci(sub$fin_yield, itr = nbs)
      # res_str <- c(res_str, mean(sub$fin_yield), sd_ci[1], sd_ci[2], sd_ci[3])
      sd_ci <- boot_sd_ci(sub$fin_yield/40000, itr = nbs)
      res_str <- c(res_str, mean(sub$fin_yield/40000), sd_ci[1], sd_ci[2], sd_ci[3])
      
      # Difference between the highest and the lowest yield at ts before last
      sd_ci <- boot_sd_ci(sub$ineq_bflast, itr = nbs)
      res_str <- c(res_str, mean(sub$ineq_bflast), sd_ci[1], sd_ci[2], sd_ci[3])
        
      # Difference between the highest and the lowest yield at last ts
      sd_ci <- boot_sd_ci(sub$ineq, itr = nbs)
      res_str <- c(res_str, mean(sub$ineq), sd_ci[1], sd_ci[2], sd_ci[3])
        
      # Sum of squared deviation from target
      sd_ci <- boot_sd_ci(sub$SAbsD/sub$final_ts, itr = nbs)
      res_str <- c(res_str, mean(sub$SAbsD/sub$final_ts), sd_ci[1], sd_ci[2], sd_ci[3])
        
    } else {
        
      print(paste("parameter set with UT = ", as.numeric(upd_thr[i])*100, "% and BB = ", as.numeric(bud_bon[j])*100, "% has less than 2 replicates"))
      
      # Actual Resource population deviation from Manager's target at ts before last
      res_str <- c(res_str, sub$act_dev_bflast, 0, sub$act_dev_bflast, sub$act_dev_bflast)
        
      # Actual Resource population deviation from Manager's target at last ts
      res_str <- c(res_str, sub$act_dev, 0, sub$act_dev, sub$act_dev)
      
      # Users' total final yield at ts before last
      res_str <- c(res_str, sub$fin_yield_bflast/40000, 0, sub$fin_yield_bflast/40000, sub$fin_yield_bflast/40000)
      
      # Users' total final yield at last ts
      res_str <- c(res_str, sub$fin_yield/40000, 0, sub$fin_yield/40000, sub$fin_yield/40000)
      
      # Difference between the highest and the lowest yield at ts before last
      res_str <- c(res_str, sub$ineq_bflast, 0, sub$ineq_bflast, sub$ineq_bflast)
        
      # Difference between the highest and the lowest yield at last ts
      res_str <- c(res_str, sub$ineq, 0, sub$ineq, sub$ineq)
      
      # Sum of squared deviation from target
      res_str <- c(res_str, sub$SAbsD/sub$final_ts, 0, sub$SAbsD/sub$final_ts, sub$SAbsD/sub$final_ts)
    } # end else loop on nbrep
      
    # binding the string to the tab
    res_tab <- rbind(res_tab, as.numeric(res_str))
    
  } # end for loop on pred options
  
  # Array of column names
  colnames(res_tab) <- c("prd", "rep", 
                         "ext_prob", "ext_prob_sd", "ext_prob_95ci_inf", "ext_prob_95ci_sup", 
                         "act_dev_bflast", "act_dev_bflast_sd", "act_dev_bflast_95ci_inf", "act_dev_bflast_95ci_sup", 
                         "act_dev", "act_dev_sd", "act_dev_95ci_inf", "act_dev_95ci_sup",
                         "fin_yield_bflast", "fin_yield_bflast_sd", "fin_yield_bflast_95ci_inf", "fin_yield_bflast_95ci_sup", 
                         "fin_yield", "fin_yield_sd", "fin_yield_95ci_inf", "fin_yield_95ci_sup", 
                         "ineq_bflast", "ineq_bflast_sd", "ineq_bflast_95ci_inf", "ineq_bflast_95ci_sup", 
                         "ineq", "ineq_sd", "ineq_95ci_inf", "ineq_95ci_sup",
                         "SumAbsDev", "SumAbsDev_sd", "SumAbsDev_95ci_inf", "SumAbsDev_95ci_sup")
  
  res_tab <- as.data.frame(res_tab)
  
  return(res_tab)
  
} # end function
