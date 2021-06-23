# Adrian Bach
# University of Stirling

#### Simulations ####

trajVSpred_replicate <- function(ts = 20, rep = 100, freq = 1,
                                bdgt = 1000, trgt = 2000, stkh = 40, obstype = 3,
                                popinit = 1000, tf = 12, cons = 0.5, surv = 4.75, repr = 5, 
                                ldim = 200,
                                out_file = "predVSregular-res.csv") {
  
  file.create(out_file); # Initialise a file in the directory.
  
  out_head <- as.character(strsplit(out_file, split = ".csv"));
  
  # Now initialise other files to print 
  bgt_file <- paste(out_head, "-flw_bgt", ".csv", sep = ""); 
  pop_file <- paste(out_head, "-flw_pop", ".csv", sep = "");
  cos_file <- paste(out_head, "-flw_cos", ".csv", sep = "");
  act_file <- paste(out_head, "-flw_act", ".csv", sep = "");
  
  # Now create the other new files
  file.create(bgt_file);
  file.create(pop_file);
  file.create(cos_file);
  file.create(act_file);
  
  # Array values to explore
  prd <- c(FALSE,TRUE)
  
  ## Create empty structures to gather simulation res
  columns <- c("pred", "rep", "extinct", "act_dev_bflast", "act_dev", "fin_yield_bflast", "fin_yield", "ineq_bflast", "ineq", "SAbsD", "final_ts") #, "overK", "param_set"             
  
  ## Create structures to receive measures and follow up on the costs, the actions, the budget and the population along the simulations
  results <- NULL
  flw_bgt <- NULL
  flw_cos <- NULL
  flw_act <- NULL
  flw_pop <- NULL
  
  ## Simulations loop
  start <- Sys.time()
    
  # loop over parameter values
  for (m in prd) {
    
    # Initialise an array of correct size 
    # Dimensions(lines = replicates, columns = measures)
    res <- array(data=NA, dim = c(rep, length(columns)), dimnames = list(NULL,columns))
    
    # With 'rep' number of replicate per parameter combo
    for (k in 1:rep) {
      
      # Run GMSE for the parameter combo
      sim <- gmse(traj_pred = m, mem_prv_observ = m, 
                  time_max = ts, land_ownership = TRUE, land_dim_1 = ldim, land_dim_2 = ldim,
                  RESOURCE_ini = popinit, res_birth_type = 0, res_death_type = 0, 
                  consume_surv = surv, consume_repr = repr, times_feeding = tf, res_consume = cons,
                  stakeholders = stkh, scaring = FALSE, manager_budget = bdgt, user_budget = bdgt, 
                  manager_sense = 0.15, manage_target = trgt, observe_type = obstype, res_move_obs = FALSE,
                  plotting = FALSE)
      
      # Store the last time step number (for extinction-related bugs)
      final_ts <- length(which(sim$paras[,1] != 0))
      
      # Pick up values for simulation res and store them in res
      
      # Replicate number
      res[k,2] <- k
      
      # Prediction?
      res[k,1] <- as.numeric(m)
      
      # Has extinction occured? (yes = 1, no = 0)
      res[k,3] <- ifelse(final_ts < dim(sim$paras)[1], 1, 0)
      
      # Resource actual pop deviation from target at ts before last
      res[k,4] <- dim(sim$resource[[final_ts-1]])[1]/sim$action[[1]][1,5,1] - 1
     
      # Users total final yield at ts before last
      res[k,6] <- sum(sim$agents[[final_ts-1]][,16])/40000
      
      # Maximum difference between Users yield at ts before last
      res[k,8] <- (max(sim$agents[[final_ts-1]][,16]) - min(sim$agents[[final_ts-1]][-1,16]))/max(sim$agents[[final_ts-1]][,16])
      
      # final ts
      res[k,11] <- final_ts
      
      ## save costs, actions and population in flw structures
      if (k %% freq == 0) {
        print("Saving budget, costs, actions, actual and observed pop")
        para <- res[k,1:3]
        para <- c(para, trgt, popinit)
        
        bdg <- rep(0, ts)
        pop <- rep(0, ts)
        cos <- rep(0, ts)
        act <- rep(0, ts)
        
        for (t in 1:final_ts) {
          bdg[t] <- sim$paras[t,132]
          pop[t] <- dim(sim$resource[[t]])[1]
          cos[t] <- sim$cost[[t]][1,9,2]
          act[t] <- mean(sim$action[[t]][1,9,2:stkh])
        }
        
        # Append the files now
        write.table(t(c(para, bdg)), file = bgt_file, append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE); 
        write.table(t(c(para, pop)), file = pop_file, append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE); 
        write.table(t(c(para, cos)), file = cos_file, append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE); 
        write.table(t(c(para, act)), file = act_file, append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE); 
      }
      
      # Next measures involve calculus that can be disturbed if extinction occured
      
      # If exctinction occured
      if (res[k,3] != 0) {
        
        # Resource actual pop deviation from target at last ts
        res[k,5] <- -1
        
        # Users total final yield at last ts
        res[k,7] <- 1
        
        # Maximum difference between Users yield at last ts
        res[k,9] <- 0
        
        # Sum of absolute deviation from target along time steps
        ssd <- 0
        for (i in 1:(final_ts-1)){
            ssd <- ssd + abs(dim(sim$resource[[i]])[1]-sim$paras[i,7])
        }
        res[k,10] <- ssd + abs(-1)
      }
      
      # If extinction did not occured
      else {    
          # Resource actual pop deviation from target at last ts
          res[k,5] <- dim(sim$resource[[final_ts]])[1]/sim$action[[1]][1,5,1] - 1
          
          # Users total final yield at last ts
          res[k,7] <- sum(sim$agents[[final_ts]][,16])/40000
          
          # Maximum difference between Users yield at last ts
          res[k,9] <- (max(sim$agents[[final_ts]][,16]) - min(sim$agents[[final_ts]][-1,16]))/max(sim$agents[[final_ts]][,16])
          
          # Sum of absolute deviation from target along time steps
          ssd <- 0
          for (i in 1:final_ts){
              ssd <- ssd + abs(dim(sim$resource[[i]])[1]-sim$paras[i,7])
          }
          res[k,10] <- ssd
      } 
      
      gc(); # Run a garbage collect here
      
    } # end rep for loop
    
    # save res table
    results <- rbind(results, res)
    
  } # end m for loop
  
  colnames(results) <- columns   
    
  write.table(x = results[,], file = out_file, append = TRUE, sep = "\t", row.names = FALSE);
  
  # save the paras vector once to double check parameter values
  write.table(sim$paras[1,], file = paste(out_head, "-paras", ".csv", sep = ""), append = TRUE, sep = "\t", row.names = FALSE, col.names = FALSE);
  
  gc(); # Run a garbage collect here
  
  # Simulation time
  end <- Sys.time()
  
  print(paste("Batch started", start, "and ended", end, sep = " "))
  
  return(results);
  
} # end function
