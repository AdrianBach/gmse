library(GMSE)

# repo
repo <- "~/Desktop/PhD/GitKraken/TrajectoryPrediction-test/"

# case parameters
{rep = 10
ts = 20
ldim = 200
stkh = 40
popinit = 1000
bdgt = 1000
trgt = 2000
repr = 5
tf = 12
cons = 0.5
surv = 4.75
obs = 3
}

regular_perf <- gmse_replicates(replicates = rep,
                          time_max = ts, mem_prv_observ = FALSE, traj_pred = FALSE,
                          RESOURCE_ini = popinit, res_birth_type = 0, res_death_type = 0, 
                          consume_surv = surv, consume_repr = repr, times_feeding = tf, res_consume = cons,
                          land_ownership = TRUE, land_dim_1 = ldim, land_dim_2 = ldim,
                          stakeholders = stkh, scaring = FALSE, manager_budget = bdgt, user_budget = bdgt, 
                          manager_sense = 0.15, manage_target = trgt,
                          observe_type = obs, res_move_obs = FALSE,
                          plotting = FALSE)
# regular <- gmse(time_max = 5, mem_prv_observ = FALSE, traj_pred = FALSE, plotting = FALSE, land_ownership = TRUE, stakeholders = 5)
write.csv(regular_perf, file = paste(repo,"regular-perf.csv", sep = ""), row.names = FALSE)

pred_pref <- gmse_replicates(replicates = rep,
                        time_max = ts, mem_prv_observ = TRUE, traj_pred = TRUE,
                        RESOURCE_ini = popinit, res_birth_type = 0, res_death_type = 0, 
                        consume_surv = surv, consume_repr = repr, times_feeding = tf, res_consume = cons,
                        land_ownership = TRUE, land_dim_1 = ldim, land_dim_2 = ldim,
                        stakeholders = stkh, scaring = FALSE, manager_budget = bdgt, user_budget = bdgt, 
                        manager_sense = 0.15, manage_target = trgt,
                        observe_type = obs, res_move_obs = FALSE,
                        plotting = FALSE)
# pred <- gmse(time_max = 5, mem_prv_observ = TRUE, traj_pred = TRUE, plotting = FALSE, land_ownership = TRUE, stakeholders = 5, scaring = FALSE)
write.csv(pred_perf, file = paste(repo,"pred-perf.csv", sep = ""), row.names = FALSE)

ext_reg <- length(which(regular[,3] == 0))/dim(regular)[1]
ext_prd <- length(which(pred[,3] == 0))/dim(pred)[1]

sim <- gmse(time_max = ts, mem_prv_observ = FALSE, traj_pred = FALSE,
            RESOURCE_ini = popinit, res_birth_type = 0, res_death_type = 0, 
            consume_surv = surv, consume_repr = repr, times_feeding = tf, res_consume = cons,
            land_ownership = TRUE, land_dim_1 = ldim, land_dim_2 = ldim,
            stakeholders = stkh, scaring = FALSE, manager_budget = bdgt, user_budget = bdgt, 
            manager_sense = 0.15, manage_target = trgt,
            observe_type = obs, res_move_obs = FALSE,
            plotting = FALSE)

sim_prd <- gmse(time_max = ts, mem_prv_observ = TRUE, traj_pred = TRUE,
            RESOURCE_ini = popinit, res_birth_type = 0, res_death_type = 0, 
            consume_surv = surv, consume_repr = repr, times_feeding = tf, res_consume = cons,
            land_ownership = TRUE, land_dim_1 = ldim, land_dim_2 = ldim,
            stakeholders = stkh, scaring = FALSE, manager_budget = bdgt, user_budget = bdgt, 
            manager_sense = 0.15, manage_target = trgt,
            observe_type = obs, res_move_obs = FALSE,
            plotting = FALSE)

plot_gmse_results(sim)
plot_gmse_results(sim_prd)
