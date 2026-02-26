################################################################################
#                                                                              #
#                              survival analysis                               #
#                                                                              #
################################################################################


# _______________________________________________                           ####                      
#
# Packages
# _______________________________________________                           ####
library(survival)
library(ggpubr)
library(survminer)
library(ggsurvfit)
library(caret)
library(glmnet)
library(pec)
library(vip)
library(missForest)
library(broom)
library(randomForestSRC)
library(Hmisc)


# _______________________________________________                           ####                      
#
# import data
# _______________________________________________                           ####

setwd("C:/Users/User/OneDrive/Uni/Master/5. Semester/Masterarbeit/Auswertung")
data <- get(load("data/data.Rdata"))


#### Kaplan-Meier Survival curve ####
Kap_curve <- survfit(Surv(time, status) ~ 1,
                     data = data)
ggsurvplot(Kap_curve, data = data, conf.int = FALSE)
ggsave("./cox/Kap_curve.pdf", device = "pdf")

summary(survfit(Surv(time, status) ~ 1, data = data), times = 21)
# prob of a couple staying together past wave 21 is 74.3 % (depends on times)


# _______________________________________________                           ####                      
#
# regularized Cox Regression 
# _______________________________________________                           ####

data_preds <- data %>% select(-status, -time)
data_out <- data %>% select(status, time)
res_cox <- list()
res_rsf <- list()

### outer loop
iter = 20 
for (i in 1:iter) { # Outer Loop with i = iterations      
  print(paste0(i, ". Iteration"))
  
  set.seed(20240203+i) 
  trainIndex <- createDataPartition(data$time, # randomly partitioning the data in train and test
                                    p = .7, # 70 % train
                                    list = FALSE,
                                    times = 1) 
  ### data splitting
  train_preds_df <- (data_preds[ trainIndex, ]) 
  train_out_df   <- (data_out[ trainIndex, ])
  test_preds_df  <- (data_preds[-trainIndex, ]) 
  test_out_df    <- (data_out[-trainIndex, ])
  
  ### imputation of missing values
  train_imp <- missForest(train_preds_df)
  train_preds_df <- train_imp$ximp
  
  test_imp <- missForest(test_preds_df)
  test_preds_df <- test_imp$ximp
  
  ### scale numeric variables
  train_preds_df <- train_preds_df %>% mutate(across(where(is.numeric), scale))
  test_preds_df <- test_preds_df %>% mutate(across(where(is.numeric), scale))
  
  ### creating data frames and matrices
  data_train <- data.frame(cbind(train_preds_df, train_out_df))
  data_test <- data.frame(cbind(test_preds_df, test_out_df))
  
  train_preds_mx <- model.matrix(~ . , data = train_preds_df)[, -1] # deleting reference category
  test_preds_mx  <- model.matrix(~ . , data = test_preds_df)[, -1]
  
  ### creating outcome
  train_surv <- Surv(train_out_df$time, train_out_df$status)
  test_surv <- Surv(test_out_df$time, test_out_df$status)
  
  ### hyperparameter tuning (alpha)
  alphas <- seq(0, 1, by = 0.1)
  cv_results <- lapply(alphas, function(a) {
    cv.glmnet(train_preds_mx, train_surv,
              family = "cox", 
              alpha = a, 
              nfolds = 5,
              type.measure = "C")})
  
  # choose alpha with highest max C-index
  max_C <- sapply(cv_results, function(mod) max(mod$cvm))
  best_alpha <- alphas[which.max(max_C)]
  
  ### model training
  cox <- cv.glmnet(train_preds_mx, train_surv,
                      family = "cox",
                      type.measure = "C",
                      nfolds = 5,    
                      alpha = best_alpha) 
  
  
  ### coefficients (numeric variables are scaled)
  coefList <- coef(cox, s = "lambda.1se") 
  coefList <- data.frame(coefList@Dimnames[[1]][coefList@i+1],coefList@x)
  names(coefList) <- c("variable", "value")
  coefList <- coefList %>% arrange(-abs(value))
  print(coefList)
  png(paste0("./cox/VIP_iter", i, ".png"))
  vip(cox, num_features = 20) 
  dev.off()
  
  
  ### C-Index
  # train
  C_train_cox <- Cindex(predict(cox,
                       newx = train_preds_mx,
                       s = cox$lambda.1se,                   
                       type = "link"), train_surv)
  # test
  C_test_cox <- Cindex(predict(cox,
                       newx = test_preds_mx,
                       s = cox$lambda.1se,
                       type = "link"), test_surv)
  
  ### Brier Score 
  beta.1se <- coef(cox, s = cox$lambda.1se) 
  vars.1se <- rownames(beta.1se)[as.numeric(beta.1se)!=0]
  vars.1se <- unique(sub("[0-9]+$", "", vars.1se))  # getting original names (not dummies)
  
  fm.1se <- as.formula(paste0("Surv(time,status)~",
                              paste0(vars.1se,collapse="+")))
  
  fit.1se <- coxph(fm.1se, data = data_train, x = TRUE, y = TRUE)
  fit.1se$coefficients[is.na(fit.1se$coefficients)] <- 0
  
  # train
  fit_pec_train <- pec(
    object=list("cox.1se" = fit.1se), 
    data = data_train, 
    formula = Surv(time, status) ~ 1, 
    splitMethod = "none")
  
  # test
  fit_pec_test <- pec(
    object=list("cox.1se" = fit.1se), 
    data = data_test, 
    formula = Surv(time, status) ~ 1, 
    splitMethod = "none")
  
  
  ### integrated Brier Score
  # in fit_pec_train and fit_pec_test
  
  ### Forest plot
  ggforest_custom <- function(model, data, fontsize = 1, main = NULL) {
    stopifnot(inherits(model, "coxph"))
    
    # coefficients and CI
    coef_df <- broom::tidy(model, conf.int = TRUE)
    coef_df$var <- gsub("`", "", coef_df$term)
  
    coef_df <- coef_df[nrow(coef_df):1, ]
    coef_df$y <- seq_len(nrow(coef_df))
    
    # hazard ratios
    coef_df$hr_label <- paste0(
      sprintf("%.2f", exp(coef_df$estimate)),  # HR
      " (",
      sprintf("%.2f", exp(coef_df$conf.low)), "-", sprintf("%.2f", exp(coef_df$conf.high)),
      ")")
    
    # plot
    ggplot(coef_df, aes(y = y, x = exp(estimate))) +
      geom_point(size = 3) +
      geom_errorbarh(aes(xmin = exp(conf.low), xmax = exp(conf.high)), height = 0.3) +
      geom_vline(xintercept = 1, linetype = 3) +
      scale_x_log10() +
      scale_y_continuous(
        breaks = coef_df$y,
        labels = paste0(coef_df$var, " (", coef_df$hr_label, ")")  # variable name + HR(CI)
      ) +
      theme_light(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.title.y = element_blank()
      ) +
      xlab("Hazard Ratio") +
      ggtitle(main)}
  
  ggforest_custom(model = fit.1se, data = data_test, fontsize = 1)
  
  ggsave(
    paste0("./cox/ggforest_iteration_", i, ".pdf"), 
    device = "pdf", 
    width = 21, 
    height = 4 + (length(fit.1se[["coefficients"]]) * 0.8), 
    units = "cm",
    limitsize = FALSE)
  
  
  ### save results
  res_cox[[i]] <- list("best_alpha" = best_alpha, 
                       "cox" = cox, 
                       "coefficients" = coefList, 
                       "C_index_train" = C_train_cox, 
                       "C_index_test" = C_test_cox, 
                       "fit_pec_train" = fit_pec_train, 
                       "fit_pec_test" = fit_pec_test)
                                    
# _______________________________________________                           ####                      
#
# random survival forest
# _______________________________________________                           ####
  
  set.seed(20240203+i) 
  
  ### data splitting
  train_data <- (data[ trainIndex, ]) 
  test_data  <- (data[-trainIndex, ]) 
  
  
  ### model training

  # hyperparameter tuning
  tuned <- tune.rfsrc(Surv(time, status) ~ ., 
                      data = train_data,
                      mtry.start = sqrt(ncol(train_data)),
                      nodesize.try = c(1:9, seq(10, 100, by = 5)), ntree.try = 100,
                      sampsize = function(x) { min(x * .632, max(150, x^(3/4))) },
                      nsplit = 1, step.factor = 1.25, improve = 1e-3, strikeout = 3, max.iter = 25,
                      method = "grid")
  
  # applying optimal hyperparameters
  rsf <- rfsrc(Surv(time, status) ~ .,
               data = train_data,
               mtry = tuned$optimal["mtry"],
               nodesize = tuned$optimal["nodesize"],
               splitrule = "logrank",
               importance = TRUE,
               na.action = "na.impute")
  
  print(rsf)

  ### model testing
  pred_test <- predict(rsf, test_data)

  ### C Index
  # train
  C_train_rsf <- get.cindex(rsf$yvar[,1], rsf$yvar[,2], rsf$predicted)
  
  # test
  C_test_rsf <- get.cindex(pred_test$yvar[,1], pred_test$yvar[,2], pred_test$predicted)
  
  
  ### Brier Score
  # train
  brier_train_rsf <- get.brier.survival(rsf, cens.mode = "rfsrc")$brier.score  
  brier_train_rsf_ref <- get.brier.survival(rsf, cens.mode = "km")$brier.score
 
  # test
  brier_test_rsf <- get.brier.survival(pred_test, cens.mode = "rfsrc")$brier.score 
  brier_test_rsf_ref <- get.brier.survival(pred_test, cens.mode = "km")$brier.score
  
  
  ### Integrated Brier Score
  trapz <- randomForestSRC:::trapz
 
   # train
  time_train_rsf <- brier_train_rsf[, "time"]
  bs_train_rsf <- brier_train_rsf[, "brier.score"]     
  ibs_train_rsf <- trapz(time_train_rsf, bs_train_rsf) / 
    (max(time_train_rsf) - min(time_train_rsf))
  
  time_train_rsf_ref <- brier_train_rsf_ref[, "time"]
  bs_train_rsf_ref <- brier_train_rsf_ref[, "brier.score"]     
  ibs_train_rsf_ref <- trapz(time_train_rsf_ref, bs_train_rsf_ref) / 
    (max(time_train_rsf_ref) - min(time_train_rsf_ref))
  
  # test
  time_test_rsf <- brier_test_rsf[, "time"]
  bs_test_rsf <- brier_test_rsf[, "brier.score"]     
  ibs_test_rsf <- trapz(time_test_rsf, bs_test_rsf) / 
    (max(time_test_rsf) - min(time_test_rsf))
  
  time_test_rsf_ref <- brier_test_rsf_ref[, "time"]
  bs_test_rsf_ref <- brier_test_rsf_ref[, "brier.score"]     
  ibs_test_rsf_ref <- trapz(time_test_rsf_ref, bs_test_rsf_ref) / 
    (max(time_test_rsf_ref) - min(time_test_rsf_ref))
  
  
  ### Variable Importance
  vi <- rsf$importance
  
  ### save results
  res_rsf[[i]] <- list("rsf" = rsf, 
                       "C_index_train" = C_train_rsf, 
                       "C_index_test" = C_test_rsf, 
                       "brier_train" = brier_train_rsf, 
                       "brier_train_ref" = brier_train_rsf_ref,
                       "brier_test" = brier_test_rsf, 
                       "brier_test_ref" = brier_test_rsf_ref,
                       "ibs_train" = ibs_train_rsf, 
                       "ibs_train_ref" = ibs_train_rsf_ref,
                       "ibs_test" = ibs_test_rsf, 
                       "ibs_test_ref" = ibs_test_rsf_ref,
                       "hyperparameters" = tuned$optimal,
                       "variable_importance" = vi)
}


#_______________________________________________                           ####                      
#
# mean brier score
# _______________________________________________                           ####

#### Cox regression ####
max_times <- max(data$time)
meanBrierScore_cox <- function(r) {
  result <- tibble(
    time = numeric(),
    test = numeric(),
    train = numeric(),
    ref = numeric()
  )
  probabilities_test <- vector(mode='list', length = max_times + 1)
  probabilities_train <- vector(mode='list', length = max_times + 1)
  probabilities_ref <- vector(mode='list', length = max_times + 1)
  
  for(loop_result in r) {
    for(i in 1:length(loop_result[["fit_pec_test"]][["time"]])) {
      time <-  as.integer(loop_result[["fit_pec_test"]][["time"]][i]) + 1
      probabilities_test[[time]] <- append(
        probabilities_test[[time]],
        loop_result[["fit_pec_test"]][["AppErr"]][["cox.1se"]][i]
      )
      probabilities_ref[[time]] <- append(
        probabilities_ref[[time]],
        loop_result[["fit_pec_test"]][["AppErr"]][["Reference"]][i]
      )
    }
    for(i in 1:length(loop_result[["fit_pec_train"]][["time"]])) {
      time <-  as.integer(loop_result[["fit_pec_train"]][["time"]][i]) + 1
      probabilities_train[[time]] <- append(
        probabilities_train[[time]],
        loop_result[["fit_pec_train"]][["AppErr"]][["cox.1se"]][i]
      )
    }
  }
  for(i in 1:(max_times + 1)) {
    result <- result %>% add_row(
      time = (i - 1), 
      test = mean(probabilities_test[[i]]), 
      train = mean(probabilities_train[[i]]), 
      ref = mean(probabilities_ref[[i]])
    )
  }
  return(result)
}

# Plot
ggplot(data = meanBrierScore_cox(res_cox), aes(x = time)) + 
  geom_line(aes(y = ref, color = "Reference model"), size = 1) +
  geom_line(aes(y = train, color = "Train model"), size = 1) +
  geom_line(aes(y = test, color = "Test model"), size = 1) +
  
  scale_color_manual(values = c(
    "Reference model" = "black",
    "Train model" = "darkorchid3",
    "Test model" = "darkolivegreen4" )) +
  
  scale_y_continuous(limits = c(0,0.4)) +
  labs(y = "Brier Score", x = "Time", color = "") +
  theme_classic() + theme(legend.position = "right")


#### random survival forest ####
meanBrierScore_rsf <- function(r) {
  result <- tibble(
    time = numeric(),
    test = numeric(),
    train = numeric(),
    ref = numeric()
  )
  prob_test  <- vector(mode ='list', length = max_times + 1)
  prob_train <- vector(mode ='list', length = max_times + 1)
  prob_ref   <- vector(mode ='list', length = max_times + 1)
  
  for (loop_result in r) {
    
    for(i in 1:length(loop_result[["brier_test"]][["time"]])) {
      time <-  as.integer(loop_result[["brier_test"]][["time"]][i]) + 1
      prob_test[[time]] <- append(
        prob_test[[time]],
        loop_result[["brier_test"]][["brier.score"]][i]
      )
      prob_ref[[time]] <- append(
        prob_ref[[time]],
        loop_result[["brier_test_ref"]][["brier.score"]][i]
      )
    }
    for(i in 1:length(loop_result[["brier_train"]][["time"]])) {
      time <-  as.integer(loop_result[["brier_train"]][["time"]][i]) + 1
      prob_train[[time]] <- append(
        prob_train[[time]],
        loop_result[["brier_train"]][["brier.score"]][i]
      )
    }
  }
  for(i in 1:(max_times + 1)) {
    result <- result %>% add_row(
      time = (i - 1), 
      test = mean(prob_test[[i]]), 
      train = mean(prob_train[[i]]), 
      ref = mean(prob_ref[[i]])
    )
  }
  return(result)
}

# Plot
ggplot(data = meanBrierScore_rsf(res_rsf), aes(x = time)) + 
  geom_line(aes(y = ref, color = "Reference model"), size = 1) +
  geom_line(aes(y = train, color = "Train model"), size = 1) +
  geom_line(aes(y = test, color = "Test model"), size = 1) +
  
  scale_color_manual(values = c(
    "Reference model" = "black",
    "Train model" = "darkorchid3",
    "Test model" = "darkolivegreen4" )) +
  
  scale_y_continuous(limits = c(0,0.4)) +
  labs(y = "Brier Score", x = "Time", color = "") +
  
  theme_classic() + theme(legend.position = "right")
  


#_______________________________________________                            ####                      
#
# mean C-Index, IBS and hyperparameter 
# _______________________________________________                           ####       (IBS is called CRPS in text)

#### Cox Regression ####
cox_perf <- NULL
cox_hyper <- NULL

# C-Indices train
C_indices_train_cox <- data.frame("Cindex" = rep(NA_real_, iter))
# C-Indices test
C_indices_test_cox <- data.frame("Cindex" = rep(NA_real_, iter))


# IBS train cox.1se model
ibs_train_cox <- data.frame("IBS" = rep(NA_real_, iter))
# IBS test cox.1se model
ibs_test_cox <- data.frame("IBS" = rep(NA_real_, iter))

# IBS train Reference model 
ibs_ref_train_cox <- data.frame("IBS" = rep(NA_real_, iter))
# IBS test Reference model
ibs_ref_test_cox <- data.frame("IBS" = rep(NA_real_, iter))

for(l in 1:iter){
  C_indices_train_cox[l,] <- res_cox[[l]]$C_index_train
  C_indices_test_cox[l,] <- res_cox[[l]]$C_index_test
  ibs_train_cox[l,] <- crps(res_cox[[l]]$fit_pec_train)["cox.1se",]
  ibs_test_cox[l,] <- crps(res_cox[[l]]$fit_pec_test)["cox.1se",]
  ibs_ref_train_cox[l,] <- crps(res_cox[[l]]$fit_pec_train)["Reference",]
  ibs_ref_test_cox[l,] <- crps(res_cox[[l]]$fit_pec_test)["Reference",]
  
  ### data frame with mean performance metrics
  cox_perf <- data.frame("mean_C_Index_train" = mean(C_indices_train_cox$Cindex),
                         "sd_C_Index_train" = sd(C_indices_train_cox$Cindex),
                         "mean_C_Index_test" = mean(C_indices_test_cox$Cindex),
                         "sd_C_Index_test" = sd(C_indices_test_cox$Cindex),
                         "mean_IBS_train" = mean(ibs_train_cox$IBS),
                         "sd_IBS_train" = sd(ibs_train_cox$IBS),
                         "mean_IBS_test" = mean(ibs_test_cox$IBS),
                         "sd_IBS_test" = sd(ibs_test_cox$IBS),
                         "mean_IBS_ref_train" = mean(ibs_ref_train_cox$IBS),
                         "sd_IBS_ref_train" = sd(ibs_ref_train_cox$IBS),
                         "mean_IBS_ref_test" = mean(ibs_ref_test_cox$IBS),
                         "sd_IBS_ref_test" = sd(ibs_ref_test_cox$IBS))
  
  ###  hyperparameters
  if(l == 1){
    cox_hyper <- data.frame("alpha" = rep(NA_real_, iter),
                            "lambda.min" = rep(NA_real_, iter),
                            "lambda.1se" = rep(NA_real_, iter),
                            "iter" = rep(NA_real_, iter))}
  cox_hyper$alpha[l] <- res_cox[[l]]$best_alpha
  cox_hyper$lambda.min[l] <- res_cox[[l]]$cox$lambda.min
  cox_hyper$lambda.1se[l] <- res_cox[[l]]$cox$lambda.1se
  cox_hyper$iter[l] <- l
  }


#### random survival forest ####
rsf_perf <- NULL
rsf_hyper <- NULL

# C-Indices train
C_indices_train_rsf <- data.frame("Cindex" = rep(NA_real_, iter))
# C-Indices test
C_indices_test_rsf <- data.frame("Cindex" = rep(NA_real_, iter))

# IBS train RSF model
ibs_train_rsf <- data.frame("IBS" = rep(NA_real_, iter))
# IBS test RSF model
ibs_test_rsf <- data.frame("IBS" = rep(NA_real_, iter))

# IBS train Reference model
ibs_ref_train_rsf <- data.frame("IBS" = rep(NA_real_, iter))
# IBS test Reference model
ibs_ref_test_rsf <- data.frame("IBS" = rep(NA_real_, iter))

for(l in 1:iter){
  C_indices_train_rsf[l,] <- res_rsf[[l]]$C_index_train
  C_indices_train_rsf[l,] <- 1 - C_indices_train_rsf[l,]
  C_indices_test_rsf[l,] <- res_rsf[[l]]$C_index_test
  C_indices_test_rsf[l,] <- 1 - C_indices_test_rsf[l,]
  ibs_train_rsf[l,] <- res_rsf[[l]]$ibs_train
  ibs_test_rsf[l,] <- res_rsf[[l]]$ibs_test
  ibs_ref_train_rsf[l,] <- res_rsf[[l]]$ibs_train_ref
  ibs_ref_test_rsf[l,] <- res_rsf[[l]]$ibs_test_ref
  
  # data frame with mean performance metrics
  rsf_perf <- data.frame("mean_C_Index_train" = mean(C_indices_train_rsf$Cindex),
                         "sd_C_Index_train" = sd(C_indices_train_rsf$Cindex),
                         "mean_C_Index_test" = mean(C_indices_test_rsf$Cindex),
                         "sd_C_Index_test" = sd(C_indices_test_rsf$Cindex),
                         "mean_IBS_train" = mean(ibs_train_rsf$IBS),
                         "sd_IBS_train" = sd(ibs_train_rsf$IBS),
                         "mean_IBS_test" = mean(ibs_test_rsf$IBS),
                         "sd_IBS_test" = sd(ibs_test_rsf$IBS),
                         "mean_IBS_ref_train" = mean(ibs_ref_train_rsf$IBS),
                         "sd_IBS_ref_train" = sd(ibs_ref_train_rsf$IBS),
                         "mean_IBS_ref_test" = mean(ibs_ref_test_rsf$IBS),
                         "sd_IBS_ref_test" = sd(ibs_ref_test_rsf$IBS))
  
  # hyperparameters
  if(l == 1){
    rsf_hyper <- data.frame("mtry" = rep(NA_real_, iter),
                            "nodesize" = rep(NA_real_, iter),
                            "iter"= rep(NA_real_, iter))}
  rsf_hyper$mtry[l] <- res_rsf[[l]]$hyperparameters["mtry"]
  rsf_hyper$nodesize[l] <- res_rsf[[l]]$hyperparameters["nodesize"]
  rsf_hyper$iter[l] <- l
  }


#_______________________________________________                            ####                      
#
# mean coefficients (Cox Regression)
# _______________________________________________                           ####
# all coefficients
coef_cox <- data.frame()
for(l in 1:iter){
  coef_list <- res_cox[[l]]$coefficients
  coef_list$iter <- l
  coef_cox <- rbind(coef_cox, coef_list)
}

# mean of coefficients
n_iter <- iter
coef_mean <- coef_cox %>%
  group_by(variable) %>%
  summarise(
    mean_beta = mean(c(value, rep(0, n_iter - n()))), # variable not selected in iteration -> 0
    sd_beta = sd(value),
    n_selected = n(),
    conf.low = quantile(c(value, rep(0, n_iter - n())), 0.025), 
    conf.up  = quantile(c(value, rep(0, n_iter - n())), 0.975),
    .groups = "drop")

# selection frequence of coefficients
coef_mean$selection_freq <- coef_mean$n_selected / iter

# top 20 coefficients
top20 <- coef_mean %>%
  arrange(desc(selection_freq)) %>%
  slice(1:20) %>%
  arrange(mean_beta) %>%
  mutate(y = row_number())

# renaming variables for plot
top20 <- top20 %>%
  mutate(variable = recode(variable,
                           "alsrelsp_f" = "satisfaction with partner (w)",
                           "amrcdur" = "duration of marriage",
                           "alsrelsp_m" = "satisfaction with partner (m)",
                           "aehtjb_m" = "years in paid work (m)",
                           "alosatfs_f" = "satisfaction with financial situation (w)",
                           "apmchb_f2" = "no premarital childbirth (w)",
                           "apmchb_m2" = "no premarital childbirth (m)",
                           "alosatfs_m" = "satisfaction with financial situation (m)",
                           "atcyng_m" = "age of youngest child (m)",
                           "alshrchd_f" = "hours per week playing with own children (w)",
                           "ahifnisi" = "non-income support payment",
                           "ahhed10x2" = "second decile of education",
                           "alsdrink_f3" = "rare alcohol consumption (w)",
                           "alssmoke_m3" = "smoking (m)",
                           "afiprosp_m4" = "just getting along financially (m)",
                           "ahgyob_m" = "year of birth (m)",
                           "aordfnum_m" = "number of relationships (m)",
                           "alssmoke_f3" = "smoking (w)",
                           "ahgyob_f" = "year of birth (w)",
                           "aordfnum_f" = "number of relationships (w)"))

# labels
max_var_len <- max(nchar(top20$variable))

top20 <- top20 %>%
  mutate(
    var_pad = str_pad(variable, width = max_var_len, side = "right"),  
    mean_beta_txt = formatC(exp(mean_beta), format = "f", digits = 3),  # HR
    conf_txt = paste0("(", formatC(exp(conf.low), format = "f", digits = 3), 
                      ", ", formatC(exp(conf.up), format = "f", digits = 3), ")"),
    label = paste0(var_pad, "   ", mean_beta_txt, " ", conf_txt)  
  )

# Forestplot
ggplot(top20, aes(x = exp(mean_beta), y = y)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 1, linetype = 3) +
  geom_errorbarh(aes(xmin = exp(conf.low), xmax = exp(conf.up)), height = 0.3) +
  scale_x_log10() + 
  scale_y_continuous(
    breaks = top20$y,
    labels = top20$label
    ) +
  theme_classic(base_size = 12) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(family = "mono", hjust = 0), 
    panel.grid = element_blank()
  ) +
  xlab("Hazard Ratio") +
  ylab("")

ggsave(
  paste0("./cox/ggforest_mean", ".pdf"), 
  device = "pdf", 
  width = 30, 
  height = 4 + (length(fit.1se[["coefficients"]]) * 0.8), 
  units = "cm",
  limitsize = FALSE)


#_______________________________________________                            ####                      
#
# mean VIP (Random Survival Forest)
# _______________________________________________                           ####

vi_rsf <- data.frame()

for(l in 1:iter){
  vi_vec <- res_rsf[[l]]$variable_importance
  tmp <- data.frame(
    variable = names(vi_vec),
    importance = as.numeric(vi_vec),
    iter = l
  )
  vi_rsf <- rbind(vi_rsf, tmp)
}

vi_mean <- vi_rsf %>%
  group_by(variable) %>%
  summarise(
    mean_importance = mean(importance, na.rm = TRUE),
    sd_importance = sd(importance, na.rm = TRUE),
    n_selected = sum(!is.na(importance) & importance > 0),
    .groups = "drop"
  )

vi_mean$selection_freq <- vi_mean$n_selected / iter

top20_vi <- vi_mean %>%
  arrange(desc(selection_freq)) %>%
  slice(1:20)

top20_vi <- top20_vi %>%
  arrange(mean_importance)

# renaming variables for plot
top20_vi <- top20_vi %>%
  mutate(variable = recode(variable,
                           "afmfo61_m" = "father's occupation (m)",  
                           "aghre_f" = "emotional health (w)",    
                           "afmpdiv_m" = "parental divorce (m)",  
                           "aatwkadc_m" = "gender roles 1 (m)",
                           "aghpf_f" = "physical functioning (w)",   
                           "aatwkmrl_m" = "gender roles (3) (m)",
                           "aatwkwrl_f" = "gender roles (4) (w)",
                           "aghsf_m" = "social functioning (m)", 
                           "aatwkseh_f" = "gender roles 2 (w)",
                           "aghgh_f" = "general health (w)",   
                           "aehtjb_f" = "years in paid work (w)",  
                           "afmsib_m" = "number of siblings (m)",  
                           "afmfemp_f" = "father employed at age 14 (w)",  
                           "aghmh_m"  = "mental health (m)", 
                           "aehtuj_m" = "years unemployed (m)" ,  
                           "afmsib_f" = "number of siblings (w)",  
                           "aedagels_m" = "age left school (m)",
                           "aedagels_f" = "age left school (w)",
                           "aehtuj_f" = "years unemployed (w)",  
                           "aehtjb_m" = "years in paid work (m)"
                           ))

importance_vals <- top20_vi$mean_importance
names(importance_vals) <- top20_vi$variable

png("./rsf/VI.png", width = 1200, height = 800)

par(oma = c(0.5, 10, 0.5, 0.5))
par(cex.axis = 2.0, cex.lab = 2.0, cex.main = 2.0,
    mar = c(6.0,17,1,1), mgp = c(4, 1, 0))

barplot(
  importance_vals,
  horiz = TRUE,
  las = 1,
  xlab = "Variable Importance",
  cex.names = 1.2
)

dev.off()

