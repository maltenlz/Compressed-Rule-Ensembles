#' Learn a compressed rule ensemble
#'
#' @param x input matrix
#' @param y outcome vector
#' @param treetype foresttype to generate the rules. Currently XGBoost and RandomForest are supported
#' @param standardize Should the rules be standardized? This essentiall removes the extra penalty on low support/high complexity rules. Can be sometimes beneficial.
#' @param linear Boolean if linear terms should be included
#' @param alpha alpha parameter in glmnet, alpha = 1 for lasso alpha = 0 for ridge.
#' @param task "regression" or "class"
#' @param forest_control parameters that are passed to xgboost
#' @param task "regression" or "class" for (binary) classification
#' @return list containing the cre model
#' @export
#' @importFrom glmnet glmnet
#' @importFrom stats sd
#' @import dplyr

cre = function(x,
               y,
               k = 4,
               treetype = "XGB",
               standardize = F,
               linear = T,
               alpha = 1,
               task = "regression",
               min_sup = 0.05,
               forest_control = NULL,
               model_type = "glmnet",
               eta = 0.5){

  if(task == "class"){
    y = as.numeric(as.factor(y))-1
  }
  rules            = genrulesXGB(x = x,
                                 y = y,
                                 forest_control = forest_control,
                                 task = task
  )
  rules_frame      = cluster_rules(rules, k = k)
  sd_y             = stats::sd(y)
  mu_y             = mean(y)
  Xr               = transformX(x = x, rules_frame)
  delete           = delete_duplicates(x = Xr, rules_frame)


  mu_lin           = apply(x, 2, mean)
  sd_lin           = apply(x, 2, stats::sd)
  mu_x             = apply(Xr, 2, mean)
  sd_x             = apply(Xr, 2, stats::sd)

  rule_depth       = rules_frame$ensemble_rules %>% dplyr::group_by(ensemble_rule) %>% dplyr::summarise(depth = dplyr::n())
  delete           = unique(append(delete, which(mu_x < min_sup | 1-mu_x < min_sup)))

  Xr               = t(apply(Xr[,-delete],1,function(x)x/rule_depth$depth[-delete]^eta))


  for(p in 1:ncol(x)){
    x[,p] = (x[,p]-mu_lin[p])/sd_lin[p]
  }

  if(model_type == "glmnet"){
    if(linear ==  T){
      Xr = cbind(x*0.4, Xr)
    }
    outer_model          = glmnet::cv.glmnet(as.matrix(Xr),
                                     y,
                                     alpha = 1,
                                     standardize = standardize,
                                     family = ifelse(task == "class",
                                                     "binomial",
                                                     "gaussian"),
                                     intercept = T)
  }
  out = list(rules         = rules_frame,
             p_lin         = ncol(x),
             outer_model   = outer_model,
             deleted       = delete,
             task          = task,
             linear        = linear,
             var_names     = colnames(x),
             mat_names     = colnames(Xr),
             mu_x          = mu_x,
             sd_x          = sd_x,
             sd_y          = sd_y,
             mu_y          = mu_y,
             mu_lin        = mu_lin,
             sd_lin        = sd_lin,
             rule_depth    = rule_depth$depth,
             eta           = eta
  )
  class(out) = "cre_mod"
  out
}
