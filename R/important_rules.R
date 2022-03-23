#' Prints the most important rules found be the CRE model
#'
#' @param model cre model
#' @param s lambda.1se or lambda.min
#' @return prints the most important rules
#' @import dplyr
#' @export
important_rules = function(model, s = "lambda.1se"){
  rules         = model$rules
  ensemble_conditions = model$rules$ensemble_conds
  reduced       = rules$ensemble_rules
  un_rules      = unique(reduced$ensemble_rule)[-c(model$deleted)]
  beta          = model$outer_model$glmnet.fit$beta[, model$outer_model$lambda == model$outer_model[s]]/c(rep(0.4, times=model$p_lin)*model$sd_lin, 1/model$rule_depth[-model$deleted]^model$eta)
  non_zero      = as.numeric(which(beta != 0))
  beta_n_zero   = as.numeric(round(beta[non_zero], digits = 6))
  intercept     = model$outer_model$glmnet.fit$a0[model$outer_model$lambda == model$outer_model[s]]

  if(model$linear == T){
    terms_to_show = c(paste("linear:", model$var_names), un_rules)[non_zero]
  }else {
    terms_to_show = c(un_rules)[non_zero]
  }
  var_names     = model$var_names
  outlist       = list()
  for(r in 1:length(terms_to_show)){
    if(grepl("linear", terms_to_show[r])){
      outlist[[r]] = c("linear",terms_to_show[r], beta_n_zero[r])
    } else {
      vars      = reduced$split_var[reduced$ensemble_rule == terms_to_show[r]]
      rulegroup = reduced$rule_group[reduced$ensemble_rule == terms_to_show[r]]
      ruledir   = reduced$direction[reduced$ensemble_rule == terms_to_show[r]]
      conditions    = c()
      for(j in 1:length(vars)){
        split_vals     = round(ensemble_conditions$split_val[(ensemble_conditions$split_var == vars[j]) & (ensemble_conditions$rule_group == rulegroup[j])], digits = 2)
        conditions[j]  = c(paste0(var_names[vars[j]], ifelse(ruledir[j] == 1, " < ", " >= "),"[", min(split_vals), ";", max(split_vals), "]"))
      }
      outlist[[r]] = c(terms_to_show[r], paste(sort(conditions), collapse = ' & '), beta_n_zero[r])
    }
  }
  outlist[[r+1]] = c("Intercept",1, intercept)
  outputframe = data.frame(do.call('rbind', outlist), stringsAsFactors = F)
  colnames(outputframe) = c("rule", "description", "coefficient")
  outputframe %>% mutate(coefficient = round(as.numeric(coefficient), digits = 2)) %>% arrange(desc(abs(coefficient)))
}
