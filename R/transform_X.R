#' Transforms the input matrix to the Rule Matrix, using the ensemble rule approach.
#'
#' @param x input matrix
#' @param rules_frame data frame containing the ensemble rules as outputted by ensemble_compression
#' @return design matrix with 1 column per rule
#' @export
#' @importFrom stats weighted.mean
#'
transformX = function(x, rules_frame){
  ensemble_conds = rules_frame$ensemble_conds
  ensemble_rules = rules_frame$ensemble_rules
  un_ec     = unique(ensemble_conds$ensemble_condition)
  n_ec      = length(un_ec)
  rule_list = list()
  for(i in 1:n_ec){
    conds        = ensemble_conds[ensemble_conds$ensemble_condition == un_ec[i],]
    #conds        = rules_frame[rules_frame$ip_rule == un_ip[i],]
    if(nrow(conds)> 1){
      rule_list[[i]] = apply(x[,conds$split_var] < conds$split_val,1, function(x)stats::weighted.mean(x, w=conds$n))
      #rule_mat[,i] = apply(x[,conds$split_var] < conds$split_val,1, function(x)mean(x))
    } else {
      rule_list[[i]] = as.numeric(x[,conds$split_var] < conds$split_val)
    }
  }

  un_rules    = unique(ensemble_rules$ensemble_rule)
  n_rule      = length(un_rules)
  out_mat     = matrix(0, nrow = nrow(x), ncol = n_rule)

  for(i in 1:n_rule){
    ip_conds        = ensemble_rules[ensemble_rules$ensemble_rule == un_rules[i],]$ensemble_condition
    ip_dirs         = ensemble_rules[ensemble_rules$ensemble_rule == un_rules[i],]$direction
    res             = rep(1, times = nrow(x))
    if(length(ip_conds)> 1){
      ### check check check
      for(k in 1:length(ip_conds)){
        if(ip_dirs[k] == 1){
          res = res*rule_list[[match(ip_conds[k], un_ec)]]
        } else {
          res = res*(1-rule_list[[match(ip_conds[k], un_ec)]])
        }
      }
      out_mat[,i] = res
    } else {
      if(ip_dirs == 1){
        res = res*rule_list[[match(ip_conds, un_ec)]]
      } else {
        res = res*(1-rule_list[[match(ip_conds, un_ec)]])
      }
      out_mat[,i] = res
    }
  }
  out_mat
}
