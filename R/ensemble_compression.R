#' Ensemble Rule Compression
#'
#' @param rules character vector containing the decision rules
#' @param k maximum number of clusters
#' @return list with conditions and rules
#' @export
#' @import dplyr
#' @importFrom stringr str_extract_all
#' @importFrom tidyr drop_na
#' @importFrom magrittr %>%
#'
cluster_rules = function(rules, k){
  res   = list()
  rules = rules[!is.na(rules)]
  for(i in 1:length(rules)){
    single_conditions = strsplit(rules[i], "&")[[1]]
    var_split         = do.call('rbind', strsplit(single_conditions, "<|>=|<=|>"))
    direction         = stringr::str_extract_all(rules[i], "<|>=|<=|>")[[1]]

    res[[i]]          = data.frame(split_var   = as.numeric(gsub('[^0-9.-]','',var_split[,1])),
                                   split_val   = as.numeric(gsub('[^0-9.-]','',var_split[,2])),
                                   d           = 1:nrow(var_split),
                                   direction   = ifelse((direction == "<")|(direction == "<="), 1, 0),
                                   rule        = i
    )
  }

  out =   do.call('rbind', res)
  rules_frame =   out %>% tidyr::drop_na() %>% dplyr::group_by(split_var) %>%
    dplyr::mutate(rule_group = ckmean_getlabel(split_val, k = k)) %>% dplyr::ungroup() %>%
    dplyr::arrange(split_var, rule_group) %>% dplyr::mutate(ensemble_condition = paste0("var", split_var, "rg",rule_group)) %>%
    dplyr::ungroup() %>% dplyr::group_by(rule, split_var) %>%
    dplyr::group_by(rule, split_var, rule_group) %>% dplyr::slice(dplyr::n()) %>% dplyr::ungroup() %>%
    dplyr::group_by(rule) %>% dplyr::arrange(split_var, rule_group) %>% dplyr::mutate(ensemble_rule = paste(ensemble_condition, direction, collapse = "_"), rule_length= dplyr::n()) %>%
    dplyr::ungroup()
  list(ensemble_conds = rules_frame[,c("ensemble_condition", "split_var", "split_val")] %>% dplyr::group_by(ensemble_condition, split_val, split_var) %>% dplyr::summarise(n = n(), .groups = 'drop') %>% dplyr::ungroup(),
       ensemble_rules = rules_frame %>% dplyr::group_by(ensemble_rule) %>% dplyr::mutate(forest_support = n(), dup = duplicated(ensemble_condition)) %>% dplyr::filter(!dup) %>% arrange(desc(rule))
  )
}
