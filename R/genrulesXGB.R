#' Extracts rules from a XGB model.
#'
#' @param x input matrix
#' @param y outcome vector
#' @param forest_control parameters that are passed to xgboost
#' @param task "regression" or "class" for (binary) classification
#' @return list of rules extracted from the xgboost model
#' @export
#' @importFrom xgboost xgb.DMatrix xgboost xgb.model.dt.tree

genrulesXGB = function(x,
                       y,
                       forest_control = NULL,
                       task = "regression") {

  N           = length(y)
  dtrain      = xgboost::xgb.DMatrix(as.matrix(x), label = as.numeric(y))
  if(task == "regression"){
    y         = y
    objective = "reg:squarederror"
  } else {
    objective = "binary:logistic"
  }
  param <- list(objective            = objective,
                max_depth            = ifelse(is.null(forest_control$max_depth), 3, forest_control$max_depth),
                eta                  = ifelse(is.null(forest_control$learning_rate), 0.01, forest_control$learning_rate),
                subsample            = ifelse(is.null(forest_control$subsample), 0.6, forest_control$subsample),
                min_child_weight     = ifelse(is.null(forest_control$min_sample), ifelse(nrow(x) < 100, 2, sqrt(nrow(x))), forest_control$min_sample),
                colsample.bytree     = ifelse(is.null(forest_control$col_sample), 1, forest_control$col_sample),
                lambda               = ifelse(is.null(forest_control$lambda ), 1, forest_control$lambda),
                gamma                = ifelse(is.null(forest_control$gamma ), 0, forest_control$gamma)

  )
  model1 = xgboost::xgboost(data    = dtrain,
                     params  = param,
                     nrounds = ifelse(is.null(forest_control$ntree), 500, forest_control$ntree),
                     verbose = F)
  xt <- as.data.frame(xgboost::xgb.model.dt.tree(feature_names = as.character(1:ncol(dtrain)), model=model1))
  tree_ids = unique(xt$Tree)
  # iterate through the trees
  rule_list = list()
  i         = 1
  for(tree_id in tree_ids){
    tree         = xt[xt$Tree == tree_id,]
    # take the non terminal nodes
    non_terminal = tree$ID[!is.na(tree$Split)]
    for(node_id in non_terminal){
      node         = tree[tree$ID == node_id, ]

      #### check if terminal nodes
      left_node     = tree[tree$ID == node$Yes,]
      right_node    = tree[tree$ID == node$No,]

      #### prefer terminal nodes, as those might be purer and indicate predictive subgroups
      if(!is.na(left_node$Split) & !is.na(right_node$Split)){
        sign_to_keep  = ifelse(abs(tree[tree$ID == node$Yes,'Quality']) > abs(tree[tree$ID == node$No,'Quality']), '<', '>=')
      } else if (is.na(left_node$Split) & !is.na(right_node$Split)){
        sign_to_keep  = "<"
      } else {
        sign_to_keep  = ">="
      }

      rule_vec      = paste(paste0('X[,',node$Feature, ']'), sign_to_keep ,node$Split)
      current_id    = node$ID
      precondition  = tree$ID[which(tree$Yes == current_id | tree$No == current_id)]
      while(length(precondition) > 0){
        pre_node      = tree[tree$ID == precondition,]
        rule_vec      = c(paste(paste0('X[,',pre_node$Feature, ']'), ifelse(pre_node$Yes == current_id, '<', '>='), pre_node$Split),rule_vec)
        current_id    = pre_node$ID
        precondition  = tree$ID[which(tree$Yes == current_id | tree$No == current_id)]
      }
      rule_list[[i]] = paste(rule_vec, collapse = " & ")
      i              = i+1
    }
  }
  rules = as.character(rule_list)
  rules
}
