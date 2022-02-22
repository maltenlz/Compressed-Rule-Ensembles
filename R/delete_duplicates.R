#' Removes colinear and redundant rules. In case of colinearity (r = 1) the rule with higher frequency in the forest is kept
#'
#' @param x rule matrix
#' @return rules to be removed
#' @export
#'
#' @importFrom dplyr summarise group_by
#' @importFrom stats cor
#' @importFrom magrittr %>%
delete_duplicates = function(x,
                             rules_frame){
  ensemble_rules = rules_frame$ensemble_rules
  un_rules       = unique(ensemble_rules$ensemble_rule)
  overall        = ensemble_rules %>% dplyr::group_by(ensemble_rule) %>% dplyr::summarise(support = n())
  Cmat           = stats::cor(x)
  p              = ncol(x)
  remove         = c()
  for(j in 1:(p-1)){
    inds   = which(abs(Cmat[j,(j+1):p]) == 1)
    if(!is.null(inds)){
      inds      = c(j, inds+j)
      how_often = overall$support[match(un_rules[inds], overall$ensemble_rule)]
      keep      = which.max(how_often)
      remove    = append(remove, inds[-keep])
    }
  }
  sort(unique(remove))
}
