#' Plots the distribution of splitpoints in each covariate
#'
#' @param model cre model
#' @param n_most How many covariates to show. Takes the n_most covariates with the most splitpoints.
#' @return histogram of splitpoint distribution
#' @export
#' @import ggplot2 dplyr tidyr
visualise_clusters = function(model, n_most = 6){
  keep          = model$rules$ensemble_conds %>% group_by(split_var) %>% summarise(overall = sum(n)) %>% arrange(desc(overall)) ### keep only the n_most

  ggplot(data   = model$rules$ensemble_conds %>% filter(split_var %in% keep$split_var[1:n_most]) %>% uncount(n) %>% ungroup()%>% mutate(split_var = as.factor(split_var),
                                                                                                                                        rule_group = as.factor(rule_group)), aes(x=split_val, color=rule_group, fill=rule_group)) +
    geom_histogram(aes(x = split_val, y = ..ncount..), bins = 30, alpha = 0.3) +
    scale_y_continuous()+
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 8)) +
    xlab("") +
    ylab("Distribution and ECDF") +
    facet_wrap(~split_var, scales="free", nrow =2) + stat_ecdf(size = 1.5, linetype = "twodash")
}
