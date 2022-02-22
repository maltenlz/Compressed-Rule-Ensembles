#' This function uses kmeans to find the cluster labels
#'
#' @param x numeric vector
#' @param k maximum number of clusters
#' @return cluster labels
#' @export
#' @import  Ckmeans.1d.dp
ckmean_getlabel = function(x, k = 5, att_type = "numeric"){
  n_un = length(unique(x))
  if(att_type == "numeric"){
    if(n_un < 3){
      as.numeric(as.factor(x))
    } else {
      res = Ckmeans.1d.dp::Ckmeans.1d.dp(x, k=c(1:k), y=1,
                            method=c("linear"),
                            estimate.k = "BIC"
        )
      res$cluster
    }
  } else {
    x
  }
}
