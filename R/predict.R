#' Ensemble Rule Compression
#'
#' @param model cre model
#' @param newdata test data
#' @param s if glmnet is used either "lambda.min" or "lambda.1se". Defaul is "lambda.1se"
#' @return list with conditions and rules
#' @export
#'
predict.cre_mod = function(model, newdata, s = "lambda.min"){
  Xtest = transformX(newdata, model$rules)
  if(length(model$delete) > 0){
    Xtest = t(apply(Xtest[,-model$delete],1,function(x)x/model$rule_depth[-model$delete]^model$eta))
  } else {
    Xtest = t(apply(Xtest,1,function(x)x/model$rule_depth^model$eta))
  }


  for(p in 1:ncol(newdata)){
    newdata[,p] = (newdata[,p]-model$mu_lin[p])/model$sd_lin[p]
  }
  if(model$linear == T){
    Xtest = cbind(newdata*0.4, Xtest)
  }
  colnames(Xtest) = model$mat_names
  outer_model     = model$outer_model
  if(class(outer_model) == "cv.glmnet"){
    predictions = as.numeric(predict(outer_model, as.matrix(Xtest), type="response", s = s))
  } else if (class(outer_model) == "ranger"){
    predictions = predict(outer_model, data.frame(Xtest))$predictions
  }else if (class(outer_model) == "horseshoe_regressor"){
    for(j in 1:ncol(Xr)){
      Xr[,j]               = (Xr[,j]-model$mu_x[j])/model$sd_y[j]
    }
    predictions = as.matrix(Xtest)%*%apply(outer_model[[1]],2,mean)*model$sd_y + model$mu_y
  }
  predictions
}
