##### 1-3 Portfolio Theory #####

#### Working with real data ####

minvariance <- function(assets, mu = 0.005){
  return <- log(tail(assets, -1) / head(assets,-1))
}