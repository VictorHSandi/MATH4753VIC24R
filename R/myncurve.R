#' myncurve
#'
#' @param mu mean vector
#' @param sigma sd vector
#' @param a area vector
#'
#' @return list
#' @importFrom graphics polygon
#' @export
#'
#' @examples
#' myncurve(mu=10,sigma=5, a=6)
myncurve = function(mu, sigma, a){
  x=NULL
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(mu-3*sigma,a,length=1000)
  ycurve=dnorm(xcurve, mu, sigma)
  polygon(x=c(mu-3*sigma,xcurve,a),y=c(0,ycurve,0),col="purple")
  list(mu = mu, sigma = sigma,a=a)

}
