#' ntickets
#'
#' @param N Vector for size n
#' @param gamma gamma value
#' @param p probability
#'
#' @return two plots and a list
#' @importFrom stats pbinom pnorm
#' @export
#'
#' @examples ntickets(N=400,gamma = 0.02, p = 0.95)
ntickets <- function(N = 200, gamma = 0.02, p = 0.95){

  # Discrete Section
  n <- seq(N,floor(1.1*N),by=1) # Generate all n
  dis_fun <- 1-gamma-pbinom(N, n, p) # Object function for Discrete
  absFn = abs(dis_fun) # Absolute value of the output for the discerte function
  nd = n[which.min(absFn)] # Finds the minimum for the output of the discrete and finds where it is in n
  #print(nd)
  plot(x=n,
       y=dis_fun,
       type="b",
       xlab="n",
       ylim=c(0,1),
       ylab="Objective",
       bg="Blue",
       pch=21,
       main=paste0("Objective Vs n to find optimal tickets sold \n(",nd,") gamma = ",gamma," N = ",N," discrete"))
  abline(h=0,col="Red")
  abline(v=nd,col="Red")


  # Continous Section
  con_fun <- function(x){ 1-gamma-pnorm(N+0.5, x*p, sqrt(x*p*(1-p))) } # Object function for Continuous

  uroot = stats::uniroot(f=con_fun, lower=n[1], upper=n[length(n)] ) # Finds the n value at root(0)
  #print(root2)
  nc=uroot$root
  #print(nc)

  curve(con_fun,
        type="l",
        xlim=c(n[1], n[length(n)]),
        xlab="n",
        ylim=c(0,1),
        ylab="Objective",
        add=FALSE,
        main=paste0("Objective Vs n to find optimal tickets sold \n(",nc,") gamma = ",gamma," N = ",N," continuous"))
  abline(h=0,col="Blue")
  abline(v=nc,col="Blue")

  return(list(nd=nd,
              nc=nc,
              N=N,
              p=p,
              gamma=gamma))
}
