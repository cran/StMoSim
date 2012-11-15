myQQNorm <- function(x, n_sim){
  n_size <- length(x)
  n_mad <- mad(x)
  n_mean <- mean(x)
  
  sim_mod <- matrix(rnorm(n_size * n_sim, n_mean, n_mad), ncol = n_sim)
  
  
  quant <- function(xx){
    quantile(xx,probs = seq(0.001,0.999,0.001))
  }
  
  n_quant <- apply(sim_mod,2,quant)
  
  
  
  qqnorm(x)
  qqx <- qqnorm(x, plot.it = FALSE)$x
  qqy <- qqnorm(x, plot.it = FALSE)$y
  
  
  matlines(qnorm(seq(0.001,0.999,0.001)), n_quant, lty = 1,pch = 1, lwd = 3, col = "#cdd2d015")
  
  points(qqx,qqy)
  qqline(x)
  box()
}


setGeneric("qqnormSim", function(x, n_sim = 500) 
			standardGeneric("qqnormSim"))


setMethod("qqnormSim","lm",
    function(x, n_sim = 500){
      myQQNorm(resid(x), n_sim)
    })

setMethod("qqnormSim","numeric",
		function(x, n_sim = 500){
		  myQQNorm(x, n_sim)
		})


