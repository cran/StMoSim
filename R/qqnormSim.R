setGeneric("qqnormSim", function(x, n_sim = 500) 
			standardGeneric("qqnormSim"))


setMethod("qqnormSim","lm",
		function(x, n_sim = 500){

			res_mod <- resid(x)

			n_size <- dim(x$model)[1]
			n_mad <- mad(res_mod)
			n_mean <- mean(res_mod)

			sim_mod <- matrix(rnorm(n_size * n_sim, n_mean, n_mad), ncol = n_sim)


			quant <- function(xx){
 			   quantile(xx,probs = seq(0.001,0.999,0.001))
			}

			n_quant <- apply(sim_mod,2,quant)

			qqnorm(res_mod)
			qqx <- qqnorm(res_mod)$x
			qqy <- qqnorm(res_mod)$y


			matlines(qnorm(seq(0.001,0.999,0.001)), n_quant, lty = 1,pch = 1, lwd = 3, col = "#cdd2d015")

			points(qqx,qqy)
			qqline(res_mod)
			box()
})


setMethod("qqnormSim","numeric",
		function(x, n_sim = 500){
			
			res_mod <- x
			
			n_size <- length(res_mod)
			n_mad <- mad(res_mod)
			n_mean <- mean(res_mod)
			
			sim_mod <- matrix(rnorm(n_size * n_sim, n_mean, n_mad), ncol = n_sim)
			
			
			quant <- function(xx){
				quantile(xx,probs = seq(0.001,0.999,0.001))
			}
			
			n_quant <- apply(sim_mod,2,quant)
			

			
			qqnorm(res_mod)
			qqx <- qqnorm(res_mod)$x
			qqy <- qqnorm(res_mod)$y
			
			
			matlines(qnorm(seq(0.001,0.999,0.001)), n_quant, lty = 1,pch = 1, lwd = 3, col = "#cdd2d015")
			
			points(qqx,qqy)
			qqline(res_mod)
			box()
		})


