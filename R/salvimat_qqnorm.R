salvimat_qqnorm <-
function(lmModel, n_sim = 500){

res_mod <- resid(lmModel)

n_size <- dim(lmModel$model)[1]
n_mad <- mad(res_mod)
n_mean <- mean(res_mod)

sim_mod <- matrix(rnorm(n_size * n_sim, n_mean, n_mad), ncol = n_sim)


quant <- function(x){
    quantile(x,probs = seq(0.001,0.999,0.001))
}

n_quant <- apply(sim_mod,2,quant)

X11(width = 600, height = 600)

qqnorm(res_mod)
qqx <- qqnorm(res_mod)$x
qqy <- qqnorm(res_mod)$y


matlines(qnorm(seq(0.001,0.999,0.001)), n_quant, lty = 1,pch = 1, lwd = 3, col = "#cdd2d015")

points(qqx,qqy)
qqline(res_mod)
box()
}

