salvimat_ta <-
function(lmModel, method = "LS", n_sim = 500){

    if(method != "LS" & method != "M" & method != "MM"){
        stop("method wird nicht unterstuetzt")
    }

## LS ##
    if(method == "LS"){

        dummy <- loess.smooth(fitted(lmModel),resid(lmModel))
        dax <- matrix(NA,ncol = n_sim, nrow=length(dummy$x))
        day <- matrix(NA,ncol = n_sim, nrow=length(dummy$y))

        for(p in seq(1,(n_sim),1)){


            erkl <- (attr(lmModel$terms,"term.labels"))
            erkldaten <- lmModel$model[,erkl]

            zieldaten <- fitted(lmModel) + rnorm(length(fitted(lmModel)), mean = 0, sd = summary(lmModel)$sigma)
            zieldaten <- data.frame(ziel = zieldaten)

            a <- summary(lmModel)$terms
            names(zieldaten) <- dimnames(attr(a,"factors"))[1][[1]][1]

            daten <- cbind(zieldaten,erkldaten)

            fun <- erkl[1]

            for(i in 2:length(erkl)){
                fun <- paste(fun,erkl[i], sep = " + ")
            }

            fun <- paste(dimnames(attr(a,"factors"))[1][[1]][1],fun, sep = " ~ ")
            fun <- paste(fun,"data = daten)", sep = ", ")
            fun <- paste("lm(", fun, sep = "")
            neumod <- eval(parse(text = fun))

            linie <- loess.smooth(fitted(neumod),resid(neumod))

            dax[,p] <- linie$x
            day[,p] <- linie$y

        }

        scatter.smooth(fitted(lmModel),resid(lmModel), main = "salvimat TA Plot",xlab = "Fitted values", ylab= "Residuals")
        title(sub = fun)
        matlines(dax,day, type = "l", col = "grey", lty = 1,pch = 1)
        pp <- loess.smooth(fitted(lmModel),resid(lmModel))
        lines(pp$x,pp$y, col = "red", lty = 1, lwd = 2)
        abline(h=0,lty=2)
    }
## M ##
    if(method == "M"){

        library("MASS")

        dummy <- loess.smooth(fitted(lmModel),resid(lmModel))
        dax <- matrix(NA,ncol = n_sim, nrow=length(dummy$x))
        day <- matrix(NA,ncol = n_sim, nrow=length(dummy$y))

        for(p in seq(1,(n_sim),1)){


            erkl <- (attr(lmModel$terms,"term.labels"))
            erkldaten <- lmModel$model[,erkl]

            zieldaten <- fitted(lmModel) + rnorm(length(fitted(lmModel)), mean = 0, sd = summary(lmModel)$sigma)
            zieldaten <- data.frame(ziel = zieldaten)

            a <- lmModel$terms
            names(zieldaten) <- dimnames(attr(a,"factors"))[1][[1]][1]

            daten <- cbind(zieldaten,erkldaten)

            fun <- erkl[1]

            for(i in 2:length(erkl)){
                fun <- paste(fun,erkl[i], sep = " + ")
            }

            fun <- paste(dimnames(attr(a,"factors"))[1][[1]][1],fun, sep = " ~ ")
            fun <- paste(fun,"data = daten, method = ", sep = ", ")
            str <- "\"M\" )"
            fun <- paste(fun,str, sep = "")
            fun <- paste("rlm(", fun, sep = "")
            neumod <- eval(parse(text = fun,prompt="\""))

            linie <- loess.smooth(fitted(neumod),resid(neumod))

            dax[,p] <- linie$x
            day[,p] <- linie$y

        }

        scatter.smooth(fitted(lmModel),resid(lmModel), main = "salvimat TA Plot",xlab = "Fitted values", ylab= "Residuals")
        title(sub = fun)
        matlines(dax,day, type = "l", col = "grey", lty = 1,pch = 1)
        pp <- loess.smooth(fitted(lmModel),resid(lmModel))
        lines(pp$x,pp$y, col = "red", lty = 1, lwd = 2)
        abline(h=0,lty=2)
    }
## MM ##
    if(method == "MM"){

        library("MASS")

        dummy <- loess.smooth(fitted(lmModel),resid(lmModel))
        dax <- matrix(NA,ncol = n_sim, nrow=length(dummy$x))
        day <- matrix(NA,ncol = n_sim, nrow=length(dummy$y))

        for(p in seq(1,(n_sim),1)){


            erkl <- (attr(lmModel$terms,"term.labels"))
            erkldaten <- lmModel$model[,erkl]

            zieldaten <- fitted(lmModel) + rnorm(length(fitted(lmModel)), mean = 0, sd = summary(lmModel)$sigma)
            zieldaten <- data.frame(ziel = zieldaten)

            a <- lmModel$terms
            names(zieldaten) <- dimnames(attr(a,"factors"))[1][[1]][1]

            daten <- cbind(zieldaten,erkldaten)

            fun <- erkl[1]

            for(i in 2:length(erkl)){
                fun <- paste(fun,erkl[i], sep = " + ")
            }

            fun <- paste(dimnames(attr(a,"factors"))[1][[1]][1],fun, sep = " ~ ")
            fun <- paste(fun,"data = daten, method = ", sep = ", ")
            str <- "\"MM\" )"
            fun <- paste(fun,str, sep = "")
            fun <- paste("rlm(", fun, sep = "")
            neumod <- eval(parse(text = fun,prompt="\""))

            linie <- loess.smooth(fitted(neumod),resid(neumod))

            dax[,p] <- linie$x
            day[,p] <- linie$y

        }

        scatter.smooth(fitted(lmModel),resid(lmModel), main = "salvimat TA Plot",xlab = "Fitted values", ylab= "Residuals")
        title(sub = fun)
        matlines(dax,day, type = "l", col = "grey", lty = 1,pch = 1)
        pp <- loess.smooth(fitted(lmModel),resid(lmModel))
        lines(pp$x,pp$y, col = "red", lty = 1, lwd = 2)
        abline(h=0,lty=2)
    }
}

