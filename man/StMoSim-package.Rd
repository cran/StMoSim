\name{StMoSim-package}
\alias{StMoSim-package}
\alias{StMoSim}
\docType{package}
\title{
Plots a QQ-Norm plot with several Gaussian simulations.
}
\description{
With this package you can simulate several lines into the QQ-Norm Plot under the assumption of Gaussian distribution. If the realised observations lie inside of the simulations tracks there is the possibility that the observations stem from a Gaussian distribution.
This can be very useful in residual analysis where you have to evaluate whether the model residuals fit the assumption of gaussian distributed terms or not.

-----------<CHANGELOG>-----------

-------< v3.0 - 2014-10-16 >-------

Computation intense code moved to C++. 

Moved to parallel computation, thanks to Rcpp/RcppParallel !

Minor bug fixes.

-------< v2.2 - 2012-02-24 >-------

Minor bug fixes, due to CHECK changes on CRAN.

-------< v2.1 - 2012-02-24 >-------

Minor bug fixes.

-------<v2.0 - 2011-03-31 >-------

Moved to S4 Classes.

-------<v1.1 - 2010-05-03 >-------

First Version on CRAN.

-----------</CHANGELOG>-----------
 
}
\details{
\tabular{ll}{
Package: \tab StMoSim\cr
Type: \tab Package\cr
Version: \tab 3.0\cr
Date: \tab 2014-10-16\cr
License: \tab  GPL-2 | GPL-3\cr
LazyLoad: \tab yes\cr
}
function:
qqnormSim
}
\author{
Matthias Salvisberg

Maintainer: Matthias Salvisberg <matthias.salvisberg@gmail.com>
}

\keyword{ package }

