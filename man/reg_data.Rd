\name{reg_data}
\alias{reg_data}
\title{
Function to compute the regularized version of COSNet (Frasca et al. 2013)
}
\description{
This function modifies the weights and the thresholds of the network to realized the COSNet regularization.
}
\usage{
reg_data(W, theta, eta, M, m, pos_num)
}
\arguments{
  \item{W}{
square symmetric named matrix of the network weights. The components of W are in the [0,1] interval. The i,j-th component is the weight between neuron i and neuron j. The components of the diagonal of W are 0
}
  \item{theta}{
vector of the neuron activation thresholds
}
  \item{eta}{
real value corresponding to the eta regularization coefficient in the energy function (Frasca et al. 2013). If eta = 0 no regularization is applied. The higher the value of eta, the more the influence of the regularization term  
}
  \item{M}{
positive neuron activation value
}
  \item{m}{
negative neuron activation value
}
  \item{pos_num}{
number of expected positive neurons in the equilibrium state of the network
}
}
\value{
list of two element: 
\item{W}{the regularized connection matrix}
\item{theta}{regularized threshold vector}
}

\examples{
library(bionetdata);
data(Yeast.STRING.data);
n <- nrow(Yeast.STRING.data);
dim(Yeast.STRING.data);
range(Yeast.STRING.data);
## setting values for parameter alpha, for the rate of positive examples,
## for neuron thresholds and for eta parameter
alpha <- 1;
pos.rate <- 0.01;
thresholds <- runif(n);
range(thresholds);
eta <- 0.001;
a <- reg_data(Yeast.STRING.data, thresholds, eta, sin(alpha),
    -cos(alpha), ceiling(pos.rate*n));
## new connection matrix
dim(a$W);
range(a$W);
## new thresholds
range(a$theta);
}

\references{
Frasca M., Bertoni A., Re M., Valentini G.: A neural network algorithm for semi-supervised node label learning from unbalanced data. Neural Networks, Volume 43, July, 2013 Pages 84-98.
}
