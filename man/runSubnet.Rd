\name{runSubnet}
\alias{runSubnet}

\title{
Realizing the network dynamics.
}
\description{
Function to simulate the dynamics of the network composed of unlabeled nodes.
}
\usage{
runSubnet(W, labeling, alpha_value, c_value, cost)
}

\arguments{
  \item{W}{
square symmetric named matrix, whose componemts are in the interval [0,1]. The i,j-th component is a similarity 
		index between node i and node j. The components of the diagonal of W are zero. Rows and colummns should be named identically.  
}
  
  \item{labeling}{
vector of node labels: 1 for positive examples, -1 for negative examples, 0 for unlabeled nodes
}
  \item{alpha_value}{
real value in [0, pi/2[, determining the neuron activation values: sin(alpha) and -cos(alpha).
}
  \item{c_value}{
real value  used as activation threshold for each neuron
}
 
  \item{cost}{
real value corresponding to beta parameter in the equation eta = beta*|tan((alpha - pi/4)*2)|, where eta is 
       the coefficient of the regularization term in the energy function (Frasca et al. 2013). If cost = 0 (default) 
       the unregularized version is executed. The higher the value of cost, the more the influence of the regularization term. 
       It is suggested using small values for this parameter (e.g. cost = 0.00001) 
}
}
\details{
Function to simulate the subnetwork composed of the unlabeled genes, in which each neuron has {sin(alpha_value), -cos(alpha_value)} as activation value, and 
	in which each neuron has a threshold "c_value" minus the contribution from the labeled neighbors.
}
\value{
list with three components:
	\item{ state }{Named vector of prediction (at equilibrium) for unlabeled nodes}
	\item{ scores}{Named vector of scores (at equilibrium)  for unlabeled nodes }
	\item{ iter }{Number of iterations of the network until convergence}
}

\references{
Frasca M., Bertoni A., Re M., Valentini G.: A neural network algorithm for semi-supervised node label learning from unbalanced data. Neural Networks, Volume 43, July, 2013 Pages 84-98.
}

\examples{
library(bionetdata);
data(Yeast.STRING.data);
data(Yeast.STRING.FunCat);
n<-nrow(Yeast.STRING.data);
## removing dummy node 00
Yeast.STRING.FunCat <- Yeast.STRING.FunCat[,
                        -which(colnames(Yeast.STRING.FunCat) == "00")];
class <- 1;
labels <- as.vector(Yeast.STRING.data[, class]);
names(labels) <- rownames(Yeast.STRING.FunCat);
labels <- as.vector(Yeast.STRING.FunCat[, class]);
names(labels) <- rownames(Yeast.STRING.FunCat);
folds <- find.division.strat(labels, 1:n, 3);
labels[labels <= 0] <- -1;
test.set <- folds[[1]];
training.set <- setdiff(1:n, test.set);
labels[test.set] <- 0;
res <- runSubnet(Yeast.STRING.data, labels, alpha=1, c=0, cost=0.0001);
str(res);
}
