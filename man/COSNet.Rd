\name{COSNet}
\alias{COSNet}
\title{
Cost Sensitive Network for node label prediction on graphs with highly unbalanced labelings
}
\description{
This function realizes the COSNet algorithm (Frasca et al. 2013). COSNet is a semi-supervised algorithm based on parametric Hopfield networks for predicting labels for unlabeled nodes in graphs which are only partially labeled.  
}
\usage{
COSNet(W, labeling, cost = 0)
}
\arguments{
  \item{W}{
 	square symmetric named matrix, whose components are in the interval [0,1]. The i,j-th component is a similarity 
		index between node i and node j. The components of the diagonal of W are zero. Rows and columns should be named identically.  
}
  \item{labeling}{
vector of node labels: 1 for positive examples, -1 for negative examples, 0 for unlabeled nodes.
}
  \item{cost}{
real value corresponding to beta parameter in the equation eta = beta*|tan((alpha - pi/4)*2)|, where eta is 
       the coefficient of the regularization term in the energy function (Frasca et al. 2013). If cost = 0 (default) 
       the unregularized version is executed. "cost" is a real value that reduces or increases the influence of regularization 
       on the network dynamics. The higher the value of cost, the more the influence of the regularization term. 
       It is suggested using small values for this parameter (e.g. cost = 0.0001) 
}
}
\details{
\code{COSNet} contructs a Hopfield network whose connection 
   matrix is W, and it applies a cost-sensitive strategy to determine the network parameters starting from W and labels "labeling". 
   COSNet distinguishes between labeled (1, -1  components in "labeling") and unlabeled (zero components in "labeling") nodes, and it is made up by three steps:
		{1) Generating a random labeling (1, -1) for unlabeled nodes. }
		{2) Learning the parameters "alpha and "c" such that the line "cos(alpha)*y - sin(alpha)*x - q*cos(alpha) = 0 " linearly separates a suitable set of labeled points (in which each point corresponds to a labeled node) and optimizes (in terms of alpha and q) the F-score criterion. The output of this phase is the intercept "q" of the optimum line and the corresponding angle "alpha". Then each neuron has threshold c = - q*cos(alpha).}
		{3) Extending "c" and "alpha" to the subnetwork composed of only unlabeled nodes, and simulating it until an equilibrium state is reached. The dynamics of this network is regularized by adding a term to the energy function that is minimized when the proportion of positive in the network state is roughly the proportion of positives in the labeled part of the network. The parameter "cost" corresponds to the parameter beta  in the equation eta = beta*|tan((alpha - pi/4)*2)| (see Frasca et al. 2013). When the equilibrium state is reached, positive nodes will be predicted as positive for the current task.}
}
\value{
\code{COSNet} returns  a list with six elements:
	\item{ alpha }{the optimum angle }
	\item{ c}{the optimum threshold}
	\item{ Fscore }{the optimum F-score computed in Step 2}
	\item{ pred }{the vector of predictions for unlabeled nodes}
	\item{ scores}{the vector of scores for unlabeled nodes}
	\item{ iter }{number of iterations until convergence in Step 3}
}
\references{
Frasca M., Bertoni A., Re M., Valentini G.: A neural network algorithm for semi-supervised node label learning from unbalanced data. Neural Networks, Volume 43, July, 2013 Pages 84-98.
}

\examples{
library(bionetdata);
## loading Binary protein-protein interactions from the STRING 
## data base (von Mering et al. 2002)
data(Yeast.STRING.data)# "Yeast.STRING.data"
## FunCat classes annotations (0/1) for the genes included
## in Yeast.STRING.data. Annotations refer the funcat-2.1
## scheme, and funcat-2.1 data 20070316 data, available from the MIPS web site.
data(Yeast.STRING.FunCat) # "Yeast.STRING.FunCat"
labels <- Yeast.STRING.FunCat;
labels[labels == 0] <- -1;		
## excluding the dummy "00" root
labels <- labels[, -which(colnames(labels) == "00")];	
n <- nrow(labels);
k <- floor(n/10);
cat("k = ", k, "\n");
## choosing the first class
labeling <- labels[, 1];
## randomly choosing a subset of genes whose labels are hidden
hidden <- sort(sample(1:n, k));
hidden.labels <- labeling[hidden];
labeling[hidden] <- 0;
out <- COSNet(Yeast.STRING.data, labeling, 0);
prediction <- out$pred;
TP <- sum(hidden.labels == 1 & prediction == 1);
FN <- sum(hidden.labels == 1 & prediction == -1);
FP <- sum(hidden.labels == -1 & prediction == 1);
out2 <- COSNet(Yeast.STRING.data, labeling, 0.0001);
prediction <- out2$pred;
TP2 <- sum(hidden.labels == 1 & prediction == 1);
FN2 <- sum(hidden.labels == 1 & prediction == -1);
FP2 <- sum(hidden.labels == -1 & prediction == 1);
}
