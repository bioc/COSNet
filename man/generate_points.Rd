\name{generate_points}
\alias{generate_points}

\title{
Compute the points to be separated in Step 2 of COSNet algorithm (Frasca et al. 2013)
}
\description{
This function associates each labeled node with a point in the plane, whose 
coordinates are respectively the weighted sum of its positive and negative neighborhoods
}

\usage{
generate_points(W, unlabeled, labeling)
}

\arguments{
  \item{W}{
square symmetric named matrix, whose components are in the [0,1] interval. The i,j-th component is the weight between node i and node j. The components of the diagonal of W are zero.
}
  \item{unlabeled}{
vector of the indices of the unlabeled nodes
}
  \item{labeling}{
vector of node labels : 1 for positive nodes, -1 for negative nodes, 0 for unlabeled nodes
}
}

\details{
For each labeld node k,  a point (pos_vect[k], neg_vect[k]) is computed, where pos_vect[k] is the whighted sum of the positive neighbors of node k and neg_vect[k] is the weighted sum  of negative neighbors of node k. 
}

\value{
List of two element: 
\item{pos_vect}{ is the vector of the abscissae; pos_vect[k] contains the whighted sum of the positive neighbors of node k}
\item{neg_vect}{ is the vector of the ordinates; neg_vect[k] contains the whighted sum of the negative neighbors of node k}
}

\references{

Frasca M., Bertoni A., Re M., Valentini G.: A neural network algorithm for semi-supervised node label learning from unbalanced data. Neural Networks, Volume 43, July, 2013 Pages 84-98.
}

\examples{
## randomly generating labels
labels <- generate_labels(100, 0.3);
unlabeled <- sample(1:100, 10);
labels[unlabeled] <- 0;
## randomly generating connection matrix
W <- matrix(sample(1:10000, 100*100)/1000, nrow = 100);
diag(W) <- 0;
points <- generate_points(W, unlabeled, labels);
points$pos_vect[1:5];
points$neg_vect[1:5];
}

