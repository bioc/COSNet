\name{optimize_pos_above}
\alias{optimize_pos_above}
\title{
Alternative algortihm for optimizing parameters
}
\description{
Alternative algorithm to compute the two parameters: 'alpha', determining neurons activation values, and the neuron thresholds 'c'.
}
\usage{
optimize_pos_above(pos_vect, neg_vect, training_labels, res)
}
\arguments{
  \item{pos_vect}{
vector of abscissae of the points to be separated
}
  \item{neg_vect}{
vector of ordinates of the points to be separated
}
  \item{training_labels}{
1/-1 vector of the point labels
}
  \item{res}{
list containing the optimum angle (field alpha), neuron threshold (field c), maximum F-score (field Fscore) and positive halfplane (field pos_half) computed by the procedure corresponding to the choice 1) of function \code{optimizep}.
}
}
\details{
Function to optimize the the parameters 'alpha' and 'c' when the function \code{optimizep} determines that the maximum F-score corresponds to the half-plane above the separation line. The algorithm works in three steps:
1) Selecting all the points which lie on the Y-axis. The aim is to choose a positive point which will be the center of the line bundle we
consider in the next step. We sort the selected points by ordinate and for each positive point we compute the F-score of the almost
vertical line (but with negative slope) crossing this point considering solely the selected points. Then we choose the
point k which corresponds to the highest F-score.
2) The algorithm computes the slopes of the lines crossing the point k and each point not lying on the Y-axis. Then it searches
the line, among those with negative slope, which maximizes the F-score criterion by sorting the computed lines according to their 
slopes in an increasing order. Consequently, the angle alpha relative to the optimum line is in the interval ]pi/2,  pi[.
3) Compute the intercepts of the straight lines whose slope is tan(alpha) and crossing each available point. The optimum line is identified
by scanning the computed lines according to their intercept in an increasing order. Let q be the intercept of the optimum line
y = tan(alpha)z + q, then we set c = -cos(alpha)q.
If there are no positive point with abscissa 0, the function returns the optimal parameters contained in input argument 'res' computed by the procedure corresponding to the choice 1) of function \code{optimizep}.
}
\value{
list res with 3 components
  \item{res$alpha}{ value of the optimum angle alpha}
  \item{res$c}{ value of the optimum threshold c}
  \item{res$Fscore}{ value of the optimum F-score}
  \item{res$pos_half}{position of the positive half-plane (-1 below, 1 above the optimum straith line)}  
}

\examples{
library(bionetdata);
data(Yeast.STRING.data);
data(Yeast.STRING.FunCat);
n <- nrow(Yeast.STRING.data);
## removing dummy node 00
Yeast.STRING.FunCat <- Yeast.STRING.FunCat[,
                        -which(colnames(Yeast.STRING.FunCat) == "00")];
## selecting the class with index 1
class <- 1;
labels <- as.vector(Yeast.STRING.data[, class]);
names(labels) <- rownames(Yeast.STRING.FunCat);
labels <- as.vector(Yeast.STRING.FunCat[, class]);
names(labels) <- rownames(Yeast.STRING.FunCat);
## partitioning the data
folds <- find.division.strat(labels, 1:n, 3);
labels[labels <= 0] <- -1;
test.set <- folds[[1]];
training.set <- setdiff(1:n, test.set);
labels[test.set] <- 0;
points <- generate_points(Yeast.STRING.data, test.set, labels);
## setting values for the parameter
res <- list(alpha=pi/2, c=0, Fscore=0, pos_half=-1);
opt_parameters <- optimize_pos_above(points$pos_vect[training.set],
                    points$neg_vect[training.set], labels[training.set], res);
str(opt_parameters);
}

\seealso{
\code{\link{optimizep}}
}
