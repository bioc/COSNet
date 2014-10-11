\name{optimizep}
\alias{optimizep}

\title{
Optimizing algorithm parameters
}
\description{
Function to learn the parameters "alpha", determining neurons activation values, and the neuron threshold "c".
}
\usage{
optimizep(pos_vect, neg_vect, training_labels)
}

\arguments{
  \item{pos_vect}{
vector of abscissae of the points to be separated
}
  \item{neg_vect}{
vector of ordinates of the points to be separated
}
  \item{training_labels}{
  1/-1 vector of point labels
}
}
\details{
This function computes the optimal angle "alpha" and the optimal threshold "c". For each labeled neuron k, a point (pos_vect[k], neg_vect[k]) is considered.
	 Points are labeled according to the labels contained in the vector "training_labels". Then the straight line, among those with positive slope, which separates these points by maximizing the F-score is learned. The line is represented by the angle alpha formed with the "x" axis, and its intercept "q" with "y" axis.
   When separating points by a straight line, there are two possibility: 1) Considering as positive the half-plane above the line; 2) Considering as positive
	 the half-plane below the line. The procedure investigates both these possibilities, and also returns which choice between 1) and 2) corresponds to the best F-score.
}
\value{
list "res" with 4 components:

  \item{res$alpha}{value of the optimum angle alpha}
  \item{res$c}{value of the optimum threshold c}
  \item{res$Fscore}{ value of the optimum F-score}
  \item{res$pos_half}{ the position of the positive half-plane : > 0  in case 1),  < 0 in case 2)  }
}

\examples{
library(bionetdata);
data(Yeast.STRING.data);
data(Yeast.STRING.FunCat);
n <- nrow(Yeast.STRING.data);
## removing dummy node 00
Yeast.STRING.FunCat <- Yeast.STRING.FunCat[, 
                        -which(colnames(Yeast.STRING.FunCat)=="00")];
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
## generating the points to be separated
points <- generate_points(Yeast.STRING.data, test.set, labels);
opt_parameters <- optimizep(points$pos_vect[training.set],
                    points$neg_vect[training.set], labels[training.set]);
str(opt_parameters);
}

\seealso{
\code{\link{optimize_pos_above}}
}

