\name{find.division.strat}
\alias{find.division.strat}
\title{
Compute a stratified random partition of the input data
}
\description{
Function to determine a random partition of the labeled input vector into a fixed number of folds, such that each fold has around the same proportion of two-class labels.
}
\usage{
find.division.strat(labels, vett, n_fold)
}

\arguments{
  \item{labels}{
binary -1/1 label vector. labels[i] is the label for the element vett[i]. 
}
  \item{vett}{
vector to be partitioned
}
  \item{n_fold}{
number of folds in which the argument vett must be partitioned
}
}
\details{
The input vector "vett" is randomly partitioned into "n_fold" folds ensuring each fold contains roughly the same proportions of positives and negative labels, according to the labeling "labels"
}
\value{
List with n_fold elements, the i-th element is a vector corresponding to i-th fold.
}


\examples{
n <- 100;
vett <- runif(n, 0, 1)
labels <- c(rep(1, floor(n/3)), rep(-1, ceiling(2*n/3)));
n_fold <- 5;
fold_list <- find.division.strat(labels, vett, n_fold);
length(fold_list);
## number of positives in the first fold
sum(labels(fold_list[[1]]) > 0);
}

\seealso{
\code{\link{find.division.not.strat}}
}
