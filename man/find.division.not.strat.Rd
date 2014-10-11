\name{find.division.not.strat}
\alias{find.division.not.strat}

\title{
Random partitioning of input data
}
\description{
Function to determine a random partition of the input vector into a specifiend number of folds.
}
\usage{
find.division.not.strat(vett, n_fold)
}

\arguments{
  \item{vett}{
vector to be partitioned
}
  \item{n_fold}{
number of folds in which the argument vett must be partitioned
}
}

\value{
List with n_fold elements, the i-th element is a vector corresponding to i-th fold.
}


\seealso{
\code{\link{find.division.strat}}
}
\examples{
n <- 100;
vett <- runif(n, 0, 1);
n_fold <- 5;
fold_list <- find.division.not.strat(vett, n_fold);
length(fold_list);
## indices of the first fold 
fold_list[[1]];
}

