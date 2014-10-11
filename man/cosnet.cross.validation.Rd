\name{cosnet.cross.validation}
\alias{cosnet.cross.validation}

\title{
Cross validation procedure for the COSNet algorithm
}
\description{
This function applies the function \code{COSNet} to the input data with a cross validation procedure.
}
\usage{
cosnet.cross.validation(labels, W, nfolds, cost)
}

\arguments{
  \item{labels}{
named matrix of node labels. The (i-j)-th component contains the label (1 for positive examples, -1 for negative examples) of node i for j-th  functional class to be predicted
}
  \item{W}{
square symmetric named matrix. The (i,j)-th component is a similarity index between node i and node j. The components of the diagonal of W are zero.
}
  \item{nfolds}{
integer corresponding to the number of desired folds
}
  \item{cost}{
real value that corresponds to the cost parameter of \code{COSNet}
}
}
\details{
\code{cosnet.cross.validation} runs the function \code{COSNet} on the input data through a cross validation procedure. For each class to be predicted (column of "labels"), both "W" and "labels" are partitioned into "nfolds" subsets and at each iteration the labels of a fold are hidden and predicted with function \code{COSNet}. When possible, input data are partitioned by ensuring the same proportion of positive and negative instances in each fold. 
}
\value{
List  with three elements:
	\item{labels}{1/-1 named input label matrix, in which rows correspond to nodes and columns to classes}
	 \item{predictions}{named 1/-1 prediction matrix,	in which rows correspond to nodes and columns to classes. The position i-j-th is 1 if the node i has been predicted as positive for the class j, -1 otherwise}
	 \item{scores}{named real score matrix, in which rows correspond to nodes and columns to classes. The position i-j-th is a real number corresponding to the internal energy at equlibrium for node i when predicting class j. This score is a "degree" of membership of  node i to the class j}
}

\seealso{
\code{\link{COSNet}}
}
\examples{
library(bionetdata);
data(Yeast.STRING.data)
data(Yeast.STRING.FunCat) # "Yeast.STRING.FunCat"
## excluding the dummy "00" root
Yeast.STRING.FunCat <- 
        Yeast.STRING.FunCat[, -which(colnames(Yeast.STRING.FunCat) == "00")];
nfolds <- 5;
res <- cosnet.cross.validation(Yeast.STRING.FunCat[, 1:50], Yeast.STRING.data,
        nfolds, 0.0001);
## computing performances
library(PerfMeas);
perf <- F.measure.single.over.classes(res$labels, res$predictions);
cat(perf$average);
}

