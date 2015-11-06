\name{COSNet-package}
\alias{COSNet-package}
\docType{package}
\title{
R package for binary classification and node ranking on partially labeled graphs with unbalanced labels.
}
\description{
Algorithm based on cost-sensitive neural network for predicting 
node labels in a semi-supervised setting.  
}
\details{
\tabular{ll}{
Package: \tab COSNet\cr
Type: \tab Package\cr
Version: \tab 1.5.1\cr
Date: \tab 2015-11-06\cr
License: \tab GPL (>= 2) \cr
LazyLoad: \tab yes\cr
}
This package implements the COSNet algorithm (Frasca et al. 2013). COSNet is a semi-supervised cost-sensitive neural network for predicting 
node labels in partially labeled graphs. The algorithm is able in inferring a labeling for unlabeled nodes in the graph starting from the graph topology and the known labels.
}
\author{
\emph{Marco Frasca}, and \emph{Giorgio Valentini}

DI, Dipartimento di Informatica

Universita' degli Studi di Milano

\email{\{frasca,valentini\}@di.unimi.it}

Maintainer: 
\emph{Marco Frasca} 
}


\references{
Frasca M., Bertoni A., Re M., Valentini G.: A neural network algorithm for semi-supervised node label learning from unbalanced data. Neural Networks, Volume 43, July, 2013 Pages 84-98.

Bertoni A., Frasca M., Valentini G.: COSNet: a Cost Sensitive Neural Network for Semi-supervised Learning in Graphs. ECML PKDD'11 Proceedings of the 2011 European conference on Machine learning and knowledge discovery in databases - Volume Part I. Pages 219-234 
}

\keyword{ package }
