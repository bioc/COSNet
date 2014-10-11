\name{generate_labels}
\alias{generate_labels}

\title{
Generate random labels
}

\description{
Function to generate a vector of random labels
}

\usage{
generate_labels(n, pos_rate)
}

\arguments{
  \item{n}{
number of labels to be generated
}
  \item{pos_rate}{
rate of expected positive labels
}
}

\details{
This function generates "n" random labels in {-1, 1} drawn from the binomial distribution B(n, pos_rate)
}

\value{
-1/1 vector of length "n" containing the generated labels
}

\seealso{
\code{generate_points}
}
\examples{
pos_rate <- 0.3;					
n <- 100;
## generating n random labels with 0.3 expected rate of positive labels
random_labels <- generate_labels(n, pos_rate);
sum(random_labels > 0);
}
