library(COSNet)
test_optimizep <- function() {
    nitems <- 1000;
    neg_vect <- runif(nitems);
    pos_vect <- runif(nitems);
    labels <- sample(c(1,-1), nitems, replace=TRUE);
    res <- optimizep(pos_vect, neg_vect, labels)
    checkTrue(res$alpha >= 0 && res$alpha < pi/2)
    checkTrue(is.numeric(res$c))
    checkTrue(res$Fscore >= 0 && res$Fscore <= 1)
    checkTrue(is.numeric(res$pos_half))
}


