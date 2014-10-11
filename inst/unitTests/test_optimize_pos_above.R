library(COSNet)
test_optimize_pos_above <- function() {
    nitems <- 1000;
    neg_vect <- runif(nitems);
    pos_vect <- runif(nitems);
    labels <- sample(c(1,-1), nitems, replace=TRUE);
    res <- list(alpha = pi/4, c = runif(1), Fscore = runif(1), pos_half = runif(1))
    optimize_pos_above(pos_vect, neg_vect, labels, res);    
    checkTrue(res$alpha >= 0 && res$alpha < pi/2)
    checkTrue(is.numeric(res$c))
    checkTrue(res$Fscore >= 0 && res$Fscore <= 1)   
}


