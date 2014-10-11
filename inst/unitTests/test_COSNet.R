library(COSNet)
test_COSNet <- function() {
    nrow <- 500;
    ncol <- 500;
    ntest <- 50;
    W <- matrix(runif(nrow*ncol), nrow=nrow);
    rownames(W) <- colnames(W) <- 1:nrow;
    labels <- sample(c(1,-1), nrow, replace = TRUE);
     names(labels) <- 1:nrow
    test <- sample(1:nrow, ntest);
    labels[test] <- 0;
    res <- COSNet(W, labels);
    checkTrue(checkEquals(min(res$pred), 1) || checkEquals(min(res$pred), -1))
    checkTrue(checkEquals(max(res$pred), 1) || checkEquals(max(res$pred), -1))
    checkEquals(sign(res$pred), sign(res$scores))
    checkTrue(res$alpha >= 0 && res$alpha < pi/2)
    checkTrue(is.numeric(res$c))
    checkTrue(res$Fscore >= 0 && res$Fscore <= 1)
    checkTrue(is.numeric(res$iter))     
}


