library(COSNet)
test_cosnet.cross.validation <- function() {
    nrow <- 500;
    ncol <- 500;
    nfolds <- 10;
    cost <- 0.0001
    W <- matrix(runif(nrow*ncol), nrow=nrow);
    rownames(W) <- colnames(W) <- 1:nrow;
    labels <- matrix(sample(c(1,-1), nrow, replace=TRUE), nrow=nrow, ncol = 1);    
    rownames(labels) <- 1:nrow
    colnames(labels) <- "c1"
    res <- cosnet.cross.validation(labels, W, nfolds, cost);    
    checkTrue(checkEquals(min(res$predictions), 1) || checkEquals(min(res$predictions), -1))
    checkTrue(checkEquals(max(res$predictions), 1) || checkEquals(max(res$predictions), -1))
    checkEquals(sign(res$predictions), sign(res$scores))     
}


