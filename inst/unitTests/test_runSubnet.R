library(COSNet)
test_runSubnet <- function() {
    nrow <- 500;
    ncol <- 500;
    ntest <- 50;
    cost <- 0.0001;
    alpha_value <- 1;
    c_value <- 0;
    W <- matrix(runif(nrow*ncol), nrow=nrow);
    rownames(W) <- colnames(W) <- 1:nrow;
    labels <- sample(c(1,-1), nrow, replace = TRUE);
    names(labels) <- 1:nrow
    test <- sample(1:nrow, ntest);
    labels[test] <- 0;
    res <- runSubnet(W, labels, alpha_value, c_value, cost)
    checkTrue(checkEquals(min(res$state), 1) || checkEquals(min(res$state), -1))
    checkTrue(checkEquals(max(res$state), 1) || checkEquals(max(res$state), -1))
    checkEquals(sign(res$state), sign(res$scores))     
    checkTrue(is.numeric(res$iter))     
}


