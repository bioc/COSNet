library(COSNet)
test_reg_data <- function() {
    nrow <- 500;
    ncol <- 500;    
    eta <- 0.5
    alpha <- pi/4;
    M <- sin(alpha);
    m <- -cos(alpha);
    W <- matrix(runif(nrow*ncol), nrow=nrow);
    W <- (W + t(W))/2;
    pos_num <- round(0.3*nrow);
    theta <- runif(nrow, -1, 1);  
    data <- reg_data(W, theta, eta, M, m, pos_num);
    checkEquals(sum(diag(data$W)), 0);
    checkTrue(isSymmetric(data$W));    
}


