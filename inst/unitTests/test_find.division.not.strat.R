library(COSNet)
test_find.division.not.strat <- function() {
    nitems <- 1000
    vect <- runif(nitems);    
    nfolds <- 10;    
    folds <- find.division.not.strat(vect, nfolds);    
    checkEquals(length(folds), nfolds)     
    checkEquals(sum(unlist(lapply(folds, length))), nitems)         
}


