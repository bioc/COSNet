library(COSNet)
test_find.division.strat <- function() {
    nitems <- 1000       
    nfolds <- 10;
    labels <- sample(c(1,-1), nitems, replace=TRUE);    
    npos <- sum(labels>0);        
    cat(npos/nitems, "\n");
    folds <- find.division.strat(labels, 1:nitems, nfolds);    
    checkEquals(length(folds), nfolds)     
    checkEquals(sum(unlist(lapply(folds, length))), nitems)
	checkEqualsNumeric(sum(unlist(lapply(folds, function(x){return(checkEqualsNumeric(sum(labels[x]>0)/length(x), npos/nitems, tolerance=1.0e-1))}))), 10);
}


