library(COSNet)
test_generate_labels <- function() {
    nitems <- 1000       
    pos.rate <- 0.2;
    labels <- generate_labels(nitems, pos.rate);       
    checkEquals(length(labels), nitems)     
    checkTrue(min(labels) == -1 || min(labels) == 1);
    checkTrue(max(labels) == -1 || max(labels) == 1);
}


