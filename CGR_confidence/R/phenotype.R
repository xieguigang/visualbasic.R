sample.phenotype <- function(sample, kb) {
    markers <- names(sample);
    phenotype <- lapply(markers, function(name) {
        phenotype(sample[[name]], kb[[name]]);
    });

    names(phenotype) <- markers;
    phenotype;
}

phenotype <- function(x, genotype) {

}