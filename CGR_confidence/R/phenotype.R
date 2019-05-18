sample.phenotype <- function(sample, kb) {
    markers <- names(sample);
    phenotype <- lapply(markers, function(name) {
        phenotype(sample[[name]], kb[[name]]);
    });

    names(phenotype) <- markers;
    phenotype;
}

phenotype <- function(x, genotype) {
    levels <- names(genotype$foldchange);
    normal <- c(genotype$low[[levels[0]]], genotype$high[[levels[1]]]);   
    last <- length(levels);

    if (x < normal[0]) {
        up <- normal[0];
        foldchange <- genotype$low[[levels[last]]];

        # test low
        for (name in levels[2:length(levels)]) {
            y <- genotype$low[[name]];

            if (in_range(x, y, up)) {
                foldchange <- genotype$foldchange[[name]];
                break;
            } else {
                up <- y;
            }
        }
    } else if (x > normal[1]) {
        low <- normal[1];
        foldchange <- genotype$high[[levels[last]]];

        # test high
        for (name in levels[2:length(levels)]) {
            y <- genotype$high[[name]];

            if (in_range(x, low, y)) {
                foldchange <- genotype$foldchange[[name]];
                break;
            } else {
                low <- y;
            }
        }
    } else {
        # (x >= normal[0] && x <= normal[1]) 
        # middle
        foldchange <- genotype$foldchange[[levels[0]]];
    }    

    foldchange;
}

in_range <- function(x, low, up) {
    (x >= low) && (x <= up)
}