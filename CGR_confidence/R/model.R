
#' Create a knowledge base for confidence test
#'
#' @param samples A sample dataframe, it should have a \code{label} column in logical value mode.
knowledge_base <- function(samples, markers, foldchange = genotype.foldchange()) {
    negative <- (samples[, "label"] %=>% as.vector %=>% as.logical) == FALSE;
    positive <- samples[!negative, ];
    negative <- samples[negative, ];

    kb <- lapply(markers, function(marker) {
        data <- negative[, marker] %=>% as.vector %=>% as.numeric;
        range <- measure.range(samples = data, foldchange = foldchange);
        frequency <- list();
        frequency$low <- (sapply(1:length(foldchange), function(level) {
            count.level(data, range$low, level);
        }) %=>% as.vector) / length(data);
        frequency$high <- (sapply(1:length(foldchange), function(level) {
            count.level(data, range$high, level);
        }) %=>% as.vector) / length(data);

        
    });

    names(kb) <- markers;
    kb;
}

count.level <- function(data, ranges, level) {
    if (level == length(ranges)) {
        if (ranges[[2]] > ranges[[1]]) {
            # high
            test <- sapply(data, function(x) {
                x > ranges[[level]];
            });
        } else {
            # low
            test <- sapply(data, function(x) {
                x < ranges[[level]];
            });
        }
    } else {
        range <- c(ranges[[level]], ranges[[level + 1]]);
        range <- c(min(range), max(range));

        test <- sapply(data, function(x) {
            in_range(x, range[1], range[2]);
        });
    }
    
    test %=>% as.vector %=>% sum;
}