
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
        frequency <- counts(data, range, foldchange);
        frequency$low <- frequency$low / length(data);
        frequency$high <- frequency$high / length(data);

        data <- positive[, marker] %=>% as.vector %=>% as.numeric;
        risk <- counts(data, range, foldchange);
        risk$low <- lapply(risk$low, function(x) log(x+1));
        risk$high <- lapply(risk$high, function(x) log(x+1));

        list(freq = frequency, risk = risk %=>% impute_missing.by_min);
    });

    names(kb) <- markers;
    kb;
}

impute_missing.by_min <- function(risk) {
    names <- names(risk$low);
    all <- sapply(names, function(name) risk$low[[name]]) %=>% as.vector;
    all <- all %+% (sapply(names, function(name) risk$high[[name]]) %=>% as.vector);
    all <- all[all > 0];
    min <- min(all);

    for (name in names) {
        if (risk$low[[name]] == 0) {
            risk$low[[name]] <- min;
        }
        if (risk$high[[name]] == 0) {
            risk$high[[name]] <- min;
        }
    }

    risk;
}

counts <- function(data, range, foldchange) {
    list(low = sapply(1:length(foldchange), function(level) {
            count.level(data, range$low, level);
        }) %=>% as.vector, 
        high = sapply(1:length(foldchange), function(level) {
            count.level(data, range$high, level);
        }) %=>% as.vector
    );
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