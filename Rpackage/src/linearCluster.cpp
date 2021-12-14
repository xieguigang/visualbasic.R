#include <Rcpp.h>
using namespace Rcpp;

NumericVector cast(int size, Nullable<NumericVector> x = R_NilValue) {
    if (x.isNotNull()) {
        NumericVector X(x);
        return X;
    } else {
        NumericVector X(size, 1.0);
        return X;
    }
}

//[[Rcpp::export]]
List linearCluster(NumericVector x, double tolerance, Nullable<NumericVector> weight = R_NilValue) {
    Function order("order");
    Function rep("rep");

    NumericVector w = cast(x.size(), weight);

    if (w.size() != x.size()) {
        String err("the vector size of 'x'(%s) should be equals to the vector size of 'w'(%s)!");
        stop(R::sprintf(err, std::to_string(x.size()), std::to_string(w.size())));

        return NULL;
    }

    IntegerVector i = order(x);
    
    // reorder of the input x and corresponding w weights
    x = x[i];
    w = w[i];

    return NULL;
}

