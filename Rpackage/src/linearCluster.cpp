#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
List linearCluster(NumericVector x, double tolerance, NumericVector w = NULL) {
    Function order("order");
    Function rep("rep");

    if (!w) {
        w = rep(1.0, Named("times") = x.size());
    } else if (w.size() != x.size()) {
BEGIN_RCPP

        std::string err = "the vector size of 'x'(%s) should be equals to the vector size of 'w'(%s)!";
        err = std::sprintf(err, x.size(), w.size());

        throw(Rcpp::exception(err, "linearCluster.cpp", 11));
        return NULL;

END_RCPP
    }

    IntegerVector i = order(x);
    
    x = x[i];
    w = w[i];

    return NULL;
}