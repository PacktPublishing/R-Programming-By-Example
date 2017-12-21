
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export(sma_cpp)]]
NumericVector smaCPP(int period, NumericVector data) {
    int position, n = data.size();
    NumericVector result(n);
    double sma;
    for (int end = 0; end < n; end++) {
        position = end;
        sma = 0;
        while(end - position < period && position >= 0) {
            sma = sma + data[position];
            position = position - 1;
        }
        if (end - position == period) {
            sma = sma / period;
        } else {
            sma = NA_REAL;
        }
        result[end] = sma;
    }
    return result;
}
