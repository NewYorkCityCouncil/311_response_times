// [[Rcpp::depends(RcppProgress)]]
#include <progress.hpp>
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector find_dups(IntegerVector unique_ids, StringVector call_types, StringVector locations, double window_size) {
  
  double n = call_types.size();
  
  NumericVector dup_ids(n);
  
  Progress p(n, true);
  
  for(double i = 0; i < n; i++) {
    
    // Check for stop
    if (int(i) % 5 == 0) {
      if (Progress::check_abort()) {
        stop("Terminated by user");
      }
    }
    
    // For every observation look through the next window_size observations
    // (or to the end)
    for (double j = i; j < std::min(i + window_size, n); j++) {
      
      if (dup_ids[j] == 0 && // Not alread a dup
         call_types[j] == call_types[i] && // same complaint_type
         locations[j] == locations[i]) { // same locations 
        dup_ids[j] = unique_ids[i]; // observation j is a dup of observation i
      }
    }
    
    p.increment();
  }
  
  // Any non-dups should be tagged with their own id
  // for(int i = 0; i < n; i++) {
  //   if(dup_ids[i] == 0) {
  //     dup_ids[i] = unique_ids[i];
  //   }
  // }
  
  
  
  return dup_ids;
  
}

