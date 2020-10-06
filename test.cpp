#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector timesTwo(NumericVector x) {
  return x * 2;
}
// [[Rcpp::export]]
NumericVector timesThree(NumericVector x) {
  return x * 3;
}


// [[Rcpp::export]] 
//=> obligatoire pour exporter la fonction
float cppRejection(float f (float), int a, int b, int M) {
  while (TRUE) {
    //float x = a + (b-a)*rand(1);
    float x = a + (b-a) * R::runif(0,1);
    //float x = R::runif(1,a,b)
    float y = M * R::runif(0,1);
    if (y < f(x)) {return(x);}
  }
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
#timesTwo(42)#
*/
