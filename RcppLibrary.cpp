#include <Rcpp.h>
#include <cstring>

using namespace Rcpp;

// [[Rcpp::export]]
double Rcpp_parse_timing(const std::string str) {
  char* cs = new char[str.length() + 1];
  strcpy(cs, str.c_str());
    
  char* tok = strtok(cs, ":");
  float val = 0;
  while (tok != NULL) {
    val = 60 * val + atof(tok);
    tok = strtok(NULL, ":");
  }
  
  delete[] cs;
    
  return val;
}

// [[Rcpp::export]]
std::vector<double> Rcpp_apply_parse_timing(const std::vector<std::string> xs) {
  std::vector<double> ys(xs.size());
  for (int i = 0; i < xs.size(); i++) {
    ys[i] = Rcpp_parse_timing(xs[i]);  
  }
  return ys; 
}

/*** R
*/
