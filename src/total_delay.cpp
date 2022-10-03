#include <Rcpp.h>
using namespace Rcpp;

NumericMatrix DFtoNM(DataFrame x) {
  int nRows=x.nrows();  
  NumericMatrix y(nRows,x.size());
  for (int i=0; i<x.size();i++) {
    y(_,i)=NumericVector(x[i]);
  }  
  return y;
}

// objective function (for external use)
// [[Rcpp::export]]
List TotalDelay(DataFrame SepTimes, DataFrame FlightsScheduled, IntegerVector sequence){
  
  NumericMatrix septimes = DFtoNM(SepTimes);
  NumericMatrix flights = DFtoNM(FlightsScheduled);
  
  int nFlights = flights.nrow();
  
  /* adapting sequence to c++ indices */
  sequence = sequence - 1;
  
  NumericMatrix f(nFlights, 2);
  NumericVector alt(nFlights);
  
  int i, j;
  bool change;
  
  /* sorting flights matrix according to sequence */
  for(i=0; i < nFlights; i++){
    f(i, _) = flights(sequence(i), _);
  }
  
  /* finding real landing times */
  alt(0) = f(0,1);
  
  for(i=1; i < nFlights; i++){
    
    j = 0;
    change = TRUE;
    while(change==TRUE){
      
      if(septimes(j,0) == f(i-1,0) & septimes(j, 1) == f(i,0)){
        
        if(f(i,1) >= alt(i-1) + septimes(j,2)){
          alt(i) = f(i,1);
        }else{
          alt(i) = alt(i-1) + septimes(j,2);
        }
        
        change = FALSE;  
        
      }else{
        j++;
      }
    }
  }
  
  int totDelay;
  totDelay = 0;
  
  for(i=0; i < nFlights; i++){
    totDelay = totDelay + alt(i) - f(i, 1);
  }
  
  List Result = List::create(Named("at") = alt, _["delay"] =totDelay);
  return(Result);
  
}