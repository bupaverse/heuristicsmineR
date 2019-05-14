// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <algorithm>
#include <unordered_map>
#include <boost/functional/hash.hpp>
using namespace Rcpp;

typedef std::pair<unsigned, unsigned> act_pair;

// [[Rcpp::export]]
DataFrame count_parallel_lifecycle(CharacterVector cases,
                                   IntegerVector activities,
                                   IntegerVector lifecycle) {

  if (cases.size() != activities.size() ||
    cases.size() != lifecycle.size()) {
    stop("Inputs should be of equal length!");
  }

	CharacterVector levels = activities.attr("levels");

	std::unordered_map<act_pair, unsigned, boost::hash<act_pair>> counts;

	String curCase;

	for (int i = 0; i < cases.size(); ++i) {
		String c = cases[i];
		int act = activities[i];
		int lc = lifecycle[i];

		if ((i+1) < activities.size()) {
		  if (lc == 1) { // start lifecycle
    	  for (int j = i+1; j < cases.size(); ++j) {
          int leadAct = activities[j];
  		    int leadLc = lifecycle[j];
          if ((leadLc == 2 && leadAct == act) || // complete for same
              (cases[i+1] != c)) { // new case
            break; // TODO check same activity instance identifer
          } else if (leadLc == 1) {
            // undirectional relation
            counts[act_pair(act, leadAct)]++;
            counts[act_pair(leadAct, act)]++;
          }
    	  }
		  }
    }

		curCase = c;
	}

	unsigned len = counts.size();
	IntegerVector antecedent(len);
	IntegerVector consequent(len);
	IntegerVector n(len);
	unsigned int count = 0;

	for (auto it = counts.begin(); it != counts.end(); ++it) {

		act_pair pair = it->first;
		antecedent[count] = pair.first;
		consequent[count] = pair.second;
		n[count] = it->second;

		count++;
	}


	CharacterVector antLev(levels.size() + 1);
 	for(int i = 0; i < levels.size(); ++i) {
    	antLev[i] = levels[i];
  	}
 	antLev[levels.size()] = "Start";
	antecedent.attr("levels") = antLev;
  antecedent.attr("class") = "factor";


  CharacterVector conLev(levels.size() + 1);
 	for(int i = 0; i < levels.size(); ++i) {
    	conLev[i] = levels[i];
  	}
 	conLev[levels.size()] = "End";
  consequent.attr("levels") = conLev;
  consequent.attr("class") = "factor";

	return DataFrame::create(Named("antecedent")=antecedent,
                           Named("consequent")=consequent,
                           Named("n")=n);
}
