// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <algorithm>
#include <unordered_map>
#include <boost/functional/hash.hpp>
using namespace Rcpp;

typedef std::pair<unsigned, unsigned> act_pair;

// [[Rcpp::export]]
DataFrame count_precedence(CharacterVector cases,
                           IntegerVector activities,
                           int lead) {

  if (cases.size() != activities.size()) {
    stop("Inputs should be of equal length!");
  }

	CharacterVector levels = activities.attr("levels");
	int startIdx = levels.size() + 1;
	int endIdx = levels.size() + 1;

	std::unordered_map<act_pair, unsigned, boost::hash<act_pair>> counts;

	String curCase;

	for (int i = 0; i < cases.size(); ++i) {
		String c = cases[i];
		int act = activities[i];

		int leadIdx = i+lead;
		int leadAct = -1;
		if (leadIdx < activities.size()) {
		  if (cases[i+lead] != c) { // i+lead is safe
		    // End event
		    leadAct = endIdx;
		  } else {
		    // Normal
        leadAct = activities[leadIdx];
		  }
		}

		if (leadAct != -1) {
		  if (c != curCase) {
		    // Start event
		    counts[act_pair(startIdx, act)]++;
		  }
			counts[act_pair(act, leadAct)]++;
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
