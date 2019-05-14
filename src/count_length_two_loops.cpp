// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <algorithm>
#include <unordered_map>
#include <boost/functional/hash.hpp>
using namespace Rcpp;

typedef std::pair<unsigned, unsigned> act_pair;

// [[Rcpp::export]]
DataFrame count_length_two_loops(CharacterVector cases, IntegerVector activities) {

  if (cases.size() != activities.size()) {
    stop("Inputs should be of equal length!");
  }

	std::unordered_map<act_pair, unsigned, boost::hash<act_pair>> counts;

	String curCase;
	int aLead = 2;
	int bLead = 1;

	for (int i = 0; i < cases.size(); ++i) {
		String c = cases[i];
		int act = activities[i]; // safe as equal length

		int aIdx = i+aLead;
		int bIdx = i+bLead;

		if (aIdx < cases.size() && bIdx < cases.size()) { // guard against overflow
		  if (cases[aIdx] == c) {
		    // Same case
		    int aAct = activities[aIdx];
		    int bAct = activities[bIdx];
        if (aAct == act && aAct != bAct) {
          counts[act_pair(aAct, bAct)]++;
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

	CharacterVector levels = activities.attr("levels");

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
