import pandas as pd
import numpy as np
import itertools
from itertools import combinations
import time 

## these functions follow the logic of the paper "Power indices and minimal winning coalitions"
def relevant_parties_for_a_year(min_win_coal_dict,year):
    relevant_parties=set()
    for (yr,coal),_ in min_win_coal_dict.items(): 
        if yr==year: 
            parties = coal.split('+')
            relevant_parties.update(parties)
    return relevant_parties
def minimal_winning_coalitions_for_a_year(min_win_coal_dict,year): 
    min_win_coals_year=[]
    for (yr,coal),_ in min_win_coal_dict.items(): 
        if yr==year:
            min_win_coals_year.append(coal)
    return min_win_coals_year
def BS_w(min_win_coals_year,relevant_parties): # to stay in notation of the paper, indicates the Banzhaf score of a voter w -> in our case score of a party 
    steps=len(min_win_coals_year)
    BS={}
    coalition_sets = {}
    for i in range(1,steps+1): 
       coalition_sets[i]= union_of_n_mwcs_as_sets(min_win_coals_year, i)
    for party_i in relevant_parties:
        BS_i = 0
        for i in range(1,steps+1): #We need a step for any value in steps (1,2,...,len(min_win_coals_year))
            coalition_sets_with_i = [coalition_set for coalition_set in coalition_sets[i] if party_i in coalition_set] #in words: list every coalition set from coalition_sets[i] that includes party i
            change_of_BS_i = sum(2**(steps - len(coalition_set)) for coalition_set in coalition_sets_with_i) #in words: for every set in coalition_sets_with_i calculate $2^{n-#U(...)}$ and add them all togehter
            if i % 2 == 1:  #  if i odd: add change_of_BS_i
                BS_i += change_of_BS_i
            else:  # if i even: substract change_of_BS_i
                BS_i -= change_of_BS_i
        BS[party_i]=BS_i
    return BS
def union_of_n_mwcs_as_sets(min_win_coals_year, n):
    mwc_sets = [set(mwc.split('+')) for mwc in min_win_coals_year]
    combined_coalitions_sets = []
    for mwc_combination in combinations(mwc_sets, n):
        combined_set = set().union(*mwc_combination)
        combined_coalitions_sets.append(combined_set)
    return combined_coalitions_sets        

    