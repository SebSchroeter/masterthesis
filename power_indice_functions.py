import pandas as pd
import numpy as np
import time 

## these functions follow the logic of the "The Coleman-Shapley-index: Being decisive within the coalition of the interested" Paper, especially Appendix A. 
## this is a rebuild of the ssi and pbi generating functions from "powerindices" which can be pip installed
## I refer to the documentation on https://github.com/frankhuettner/powerindices and credit for the algorithm goes to Hüttner, Frank

""" def relevant_parties_for_a_year(min_win_coal_dict,year):
    relevant_parties=set()
    for (yr,coal),_ in min_win_coal_dict.items(): 
        if yr==year: 
            parties = coal.split('+')
            relevant_parties.update(parties)
    return relevant_parties """
    ##basic variables
def minimal_winning_coalitions_for_a_year(min_win_coal_dict,year): 
    min_win_coals_year=[]
    for (yr,coal),_ in min_win_coal_dict.items(): 
        if yr==year:
            min_win_coals_year.append(coal)
    return min_win_coals_year 

def mincardinality(min_win_coals_year): 
    cardinalities = []
    for i in min_win_coals_year: 
        parties = i.split('+')
        cardinalities.append(len(parties))
    min_cardinality = min(cardinalities)
    return min_cardinality

def grab_relevant_weights(weights_with_names_dict,year): 
    '''currently does not work with self.optimal_seats'''
    weights_dict={}
    for (yr,party),seats in weights_with_names_dict.items(): 
        if yr==year:
            weights_dict[party]=seats
    return weights_dict

def weight_dict_to_array(weights_dict): 
    '''weights should be real numbers, for tuples only considers the first value'''
    weights = np.zeros(len(weights_dict),dtype=np.int64)
    for i,(party,weight) in enumerate(weights_dict.items()):
        if isinstance(weight,tuple):
            weights[i]=weight[0]
        else:
            weights[i]=weight
    return weights

##sort and count all coals by weight and #(coal)
def coals(W,Q,q,n): 
    '''input: passed '''
    '''output: M as an np.array of dim(Q+1,n+1) where M[x][y] stores how many winning coalitions exist with weight=x and #(coal)=y'''
    ## similar to number_coalitions_weighting_x_having_size_s
    M=np.zeros((Q+1,n+1),dtype=np.int64) ## equivalent to C 
    M[Q,n]=1 # Grand Coalition exact once
    
    dynamic_change = np.maximum(q,Q-np.cumsum(W),dtype=np.int64)+W
    for i in range(n): 
        M[dynamic_change[i]-W[i]:Q+1-W[i],:n]+= M[dynamic_change[i]:Q+1,1:n+1]
    return M

def coals_with_i(w_i,Q,q,n,M,i): 
    '''input: passed'''
    '''output: M as an np.array of dim(Q+1,n+1) where M[x][y] stores how many winning coalitions including i exist with weight=x and #(coal)=y'''
    ## similar to number_coalitions_weighting_x_having_size_s_including_i
    M_only_i = np.zeros((Q+1,n+1),dtype=np.int64)
    M_only_i[Q-w_i+1:Q+1,:]=M[Q-w_i+1:Q+1,:] #get M_only_i from M (take only those rows which have at least weigth Q-w_i)
    if w_i==0: 
        M_only_i[Q,n]=M[Q,n] # if weight is 0, then there is never a change in counts
    for row in range(Q-w_i,q-1,-1): #iterate backwards through last rows
        for col in range(n-1,-1,-1): #iterate backwards though relevant cols 
            M_only_i[row,col]=M[row,col]-M_only_i[row+w_i,col+1] #dynamically iterate 
    return M_only_i
            
def pb_i(weights,min_cardinality):
    '''almost correct but not finished ''' 
    W= np.array(weights)
    Q=sum(W)
    q=round((Q+1)/2) 
    n=len(weights)
    scaling = (1/2)*(n-1)
    pb_list = []
    M = coals(W,Q,q,n)
    for i in range(n):
        w_i=W[i]
        M_only_i= coals_with_i(w_i,Q,q,n,M,i)  
        pb_counter = 0 
        for cols in range(min_cardinality-1,n): 
            pb_counter+= M_only_i[q:q+w_i,cols+1].sum(axis=0)
        pb_counter+= M_only_i[q+w_i:Q+1,min_cardinality].sum(axis=0)
        pb_counter= pb_counter*scaling
        pb_list.append(pb_counter)
    return pb_list
        