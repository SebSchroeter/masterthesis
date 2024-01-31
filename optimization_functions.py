import pandas as pd
import numpy as np
import itertools
from scipy import optimize
from scipy.optimize import LinearConstraint
from scipy.optimize import milp


def create_all_year_dfs(winning_coal_dict, parties_in_year):
    ##create dataframes fro every year from coalition dict and indicate whether the coal was winning 
    ##stores all dataframes in a dict with key= year
    all_year_dfs = {}

    for (year, coalition), is_winning in winning_coal_dict.items():
        if year not in all_year_dfs:#make new df if not there already
            all_year_dfs[year] = pd.DataFrame(columns=parties_in_year[year] + ['Winning']) #make column for every party and one to indicate if winning

        # Rows
        row = {party: 1 if party in coalition.split('+') else 0 for party in parties_in_year[year]} #logic: write 1 in cell if party is member of the coalition, 0 if not
        row['Winning'] = is_winning # write 1 in winning column if coal was winning

        # Add rows to df
        all_year_dfs[year].loc[coalition] = row

    return all_year_dfs

def generate_constraints_df(df):
    '''outer-loop iterates over each winning coalition and inner-loop iterates over each losing coalition '''
    '''If a party is in the winning coal but not in the losing it gets 1 as constraint.
    If the party is only in the losing coal, it gets -1.
    If a party is in both or neither, it gets 0.
    Adds non-negativity constraints'''
    ## heart of the optimization pipeline 
    ## takes in a df which is a value of all_year_dfs dict
    ## creats new df with party-weights as columns
    ## adds non-neg constraints
    ## adds w(S)>w(R) \for all S \in W, R \notin W constraints 
    ## outputs new df with lhs. of constraints as rows 
    
    parties = df.columns[:-1] 
    constraints_df = pd.DataFrame(columns=parties) #create new empty df with weights as columns  
    constraint_index = 0 # to allow .loc[] instead of .append, basically counts constraints  
    
    winning_coalitions = df[df['Winning'] == 1] # Split original DataFrame into winning and losing coalitions
    losing_coalitions = df[df['Winning'] == 0]
    
    # non-negativity constraints
    for party in parties:
        non_negative_constraint = {p: 1 if p == party else 0 for p in parties} # puts a 1 where respective p is equal to the party and 0 else 
        constraints_df.loc[constraint_index] = non_negative_constraint #adds rows for every constraint
        constraint_index += 1 #update index counter

    # w(S)>w(R) constraints
    for _, winning_row in winning_coalitions.iterrows(): #outerloop with the winning coals
        for _, losing_row in losing_coalitions.iterrows(): #inner loop with the losing coals
            
            # Create a dict for the constraint (party,value) where value \in {-1,0,1}
            constraint_row = {}
            for party in parties: #calculate difference between the winning and the losing coal
                constraint_row[party] = winning_row[party] - losing_row[party] #checks the value of the party in the winning coal (either 1 if present or 0 if not) and the losing coal. Example S={CDU,SPD,B90}, R={SPD,B90,FDP} then CDU=1-0, SPD=1-1, B90=1-1, FDP=0-1. Resulting constraint is w_cdu-w_fdp>0 

            # Add the constraint to the DataFrame
            constraints_df.loc[constraint_index] = constraint_row
            constraint_index += 1
    # Renaming 
    constraints_df.columns = [f'w_{party}' for party in parties]
    
    return constraints_df

def verify_conditions(year, winning_coal_dict, constraints_df, n_in_year):
    ## verifys the above conditions to test if: 
    # a) all coals have been found (2^n)
    # b) all constraints have been found (|W|*(2^n-|W|)+n) 
    # takes in year as int to be tested, a dict with all coals and information whether they were winning, takes is the df storing the constraints of the year, takes in dict that stores n per year
    n = n_in_year[year]
    shouldbe_coals = 2 ** n

    # |W|, number of winning coals
    total_coalitions_in_year = sum(1 for key in winning_coal_dict if key[0] == year) # give a 1 to all elements with key=year and sum them to get number of coalitions (should be tested against the df_by_year as well, tbd.)
    W = sum(value for key, value in winning_coal_dict.items() if key[0] == year) #count 1´s in dict with key=year and sum to get number of winning coals 

    # Check a)
    if total_coalitions_in_year != shouldbe_coals:
        return f"Error: Number of coalitions for {year} does not match 2^n, Coals should be {shouldbe_coals} but found {total_coalitions_in_year}"

    # W * (2^n - W) + n
    shouldbe_constraints = W * (shouldbe_coals - W) + n

    # Check b)
    actual_constraints = len(constraints_df)

    if actual_constraints != shouldbe_constraints:
        return f"Error: Number of constraints for {year} does not match W*(2^n-W)+n, Contraints should be {shouldbe_constraints} but found {actual_constraints}"

    return "Correct"

def get_all_constrains(all_year_dfs):
    #simple loop to get all constraints into one dict
    #takes in all_year_df, returns dict with (year,yearly_constraints_df)
    all_constraints_dict={}
    for year,yearly_dfs in all_year_dfs.items(): 
        yearly_constraints= generate_constraints_df(yearly_dfs)
        all_constraints_dict[year]= yearly_constraints
    return all_constraints_dict
    
    
def get_lin_cons(constraints_df):
    # creates constraints usable for milp
    # gets matrix A from constraints_df
    # first n constraints are lhs>=0, 
    # remaining constraints are lhs>0, since lhs will always be integer-valued this is equivalent to lhs>=1 
    # returns lin_constraint element   
    A = constraints_df.to_numpy()
    lbnd = np.zeros(len(constraints_df)) # non-negativity constraints 
    lbnd[constraints_df.shape[1]:] = 1 #set all remaining lower bounds to 1
    upbnd = np.full(len(constraints_df), np.inf) #no upper bound 

    lin_cons = LinearConstraint(A, lbnd, upbnd)

    return lin_cons
def get_all_lin_cons(all_constraints_dict): 
    ##simple loop again to transform all constraints into linear constraints
    all_lin_cons_dict={}
    for year,yearly_constraints in all_constraints_dict.items():
        constraints = yearly_constraints
        lin_cons = get_lin_cons(constraints)
        all_lin_cons_dict[year]=lin_cons
    return all_lin_cons_dict
     
def get_min_vote_weights(year,n_in_year,constraints): 
    ## passes constraints to optimizer
    ## sets coefficients to 1 (since we need an unweighted sum to be minimized), sets all weights to be full-integers
    ## returns optimization object 
    mvw = optimize.milp(np.full(n_in_year[year],1), integrality=np.full(n_in_year[year],1), constraints=constraints)
    return mvw

def get_all_min_vote_weights(all_lin_cons_dict,n_in_year): 
    #simple loop over all opimization problems
    all_min_vote_weights = {}
    for year,lin_cons in all_lin_cons_dict.items(): 
        yearly_lincons = lin_cons
        yearly_mvw_results = get_min_vote_weights(year,n_in_year,yearly_lincons)
        yearly_mvws = yearly_mvw_results.x
        all_min_vote_weights[year]=yearly_mvws
    return all_min_vote_weights

def mvw_to_parties(year, optimizer_results, parties_in_year):
    ## takes as input milp object and party dict as well as year. 
    ## zips optimal weights to repsective parties 
    weights = optimizer_results.x
    optimized_seats = {party: result for party, result in zip(parties_in_year[year], weights)} #object.x stores the values 

    return optimized_seats

def get_all_optimized_seats(all_min_vote_weights,parties_in_year,winning_coal_dict): 
    # does what mvw_to_parties does but now for the dict from get_all_min_vote_weights
    all_optimized_Seats = {}
    for year,optimized_Seats in all_min_vote_weights.items(): 
        yearly_optimized_Seats = optimized_Seats
        yearly_matching =  {party: result for party, result in zip(parties_in_year[year], yearly_optimized_Seats)} 
        all_optimized_Seats[year]=yearly_matching
    ##submodul to verify whether seats are correct: 
    for year, optimized_Seats in all_optimized_Seats.items(): 
        yearly_optimized_Seats= optimized_Seats
        yearly_mw_winning_dict = help_test_mvws(yearly_optimized_Seats)
        test_boolean = test_mvws(year,winning_coal_dict,yearly_mw_winning_dict)
        if test_boolean == False: return f"Error: test_boolean is {test_boolean} for year {year}"
    return all_optimized_Seats

def help_test_mvws(optimized_seats):
    '''helper function for test_mvws'''
    ## creates dict just like winning_coal_dict but from mvw´s, used later to ensure equivalency of games 
    ## takes in dict from mvw_to_parties and uses same logic as coalition_combinatorics_generator and win_coals but for dicts
    
    mw_winning_coal_dict = {}
    total_seats = sum(optimized_seats.values()) #new Q
    parties = list(optimized_seats.keys())

    # Generate all unique coalitions
    mw_winning_coal_dict[''] = 0
    for r in range(1, len(parties) + 1):
        for combo in itertools.combinations(parties, r):
            coalition = '+'.join(combo) #name of coalition 
            coalition_seats = sum(optimized_seats[party] for party in combo) #seats is sum of their weights 
            mw_winning_coal_dict[coalition] = 1 if coalition_seats > (total_seats / 2) else 0 #indicate winning or losing (again, with strict inequality)

    return mw_winning_coal_dict


def test_mvws(year, winning_coal_dict, mw_winning_coal_dict):
    ## takes in mw_winning_coal_dict and compares it to winning_coal_dict[year] 
    ## if the set of coalitions is the same and if all coalitions have the same value, the game is identical and thus the mvs represent the same game as the original parliament 
    ## returns boolean  
    winning_coals_by_year = {coalition: value for (yr, coalition), value in winning_coal_dict.items() if yr == year} # bit convoluted since I need 'year' twice, basically just separates the relevant year from the winning_coal_dict

    # Compare 
    for coalition, value in winning_coals_by_year.items():
        if coalition not in mw_winning_coal_dict or mw_winning_coal_dict[coalition] != value:
            return False

    return True