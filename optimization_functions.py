import pandas as pd
import numpy as np
import itertools
from itertools import combinations
import time 
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
        row['Winning'] = is_winning # write 1 in winning column if coal was winning, 0 if it was losing

        # Add rows to df
        all_year_dfs[year].loc[coalition] = row

    return all_year_dfs

def generate_constraints_df(df):
    '''outdated method - very slow for n>10'''
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

def generate_eff_cons(df):
    '''main method to create constraints'''
    ## much faster version of generate_constraints_df
    ##takes in df which is a value of all_year_dfs dict
    ## translates df to np.array 
        # by first defining the size of the array and filling it with 0s
        # writing constraints from winning_array-losing_array row by row 
        # writing non-neg- cons 
        # sending array back to Df
    
    #variables (faster than looking them up)
    parties = df.columns[:-1].tolist()
    n = len(parties) 

    #sep df 
    winning_coalitions = df[df['Winning'] == 1] 
    losing_coalitions = df[df['Winning'] == 0]
    #win/lose_df --> win/lose_array
    winning_array = winning_coalitions.drop(columns=['Winning']).to_numpy()
    losing_array = losing_coalitions.drop(columns=['Winning']).to_numpy()
    #nr of constraints = length of constraints array
    n_constraints = len(winning_array) * len(losing_array) + n
    constraints_array = np.zeros((n_constraints, n),dtype='int') #store constraints in an array, as its much faster than a df, initialize 0´s everywhere
    
    # Non-negativity constraints
    for i, party in enumerate(parties): #enum creates (1,party_1),(2,party_2),... elements thus needs to call i,party even if party is never accessed 
        constraints_array[i, i] = 1 # write a 1 where party=party 
    
    # w(S) > w(R) constraints
    constraint_index = n  # Start after non-negativity cons
    for win_row in winning_array:
        for lose_row in losing_array:
            # #calculate difference between the winning and the losing coal
            constraints_array[constraint_index] = win_row - lose_row #checks the value of the party in the winning coal (either 1 if present or 0 if not) and the losing coal. Example S={CDU,SPD,B90}, R={SPD,B90,FDP} then CDU=1-0, SPD=1-1, B90=1-1, FDP=0-1. Resulting constraint is w_cdu-w_fdp>0 
            constraint_index += 1
            
            
    # array to original df 
    constraints_df = pd.DataFrame(constraints_array, columns=[f'w_{party}' for party in parties])
    
    return constraints_df

def verify_conditions(year, winning_coal_dict, constraints_df, n_in_year):
    '''not to be used when using min_winning_coals to create constraints'''
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

def get_all_constrains(all_year_dfs,find_error= False):
    #simple loop to get all constraints into one dict
    #takes in all_year_df, returns dict with (year,yearly_constraints_df)
    all_constraints_dict={}
    for year,yearly_dfs in all_year_dfs.items(): 
        if find_error: 
            time_1= time.time()
            print(f'year:{year}, started at {time_1} seconds')
        yearly_constraints= generate_eff_cons(yearly_dfs)
        if find_error: 
            time_2= time.time()
            time_diff = time_2-time_1
            print(f'year:{year}, ended at {time_2} seconds, took {time_diff} seconds')       
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
        yearly_mvws =np.round_(yearly_mvw_results.x) ##for some very useful reason milp outputs int64 values. np.round() rounds to the neares integer
        all_min_vote_weights[year]=yearly_mvws
    return all_min_vote_weights

def mvw_to_parties(year, optimizer_results, parties_in_year):
    '''dated function, not used anymore'''
    ## takes as input milp object and party dict as well as year. 
    ## zips optimal weights to repsective parties 
    weights = optimizer_results.x #object.x stores the values 
    optimized_seats = {party: result for party, result in zip(parties_in_year[year], weights)} #

    return optimized_seats

def get_all_optimized_seats(all_min_vote_weights,parties_in_year): 
    # does what mvw_to_parties does but now for the dict from get_all_min_vote_weights
    # takes in boolean verify to check if mvw actually represent the full parliament--> slow for big parliaments 
    all_optimized_Seats = {}
    for year,optimized_Seats in all_min_vote_weights.items(): 
        yearly_optimized_Seats = optimized_Seats
        yearly_matching =  {party: result for party, result in zip(parties_in_year[year], yearly_optimized_Seats)} 
        all_optimized_Seats[year]=yearly_matching
    ##submodul to verify whether seats are correct:   
    return all_optimized_Seats

def verify_coals(all_optimized_Seats,winning_coal_dict): 
    test_dict={}
    errors= {}
    
    for year, yearly_matching in all_optimized_Seats.items(): 
            yearly_optimized_Seats= yearly_matching
            yearly_mw_winning_dict = help_test_mvws(yearly_optimized_Seats)
            test_boolean,wrong_coals = test_mvws(year,winning_coal_dict,yearly_mw_winning_dict)
            test_dict[year]=test_boolean
            errors[year]=wrong_coals
    return test_dict,errors            

def help_test_mvws(optimized_seats):
    '''helper function for test_mvws'''
    ## creates dict just like winning_coal_dict but from mvw´s, used later to ensure equivalency of games 
    ## takes in dict from mvw_to_parties and uses same logic as coalition_combinatorics_generator and win_coals but for dicts
    
    mw_winning_coal_dict = {}
    total_seats = sum(optimized_seats.values()) #new Q
    parties = list(optimized_seats.keys())

    # Generate all unique coalitions
    mw_winning_coal_dict[''] = 0 #add empty coal manually 
    for r in range(1, len(parties) + 1):
        for combo in itertools.combinations(parties, r):
            coalition = '+'.join(combo) #name of coalition 
            coalition_seats = sum(optimized_seats[party] for party in combo) #seats is sum of their weights 
            mw_winning_coal_dict[coalition] = 1 if coalition_seats > (total_seats / 2) else 0 #indicate winning or losing (again, with strict inequality)

    return mw_winning_coal_dict


def test_mvws(year, winning_coal_dict, mw_winning_coal_dict):
    ## takes in mw_winning_coal_dict and compares it to winning_coal_dict[year] 
    ## if the set of coalitions is the same and if all coalitions have the same value, the game is identical and thus the mvs represent the same game as the original parliament 
    ## returns boolean as well as dict with errors:
    winning_coals_by_year = {coalition: value for (yr, coalition), value in winning_coal_dict.items() if yr == year} # bit convoluted since I need 'year' twice, basically just separates the relevant year from the winning_coal_dict
    errors = {}
    # Compare 
    for coalition, value in winning_coals_by_year.items():
        if coalition not in mw_winning_coal_dict or mw_winning_coal_dict[coalition] != value:
            errors[(year,coalition)]=(value,mw_winning_coal_dict.get(coalition,'not in here')) #writes to a coalition the value from the winning coal and the min_winning_coal (or indicates that the coal is not actually mi-w)
    check = len(errors)==0
    return check, errors

def collect_all_representations(weights, year, n_in_year, constraints_df, find_error= False):
    '''reports all possible sets of weights given a found minimal sum, up to changes of +1/-1 seats'''
    ##takes in a vector of weights such as an element from all_min_vote_weights, a year and the standard dict n_in_year, takes in a constraint df such as an element from the all_constraints_dict
    # creates a list of all other possbile weight vectors with the same sum and changes of +1/-1 for any weight
    # tests every one of them to be a solution to the same optimization problem 
    # returns a list of all possible weights  
    # find_error prints time to test %ages of all possible combinations
    alternative_weights = possible_other_weights(weights)
    optimal_weights = []
    total_weights = len(alternative_weights)
    one_percent = round(total_weights / 100)
    start_time= time.time()
    for i, alt_weights in enumerate(alternative_weights):
        lin_cons = alt_lin_cons(alt_weights, constraints_df)
        mvw_alt = get_min_vote_weights(year, n_in_year, lin_cons)
        if mvw_alt.status == 0:
            optimal_weights.append(alt_weights)
        if find_error and ((i + 1) % one_percent == 0 or i == total_weights - 1): ## exclude annoying division by 0 error with i == total_weights - 1
            current_time = time.time()
            elapsed_time = current_time - start_time
            print(f"Tested {((i + 1) / total_weights) * 100:.2f}% of {total_weights} elements in {elapsed_time:.2f} seconds")
    return optimal_weights    

def possible_other_weights(minimal_weights):
    '''helper function for collect_all_representations'''
    #takes in minimal weights from collect_all_representations
    #iterates over all possible combinations from adding 1 to a weigth and substracting 1 form a different one
    #returns a list of all possible alternative weights 
    
    weight = np.round(minimal_weights).astype(int) # strictly not really necessary....
    
    # Get unique pairs from ogiginal weights (if w=(1,2,3) this returns ((1,2),(1,3),(2,3)))
    pairs = list(combinations(range(len(weight)), 2)) # get all possbible pairs of weights 
    
    alt_weights = [] 
    
    for i, j in pairs: 
        for change in [(1, -1), (-1, 1)]: #any change is either (i+1,j-1) or (i-1,j+1)
            new_weights = weight.copy()
            if new_weights[i] + change[0] >= 0 and new_weights[j] + change[1] >= 0: #test non-negativity
                new_weights[i] += change[0]
                new_weights[j] += change[1]
                if not any(np.array_equal(new_weights, arr) for arr in alt_weights): #test whether new weights already in the list
                    alt_weights.append(new_weights)
    alt_weights.append(weight) #add original weights to the list
    return alt_weights

def alt_lin_cons(minimal_weights, constraints_df):
    '''helper function for collect_all_representations'''
    #takes in minimal_weights and constraints_df from collect_all_representations 
    #sets the lower and upper bounds for the non-negativity constraints to be the weights to be tested
    #returns linear cosntraint object
    A = constraints_df.to_numpy()
    if isinstance(minimal_weights, list): # necessary from output of possible_other_weights function 
        weights = minimal_weights[0]
    else: weights=minimal_weights
    lbnd = np.zeros(len(constraints_df))  
    lbnd[:len(weights)] = weights  
    lbnd[len(weights):] = 1  
    upbnd = np.full(len(constraints_df), np.inf) 
    upbnd[:len(weights)] = weights  
    lin_cons = LinearConstraint(A, lbnd, upbnd)

    return lin_cons

def all_year_all_possible_weights(all_min_weights,n_in_year,all_constraints,find_error = False): 
    ##lets collect_all_representations function run over all years
    
    all_year_all_weights = {}
    for year,weights in all_min_weights.items(): 
        yearly_weights = weights
        if len(yearly_weights)>8: #test whether more than 8 players are in the game, for all games with <=8 players min integers are always unique
            constraints=all_constraints.get(year)
            all_yearly_weights=collect_all_representations(yearly_weights,year,n_in_year,constraints,find_error)
            all_year_all_weights[year]=all_yearly_weights
        else: all_year_all_weights[year]=yearly_weights
    return all_year_all_weights

def mvw_to_parties2(all_year_all_weights, parties_in_year):
    ## does what get_all_optimized_seats does but for collect_all_representations lists of weights 
    ##needs some serious polishing 
    possible_weights = {}
    for year, yearly_weight_lists in all_year_all_weights.items():
        for i, party in enumerate(parties_in_year[year]):
            party_weights = [weight_list[i] for weight_list in yearly_weight_lists]
            possible_weights[(year, party)] = tuple(party_weights)

    return possible_weights
