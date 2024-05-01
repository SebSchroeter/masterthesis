import os
import pandas as pd
import itertools

def read_csv_to_dataframe(filename, encoding='utf-16',delimiter='\t'):
    ## takes in the filename as 'xy.csv' and allows for an encoding parameter (uft-16 is apparently used by the Political Data Yearbook)
    ## csv files are stroed in folder 'data'

    file_path = os.path.join('data', filename)
    try:
        dataframe = pd.read_csv(file_path, delimiter=delimiter, encoding=encoding)
        return dataframe
    except UnicodeDecodeError:
        print(f"UnicodeDecodeError: The file might not be in {encoding} encoding.")
        return None
    except Exception as e:
        print(f"An error occurred: {e}")
        return None


def transform_and_sort_dataframe(df):
    ## takes in a dataframe from read_csv_to_dataframe
    ## drops empty rows 
    ## substitutes "+" signs in party name for "plus" since many functions use '+'.join(combi) syntax and thus join parties at false places 
    ## strips partys with 0 seats
    ## accounts for multiple elections per year (hopefully)
    ## sorts by year-party
    df.dropna(axis=0,how='any',inplace=True)
    df = df[df['# of Seats'] != 0].copy()
    #df['Party'] = df['Party'].str[:30]
    df['Date']=df['Date'].apply(str) # force str formatting
    df['Party'] = df['Party'].str.replace('+', 'plus', regex=False)
    #try df magic:
    df['YearMonth'] = pd.to_datetime(df['Date'], errors='coerce', format='%d. %b %y').dt.strftime('%Y-%m')
    #whenever magic fails: 
    df['YearMonth'] = df['YearMonth'].fillna(df['Date'].str[-7:])
        #df['Year'] = df['Year'].apply(lambda x: '19' + x if int(x) > 75 else '20' + x) # Adding '19' or '20' to make it full years
    df = df[['YearMonth', 'Party', '# of Seats']].copy()
    df.sort_values(by=['YearMonth', 'Party'], inplace=True)

    return df

def variables_by_year(df):
    ##takes is a dataframe from transform_and_sort_dataframe
    ## extracts basic variables and stroes them in dicts per year
    ## variables currently are: List of parties i=1,2,... , total number of seats Q, Number of parties n  
    parties_in_year = {}
    totalseats_in_year = {}
    n_in_year = {}

    for YearMonth, group in df.groupby('YearMonth'):
        parties = group['Party'].unique().tolist() #add unique elements of group to list 
        parties_in_year[YearMonth] = parties #add list to dict 
        totalseats_in_year[YearMonth] = group['# of Seats'].sum() #add sum of group to dict 
        n_in_year[YearMonth] = len(parties) # get n for every year and add to dict

    return parties_in_year, totalseats_in_year, n_in_year


def coalition_combinatorics_generator(df,parties_in_year):
    ## takes in a dataframe from transform_and_sort_dataframe
    ## creates a dict for all coalitions in a given year and their seats
    coalition_dict = {}

    for YearMonth, year_df in df.groupby('YearMonth'): # Iterate over each year in the DataFrame
        parties = parties_in_year[YearMonth]
        seats = dict(zip(df[df['YearMonth'] == YearMonth]['Party'], df[df['YearMonth'] == YearMonth]['# of Seats']))  # boolean mask of df(year)=year to get new df of only the relevant year. Then zips parties and seats togeterh

        #add empty coalition 
        coalition_dict[(YearMonth, '')]=0
        
        # Generate all unique coalitions
        for r in range(1, len(parties) + 1):
            for combi in itertools.combinations(parties, r): #itertools.combinations creates r-length tuples, in sorted order, no repeated elements
                coalition = '+'.join(combi) #naming the coalitions
                total_seats = sum(seats[party] for party in combi) # calc seats of the coalition
                coalition_dict[(YearMonth, coalition)] = total_seats # add seats as values to dict with (year,coalition) tuple as key

    return coalition_dict
   

def win_coals(coalition_dict, totalseats_in_year):
    ## takes in a dict containing all coalitions from coalition_combinatorics_generator
    ## checks whether value is greater than half of the grandcoalition
    ## outputs a dict with similar keys and value {0,1} depending on being winning
    winning_coal_dict = {}

    # Determine if coal is winning
    for (year, coalition), seats in coalition_dict.items():
        winning_coal_dict[(year, coalition)] = 1 if seats > totalseats_in_year[year] / 2 else 0 #assumes strict inequaltiy needed 

    return winning_coal_dict

def min_winning_coals(winning_coal_dict):
   ## takes in a dict from win_coals_and_parties
   ## for each element tests whether coal would be losing in all combinations losing one member 
   ## returns all those MWC  
    min_win_coal_dict = {}

    for (year, coalition), is_winning in winning_coal_dict.items(): #"is_winning" either 0 or 1, interpret as boolean
        if is_winning:
            parties = coalition.split('+') #gets indiv. parties of this coal
            is_minimal = True #initialize boolean state

            # Get all coals with one less member
            for sub_coalition in itertools.combinations(parties, len(parties) - 1): #gets all combinations from indiv. parties with -1 total members
                sub_coalition_str = '+'.join(sub_coalition) #create sub-coals

                # Check if subcoals are winning
                if winning_coal_dict.get((year, sub_coalition_str)): #looks up value attributed to sub-coaliton, either 0 or 1 --> boolean
                    is_minimal = False #if any sub coal is winning then coal is not minimal 
                    break

            # If no sub-coalition is winning, then it's MWC
            if is_minimal:
                min_win_coal_dict[(year, coalition)] = winning_coal_dict[(year, coalition)]

    return min_win_coal_dict

def party_types(min_win_coal_dict,coal_dict):
    '''checks whether any two parties are of the same type for a given country'''
    ##input: dict of all minimal winning coalitions and dict of all coalitions 
    ##ouput: dict with keys: year and value: list of tuples, each tuple indicates parties of the same type.  
    types={}
    for (year,_),_ in min_win_coal_dict.items(): 
        if year not in types: 
            types[year]= set()

        relevant_parties=set()
        #generate set of parties from mwc, to drop dummy players: 
        for (yr,coal),_ in min_win_coal_dict.items(): 
            if yr==year: 
                parties=coal.split('+')
                relevant_parties.update(parties)
        ##pairwise check if of same type        ##
        #outer loops over all relevant parties
        for party_a in relevant_parties: 
            for party_b in relevant_parties: 
                if party_a != party_b: 
                    #check whether substituting a for b or b for a swaps any coalition status
                    same_type = True #initiate boolean 
                    for (yr,coal),_ in min_win_coal_dict.items(): 
                        #check all coalitions of the parliament
                        if yr==year: 
                            parties=coal.split('+')
                            if party_a in parties or party_b in parties: 
                                #swap
                                # If swapping a and b (only where they acutally appear) does not change the outcome of any coalition, they are of the same type 
                                swap_party_a=[party_b if party == party_a else party for party in parties] 
                                swap_party_b=[party_a if party == party_b else party for party in parties]
                                #check with helper function, see below 
                                if not (type_helper_fnc(swap_party_a,coal_dict,year)==type_helper_fnc(swap_party_b,coal_dict,year)): 
                                   same_type= False
                                   break
                    if same_type: types[year].add(tuple(sorted((party_a,party_b)))) # ensure pairings are listed only once
    for year in types: 
        types[year]= list(types[year])
    return types
                    
def type_helper_fnc(parties,coal_dict,year): 
    '''helper function for party_types'''
    # looks up value from the coal_dict to check whether switched parties make a difference 
    coal_name='+'.join(sorted(parties))
    return coal_dict.get((year,coal_name),0)                    

def max_loosing_coals(winning_dict, parties_in_year):
    ##inverse to min_winning_coals with similar logic
    ## takes in one dict containing all coalitions with boolean values and a dict listing all parties for a given year
    
    maximal_losing = {}

    for (year, coalition), is_winning in winning_dict.items():
        if not is_winning:  # now only losing coals 
            parties = set(coalition.split('+')) #get parties of the coalition, use of set logic important later
            all_parties = set(parties_in_year[year]) #get all parties 
            remaining_parties = all_parties - parties  #here sets work wonders
            is_maximal = True

            # Get all coalitions from proto-coals
            for party in remaining_parties:
                big_coalition = '+'.join(sorted(parties | {party})) #joins the set of parties with a single party from the remaining parties

                # Check if the expanded coalition is still losing
                if not winning_dict.get((year, big_coalition)): #looks up value to big_coalition test wheter its 0
                    is_maximal = False 
                    break

            # If all expanded coalitions are winning, then it's a maximal losing coalition
            if is_maximal:
                maximal_losing[(year, coalition)] = 0

    return maximal_losing
def combine_dicts(dict1, dict2):
    #helper function to get only relevant coals later 
    return {**dict1, **dict2} ##apparently this "unpacks the dict"

def unique_tying_coals(coalition_dict, totalseats_in_year, parties_in_year):
    #takes in coal,totalseats and parties dict
    #in even parliaments unique tying coals can emerge, which later should be attributed the same weight 
    #those will be stored in a dict as tuples 
    unique_tying = {}

    for (year, coalition), seats in coalition_dict.items():
        if totalseats_in_year[year] % 2 == 0: #even-check
            half_seats = totalseats_in_year[year] // 2 # // to ensure integer values 
            if seats == half_seats:
                coal_parties = set(coalition.split('+')) # get all parties participating the coalition
                complementary_coal = '+'.join(sorted(set(parties_in_year[year]) - coal_parties)) #ahh set logic for the win 
                coal_tuple = (year,coalition)
                complment_tuple = (year,complementary_coal)               
                if not any(coal in unique_tying for coal in [coal_tuple,complment_tuple]): #any checks whether either the coal_tuple or the complement tuple are already in the unique tying dict. (testing for one should also work if one were to check for both sides of the key)
                    unique_tying[(year, (coalition, complementary_coal))] = 0 

    return unique_tying