import os
import pandas as pd
import itertools

def read_csv_to_dataframe(filename, encoding='utf-16'):
    ## takes in the filename as 'xy.csv' and allows for an encoding parameter (uft-16 is apparently used by the Political Data Yearbook)
    ## csv files are stroed in folder 'data'

    file_path = os.path.join('data', filename)
    try:
        dataframe = pd.read_csv(file_path, delimiter='\t', encoding=encoding)
        return dataframe
    except UnicodeDecodeError:
        print(f"UnicodeDecodeError: The file might not be in {encoding} encoding.")
        return None
    except Exception as e:
        print(f"An error occurred: {e}")
        return None


def transform_and_sort_dataframe(df):
    ## takes in a dataframe from read_csv_to_dataframe
    ## strips partys with 0 seats, strips days and months
    ## renames party names, sorts by year-party
    df = df[df['# of Seats'] != 0]
    df['Year'] = df['Date'].str[-2:]
    # adding 19xx to values bigger than 90 and 20xx to values lower than 90 --> creates full year integers
    df['Year'] = df['Year'].apply(lambda x: '19' + x if int(x) > 90 else '20' + x)
    df['Party'] = df['Party'].str[:3]
    df = df[['Year', 'Party', '# of Seats']]

    df.sort_values(by=['Year', 'Party'], inplace=True)

    return df



def coalition_combinatorics_generator(df):
    ## takes in a dataframe from transform_and_sort_dataframe
    ## creates a dict for all coalitions in a given year and their seats
    coalition_dict = {}

    for year, year_df in df.groupby('Year'): # Iterate over each year in the DataFrame
        parties = year_df['Party'].tolist() #Create a list of parties
        seats = dict(zip(year_df['Party'], year_df['# of Seats'])) #map seats to party in a dict

        # Generate all unique combinations of parties for each year
        for r in range(1, len(parties) + 1):
            for combo in itertools.combinations(parties, r): #itertools.combinations creates r-length tuples, in sorted order, no repeated elements
                coalition = '+'.join(combo) #naming the coalitions
                total_seats = sum(seats[party] for party in combo) # calc seats of the coalition
                coalition_dict[(year, coalition)] = total_seats # add seats as values to dict with (year,coalition) tuple as key

    return coalition_dict
   

def win_coals_and_parties(coalition_dict):
    ## takes in a dict containing all coalitions from coalition_combinatorics_generator
    ## checks whether value is greater than half of the grandcoalition
    ## outputs a dict with similar keys and value {0,1} depending on being winning
    ## outputs a list with number of parties for every given year (this could and should have been done much earlier, might change later)
   
    winning_coal_dict = {}
    nrofseats = {}
    parties_per_year = {}
    n_list = []

    # number of parties for each year
    for (year, coalition), seats in coalition_dict.items():
        parties = coalition.split('+') #split coalitions again into parties
        if year in parties_per_year:
            parties_per_year[year].update(parties) #add parties to list
        else:
            parties_per_year[year] = set(parties) #gets rid of duplicates

    # total number of seats from grandcoalition
    for (year, coalition), seats in coalition_dict.items():
        if year in nrofseats:
            if len(coalition.split('+')) == len(parties_per_year[year]): #test whether current coalition is grandcoalition
                nrofseats[year] = seats
        else:
            nrofseats[year] = seats #adds the year to the list with a starting coalition

    # Determine if coalition is winning
    for (year, coalition), seats in coalition_dict.items():
        winning_coal_dict[(year, coalition)] = 1 if seats > nrofseats[year] / 2 else 0 #assumes that coalition needs a strict majority to be winning

    # list parties for each year
    for year, parties in parties_per_year.items():
        n_list.append((year, len(parties)))

    return winning_coal_dict, n_list

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
                if winning_coal_dict.get((year, sub_coalition_str), 0):
                    is_minimal = False
                    break

            # If no sub-coalition is winning, then it's MWC
            if is_minimal:
                min_win_coal_dict[(year, coalition)] = winning_coal_dict[(year, coalition)]

    return min_win_coal_dict