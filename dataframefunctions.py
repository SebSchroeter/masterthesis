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

