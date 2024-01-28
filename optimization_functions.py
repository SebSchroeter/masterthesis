import pandas as pd

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