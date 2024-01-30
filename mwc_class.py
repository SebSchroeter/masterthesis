import os
import pandas as pd
import itertools
from scipy import optimize
from scipy.optimize import LinearConstraint
from scipy.optimize import milp
from mwc_functions import * 
from optimization_functions import *

class getMVWs:
    def __init__(self, csv_file_path, encoding='utf-16', save_results=False, results_folder='results'):
        self.csv_file_path = csv_file_path
        self.saveresults = save_results
        self.results_folder = results_folder
        self.encoding = encoding
        # Ini prelims
        self.dataframe = None
        self.transformed_dataframe = None
        self.parties_in_year = None
        self.totalseats_in_year = None
        self.coalition_dict = None
        self.winning_coal_dict = None
        self.minimal_winning_coalitions = None
        self.maximal_losing_coalitions = None
        self.unique_tying_coalitions = None
        self.n_in_year = None
        #Ini pipeline
        self.all_dfs = None
        self.all_constraints = None
        self.all_lin_cons = None
        self.all_min_weights = None
        self.optimal_seats = None
#prelim wrapper
    def read_and_transform_data(self):
        self.dataframe = read_csv_to_dataframe(self.csv_file_path, self.encoding)
        self.transformed_dataframe = transform_and_sort_dataframe(self.dataframe)

    def get_variables(self):
        self.parties_in_year, self.totalseats_in_year, self.n_in_year = variables_by_year(self.transformed_dataframe)

    def generate_coalition_combinatorics(self):
        self.coalition_dict = coalition_combinatorics_generator(self.transformed_dataframe, self.parties_in_year)

    def identify_winning_coalitions(self):
        self.winning_coal_dict = win_coals(self.coalition_dict, self.totalseats_in_year)

    def find_minimal_winning_coalitions(self):
        self.minimal_winning_coalitions = min_winning_coals(self.winning_coal_dict)

    def find_maximal_losing_coalitions(self):
        self.maximal_losing_coalitions = max_loosing_coals(self.winning_coal_dict, self.parties_in_year)
    def find_tying_coalitions(self):
        self.unique_tying_coalitions = tying_coals(self.coalition_dict, self.totalseats_in_year)
   
#pipeline wrapper   
    def get_all_dfs(self): 
       self.all_dfs = create_all_year_dfs(self.winning_coal_dict,self.parties_in_year)
    def Find_all_contraints(self): 
       self.all_constraints = get_all_constrains(self.all_dfs)       
    def Find_all_lin_cons(self): 
       self.all_lin_cons = get_all_lin_cons(self.all_constraints)
    def Find_all_min_weights(self): 
       self.all_min_weights = get_all_min_vote_weights(self.all_lin_cons,self.n_in_year) 
    def All_the_optimal_seats(self): 
        self.optimal_seats = get_all_optimized_seats(self.all_min_weights,self.parties_in_year)
       
#saving functions    
    def save_prelims(self):
        """requires XlsxWriter Module"""
        """install by: pip install xlsxwriter """
        if self.saveresults:
            # Creates folder if necessary
            if not os.path.exists(self.results_folder):
                os.makedirs(self.results_folder)

            # Prepare the path for the Excel file
            output_file = os.path.join(self.results_folder, 'Preliminaries.xlsx')

            # somehow this works.... creates the excel file
            with pd.ExcelWriter(output_file, engine='xlsxwriter') as writer:
                # save the df
                if self.transformed_dataframe is not None:
                    self.transformed_dataframe.to_excel(writer, sheet_name='Transformed Data', index=False)
                # save the dicts
                self._save_dict_to_excel_sheet(self.parties_in_year, 'Parties per Year', writer)
                self._save_dict_to_excel_sheet(self.n_in_year, 'n per Year', writer)
                self._save_dict_to_excel_sheet(self.totalseats_in_year, 'Total Seats per Year', writer)
                self._save_dict_to_excel_sheet(self.coalition_dict, 'Coalitions', writer)
                self._save_dict_to_excel_sheet(self.winning_coal_dict, 'Winning Coalitions', writer)
                self._save_dict_to_excel_sheet(self.minimal_winning_coalitions, 'Minimal Winning Coalitions', writer)
                self._save_dict_to_excel_sheet(self.maximal_losing_coalitions, 'Maximal Losing Coalitions', writer)
                self._save_dict_to_excel_sheet(self.unique_tying_coalitions, 'Unique Tying Coalitions', writer)

    def _save_dict_to_excel_sheet(self, data, sheet_name, writer):
        """Supporting function for dict to xls."""
        #translates dict to df then to xls
        #kinda convoluted since dicts can have tuples as key and lists as elements
        #order of columns questionable, will fix later, proably seperate df in two and then add them together later...

        if data is not None:
            df = pd.DataFrame.from_dict(data, orient='index').reset_index() # resetting index important to get keys as column not rowname
            #"key columns"
            if isinstance(df['index'][0], tuple):#expand tuples into separate columns
                tuple_columns = len(df['index'][0]) #will always be 2 for now
                df[[f'Key_{i+1}' for i in range(tuple_columns)]] = pd.DataFrame(df['index'].tolist(), index=df.index)
                df.drop('index', axis=1, inplace=True)
            else:
                df.rename(columns={'index': 'Key'}, inplace=True)# if not a tuple just call it 'key'
                tuple_columns=1 

            # "value" columns
            for i in range(len(df.columns) - tuple_columns):
                df.rename(columns={i: f'Value_{i+1}'}, inplace=True)

            df.to_excel(writer, sheet_name=sheet_name, index=False)        

  
    def save_pipeline(self):
        """requires XlsxWriter Module"""
        """install by: pip install xlsxwriter """
        if self.saveresults:
            # Creates folder if necessary
            if not os.path.exists(self.results_folder):
                os.makedirs(self.results_folder)

            # Prepare the path for the Excel file
            output_file = os.path.join(self.results_folder, 'minimal_seats.xlsx')

            # somehow this works.... creates the excel file
            with pd.ExcelWriter(output_file, engine='xlsxwriter') as writer:
                # save the dict
                self._save_dict_to_excel_sheet(self.optimal_seats, 'Minimal Seats Per Party', writer)

    

    def preliminaries(self):
        self.read_and_transform_data()
        self.get_variables()
        self.generate_coalition_combinatorics()
        self.identify_winning_coalitions()
        self.find_minimal_winning_coalitions()
        self.find_maximal_losing_coalitions()
        self.find_tying_coalitions()
        
        if self.saveresults:
            self.save_prelims()
            return "Prelims completed successfully."
        else:
            return {
                "transformed_dataframe": self.transformed_dataframe,
                "parties_in_year": self.parties_in_year,
                "n_in_year": self.n_in_year,
                "totalseats_in_year": self.totalseats_in_year,
                "coalition_dict": self.coalition_dict,
                "winning_coal_dict": self.winning_coal_dict,
                "minimal_winning_coalitions": self.minimal_winning_coalitions,
                "maximal_losing_coalitions": self.maximal_losing_coalitions,
                "unique_tying_coalitions": self.unique_tying_coalitions
            }
    def minimal_voting_weights_pipeline(self):
        self.get_all_dfs()
        self.Find_all_contraints()
        self.Find_all_lin_cons()
        self.Find_all_min_weights()
        self.All_the_optimal_seats()
        if self.saveresults:
            self.save_pipeline()
            return "Pipeline completed successfully."
        else:
            return {"Dataframes":self.all_dfs,
                "Constraints":self.all_constraints,
                "Linear Constraints": self.all_lin_cons,
                "Minimal Integer Weights" : self.all_min_weights,
                "Optimal Seats": self.optimal_seats
            }
        
