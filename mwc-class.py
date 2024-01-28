import os
import pandas as pd
import itertools

from 'mwc-functions' import * 

class getMVCs:
    def __init__(self, csv_file_path, save_results=False, results_folder='results'):
        self.csv_file_path = csv_file_path
        self.save_results = save_results
        self.results_folder = results_folder

        # Ini
        self.dataframe = None
        self.transformed_dataframe = None
        self.parties_in_year = None
        self.totalseats_in_year = None
        self.coalition_dict = None
        self.winning_coal_dict = None
        self.minimal_winning_coalitions = None
        self.maximal_losing_coalitions = None
        self.unique_tying_coalitions = None

    def read_and_transform_data(self):
        self.dataframe = read_csv_to_dataframe(self.csv_file_path, self.encoding)
        self.transformed_dataframe = transform_and_sort_dataframe(self.dataframe)

    def get_variables(self):
        self.parties_in_year, self.totalseats_in_year, self.n_in_year = variables_by_year(self.transformed_dataframe)

    def generate_coalition_combinatorics(self):
        self.coalition_dict = coalition_combinatorics_generator(self.transformed_dataframe, self.parties_in_year)

    def identify_winning_coalitions(self):
        self.winning_coal_dict = win_coals(self.coalition_dict, self.totalseats_in_year, self.parties_in_year)

    def find_minimal_winning_coalitions(self):
        self.minimal_winning_coalitions = minimal_winning_coalitions(self.winning_coal_dict)

    def find_maximal_losing_coalitions(self):
        self.maximal_losing_coalitions = max_loosing_coals(self.winning_dict, self.parties_in_year)
    def find_tying_coalitions(self):
        self.unique_tying_coalitions = tying_coals(self.coalition_dict, self.totalseats_in_year)
   
    def save_results(self):
        # Optionally save results to a folder
        if self.save_results:
            if not os.path.exists(self.results_folder):
                os.makedirs(self.results_folder)
                
    def run_pipeline(self):
        # Main method to run the entire pipeline
        self.read_and_transform_data()
        self.generate_coalition_combinatorics()
        self.identify_winning_coalitions()
        self.find_minimal_winning_coalitions()
        self.find_maximal_losing_coalitions()
        self.save_results()

        return "Pipeline completed successfully."



