import pandas as pd 
import warnings
warnings.formatwarning = lambda msg, *args, **kwargs: f'{msg}\n'
def process_gov_csv(file_name): 
    ## takes in a csv file from political yearbook, containing individual ministry information for a country
    df= pd.read_csv(file_name, delimiter="\t",encoding='utf-16')
    df.drop('Gender',axis=1,inplace=True)
    df.drop('Year of Birth',axis=1,inplace=True)
    df['Start']=pd.to_datetime(df['Start'],errors='coerce',format='%d-%b-%y')
    df['End']=pd.to_datetime(df['End'],errors='coerce',format='%d-%b-%y')
    #df['Start']=df['Start'].dt.strftime('-%b-%y')
    #df['End']=df['End'].dt.strftime('-%b-%y')
    return df 
def match_ministries_and_elections(countryname,election_dates,df):
    ## takes in the name of the country, a list of election dates and the df from process_gov_csv
    ## to accomodate errors from the political yearbook dataset, begin and end of election cycles are off-set by one month
    ## see i.e. that Germany 09/2013 elected ministers are appointed 31.08.2013 by the political yearbook data
    ## outputs a dict with (election_date,df_for_electionperiod)
    ## df_for_electionperiod has cols: 'Position' and 'Party'

    sorted_elections= sorted(pd.to_datetime(election_dates, format=f'{countryname}-%b-%y'))
    election_period_dict = {}
    for i in range((len(sorted_elections))): 
        #get timeframe for ministerial appointments per election (start: election -1 month)
        start=sorted_elections[i]
        adjusted_start= start - pd.DateOffset(months=1)
        end = (sorted_elections[i+1] if i+1<len(sorted_elections) else pd.Timestamp.today())- pd.DateOffset(months=1)#.strftime('-%b-%y')
        #select all datapoints in this timeframe
        boolean_mask = ((df['Start']>=adjusted_start)&(df['Start']<end))
        df_for_electionperiod=df[boolean_mask]
        key=start.strftime('-%b-%y')
        #however keep correct election date as key 
        election_period_dict[key]=df_for_electionperiod
        
    return election_period_dict

def starting_gov_dict_old(election_period_dict): 
    ### depreciated
    governments_dict={}
    for election,df_for_electionperiod in election_period_dict.items(): 
        starting_gov=df_for_electionperiod.loc[df_for_electionperiod['Start']==df_for_electionperiod['Start'].min(),['Position','Party']]
        governments_dict[election]=starting_gov
    return governments_dict


def starting_gov_dict(election_period_dict,countryname): 
    ## takes in a dict from match_ministries_and_elections and the countryname for the warning functions
    ## outputs a dict which matches {election_date,(position,party)} and a list of those elections where no government was found in 12 months after or 1 month before the election 
    ## assumes that the start date which appears most often in the period after the election is the date of inauguration of the new cabinet 
    ## includes some warnings for "suspicious results"
    governments_dict = {}
    edge_cases = []
    for election, df_for_election_period in election_period_dict.items():
        mode = df_for_election_period['Start'].mode() #check which is the most often appearing start date for this period
        if not mode.empty:
            start_date = mode[0] #set this date as the beginning for the new government
            starting_gov = df_for_election_period.loc[df_for_election_period['Start'] == start_date, ['Position', 'Party']]
            
            # Handling of cases where the found government was very late in the election period --> likely errors
            election_date = pd.to_datetime(election, format='-%b-%y')
            if not (election_date.year <= start_date.year <= election_date.year + 1):
                shorter_df = df_for_election_period[(df_for_election_period['Start'] > election_date-pd.DateOffset(months=1)) & (df_for_election_period['Start'] <= (election_date + pd.DateOffset(years=1)))]
                new_mode=shorter_df['Start'].mode()
                if not new_mode.empty:
                    start_date = new_mode[0] #set this new date as the beginning for the new government
                    starting_gov = shorter_df.loc[shorter_df['Start'] == start_date, ['Position', 'Party']]
                    #if start_date != mode[0]:  # Check if new mode is different from the original mode
                    warnings.warn(f"{countryname}{election}: Use government from {new_mode[0]}")
                else:
                    warnings.warn(f"{countryname}{election}: No suitable government found within a year after the election. Fall back to late government at {mode[0]}.")
                    edge_cases.append(f"{countryname}{election}")
            # Warning if the found government has few ministers
            if starting_gov.shape[0] <= 8:
                warnings.warn(f"{countryname}-{election}: Only found {starting_gov.shape[0]} ministers.")
            
            # Warning if the found government was before the actual election 
            if start_date < election_date:
                warnings.warn(f"{countryname}-{election}: Careful, check db error. Found Government at {start_date}")

        else:
            starting_gov = pd.DataFrame(columns=['Position', 'Party']) #add empty df when no government info is given for the period
        
        governments_dict[election] = starting_gov
    return governments_dict,edge_cases


def get_ministry_dicts(df,parties): 
    ## takes in a df with cols position and party and a list of all parties like a value from governments_dict
    ## creates 3 dicts, which store the following information
    dict_1={} #(party,list_of_ministries)
    dict_2={} #(party,number_of_ministries)
    dict_3={} #(party,weighted_number_of_ministries) ## weighting prime minister = 3, all other 1
    prime_minister=df.iloc[0]['Position'] #prime minister, chancellor whatever is always the first entry of the df, if created correctly from political yearbook data
    for party in parties: 
        ministries=df[df['Party']==party]['Position'].tolist()
        dict_1[party]=ministries
        dict_2[party]=len(ministries)
        weighted_ministries= len(ministries) + 2 if prime_minister in ministries else len(ministries)
        dict_3[party]=weighted_ministries
    return dict_1,dict_2,dict_3
        