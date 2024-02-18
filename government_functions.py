import pandas as pd 
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
    ## outputs a dict with (election_date,df_for_electionperiod)
    ## df_for_electionperiod has cols: 'Position' and 'Party'
    sorted_elections= sorted(pd.to_datetime(election_dates, format=f'{countryname}-%b-%y'))
    election_period_dict = {}
    for i in range((len(sorted_elections))): 
        start=sorted_elections[i]
        end = sorted_elections[i+1] if i+1<len(sorted_elections) else pd.Timestamp.today()#.strftime('-%b-%y')
        boolean_mask = ((df['Start']>=start)&(df['Start']<end))
        df_for_electionperiod=df[boolean_mask]
        key=start.strftime('-%b-%y')
        election_period_dict[key]=df_for_electionperiod
    return election_period_dict

def get_ministry_dicts(df,parties): 
    ## takes in a df_for_electionperiod with cols position and party and a list of all parties
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
        