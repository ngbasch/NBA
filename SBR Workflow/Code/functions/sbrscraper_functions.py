#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Dec  4 13:51:14 2019

From: https://github.com/SharpChiCity/SBRscraper

@author: nathanbasch
"""

# import socket
# fimport socks
import requests
from bs4 import BeautifulSoup
import datetime
from datetime import date
import time
from pandas import DataFrame
import os
import pandas as pd

# def connectTor():
#   ## Connect to Tor for privacy purposes
#      socks.setdefaultproxy(socks.PROXY_TYPE_SOCKS5, '127.0.0.1', 9150, True)
#      socket.socket = socks.socksocket
#      print "connected to Tor!"

def soup_url(type_of_line, tdate):
## get html code for odds based on desired line type and date
    if type_of_line == 'Spreads':
        url_addon = ''
    elif type_of_line == 'ML':
        url_addon = 'money-line/'
    elif type_of_line == 'Totals':
        url_addon = 'totals/'
    # elif type_of_line == '1H':
        # url_addon = '1st-half/'
    # elif type_of_line == '1HRL':
        # url_addon = 'pointspread/1st-half/'
    # elif type_of_line == '1Htotal':
        # url_addon = 'totals/1st-half/'
    else:
        print("Wrong url_addon")
    url = 'https://classic.sportsbookreview.com/betting-odds/nba-basketball/' + url_addon + '?date=' + tdate
    now = datetime.datetime.now()
    #Need to add headers or SBR Will reject request
    headers = {'User-Agent': 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.106 Safari/537.36'}
    raw_data = requests.get(url, headers = headers)
    soup_big = BeautifulSoup(raw_data.text, 'html.parser')
    #There is an issue with some of the days (for example, 2/11/2010)
    #The classic site doesn't have the data while the non-classic does.
    #For now, let's just skip these ones and deal with them later
    try:
        soup = soup_big.find_all('div', id='OddsGridModule_5')[0]
    except IndexError:
        soup = soup_big
    timestamp = time.strftime("%H:%M:%S")
    return soup, timestamp

def parse_and_write_data(soup, date, time, not_ML = True):
## Parse HTML to gather line data by book
    def book_line(book_id, line_id, homeaway):
        ## Get Line info from book ID
        line = soup.find_all('div', attrs = {'class':'el-div eventLine-book', 'rel':book_id})[line_id].find_all('div')[homeaway].get_text().strip()
        return line
    def get_consensus(line_id, homeaway):
        ## Get percentage consensus info for each game
        cons = soup.find_all('div', attrs = {'class':'el-div eventLine-consensus'})[line_id].get_text().strip().split('%')[homeaway]
        return(cons)
    
    '''
    BookID  BookName
    238     Pinnacle
    19      5Dimes
    93      Bookmaker
    1096    BetOnline
    169     Heritage
    123     BetDSI
    999996  Bovada
    139     Youwager
    999991  SIA
    '''
    if not_ML:
        df = DataFrame(
                columns=('key','date','time',
                         'team','opp_team','consensus',
                         'pinnacle_line','pinnacle_odds',
                         '5dimes_line','5dimes_odds',
                         'heritage_line','heritage_odds',
                         'bovada_line','bovada_odds',
                         'betonline_line','betonline_odds'))
    else:
        df = DataFrame(
            columns=('key','date','time',
                     'team','opp_team', 'consensus',
                     'pinnacle','5dimes',
                     'heritage','bovada','betonline'))
    counter = 0
    number_of_games = len(soup.find_all('div', attrs = {'class':'el-div eventLine-rotation'}))
    for i in range(0, number_of_games):
        A = []
        H = []
        print(str(i+1)+'/'+str(number_of_games))
        
        #NB TESTING
        #test = soup.find_all('div', attrs = {'class':'el-div eventLine-team'})[6]
        
        ## Gather all useful data from unique books
        # consensus_data = 	soup.find_all('div', 'el-div eventLine-consensus')[i].get_text()
        try:
            info_A = 		        soup.find_all('div', attrs = {'class':'el-div eventLine-team'})[i].find_all('div')[0].get_text().strip()
        except IndexError:
            info_A = 'MISSING'
        # hyphen_A =              info_A.find('-')
        # paren_A =               info_A.find("(")
        team_A =                info_A
        # pitcher_A =             info_A[hyphen_A + 2 : paren_A - 1]
        # hand_A =                info_A[paren_A + 1 : -1]
        #Get consensus data
        try:
            consensus_A = 	    get_consensus(i, 0)
        except IndexError:
            consensus_A = ''
            
        ## get line/odds info for unique book. Need error handling to account for blank data
        try:
            pinnacle_A = 	    book_line('238', i, 0)
        except IndexError:
            pinnacle_A = ''
        try:
            fivedimes_A = 	    book_line('19', i, 0)
        except IndexError:
            fivedimes_A = ''
        try:
            heritage_A =        book_line('169', i, 0)
        except IndexError:
            heritage_A = ''
        try:
            bovada_A = 		    book_line('999996', i, 0)
        except IndexError:
            bovada_A = ''
        try:
            betonline_A = 		book_line('1096', i, 0)
        except IndexError:
            betonline_A = ''
        
        try:
            info_H = 		        soup.find_all('div', attrs = {'class':'el-div eventLine-team'})[i].find_all('div')[1].get_text().strip()
        except IndexError:
            info_H = 'MISSING'
        # hyphen_H =              info_H.find('-')
        # paren_H =               info_H.find("(")
        team_H =                info_H
        # pitcher_H =             info_H[hyphen_H + 2 : paren_H - 1]
        # hand_H =                info_H[paren_H + 1 : -1]
        
        try:
            consensus_H = 	    get_consensus(i, 1)
        except IndexError:
            consensus_H = ''  

        try:
            pinnacle_H = 	    book_line('238', i, 1)
        except IndexError:
            pinnacle_H = ''
        try:
            fivedimes_H = 	    book_line('19', i, 1)
        except IndexError:
            fivedimes_H = ''
        try:
            heritage_H = 	    book_line('169', i, 1)
        except IndexError:
            heritage_H = '.'
        try:
            bovada_H = 		    book_line('999996', i, 1)
        except IndexError:
            bovada_H = '.'
        try:
            betonline_H = 		book_line('1096', i, 1)
        except IndexError:
            betonline_H = ''
        if team_H ==   'Detroit':
            team_H =   'Detroit'
        elif team_H == 'Indiana':
            team_H =   'Indiana'
        elif team_H == 'Brooklyn':
            team_H =   'Brooklyn'
        elif team_H == 'L.A. Lakers':
            team_H =   'L.A. Lakers'
        elif team_H == 'Washington':
            team_H =   'Washington'
        elif team_H == 'Miami':
            team_H =   'Miami'
        elif team_H == 'Minnesota':
            team_H =   'Minnesota'
        elif team_H == 'Chicago':
            team_H =   'Chicago'
        elif team_H == 'Oklahoma City':
            team_H =   'Oklahoma City'
        if team_A ==   'New Orleans':
            team_A =   'New Orleans'
        elif team_A == 'Houston':
            team_A =   'Houston'
        elif team_A == 'Dallas':
            team_A =   'Dallas'
        elif team_A == 'Cleveland':
            team_A =   'Cleveland'
        elif team_A == 'L.A. Clippers':
            team_A =   'L.A. Clippers'
        elif team_A == 'Golden State':
            team_A =   'Golden State'
        elif team_A == 'Denver':
            team_A =   'Denver'
        elif team_A == 'Boston':
            team_A =   'Boston'
        elif team_A == 'Milwaukee':
            team_A =   'Milwaukee'            
       # A.append(str(date) + '_' + team_A.replace(u'\xa0',' ') + '_' + team_H.replace(u'\xa0',' '))
        A.append(date)
        A.append(time)
        A.append('away')
        A.append(team_A)
        # A.append(pitcher_A)
        # A.append(hand_A)
        A.append(team_H)
        # A.append(pitcher_H)
        # A.append(hand_H)
        if not_ML:
            A.append(consensus_A)
            pinnacle_A = pinnacle_A.replace(u'\xa0',' ').replace(u'\xbd','.5')
            pinnacle_A_line = pinnacle_A[:pinnacle_A.find(' ')]
            pinnacle_A_odds = pinnacle_A[pinnacle_A.find(' ') + 1:]
            A.append(pinnacle_A_line)
            A.append(pinnacle_A_odds)
            fivedimes_A = fivedimes_A.replace(u'\xa0',' ').replace(u'\xbd','.5')
            fivedimes_A_line = fivedimes_A[:fivedimes_A.find(' ')]
            fivedimes_A_odds = fivedimes_A[fivedimes_A.find(' ') + 1:]
            A.append(fivedimes_A_line)
            A.append(fivedimes_A_odds)
            heritage_A = heritage_A.replace(u'\xa0',' ').replace(u'\xbd','.5')
            heritage_A_line = heritage_A[:heritage_A.find(' ')]
            heritage_A_odds = heritage_A[heritage_A.find(' ') + 1:]
            A.append(heritage_A_line)
            A.append(heritage_A_odds)
            bovada_A = bovada_A.replace(u'\xa0',' ').replace(u'\xbd','.5')
            bovada_A_line = bovada_A[:bovada_A.find(' ')]
            bovada_A_odds = bovada_A[bovada_A.find(' ') + 1:]
            A.append(bovada_A_line)
            A.append(bovada_A_odds)
            betonline_A = betonline_A.replace(u'\xa0',' ').replace(u'\xbd','.5')
            betonline_A_line = betonline_A[:betonline_A.find(' ')]
            betonline_A_odds = betonline_A[betonline_A.find(' ') + 1:]
            A.append(betonline_A_line)
            A.append(betonline_A_odds)
        else:
            A.append(consensus_A)
            A.append(pinnacle_A.replace(u'\xa0',' ').replace(u'\xbd','.5'))
            A.append(fivedimes_A.replace(u'\xa0',' ').replace(u'\xbd','.5'))
            A.append(heritage_A.replace(u'\xa0',' ').replace(u'\xbd','.5'))
            A.append(bovada_A.replace(u'\xa0',' ').replace(u'\xbd','.5'))
            A.append(betonline_A.replace(u'\xa0',' ').replace(u'\xbd','.5'))
        #H.append(str(date) + '_' + team_A.replace(u'\xa0',' ') + '_' + team_H.replace(u'\xa0',' '))
        H.append(date)
        H.append(time)
        H.append('home')
        H.append(team_H)
        # H.append(pitcher_H)
        # H.append(hand_H)
        H.append(team_A)
        # H.append(pitcher_A)
        # H.append(hand_A)
        if not_ML:
            H.append(consensus_H)
            pinnacle_H = pinnacle_H.replace(u'\xa0',' ').replace(u'\xbd','.5')
            pinnacle_H_line = pinnacle_H[:pinnacle_H.find(' ')]
            pinnacle_H_odds = pinnacle_H[pinnacle_H.find(' ') + 1:]
            H.append(pinnacle_H_line)
            H.append(pinnacle_H_odds)
            fivedimes_H = fivedimes_H.replace(u'\xa0',' ').replace(u'\xbd','.5')
            fivedimes_H_line = fivedimes_H[:fivedimes_H.find(' ')]
            fivedimes_H_odds = fivedimes_H[fivedimes_H.find(' ') + 1:]
            H.append(fivedimes_H_line)
            H.append(fivedimes_H_odds)
            heritage_H = heritage_H.replace(u'\xa0',' ').replace(u'\xbd','.5')
            heritage_H_line = heritage_H[:heritage_H.find(' ')]
            heritage_H_odds = heritage_H[heritage_H.find(' ') + 1:]
            H.append(heritage_H_line)
            H.append(heritage_H_odds)
            bovada_H = bovada_H.replace(u'\xa0',' ').replace(u'\xbd','.5')
            bovada_H_line = bovada_H[:bovada_H.find(' ')]
            bovada_H_odds = bovada_H[bovada_H.find(' ') + 1:]
            H.append(bovada_H_line)
            H.append(bovada_H_odds)
            betonline_H = betonline_H.replace(u'\xa0',' ').replace(u'\xbd','.5')
            betonline_H_line = betonline_H[:betonline_H.find(' ')]
            betonline_H_odds = betonline_H[betonline_H.find(' ') + 1:]
            H.append(betonline_H_line)
            H.append(betonline_H_odds)
        else:
            H.append(consensus_H)
            H.append(pinnacle_H.replace(u'\xa0',' ').replace(u'\xbd','.5'))
            H.append(fivedimes_H.replace(u'\xa0',' ').replace(u'\xbd','.5'))
            H.append(heritage_H.replace(u'\xa0',' ').replace(u'\xbd','.5'))
            H.append(bovada_H.replace(u'\xa0',' ').replace(u'\xbd','.5'))
            H.append(betonline_H.replace(u'\xa0',' ').replace(u'\xbd','.5'))
        
	##For testing purposes..
	#for j in range(len(A)):
		#print 'Test: ', A[j]

        ## Take data from A and H (lists) and put them into DataFrame
        df.loc[counter]   = ([A[j] for j in range(len(A))])
        df.loc[counter+1] = ([H[j] for j in range(len(H))])
        counter += 2
    return df

def select_and_rename(df, text):
    ## Select only useful column names from a DataFrame
    ## Rename column names so that when merged, each df will be unique 
    if text[-2:] == 'ml':
        df = df[['key','time','team','opp_team', 'consensus',
                 'pinnacle','5dimes','heritage','bovada','betonline']]
    ## Change column names to make them unique
        df.columns = ['key',text+'_time','team','opp_team',
                      text+'_PIN',text+'_FD',text+'_HER',text+'_BVD',text+'_BOL']
    else:
        df = df[['key','time','team','opp_team', 'consensus',
                 'pinnacle_line','pinnacle_odds',
                 '5dimes_line','5dimes_odds',
                 'heritage_line','heritage_odds',
                 'bovada_line','bovada_odds',
                 'betonline_line','betonline_odds']]
        df.columns = ['key',text+'_time','team','opp_team', text+'_consensus',
                      text+'_PIN_line',text+'_PIN_odds',
                      text+'_FD_line',text+'_FD_odds',
                      text+'_HER_line',text+'_HER_odds',
                      text+'_BVD_line',text+'_BVD_odds',
                      text+'_BOL_line',text+'_BOL_odds']
    return df
    

def DoAnalysis(todays_date):
    print("DATE: "+todays_date)
    
    # connectTor()

    ## change todays_date to be whatever date you want to pull in the format 'yyyymmdd'
    ## One could force user input and if results in blank, revert to today's date. 
    # todays_date = '20141008'

    ## store BeautifulSoup info for parsing
    soup_ml, time_ml = soup_url('ML', todays_date)
    #print("getting today's MoneyLine (1/6)")
    soup_rl, time_rl = soup_url('Spreads', todays_date)
    #print("getting today's Spreads (2/6)")
    soup_tot, time_tot = soup_url('Totals', todays_date)
    #print("getting today's totals (3/6)")
    # soup_1h_ml, time_1h_ml = soup_url('1H', todays_date)
    # print "getting today's 1st-half MoneyLine (4/6)"
    # soup_1h_rl, time_1h_rl = soup_url('1HRL', todays_date)
    # print "getting today's 1st-half RunLine (5/6)"
    # soup_1h_tot, time_1h_tot = soup_url('1Htotal', todays_date)
    # print "getting today's 1st-half totals (6/6)"

    
    #### Each df_xx creates a data frame for a bet type
    print("writing today's MoneyLine (1/3)")
    df_ml = parse_and_write_data(soup_ml, todays_date, time_ml, not_ML = False)
    # print(df_ml)
    ## Change column names to make them unique
    df_ml.columns = ['key','date','ml_time','team',
                     'opp_team', 'ml_consensus',
                     'ml_PIN','ml_FD','ml_HER','ml_BVD','ml_BOL']    

    print("writing today's RunLine (2/3)")
    df_rl = parse_and_write_data(soup_rl, todays_date, time_rl)
    df_rl = select_and_rename(df_rl, 'rl')
    df_ml.drop(columns=['date', 'ml_time'])

    
    print("writing today's totals (3/3)")
    df_tot = parse_and_write_data(soup_tot, todays_date, time_tot)
    df_tot = select_and_rename(df_tot, 'tot')
    
    # print "writing today's 1st-half MoneyLine (4/6)"
    # df_1h_ml = parse_and_write_data(soup_1h_ml, todays_date, time_1h_ml, not_ML = False)
    # df_1h_ml = select_and_rename(df_1h_ml,'1h_ml')
    
    # print "writing today's 1st-half RunLine (5/6)"
    # df_1h_rl = parse_and_write_data(soup_1h_rl, todays_date, time_1h_rl)
    # df_1h_rl = select_and_rename(df_1h_rl,'1h_rl')
    
    # print "writing today's 1st-half totals (6/6)"
    # df_1h_tot = parse_and_write_data(soup_1h_tot, todays_date, time_1h_tot)
    # df_1h_tot = select_and_rename(df_1h_tot,'1h_tot')
    ## Merge all DataFrames together to allow for simple printout
    write_df = df_ml
    write_df = write_df.merge(
                df_rl, how='left', on = ['key','team','opp_team'])
    write_df = write_df.merge(
                df_tot, how='left', on = ['key','team','opp_team'])
    # write_df = write_df.merge(
                # df_1h_ml, how='left', on = ['key','team','pitcher','hand','opp_team'])
    # write_df = write_df.merge(
                # df_1h_rl, how='left', on = ['key','team','pitcher','hand','opp_team'])
    # write_df = write_df.merge(
                # df_1h_tot, how='left', on = ['key','team','pitcher','hand','opp_team'])
    
#    with open('Users\nathanbasch\Documents_NB\Projects\Betting\Intermediate\SBR_NBA_Lines.csv', 'a') as f:    
    #write_df.to_csv(r'/Users/nathanbasch/Documents_NB/Projects/Betting/Intermediate/SBR_NBA_Lines.csv', index=False)#, header = False)
  
    return write_df
    ## Code to pull tomorrow's data --- work in progress
    # if time.ml[:2] >= 12:
        # tomorrows_date = str(datetime.date.today() + datetime.timedelta(days=1)).replace('-','')
        # ## store BeautifulSoup info for parsing
        # soup_ml, time_ml = soup_url('ML')
        # print "getting tomorrow's MoneyLine"
        # soup_rl, time_rl = soup_url('RL')
        # print "getting tomorrow's RunLine"
        # soup_tot, time_tot = soup_url('total')
        # print "getting tomorrow's totals"
        # soup_1h_ml, time_1h_ml = soup_url('1H')
        # print "getting tomorrow's 1st-half MoneyLine"
        # soup_1h_rl, time_1h_rl = soup_url('1HRL')
        # print "getting tomorrow's 1st-half RunLine"
        # soup_1h_tot, time_1h_tot = soup_url('1Htotal')
        # print "getting tomorrow's 1st-half totals"

        # parse_and_write_data(soup_ml, todays_date, time_ml, f)
        # parse_and_write_data(soup_rl, todays_date, time_rl, f)
        # parse_and_write_data(soup_tot, todays_date, time_tot, f)
        # parse_and_write_data(soup_1h_ml, todays_date, time_1h_ml, f)
        # parse_and_write_data(soup_1h_rl, todays_date, time_1h_rl, f)
        # parse_and_write_data(soup_1h_tot, todays_date, time_1h_tot, f)

#if __name__ == '__main__':
#    main()

def DoAnalysisPeriod (list_date): 
    appended_data = []
    
    for date in list_date:
        data = DoAnalysis(date)
        # store DataFrame in list
        appended_data.append(data)
    # see pd.concat documentation for more info
    appended_data = pd.concat(appended_data)

    return appended_data
    
    
        
## Get Historical Lines --------------------
      
    #historical_date_df = pd.read_csv("C:/Users/nated/Documents/Documents_NB/Projects/Betting/SBR Workflow/Intermediate/_old/games_to_pull_10_20.csv") 
   
   # date_list = historical_date_df['formatted_date'].values.tolist()
   # date_list_char = [str(i) for i in date_list]
    
    #Data for 2009
    #for year in range(2009, 2013):
    #    print(year)
    #    dates = [s for s in date_list_char if s[:4] == str(year)] 
        
    #    data = DoAnalysisPeriod(dates)
        # write DataFrame to an excel sheet 
    #    data.to_csv(r'C:/Users/nated/Documents/Documents_NB/Projects/Betting/SBR Workflow/Intermediate/Downloaded Books/'+str(year)+ '_SBR_NBA_Lines.csv', index=False)#, header = False)

    


#Get recent lines ---
    #todays_date = str(date.today()).replace('-','')
    #test = DoAnalysis(todays_date)
    
    
    #dates_test = [s for s in date_list_char if s[:4] == str(2010)][35:45] 

    #data_test = DoAnalysisPeriod(dates_test)

    
    
