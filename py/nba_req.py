import pandas as pd
import requests

def get_url(season, season_type):
    return f"https://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&ISTRound=&LeagueID=00&PlayerOrTeam=P&Season={season}&SeasonType={season_type.replace(' ', '%20')}&Sorter=DATE"

headers = {
    "Connection": "keep-alive",
    "Accept": "application/json, text/plain, */*",
    "x-nba-stats-token": "true",
    "User-Agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36",
    "x-nba-stats-origin": "stats",
    "Sec-Fetch-Site": "same-origin",
    "Sec-Fetch-Mode": "cors",
    "Referer": "https://stats.nba.com/",
    "Accept-Encoding": "gzip, deflate, br",
    "Accept-Language": "en-US,en;q=0.9",
}

season_list = [
    # '1996-97',
    # '1997-98',
    # '1998-99',
    # '1999-00',
    # '2000-01',
    # '2001-02',
    # '2002-03',
    # '2003-04',
    # '2004-05',
    # '2005-06',
    # '2006-07',
    # '2007-08',
    # '2008-09',
    # '2009-10',
    # '2010-11',
    # '2011-12',
    # '2012-13',
    # '2013-14',
    # '2014-15',
    # '2015-16',
    # '2016-17',
    # '2017-18',
    # '2018-19',
    # '2019-20',
    # '2020-21',
    "2021-22",
    "2022-23",
    "2023-24",
]

season_type_list = [
    "Regular Season",
    "Preseason",
    "Playoffs",
    "All-Star",
    "Play In",
    "In-Season Tournament",
]

for i, season in enumerate(season_list):
    response = requests.get(
        url=get_url(season, season_type_list[0]), headers=headers
    ).json()
    cols = response["resultSets"][0]["headers"]
    data = response["resultSets"][0]["rowSet"]
    if not i: # if i is 0
        df = pd.DataFrame(data, columns=cols)
    else:
        df = pd.concat(
            [df, pd.DataFrame(data, columns=cols)], ignore_index=True, sort=False
        )
    print(f"Season {season} done!")

df.to_csv(r"data/boxscores.csv", index=False)