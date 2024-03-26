import pandas as pd
import sqlite3

# Data from: https://www.kaggle.com/datasets/wyattowalsh/basketball?resource=download
conn = sqlite3.connect(
    r"data/nba.sqlite", isolation_level=None, detect_types=sqlite3.PARSE_COLNAMES
)
db_df = pd.read_sql_query("SELECT * FROM game", conn)
db_df.to_csv(r"data/game_data.csv", index=False)
