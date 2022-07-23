

wd = "C:/Users/USER/Documents/Github_Repos/Analytics-Experiments/Soccer/Predictability_LigaMX"


# %% Imports:
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm
import matplotlib.ticker as ticker
import matplotlib.gridspec as gridspec
import matplotlib.patheffects as path_effects
from matplotlib import rcParams
from highlight_text import ax_text, fig_text
import pandas as pd

from PIL import Image
import urllib
import os
# %% LECTURA DE DATOS

# Leemos datos:
df = pd.read_csv(f"{wd}/data/spi_matches.csv")

# Filter CL matches:
df["date"] = pd.to_datetime(df["date"])
cl = df[(df.league_id == 1818) & (df.season == 2021)]

t = sorted(cl["team1"].unique())
# Muchos equipos, seleccionamos solo los que jugaron 16avos:
teams = cl.loc[cl['date'] > "01/01/2022",['date','team1']]
toi = teams["team1"].unique()

# %%  CALCULATIONS Y PLOT DF
# Ya tenemos teams of interest, entonces vemos quien ganó:
cl["home_win"] = [1 if x > y else 0 for x,y in zip(cl["score1"],cl["score2"])]
cl["away_win"] = [1 if x < y else 0 for x,y in zip(cl['score1'],cl['score2'])]
cl["tie"] = [1 if x == y else 0 for x,y in zip(cl['score1'],cl['score2'])]

# Compute Brier Score:
cl = cl.assign(
    brier_score = lambda x:
        1/3 *((x.prob1 - x.home_win)**2 +
              (x.probtie - x.tie)**2 +
              (x.prob2 - x.away_win)**2) 
    )

# Cumulative avg of Brier Score over season:
teams = cl[["team1"]].drop_duplicates()
plot_df = pd.DataFrame()

for team in teams['team1']:
    aux_df = cl[(cl['team1'] == team) | (df['team2'] == team)].copy() # agarra de los partidos de champions, los que se juega cada equipo ya sea de local o visita
    cum_mean = (aux_df['brier_score'].expanding().mean()) # crea un "vector" de los brier scores
    new_df = pd.DataFrame()
    new_df['cum_mean'] = cum_mean
    new_df['team_id'] = team
    plot_df = plot_df.append(new_df)
    
# %% PLOT
