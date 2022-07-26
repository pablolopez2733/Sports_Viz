

wd = "C:/Users/USER/Documents/Github_Repos/Analytics-Experiments/Soccer/Predictability"


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
    
toi = pd.read_csv("C:/Users/USER/Documents/Github_Repos/Analytics-Experiments/Soccer/Predictability/data/toi.csv")
plot_df = pd.merge(plot_df, toi, how="left", left_on= "team_id",right_on="team",right_index=False)
plot_df = plot_df[~plot_df["team"].isna()]
plot_df = plot_df[["team","cum_mean","color"]]
    
# %% PLOT
def plot_team_brier_score(ax, team, data, label_y=True, label_x=True):
    '''
    Plots the cumulative Brier Score for a given side
    with all the other teams in the backgorund in a lighter
    color.
    
    '''
    df = data.copy()
    team_df = df[df["team"] == team].reset_index(drop = True)
    color = team_df["color"].iloc[0]
    
    ax.plot(
        team_df.index,
        team_df["cum_mean"],
        color = color,
        lw = 1.75,
        zorder = 3,
        marker = "o",
        markerevery = [-1],
        markeredgecolor = "#EFE9E6"
        )
    
    ax.annotate(
        xy = (team_df.index[-1], team_df["cum_mean"].iloc[-1]),
        xytext = (15,0),
        text = f'{team_df["cum_mean"].iloc[-1]:.3f}',
        textcoords = "offset points",
        ha = "center",
        va = "center",
        weight = 'bold',
        color = color,
        size = 8
        )
    
    for x in df["team"].unique():
        if x == team:
            continue
        aux_df = df[df['team'] == x].reset_index(drop = True)
    
    ax.plot(aux_df.index,
            aux_df['cum_mean'],
            color = "gray",
            alpha = 0.15,
            lw = 1.25,
            zorder = 2)
    ax.grid(ls = ':', color = 'lightgrey')
    ax.spines['top'].set_visible (False)
    ax.spines['right'].set_visible(False)
    ax.yaxis.set_major_locator(ticker.MultipleLocator(0.05))
    
    if label_y:
        ax.set_ylabel("Brier score")
    else:
        ax.set_yticklabels([])
    if label_x:
        ax.set_xlabel("Match day")
    else:
        ax.set_xlabel([])
    return ax

order_teams = (
    plot_df.groupby(["team"])
    ["cum_mean"]
    .last()
    .reset_index()
    .sort_values(by = "cum_mean")
)

fig = plt.figure(figsize = (14,14), dpi = 200)
nrows = 10
ncols = 4
gspec = gridspec.GridSpec(
    ncols=ncols, nrows=nrows, figure=fig, 
    height_ratios = [(1/nrows)*2. if x % 2 != 0 else (1/nrows)/2. for x in range(nrows)], hspace = 0.3
)


plot_counter = 0
logo_counter = 0

for row in range(nrows):
    for col in range(ncols):
        if row % 2 != 0:
            ax = plt.subplot(
                gspec[row, col],
                facecolor = "#EFE9E6"
            )

            teamId = order_teams["team"].iloc[plot_counter]

            if col == 0:
                labels_y = True
            else:
                labels_y = False
            
            if row == nrows - 1:
                labels_x = True
            else:
                labels_x = False
            
            plot_team_brier_score(ax, teamId, plot_df, labels_y, labels_x)           

            plot_counter += 1

        else:

            teamId = order_teams["team"].iloc[logo_counter]
            teamName = plot_df[plot_df["team"] == teamId]["team"].iloc[0]

            # fotmob_url = "https://images.fotmob.com/image_resources/logo/teamlogo/"
            # logo_ax = plt.subplot(
            #     gspec[row,col],
            #     anchor = "NW", facecolor = "#EFE9E6"
            # )
            # club_icon = Image.open(urllib.request.urlopen(f"{fotmob_url}{teamId:.0f}.png")).convert("LA")
            # logo_ax.imshow(club_icon)
            # logo_ax.axis("off")



            # # Add the team name
            ax_text(
                x = 1.1, 
                y = 0.7,
                s = f"{team}",
                #ax = logo_ax, 
                weight = "bold", 
                font = "Karla", 
                ha = "left", 
                size = 13, 
                annotationbbox_kw = {"xycoords":"axes fraction"}
            )

            logo_counter += 1

fig_text(
    x = 0.11, y = .96, 
    s = "Which Champions League teams are the most predictable?",
    va = "bottom", ha = "left",
    fontsize = 25, color = "black", font = "DM Sans", weight = "bold"
)
fig_text(
	x = 0.11, y = .9, 
    s = "Cumulative average Brier Score of 538's Premier League match predictions | Season 2021/2022 | viz by @sonofacorner\n<Brier Score> is a proper score function that measures the accuracy of probabilistic predictions.\nThe higher the Brier Score, the less accurate the prediction.",
    highlight_textprops=[{"weight": "bold", "color": "black"}],
	va = "bottom", ha = "left",
	fontsize = 13, color = "#4E616C", font = "Karla"
)
