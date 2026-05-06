#########################################################
# Fig3F.py 
#########################################################
# Extended simulation overview of allele loss probability 
# with additional fitness benefit ranging from 0% to 50% with 5% interval
# Fly attack rate: 0.15

import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.ticker import FuncFormatter

from simulation import simulate_selection_drift_xlinked
np.random.seed(2016)

# Layout
sns.set_theme(
    style="ticks",  
    font="Arial",
    font_scale=1.3,
    rc={
        "axes.edgecolor": "black",    
        "axes.linewidth": 1.2,       
        "axes.facecolor": "white",    
        "grid.color": "0.5",         
        "grid.linestyle": "-",        
        "grid.linewidth": 0.6,
        "xtick.direction": "in",      
        "ytick.direction": "in",
        "xtick.major.width": 1.0,
        "ytick.major.width": 1.0,
        "axes.titlesize": 20,
        "axes.labelsize": 20,
        "legend.frameon": False,      
        "axes.grid": False,           
    }
)


# fit_gain_small ∈ [0.0, 0.5] 
# addtional fitness range from 0 to 50%
fit_range = np.linspace(1.0, 1.5, 11)   
loss_prob, mean_final, sd_final = [], [], []

for fit_gain in fit_range:
    time_series, final_freqs, stats = simulate_selection_drift_xlinked(
        N=200, generations=50, repeats=10000,
        S_long=0.15, S_small=0.0, 
        att_long=0.8, att_small=0.2,
        fit_gain_small=fit_gain
    )
    loss_prob.append(stats["loss_prob"])
    mean_final.append(stats["mean_final"])
    sd_final.append(stats["sd_final"])
    print(f"fit_gain_small={fit_gain:.2f} → loss_prob={stats['loss_prob']:.3f}, mean={stats['mean_final']:.3f} ± {stats['sd_final']:.3f}")

# Plot Figure 3F
fig = plt.figure(figsize=(8, 5))

plt.plot(
    fit_range, loss_prob,    
    '-o',  
    color="#5D9CEE", lw=2, markersize=5,  
    markerfacecolor='white', markeredgewidth=1.5
)

plt.xlabel("Additional fitness of Sw associated allele Xˢ (%)", fontsize=18, fontname='Arial')
plt.ylabel("Allele loss probability", fontsize=18, fontname='Arial')

def percent_fmt(x, _):
    return f"{(x - 1) * 100:.0f}%"

ax = plt.gca()
ax.xaxis.set_major_formatter(FuncFormatter(percent_fmt)) 
ax.set_yticks(np.arange(0.2, 1.01, 0.2))
ax.tick_params(axis='x', labelsize=18)
ax.tick_params(axis='y', labelsize=18)
plt.tight_layout()
plt.show()