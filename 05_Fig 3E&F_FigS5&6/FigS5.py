#########################################################
# FigS5.py 
#########################################################
# Extended simulation with fly attack rate ranging from 0.25 to 0.75 
# Additional fitness gain: 0

import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np

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
        "axes.grid": False        
    }
)

# Set up fly attack rate: prelong 
prelong_list = [0.25, 0.5, 0.75]

fig, axes = plt.subplots(1, 3, figsize=(18, 5)) 
axes = axes.flatten()  

for i, prelong in enumerate(prelong_list):
    ax = axes[i]  

    time_series, final_freqs, stats = simulate_selection_drift_xlinked(
        N=200, generations=50, repeats=10000,
        S_long=prelong, S_small=0.0, att_long=0.8, att_small=0.2,
        fit_gain_small=1.0  # Fitness gain = 0
    )

    time_series = np.array(time_series)
    gens = np.arange(time_series.shape[1])
    mean_traj = np.mean(time_series, axis=0)
    lower_ci = np.percentile(time_series, 2.5, axis=0)
    upper_ci = np.percentile(time_series, 97.5, axis=0)

    labels = ["A", "B", "C", "D"]
    ax.text(-0.12, 1.05, labels[i],
            transform=ax.transAxes,
            fontsize=22,
            fontname='Arial',
            fontweight='bold')

    for traj in time_series[:100]:
        ax.plot(gens, traj, color='gray', alpha=0.2, lw=0.7)
    ax.plot(gens, mean_traj, color='red', lw=2, label='Mean allele trajectory')
    ax.fill_between(gens, lower_ci, upper_ci, color='royalblue', alpha=0.15, label='95% CI')
    ax.legend(loc='upper left', fontsize=12)
    ax.text(0.03, 0.8, f"loss probability = {stats['loss_prob']:.2f}",
            transform=ax.transAxes, fontsize=12,
            bbox=dict(facecolor='white', alpha=0.7))
    ax.set_xlabel('Generation', fontsize=18)
    if i == 0:
        ax.set_ylabel('Allele frequency (Xˢ)', fontsize=18)
    ax.set_title(f'fly predation = {prelong}', fontsize=16)
    ax.grid(alpha=0.3)
    ax.set_xlim(0, 50)
    ax.set_xticks(np.arange(0, 51, 10))
    ax.set_ylim(0, 1.0)
    ax.set_yticks(np.arange(0.2, 1.01, 0.2))

plt.tight_layout()
plt.show()