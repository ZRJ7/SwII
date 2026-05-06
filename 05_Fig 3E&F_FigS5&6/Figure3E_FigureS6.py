#########################################################
# Fig3E_FigS5.py 
#########################################################
# General simulation without additional fitness benefit
# Fly attack rate: 0.15

import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

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

# plot function
def plot_simulation(ax, fit_gain, label_letter=None):
    
    # run simulation
    time_series, final_freqs, stats = simulate_selection_drift_xlinked(
        N=200,
        generations=50,
        repeats=10000,
        S_long=0.15,
        S_small=0.0,
        att_long=0.8,
        att_small=0.2,
        fit_gain_small=fit_gain
    )

    time_series = np.array(time_series)
    gens = np.arange(time_series.shape[1])
    mean_traj = np.mean(time_series, axis=0)
    lower_ci = np.percentile(time_series, 2.5, axis=0)
    upper_ci = np.percentile(time_series, 97.5, axis=0)

    for traj in time_series[:100]:
        ax.plot(gens, traj, color='dimgray', alpha=0.15, lw=0.7)
    
    ax.fill_between(
        gens,
        lower_ci,
        upper_ci,
        linewidth=0,
        color='royalblue', 
        alpha=0.15, 
        label='95% CI'
    )

    # mean trajectory
    ax.plot(
        gens,
        mean_traj,
        color="#D81B1B",
        lw=2,
        label='Mean trajectory'
    )
    
    # axis settings 
    ax.set_xlabel('Generation', fontsize=20)
    ax.set_ylabel('Allele frequency (Xˢ)', fontsize=20)
    ax.set_xlim(0, 50)
    ax.set_xticks(np.arange(0, 51, 10))
    ax.set_ylim(0, 1.0)
    ax.set_yticks(np.arange(0.2, 1.01, 0.2))
    ax.tick_params(axis='x', labelsize=18)
    ax.tick_params(axis='y', labelsize=18)
    ax.legend(loc='upper left', fontsize=12)

    info_text = (
    f"Fly selection = 0.15\n"
    f"Allele loss prob = {stats['loss_prob']:.2f}"
    )
    ax.text(0.025, 0.75, info_text,
        transform=ax.transAxes,
        fontsize=12,
        fontname='Arial',
        bbox=dict(facecolor='white', alpha=0.7))
    ax.set_title(f'Additional fitness = {(fit_gain - 1) * 100:.0f}%', fontsize=16)
    ax.grid(alpha=0.3)

    # panel label A/B/C
    if label_letter is not None:
        ax.text(
            -0.12, 1.05,
            label_letter,
            transform=ax.transAxes,
            fontsize=22,
            fontweight='bold',
            va='top',
            ha='left'
        )

    return stats

# Fig3E: fit_gain = 0; fly predation = 0.15
fig1, ax1 = plt.subplots(figsize=(8, 5)) 

stats_1 = plot_simulation(ax1, fit_gain=1.0)

plt.tight_layout()
plt.show()

print("\n=== Simulation summary for fit_gain_small = 1.0 ===")
for k, v in stats_1.items():
    print(f"{k:12s}: {v}")

# FigS6: fit_gain = 20%-50%; fly predation = 0.15
fit_list = [1.2, 1.3, 1.4, 1.5]
panel_labels = ["A", "B", "C", "D"]

fig2, axes = plt.subplots(2, 2, figsize=(14, 10), sharey=True)  

axes = axes.flatten()

for ax, fit_gain, label in zip(axes, fit_list, panel_labels):
    stats = plot_simulation(ax, fit_gain=fit_gain, label_letter=label)

    ax.set_ylabel('Allele frequency (Xˢ)', fontsize=20)
    ax.tick_params(axis='y', labelsize=18, labelleft = True)

plt.tight_layout()
fig2.subplots_adjust(wspace=0.25)
plt.show()