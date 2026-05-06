#########################################################
# simulation.py
#########################################################

import numpy as np

# simulate allele frequency trajectories of Sw associated allele (Xˢ) under:
# (1) natural selection (fly predation)
# (2) sexual selection (mate attraction)
# (3) optional fitness benefits
# with genetic drift

# Parameters:
# S_long           # fly selection on Long-wing males
# S_small          # fly selection on Small-wing males
# att_long,        # relative mate attraction of Long-wing males
# att_small,       # relative mate attraction of Small-wing males
# fit_gain_small   # optional general fitness advantage in Small-wing associated allele (Xˢ)
# N                # population size
# generations      # number of simulation generations
# init_counts      # initial individual counts of each genotype
# repeats          # number of simulation replicates


def simulate_selection_drift_xlinked(
        S_long,    
        S_small,    
        att_long,      
        att_small,     
        fit_gain_small, 
        N=200,          
        generations=50, 
        init_counts=None,
        repeats=10000      
        
):
    
    if init_counts is None:     
        half = N // 2                            # sex ratio = 1:1, Nmale = Nfemale = N//2
        init_counts = {
            'f_XᴸXᴸ': int(round(0.49 * half)),   # 49% Homozygous long-wing female XᴸXᴸ
            'f_XᴸXˢ': int(round(0.42 * half)),   # 42% Heterozygous long-wing female XᴸXˢ
            'f_XˢXˢ': int(round(0.09 * half)),   #  9% Homozygous small-wing female XˢXˢ
            'm_XᴸO': int(round(0.70 * half)),    # 70% Homozygous long-wing male XᴸO
            'm_XˢO': int(round(0.30 * half))     # 30% Homozygous small-wing male XˢO
        }

    final_freqs = []
    time_series = []

    for r in range(repeats):          
        counts = init_counts.copy()  # reset to initial genotype counts for each replicate                              
        p_traj = []                  # store allele frequency trajectory for this replicate    

        for gen in range(generations + 1):
            # calculate count of each genotype and sex
            f_XᴸXᴸ, f_XᴸXˢ, f_XˢXˢ = counts['f_XᴸXᴸ'], counts['f_XᴸXˢ'], counts['f_XˢXˢ']
            m_XᴸO, m_XˢO = counts['m_XᴸO'], counts['m_XˢO']
            
            # sum of X chromosomes of all individuals
            female_Xˢ = f_XˢXˢ*2 + f_XᴸXˢ   # sum of Xˢ in females
            male_Xˢ = m_XˢO                 # sum of Xˢ in males
            
            total_female_X = (f_XᴸXᴸ + f_XᴸXˢ + f_XˢXˢ) * 2 # total female X chromosomes 
            total_male_X = m_XᴸO + m_XˢO                    # total male X chromosomes

            # frequency of Xˢ across all individuals
            f_Xˢ_overall = (female_Xˢ + male_Xˢ) / (total_female_X + total_male_X) 

            # record allele frequncy of Xˢ in current generation
            p_traj.append(f_Xˢ_overall)

            # Break in last generation
            if gen == generations:
                break

            # 1) Natural selection: fly attack
            surv_m_XᴸO = np.random.binomial(m_XᴸO, 1 - S_long) if m_XᴸO>0 else 0
            surv_m_XˢO = np.random.binomial(m_XˢO, 1 - S_small) if m_XˢO>0 else 0

            # 2) Sexual selection: mate attraction
            weight_XᴸO = att_long
            weight_XˢO = att_small

            # 3) Paternal and maternal gamete contribution
            # Paternal gamete 
            paternal_Xᴸ = surv_m_XᴸO * weight_XᴸO   
            paternal_Xˢ = surv_m_XˢO * weight_XˢO
            paternal_total_X = paternal_Xᴸ + paternal_Xˢ

            prob_pat_Xᴸ = paternal_Xᴸ / paternal_total_X
            prob_pat_Xˢ = paternal_Xˢ / paternal_total_X

            # Maternal gamete 
            maternal_Xᴸ = f_XᴸXᴸ*2 + f_XᴸXˢ*1
            maternal_Xˢ = f_XˢXˢ*2 + f_XᴸXˢ*1
            maternal_total_X = maternal_Xᴸ + maternal_Xˢ
            
            prob_mat_Xᴸ = maternal_Xᴸ / maternal_total_X
            prob_mat_Xˢ = maternal_Xˢ / maternal_total_X           

            # 4) optional: general fitness advantage in small-wing associated allele (Xˢ)
            # base model (no additional benefit): fit_gain_small = 1.0
            prob_pat_Xˢ *= fit_gain_small 
            prob_mat_Xˢ *= fit_gain_small 

            # Normalization 
            sum_mat = prob_mat_Xᴸ + prob_mat_Xˢ
            sum_pat = prob_pat_Xᴸ + prob_pat_Xˢ

            prob_mat_Xᴸ /= sum_mat
            prob_mat_Xˢ /= sum_mat
            prob_pat_Xᴸ /= sum_pat
            prob_pat_Xˢ /= sum_pat

            # 5) Probability of offspring genotype (sex ratio 1:1)             
            # expected offspring genotype frequencies
            # females
            prob_daughter_XᴸXᴸ = prob_mat_Xᴸ * prob_pat_Xᴸ 
            prob_daughter_XᴸXˢ = prob_mat_Xᴸ * prob_pat_Xˢ + prob_mat_Xˢ * prob_pat_Xᴸ
            prob_daughter_XˢXˢ = prob_mat_Xˢ * prob_pat_Xˢ

            # males 
            # only inherited from their mother solely
            prob_son_XᴸO = prob_mat_Xᴸ
            prob_son_XˢO = prob_mat_Xˢ

            sexual_ratio = 0.5

            prob_off = {
                'f_XᴸXᴸ': sexual_ratio * prob_daughter_XᴸXᴸ,
                'f_XᴸXˢ': sexual_ratio * prob_daughter_XᴸXˢ,
                'f_XˢXˢ': sexual_ratio * prob_daughter_XˢXˢ,
            
                'm_XᴸO': sexual_ratio * prob_son_XᴸO,
                'm_XˢO': sexual_ratio * prob_son_XˢO
            }

            probs = np.array(list(prob_off.values()), dtype=float)
            
            s = probs.sum()

            probs /= s

            # 6) Genetic drift
            outcome = np.random.multinomial(N, probs)

            # update population counts
            counts = dict(zip(prob_off.keys(), outcome))

        # record allele frequency and the trajectory
        final_freqs.append(f_Xˢ_overall)
        time_series.append(p_traj)

    # Record allele fixation and loss probability
    final_freqs = np.array(final_freqs)
    n_fix = np.sum(final_freqs >= 0.999)
    n_loss = np.sum(final_freqs <= 0.001)
    n_mid = repeats - n_fix - n_loss

    stats = {
        "repeats": repeats,
        "n_fix": n_fix,
        "n_loss": n_loss,
        "segregating": n_mid,
        "fix_prob": n_fix / repeats,
        "loss_prob": n_loss / repeats,
        "mean_final": np.mean(final_freqs),
        "sd_final": np.std(final_freqs)
    }

    return np.array(time_series), final_freqs, stats