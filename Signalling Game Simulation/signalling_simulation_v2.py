"""
Signaling Equilibrium Simulation for Adolescent Marriage Markets
----------------------------------------------------------------
Author: Md Johirul Islam, BIGD, BRAC University
Date: November 2025
Description: This script simulates a Signaling Game to find the optimal 
Conditional Cash Transfer (CCT) amount that induces a Separating Equilibrium.
It includes modules to calibrate parameters from survey vignette data. 
For now, it uses the assumed calibrated values based on literature and field tests.
"""

import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# ==========================================
# 1. DATA CALIBRATION MODULE
# ==========================================

def calibrate_parameters_from_survey(file_path=None):
    """
    Calibrates simulation parameters using Vignette data from the Midline Survey.
    
    If file_path is None, returns calibrated default values based on 
    preliminary field findings (Section F1 of the Instrument).
    """
    
    if file_path is None:
        print("⚠️ No survey file provided. Using calibrated defaults from Field Test...")
        # Stylized facts from Bangladesh rural marriage market (in BDT)
        return {
            'dowry_base': 50000,       # Cost for 15yo, Class 5
            'age_penalty': 20000,      # Extra cost for being 19yo (Unskilled)
            'skill_premium_H': 25000,  # Dowry reduction for HSC/Vocational (High Type)
            'skill_premium_L': 5000    # Dowry reduction for Class 8 (Low Type)
        }
    
    # Logic for processing actual survey data: for future implementation
    try:
        df = pd.read_csv(file_path)
        
        # Vignette IDs from Questionnaire (Page 55-61)
        # 4a: Age 15, Class 5
        # 6a: Age 19, Class 8 (Unskilled Control)
        # 7a: Age 19, HSC (Skilled Treatment)
        
        # Handle -999 or missing values if necessary
        dowry_base = df['q4a'].mean()
        dowry_19_unskilled = df['q6a'].mean()
        dowry_19_skilled = df['q7a'].mean()
        
        # Calculate Derivatives
        age_penalty = dowry_19_unskilled - dowry_base
        skill_premium = dowry_19_unskilled - dowry_19_skilled
        
        return {
            'dowry_base': dowry_base,
            'age_penalty': age_penalty,
            'skill_premium_H': skill_premium * 1.5, # Assumes High types leverage skill better
            'skill_premium_L': skill_premium * 0.5
        }
        
    except FileNotFoundError:
        print("Error: Survey file not found.")
        return None

# ==========================================
# 2. SIMULATION ENGINE
# ==========================================

class SignalingGame:
    def __init__(self, params):
        self.dowry_base = params['dowry_base']
        self.age_penalty = params['age_penalty']
        self.skill_premium_H = params['skill_premium_H']
        self.skill_premium_L = params['skill_premium_L']
        
    def calculate_net_utility(self, cash_transfer):
        """
        Calculates the Net Utility of DELAYING marriage vs. EARLY marriage.
        Formula: U(Delay) - U(Early)
        """
        # Utility of Early Marriage (Baseline reference point = 0 cost delta)
        # We normalize relative to the base dowry.
        
        # Scenario: Delaying until 19
        # Cost: Family pays extra 'Age Penalty'
        # Benefit: Family receives 'Skill Premium' (lower dowry) + Cash Transfer
        
        # Net Benefit for High Ability Type
        net_utility_H = cash_transfer + self.skill_premium_H - self.age_penalty
        
        # Net Benefit for Low Ability Type
        net_utility_L = cash_transfer + self.skill_premium_L - self.age_penalty
        
        return net_utility_H, net_utility_L

# ==========================================
# 3. MAIN EXECUTION
# ==========================================

if __name__ == "__main__":
    # A. Load Parameters
    params = calibrate_parameters_from_survey() # Uses default calibration
    
    # B. Run Simulation over a range of Cash Transfer values (0 to 40k BDT)
    cash_values = np.linspace(0, 40000, 100)
    
    results_H = []
    results_L = []
    
    game = SignalingGame(params)
    
    for c in cash_values:
        u_h, u_l = game.calculate_net_utility(c)
        results_H.append(u_h)
        results_L.append(u_l)
        
    # --- CRITICAL FIX: Convert to arrays for vector logic ---
    results_H = np.array(results_H)
    results_L = np.array(results_L)
    
    # C. Identify Equilibria Zones
    # Separating Equilibrium: High types gain from waiting (>0), Low types lose (<0)
    separating_mask = (results_H > 0) & (results_L < 0)
    
    # D. Visualization
    plt.figure(figsize=(10, 6))
    
    # Plot Utility Curves
    plt.plot(cash_values, results_H, label='High Ability Household', color='green', linewidth=2)
    plt.plot(cash_values, results_L, label='Low Ability Household', color='red', linewidth=2, linestyle='--')
    
    # Highlight The Separating Interval
    if np.any(separating_mask):
        plt.fill_between(cash_values, np.minimum(results_L, 0), np.maximum(results_H, 0), 
                         where=separating_mask, color='yellow', alpha=0.3, 
                         label='Separating Equilibrium (Optimal Policy)')
        
        # Find numeric bounds using boolean indexing
        valid_indices = np.where(separating_mask)[0]
        if len(valid_indices) > 0:
            min_c = cash_values[valid_indices[0]]
            max_c = cash_values[valid_indices[-1]]
            
            plt.axvline(min_c, color='gray', linestyle=':')
            plt.axvline(max_c, color='gray', linestyle=':')
            
            # Annotate
            plt.text(min_c, 5000, f' Min: {int(min_c)} BDT', rotation=90, verticalalignment='center')
            plt.text(max_c, 5000, f' Max: {int(max_c)} BDT', rotation=90, verticalalignment='center')

    plt.axhline(0, color='black', linewidth=1)
    plt.title('Mechanism Design: Optimal Cash Transfer for Child Marriage Prevention', fontsize=14)
    plt.xlabel('Cash Transfer Value (Cumulative BDT)', fontsize=12)
    plt.ylabel('Net Economic Incentive to Delay Marriage', fontsize=12)
    plt.legend()
    plt.grid(True, alpha=0.3)
    
    print("Simulation complete. Generating plot...")
    plt.show()

    