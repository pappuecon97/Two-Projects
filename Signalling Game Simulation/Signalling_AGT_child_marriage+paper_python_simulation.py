import numpy as np
import matplotlib.pyplot as plt

class SignalingGame:
    def __init__(self):
        # Market Parameters (in BDT)
        self.dowry_base = 50000        # Base dowry (parent pays groom)
        self.age_penalty = 15000       # Grooms demand 15k more if girl is older (18 vs 15)
        
        # Productivity Gains from SS Training
        # High Ability girls learn fast -> High return
        self.skill_premium_H = 25000   
        # Low Ability girls learn slow -> Low return
        self.skill_premium_L = 5000    
        
    def solve_incentives(self, cash_transfer):
        """
        Calculates the Net Utility of WAITING vs MARRYING EARLY
        for both types of families.
        """
        # --- SCENARIO 1: EARLY MARRIAGE (Age 15) ---
        # Parent pays Base Dowry. 
        # Utility = -Dowry (Negative because it's a cost)
        u_early = -self.dowry_base
        
        # --- SCENARIO 2: LATE MARRIAGE (Age 18 + SS Training) ---
        # Cost: Base Dowry + Age Penalty (Grooms want more money for older brides)
        # Benefit: Cash Transfer (C) + Skill Premium (Groom accepts lower dowry for skilled bride)
        
        # Note: We assume the Groom passes the 'Skill Value' back to the parent 
        # via a dowry reduction.
        
        # High Type Utility
        u_late_H = -(self.dowry_base + self.age_penalty) + self.skill_premium_H + cash_transfer
        
        # Low Type Utility
        u_late_L = -(self.dowry_base + self.age_penalty) + self.skill_premium_L + cash_transfer
        
        # Net Incentive to Wait (Late Utility - Early Utility)
        incentive_H = u_late_H - u_early
        incentive_L = u_late_L - u_early
        
        return incentive_H, incentive_L

# --- SIMULATION SETUP ---
sim = SignalingGame()
cash_values = np.linspace(0, 25000, 100) # Test transfers from 0 to 25k BDT

incentives_H = []
incentives_L = []

for c in cash_values:
    h_val, l_val = sim.solve_incentives(c)
    incentives_H.append(h_val)
    incentives_L.append(l_val)

# --- CRITICAL FIX: Convert lists to NumPy arrays for vector operations ---
incentives_H = np.array(incentives_H)
incentives_L = np.array(incentives_L)

# --- PLOTTING ---
plt.figure(figsize=(12, 7))

# Plot lines
plt.plot(cash_values, incentives_H, label='Incentive for HIGH Ability', color='green', linewidth=2.5)
plt.plot(cash_values, incentives_L, label='Incentive for LOW Ability', color='red', linewidth=2.5, linestyle='--')

# Add Zero Line (Decision Boundary)
plt.axhline(0, color='black', linewidth=1)

# FIND THE REGIONS
# 1. Separating Interval: High > 0 AND Low < 0
sep_mask = (incentives_H > 0) & (incentives_L < 0)

if np.any(sep_mask):
    # Fill the "Sweet Spot"
    plt.fill_between(cash_values, -15000, 20000, where=sep_mask, 
                     color='yellow', alpha=0.3, label='Separating Equilibrium (Sweet Spot)')
    
    # Annotate limits
    # We use np.argmax to find the first index where the condition is True
    min_idx = np.argmax(incentives_H > 0) 
    max_idx = np.argmax(incentives_L > 0)
    
    min_c = cash_values[min_idx]
    max_c = cash_values[max_idx]
    
    plt.axvline(min_c, color='gray', linestyle=':')
    plt.axvline(max_c, color='gray', linestyle=':')
    
    plt.text(min_c, 5000, f' Min CCT: {int(min_c)}', rotation=90, verticalalignment='center')
    plt.text(max_c, 5000, f' Max CCT: {int(max_c)}', rotation=90, verticalalignment='center')

# Labels and Styling
plt.title('Designing the Optimal Policy: The Separating Equilibrium', fontsize=16)
plt.xlabel('Cash Transfer Amount (BDT)', fontsize=14)
plt.ylabel('Net Incentive to Delay Marriage (BDT)', fontsize=14)
plt.legend(loc='upper left', fontsize=12)
plt.grid(True, alpha=0.3)
plt.tight_layout()

plt.show()

print("SIMULATION INTERPRETATION:")
print("1. If CCT < Min: Market Failure (Everyone Marries Early)")
print("2. If CCT is in Yellow Zone: Success (High potential girls wait, Low potential marry)")
print("3. If CCT > Max: Fiscal Waste (Everyone waits just for the cash, regardless of skill)")



