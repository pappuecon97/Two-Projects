# Signaling Equilibrium Simulation: Mechanism Design for Child Marriage Interventions

**Author:** Md Johirul Islam\
**Context:** Research Project at BRAC Institute of Governance and Development (BIGD)\
**Topic:** Algorithmic Game Theory, Mechanism Design, Development Economics

## 📌 Project Overview

This project uses **Mechanism Design** and **Signaling Game Theory** to model household decision-making regarding child marriage in rural Bangladesh.

In marriage markets characterized by asymmetric information, a bride's youth often acts as a proxy signal for unobservable qualities (e.g., docility). Consequently, delaying marriage to acquire skills can paradoxically generate a negative signal, resulting in an "Age Penalty" (higher dowry). Standard Conditional Cash Transfers (CCTs) often fail because they treat the problem as a budget constraint rather than a signaling friction.

This simulation identifies the **optimal cash transfer value ($C^*$)** required to induce a **Separating Equilibrium**—a state where high-ability candidates delay marriage to signal quality, while low-ability candidates do not, preventing fiscal waste (rent-seeking).

> **Note:** The variable $C^*$ represents the **Total Cumulative Transfer** over the intervention period (e.g., 2–3 years), which counters the lump-sum "Age Penalty" in the marriage market.

## ⚙️ Theoretical Framework

The simulation models a **Spence Signaling Game** with the following agents:

1. **Senders (Parents):** Choose strategy $S \in \{\text{Early}, \text{Late}\}$ based on their daughter's latent type $\theta \in \{\text{High}, \text{Low}\}$.
2. **Receivers (Grooms):** Update beliefs about $\theta$ based on $S$ and offer a market-clearing dowry.
3. **Planner:** Sets a transfer $C$ to satisfy Incentive Compatibility Constraints (ICC).

### The Objective Function

Find the interval for $C$ such that:

$$
\text{Payoff}(Late, \text{High}) > \text{Payoff}(Early, \text{High})
$$

$$
\text{Payoff}(Late, \text{Low}) < \text{Payoff}(Early, \text{Low})
$$

## 📊 Data Calibration

The simulation parameters are not arbitrary; they are calibrated using data from a **Midline Survey (N=11,250)** conducted in Bangladesh (November 2024). Specifically, we utilize a **Vignette Experiment** (Section F1 of the survey instrument) to estimate:

* **Base Dowry ($\tau_{base}$):** Derived from vignettes describing 15-year-old brides (Class 5).
* **Age Penalty ($\beta$):** Derived by comparing dowry demands for 15-year-old vs. 19-year-old brides with identical education.
* **Skill Premium ($\alpha$):** Derived by comparing dowry demands for 19-year-old brides with Vocational Skills (proxied by HSC) vs. those without.

## 🚀 Key Results

The simulation generates a "Policy Feasibility Graph" that identifies three zones:

1. **Market Failure (Red Zone):** Transfer is too low to overcome the Age Penalty.
2. **Separating Equilibrium (Yellow Zone):** The optimal policy window. High-types wait; Low-types marry early. Signal quality is preserved.
3. **Rent Seeking (Green Zone):** Transfer is too high. Everyone delays marriage solely for the cash, diluting the signal value of the training program.

