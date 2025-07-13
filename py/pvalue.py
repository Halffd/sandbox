import numpy as np
from scipy import stats
import matplotlib.pyplot as plt

# Generate some fake data like your last "groundbreaking" model
np.random.seed(42)  # because reproducibility is apparently optional
group_a = np.random.normal(10, 2, 100)  # control group
group_b = np.random.normal(10.5, 2, 100)  # "revolutionary" treatment

# T-test because apparently this is news to some of you
t_stat, p_value = stats.ttest_ind(group_a, group_b)
print(f"P-value: {p_value:.6f}")
print(f"Statistically significant: {p_value < 0.05}")# Simulate running 100 A/B tests (like your startup definitely does)
num_tests = 100
p_values = []

for i in range(num_tests):
    # All null hypothesis true - no real difference
    fake_control = np.random.normal(0, 1, 50)
    fake_treatment = np.random.normal(0, 1, 50)  # literally the same
    
    _, p = stats.ttest_ind(fake_control, fake_treatment)
    p_values.append(p)

# How many "significant" results from pure noise?
false_positives = sum(p < 0.05 for p in p_values)
print(f"False positives at α=0.05: {false_positives}/{num_tests}")
print(f"Expected: ~{num_tests * 0.05}")
def bonferroni_correction(p_values, alpha=0.05):
    """
    For when you need to crush dreams mathematically
    """
    corrected_alpha = alpha / len(p_values)
    significant = [p < corrected_alpha for p in p_values]
    
    print(f"Original α: {alpha}")
    print(f"Corrected α: {corrected_alpha:.6f}")
    print(f"Significant before correction: {sum(p < alpha for p in p_values)}")
    print(f"Significant after correction: {sum(significant)}")
    
    return significant

# Reality check time
bonferroni_correction(p_values)
def benjamini_hochberg(p_values, fdr=0.05):
    """
    Less brutal than Bonferroni, more honest than raw p-values
    """
    p_sorted = np.sort(p_values)
    n = len(p_values)
    
    # Find largest k where p(k) <= (k/n) * FDR
    threshold_line = np.arange(1, n+1) / n * fdr
    
    # Find discoveries
    discoveries = p_sorted <= threshold_line
    if np.any(discoveries):
        threshold = p_sorted[np.where(discoveries)[0][-1]]
        significant = [p <= threshold for p in p_values]
    else:
        significant = [False] * n
    
    print(f"BH significant: {sum(significant)}/{n}")
    return significant

# The reasonable approach your manager will ignore
bh_results = benjamini_hochberg(p_values)
plt.figure(figsize=(12, 4))

plt.subplot(1, 3, 1)
plt.hist(p_values, bins=20, alpha=0.7, color='red')
plt.axvline(0.05, color='black', linestyle='--', label='α = 0.05')
plt.title('Raw P-values\n(Your False Hope)')
plt.xlabel('P-value')
plt.legend()

plt.subplot(1, 3, 2)
plt.hist(p_values, bins=20, alpha=0.7, color='blue')
plt.axvline(0.05/num_tests, color='black', linestyle='--', 
           label=f'Bonferroni α = {0.05/num_tests:.4f}')
plt.title('After Bonferroni\n(Dreams Crushed)')
plt.xlabel('P-value')
plt.legend()

plt.tight_layout()
plt.show()

def reality_check(your_model_performance):
    """
    What your model actually is
    """
    if your_model_performance > 0.99:
        return "Overfit as fuck, try again"
    elif your_model_performance > 0.95:
        return "Probably data leakage, chief"
    elif your_model_performance > 0.85:
        return "Suspiciously good, check your test set"
    else:
        return "Actually might be legit, congrats"

print(reality_check(0.987))  # We all know what this returns
plt.figure(figsize=(15, 5))

plt.subplot(1, 3, 1)
plt.hist(p_values, bins=20, alpha=0.7, color='red', edgecolor='black')
plt.axvline(0.05, color='blue', linestyle='--', linewidth=2, label='α = 0.05')
plt.title(f'Raw P-values\nSignificant: {sum(p < 0.05 for p in p_values)}/{len(p_values)}')
plt.xlabel('P-value')
plt.ylabel('Count')
plt.legend()

plt.subplot(1, 3, 2)
bonf_alpha = 0.05/len(p_values)
plt.hist(p_values, bins=20, alpha=0.7, color='orange', edgecolor='black')
plt.axvline(bonf_alpha, color='red', linestyle='--', linewidth=2, 
           label=f'Bonferroni α = {bonf_alpha:.4f}')
plt.title(f'Bonferroni Corrected\nSignificant: {sum(p < bonf_alpha for p in p_values)}/{len(p_values)}')
plt.xlabel('P-value')
plt.ylabel('Count')
plt.legend()

plt.subplot(1, 3, 3)
plt.scatter(range(len(p_values)), sorted(p_values), alpha=0.6, color='green')
plt.plot(range(len(p_values)), [i/len(p_values) * 0.05 for i in range(1, len(p_values)+1)], 
         'r--', label='BH threshold line')
plt.xlabel('Rank')
plt.ylabel('P-value')
plt.title('Benjamini-Hochberg')
plt.legend()

plt.tight_layout()
plt.show()
