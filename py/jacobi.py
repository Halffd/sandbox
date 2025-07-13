
import numpy as np
import matplotlib.pyplot as plt
from scipy import special

def log_step(message, value=None):
    """Log computation steps with values"""
    if value is None:
        print(f">>> {message}")
    else:
        print(f">>> {message}: {value}")

def jacobi_elliptic_robust(u, m):
    """Calculate Jacobi elliptic functions even when m > 1"""
    if m <= 1:
        # Standard case
        return special.ellipj(u, m)
    else:
        # Apply modular transformation for m > 1
        m_inv = 1/m
        u_scaled = u * np.sqrt(m)
        sn, cn, dn, phi = special.ellipj(u_scaled, m_inv)
        
        # Transform back
        sn_new = sn/np.sqrt(m)
        cn_new = np.sqrt(1 - m * sn**2)
        dn_new = np.sqrt(1 - sn**2)
        phi_new = np.arcsin(sn_new)
        
        return sn_new, cn_new, dn_new, phi_new

def pendulum_with_all_jacobi(t, g, L, c1, c2):
    """Calculate pendulum angle and all Jacobi elliptic functions with robust handling"""
    # Calculate parameter m
    m = 4*g/(2*g + L*c1)
    
    # Calculate argument u
    u = (np.sqrt(2*g + L*c1)*(t + c2))/(2*np.sqrt(L))
    
    # Use robust calculation
    try:
        sn, cn, dn, am = jacobi_elliptic_robust(u, m)
    except Exception as e:
        return {'theta': np.nan, 'am': np.nan, 'sn': np.nan, 'cn': np.nan, 'dn': np.nan, 
                'sc': np.nan, 'sd': np.nan, 'cd': np.nan, 'ns': np.nan, 'nc': np.nan, 
                'nd': np.nan, 'cs': np.nan, 'ds': np.nan, 'dc': np.nan, 'u': u, 'm': m}
    
    # Calculate all 12 Jacobi elliptic functions (with safety checks)
    sc = sn/cn if cn != 0 else np.nan
    sd = sn/dn if dn != 0 else np.nan
    cd = cn/dn if dn != 0 else np.nan
    
    ns = 1/sn if sn != 0 else np.nan
    nc = 1/cn if cn != 0 else np.nan
    nd = 1/dn if dn != 0 else np.nan
    
    cs = cn/sn if sn != 0 else np.nan
    ds = dn/sn if sn != 0 else np.nan
    dc = dn/cn if cn != 0 else np.nan
    
    # Calculate pendulum angle
    theta = 2*am
    
    return {
        'theta': theta,
        'am': am,
        'sn': sn,
        'cn': cn,
        'dn': dn,
        'sc': sc,
        'sd': sd,
        'cd': cd,
        'ns': ns,
        'nc': nc,
        'nd': nd,
        'cs': cs,
        'ds': ds,
        'dc': dc,
        'u': u,
        'm': m
    }

# Create visualization for a range of values
log_step("GENERATING COMPLETE DATA SET")
g = 9.81
L = 1.0
c1 = 2.0  # Larger c1 gives m < 1
c2 = 0.0
t_values = np.linspace(0, 10, 500)

# Compute once for all plots
results = []
for t in t_values:
    res = pendulum_with_all_jacobi(t, g, L, c1, c2)
    results.append(res)

# Extract data
am_values = [r['am'] for r in results]
sn_values = [r['sn'] for r in results]
cn_values = [r['cn'] for r in results]
dn_values = [r['dn'] for r in results]
sc_values = [r['sc'] for r in results]
sd_values = [r['sd'] for r in results]
cd_values = [r['cd'] for r in results]
ns_values = [r['ns'] for r in results]
nc_values = [r['nc'] for r in results]
nd_values = [r['nd'] for r in results]
cs_values = [r['cs'] for r in results]
ds_values = [r['ds'] for r in results]
dc_values = [r['dc'] for r in results]
theta_values = [r['theta'] for r in results]

# Create multi-page visualization to show all functions
log_step("CREATING COMPREHENSIVE VISUALIZATIONS")

# Page 1: Primary functions
plt.figure(figsize=(15, 12))
plt.suptitle(f"Jacobi Elliptic Functions (m = {results[0]['m']:.4f})", fontsize=16)

# Plot 1: Core functions sn, cn, dn
plt.subplot(2, 2, 1)
plt.plot(t_values, sn_values, 'r-', label='sn(u,m)')
plt.plot(t_values, cn_values, 'g-', label='cn(u,m)')
plt.plot(t_values, dn_values, 'b-', label='dn(u,m)')
plt.xlabel('Time (s)')
plt.ylabel('Value')
plt.title('Core Jacobi Functions')
plt.grid(True)
plt.legend()

# Plot 2: Amplitude and pendulum angle
plt.subplot(2, 2, 2)
plt.plot(t_values, am_values, 'r-', label='am(u,m)')
plt.plot(t_values, theta_values, 'k-', label='θ(t) = 2am(u,m)')
plt.xlabel('Time (s)')
plt.ylabel('Value (radians)')
plt.title('Amplitude and Pendulum Angle')
plt.grid(True)
plt.legend()

# Plot 3: sc, sd, cd functions
plt.subplot(2, 2, 3)
plt.plot(t_values, sc_values, 'm-', label='sc(u,m)')
plt.plot(t_values, sd_values, 'c-', label='sd(u,m)')
plt.plot(t_values, cd_values, 'y-', label='cd(u,m)')
plt.xlabel('Time (s)')
plt.ylabel('Value')
plt.title('Ratio Functions (First Group)')
plt.grid(True)
plt.legend()
plt.ylim(-10, 10)  # Limit y to see pattern without extreme values

# Plot 4: ns, nc, nd functions
plt.subplot(2, 2, 4)
plt.plot(t_values, ns_values, 'c-', label='ns(u,m)')
plt.plot(t_values, nc_values, 'm-', label='nc(u,m)')
plt.plot(t_values, nd_values, 'y-', label='nd(u,m)')
plt.xlabel('Time (s)')
plt.ylabel('Value')
plt.title('Inverse Functions')
plt.grid(True)
plt.legend()
plt.ylim(-10, 10)  # Limit y to see pattern without extreme values

plt.tight_layout(rect=[0, 0, 1, 0.96])
plt.savefig('jacobi_all_functions_page1.png', dpi=300)

# Page 2: Remaining functions and phase plots
plt.figure(figsize=(15, 12))
plt.suptitle(f"More Jacobi Functions and Phase Plots (m = {results[0]['m']:.4f})", fontsize=16)

# Plot 1: cs, ds, dc functions
plt.subplot(2, 2, 1)
plt.plot(t_values, cs_values, 'r-', label='cs(u,m)')
plt.plot(t_values, ds_values, 'g-', label='ds(u,m)')
plt.plot(t_values, dc_values, 'b-', label='dc(u,m)')
plt.xlabel('Time (s)')
plt.ylabel('Value')
plt.title('Ratio Functions (Second Group)')
plt.grid(True)
plt.legend()
plt.ylim(-10, 10)  # Limit y to see pattern without extreme values

# Plot 2: Phase plot (sn vs cn) - classic elliptic curve
plt.subplot(2, 2, 2)
plt.plot(sn_values, cn_values, 'r-')
plt.xlabel('sn(u,m)')
plt.ylabel('cn(u,m)')
plt.title('Phase Plot: sn vs cn')
plt.grid(True)
plt.axis('equal')

# Plot 3: Phase plot (sn vs dn)
plt.subplot(2, 2, 3)
plt.plot(sn_values, dn_values, 'g-')
plt.xlabel('sn(u,m)')
plt.ylabel('dn(u,m)')
plt.title('Phase Plot: sn vs dn')
plt.grid(True)
plt.axis('equal')

# Plot 4: 3D visualization of all three core functions
ax = plt.subplot(2, 2, 4, projection='3d')
ax.plot(sn_values, cn_values, dn_values, 'r-')
ax.set_xlabel('sn(u,m)')
ax.set_ylabel('cn(u,m)')
ax.set_zlabel('dn(u,m)')
ax.set_title('3D Trajectory of (sn, cn, dn)')

plt.tight_layout(rect=[0, 0, 1, 0.96])
plt.savefig('jacobi_all_functions_page2.png', dpi=300)

# Page 3: Special relationship plots
plt.figure(figsize=(15, 12))
plt.suptitle(f"Jacobi Function Relationships (m = {results[0]['m']:.4f})", fontsize=16)

# Plot 1: Amplitude vs sn
plt.subplot(2, 2, 1)
plt.plot(am_values, sn_values, 'm-')
plt.xlabel('am(u,m)')
plt.ylabel('sn(u,m)')
plt.title('Amplitude vs sn (should be sine!)')
plt.grid(True)

# Plot 2: Amplitude vs cn
plt.subplot(2, 2, 2)
plt.plot(am_values, cn_values, 'g-')
plt.xlabel('am(u,m)')
plt.ylabel('cn(u,m)')
plt.title('Amplitude vs cn (should be cosine!)')
plt.grid(True)

# Plot 3: Verify sn² + cn² = 1 - m·sn²·cn²
plt.subplot(2, 2, 3)
identity1 = [s**2 + c**2 for s, c in zip(sn_values, cn_values)]
identity2 = [1 - results[0]['m']*s**2*c**2 for s, c in zip(sn_values, cn_values)]
plt.plot(t_values, identity1, 'r-', label='sn² + cn²')
plt.plot(t_values, identity2, 'b--', label='1 - m·sn²·cn²')
plt.xlabel('Time (s)')
plt.ylabel('Value')
plt.title('Verifying Identities')
plt.grid(True)
plt.legend()

# Plot 4: Verify dn² = 1 - m·sn²
plt.subplot(2, 2, 4)
identity3 = [d**2 for d in dn_values]
identity4 = [1 - results[0]['m']*s**2 for s in sn_values]
plt.plot(t_values, identity3, 'g-', label='dn²')
plt.plot(t_values, identity4, 'm--', label='1 - m·sn²')
plt.xlabel('Time (s)')
plt.ylabel('Value')
plt.title('Another Identity Check')
plt.grid(True)
plt.legend()

plt.tight_layout(rect=[0, 0, 1, 0.96])
plt.savefig('jacobi_all_functions_page3.png', dpi=300)

log_step("EXECUTION COMPLETE - All visualizations saved!")
plt.show()