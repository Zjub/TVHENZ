# Replicate James Graham lifecycle retirement model.
# 30/03/2024
# Author: Matt Nolan

import Pkg
Pkg.add(["Distributions", "LinearAlgebra", "Interpolations", "Plots", "Optim"])

using LinearAlgebra
using Distributions
using Interpolations
using Plots
using Optim

## Basic setup
modelname = "mod_retire"
figdir = "./../Figures/"

# Parameters
beta = 0.95
chi = 0.31
psi = 1
r = 0.065
a_1 = 0
omega_jret = 0.5

minage = 20
ages = 1:60
J = length(ages)
Jret = 45

# Mortality risk
pi_j = 0.025 .* (ages ./ maximum(ages)).^6

# Life-cycle profile of income
y_j = 3 .+ 5 * (ages ./ maximum(ages)) - 4 * (ages ./ maximum(ages)).^4
y_j ./= maximum(y_j)

# Asset gridspace
Na = 100
amin = 0
amax = 10
agrid = range(amin, stop=amax, length=Na)

zgrid = [0.5, 0.85, 1, 1.15, 1.5]
Nz = length(zgrid)

# Markov chain probability transition matrix
Gamma_z = [0.80 0.10 0.075 0.025 0; 0.025 0.90 0.05 0.025 0; 0 0.025 0.95 0.025 0; 0 0.025 0.05 0.90 0.025; 0 0.025 0.075 0.10 0.80]

# Create expectations matrix
E = kron(Gamma_z, I(Na))
Eret = kron(I(Nz), I(Na))

# Combined grid on s=(a,z)
s = [repeat(agrid, inner=Nz) repeat(zgrid, outer=Na)]

# Constructing grids separately (to match output of "meshgrid" in MATLAB)
s_a = repeat(agrid', length(zgrid), 1)
s_z = repeat(zgrid, 1, length(agrid))

# Pre-allocate value function and choice function vectors
VW_j = zeros(Na*Nz, J+1)
VR_j = zeros(Na*Nz, J+1)
aprimeW_j = zeros(Na*Nz, J)
consW_j = zeros(Na*Nz, J)
aprimeR_j = zeros(Na*Nz, J)
consR_j = zeros(Na*Nz, J)

# Utility and bequest functions
util_func(cons) = log(cons)
bequest_func(aprime) = psi * log(aprime)

# Final period of life: j = J
jj = J + 1
a = s[:,1]
z = s[:,2]
VW_j[:,jj] .= psi * log.(a)
VR_j[:,jj] .= psi * log.(a)
scale = -1e+10
VW_j[:,jj] .= max.(VW_j[:,jj], scale)
VR_j[:,jj] .= max.(VR_j[:,jj], scale)

# Solve for all previous ages: j = J-1, ..., 1
for jj in J:-1:1
    # RETIRED PROBLEM
    # Implementation of golden section search and interpolation
    # working and retired problems are omitted for brevity, but follow a similar pattern
end

## Optimisation
# Define utility and bequest functions
util(cons) = log.(cons)
bequest(aprime) = psi * log.(aprime)

# Initialize value functions for working (VW) and retired (VR) states
VW = zeros(Na*Nz, J)
VR = zeros(Na*Nz, J)

# Placeholder for decision rules (asset holdings next period)
a_primeW = zeros(Na*Nz, J)
a_primeR = zeros(Na*Nz, J)

# Starting from the last period moving backwards
for jj in reverse(1:J-1) # Assuming final period J is already initialized

    # Interpolate the next period's value functions for both working and retired states
    VW_next = reshape(VW[:, jj+1], Na, Nz)
    VR_next = reshape(VR[:, jj+1], Na, Nz)
    interp_VW_next = LinearInterpolation((agrid, zgrid), VW_next, extrapolation_bc=Line())
    interp_VR_next = LinearInterpolation((agrid, zgrid), VR_next, extrapolation_bc=Line())
    
    # Loop over all state space grid points (assets and productivity shocks)
    for (i, a) in enumerate(agrid), (j, z) in enumerate(zgrid)
        index = (i-1)*Nz + j # Linear index for the 2D grid
        
        # Working state optimization
        objectiveW(a_prime) = -(
            util(y_j[jj]*z + a*(1+r) - a_prime) +
            beta * (1 - pi_j[jj]) * interp_VW_next(a_prime, z) +
            beta * pi_j[jj] * bequest(a_prime)
        )
        resW = optimize(objectiveW, amin, min(y_j[jj]*z + a*(1+r), amax), Brent())
        a_primeW[index, jj] = Optim.minimizer(resW)
        VW[index, jj] = -Optim.minimum(resW)
        
        # Retired state optimization
        objectiveR(a_prime) = -(
            util(omega_jret + a*(1+r) - a_prime) +
            beta * interp_VR_next(a_prime, z)
        )
        resR = optimize(objectiveR, amin, omega_jret + a*(1+r), Brent())
        a_primeR[index, jj] = Optim.minimizer(resR)
        VR[index, jj] = -Optim.minimum(resR)
    end
end

## Simulation
# Simulate productivity shocks and decisions over time
Npop = 25000  # Number of households to simulate
sim_assets = zeros(Npop, J+1)  # Assets held at each age, initialized to 0
sim_work = zeros(Npop, J)  # Working status at each age
sim_income = zeros(Npop, J)  # Income at each age
sim_cons = zeros(Npop, J)  # Consumption at each age

# Initial productivity shock for each household
initial_z_dist = Categorical(Gamma_z[:,1])  # Assuming stationary distribution
z_indices = rand(initial_z_dist, Npop)  # Initial shock indices
sim_prod = zgrid[z_indices]  # Map indices to productivity levels

# Assume the decision rules (a_primeW and a_primeR) and value functions (VW, VR) are computed

for jj in 1:J
    for i in 1:Npop
        a = sim_assets[i, jj]  # Current assets
        z = sim_prod[i]  # Current productivity shock
        
        # Decision whether to work or retire based on the value function
        work_decision = VW[(a, z), jj] > VR[(a, z), jj]
        sim_work[i, jj] = work_decision
        
        if work_decision
            sim_assets[i, jj+1] = a_primeW[(a, z), jj]
            sim_income[i, jj] = y_j[jj] * z
            sim_cons[i, jj] = y_j[jj] * z + a * (1 + r) - sim_assets[i, jj+1]
        else
            sim_assets[i, jj+1] = a_primeR[(a, z), jj]
            sim_income[i, jj] = omega_jret
            sim_cons[i, jj] = omega_jret + a * (1 + r) - sim_assets[i, jj+1]
        end
    end
end

#Plot
# Plot lifecycle income
plot(minage:minage+J-1, mean(sim_income, dims=1), label="Mean Income", color=:blue, linewidth=2)
plot!(minage:minage+J-1, mean(sim_income, dims=1) .- std(sim_income, dims=1), label="Mean - Std", linestyle=:dash, color=:red)
plot!(minage:minage+J-1, mean(sim_income, dims=1) .+ std(sim_income, dims=1), label="Mean + Std", linestyle=:dash, color=:red)
xlabel!("Age")
ylabel!("Income")
title!("Life-cycle Income")

# Plot fraction working
plot(minage:minage+J-1, mean(sim_work, dims=1), label="Fraction Working", color=:blue, linewidth=2)
xlabel!("Age")
ylabel!("Fraction")
title!("Fraction Working Over Lifecycle")

# Similar plots can be created for consumption and assets

# Saving figures (assuming figures directory is correctly set up)
savefig("lifecycle_income.png")
