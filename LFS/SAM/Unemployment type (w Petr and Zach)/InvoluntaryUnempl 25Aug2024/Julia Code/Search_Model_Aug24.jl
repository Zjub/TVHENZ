using LinearAlgebra
using Optim
using Random
using Plots
using Statistics

include("get_piH_new.jl")
include("get_kappas.jl")
include("get_prods.jl")

# 0. Parameters/targets
#==========================================================================#

# Standard choices
par = Dict(
    :beta => 0.996,          # discount factor (monthly frequency)
    :mu => 0.5,              # elasticity of matching function (estimate) # 0.5
    :eta => 0.5,             # bargaining power (Hosios condition)
    :b => 0              # Australian replacement rate # 0.53
)

# Production parameters (some are normalizations, others need calibrating)
par[:pL] = 1                # worker productivity, low
par[:zL] = par[:pL]         # firm productivity, low
par[:pH] = 1.5              # worker productivity, high (calibrate to match pre-separation premium)
par[:zH] = par[:pH]         # firm productivity, high
par[:a] = 10                # strength of mismatch (calibrate s.t. mismatched jobs separate)

# Vacancy posting costs (need calibrating) [note commented out in base code at present]
# par[:kappaL] = 1          # low type vacancy posting (to target LM tightness)
# par[:kappaH] = 1.2        # high-type vacancy posting (to target pi_H^v=pi_H^u)

# Targets
par[:s] = 0.01             # separation rate 
par[:f] = 0.17             # job filling probability of 17%
par[:theta] = 0.28         # labor market tightness
par[:volsh] = 0.146        # share of voluntary separations (loose, including temp.job ends)
par[:vol_penalty] = -0.132 # wage penalty of voluntary separators (vs involuntary separators)
par[:sep_penalty] = -0.163 # wage penalty of separators (vs remainers)
par[:replacement] = 0.32   # replacement rate

# Implied values of endogenous variables
par[:u] = par[:s] / (par[:s] + par[:f])  # implied unemployment rate
par[:q] = par[:f] / par[:theta]          # job filling probability

# Compute share of high-types
xini = log((1 - 0.99) / 0.99)
#result = optimize(x -> get_piH_new(par, [x]), [xini], BFGS())
result = optimize(x -> get_piH_new(par, x), [xini], BFGS())

res = result.minimizer
par[:piH] = 1 / (1 + exp(res[1]))

if result.minimum > 1e-6
    println("not solved")
    return
end

par[:delta] = (par[:s] - (par[:f] * par[:u] * par[:piH] * (1 - par[:piH]) / (1 - par[:u]) + par[:volsh] * par[:s])) / (1 - (par[:f] * par[:u] * par[:piH] * (1 - par[:piH]) / (1 - par[:u]) + par[:volsh] * par[:s]))

par[:eHL] = par[:f] * par[:u] * (1 - par[:piH]) * par[:piH]
par[:eLH] = par[:eHL]
par[:eHH] = par[:f] * par[:u] * par[:piH]^2 / par[:delta]
par[:eLL] = par[:f] * par[:u] * (1 - par[:piH])^2 / par[:delta]

par[:uH] = (par[:delta] * par[:eHH] + par[:eLH]) / par[:f]
par[:uL] = (par[:delta] * par[:eLL] + par[:eHL]) / par[:f]

println("cross-check flows")
println("-----------------")
println("eHL+eLH+eLL+eHH=1-u")
println(par[:eHL] + par[:eLH] + par[:eHH] + par[:eLL] - (1 - par[:u]))
println("uH+uL=u")
println(par[:uH] + par[:uL] - par[:u])

# Implied parameter values
par[:m] = par[:f] / (par[:theta]^(1 - par[:mu]))  # matching efficiency

# CALIBRATE PRODUCTION FUNCTION PARAMETERS
#===========================================#
xini = [log(par[:pH]), log(par[:a])]
result = optimize(x -> get_prods(par, x), xini, NelderMead(), Optim.Options(iterations=2000, show_trace=true))
res = result.minimizer
par[:pH] = exp(res[1])
par[:a] = exp(res[2])
par[:zH] = par[:pH]

par[:yHH] = 1/2 * (par[:pH] + par[:zH])
par[:yLL] = 1/2 * (par[:pL] + par[:zL])
par[:yHL] = 1/2 * (par[:zH] + par[:pL]) * exp(-par[:a] * (par[:zH] - par[:pL])^2)
par[:yLH] = 1/2 * (par[:zL] + par[:pH]) * exp(-par[:a] * (par[:zL] - par[:pH])^2)

xini = log.([0.1, 0.1])
result = optimize(x -> get_kappas(par, x), xini, BFGS())
resk = result.minimizer
par[:kappaH] = exp(resk[1])
par[:kappaL] = exp(resk[2])

# Implied wages
par[:factor] = par[:beta] * (1 - par[:delta]) * par[:f] * par[:eta] * par[:theta] * (par[:kappaH] - par[:kappaL]) / (1 - par[:eta] * par[:beta] * (1 - par[:delta]) * par[:f])

wHH = par[:eta] * (par[:yHH] + par[:kappaH] * par[:theta] + (1 - par[:piH]) * par[:factor]) + (1 - par[:eta]) * par[:b]
wLL = par[:eta] * (par[:yLL] + par[:kappaL] * par[:theta] - par[:piH] * par[:factor]) + (1 - par[:eta]) * par[:b]
wLH = par[:eta] * (par[:yLH] + par[:kappaH] * par[:theta] + (1 - par[:piH]) * par[:factor]) + (1 - par[:eta]) * par[:b]
wHL = par[:eta] * (par[:yHL] + par[:kappaL] * par[:theta] - (1 - par[:piH]) * par[:factor]) + (1 - par[:eta]) * par[:b]

# Separating
JHL = par[:yHL] - wHL
JLH = par[:yLH] - wLH

# Check properties
par[:eHH] = par[:f] * par[:u] * par[:piH]^2 / par[:delta]
par[:eLL] = par[:f] * par[:u] * (1 - par[:piH])^2 / par[:delta]

sep_invol = par[:s] * (1 - par[:u]) - par[:eLH]
remain = (1 - par[:s]) * (1 - par[:u])

w_vol_seps = wLH
w_invol_seps = par[:delta] * (par[:eHH] / sep_invol * wHH + par[:eLL] / sep_invol * wLL) + par[:eHL] / sep_invol * wHL
w_remain = (1 - par[:delta]) / remain * (par[:eHH] * wHH + par[:eLL] * wLL)
w_all_seps = par[:volsh] * w_vol_seps + (1 - par[:volsh]) * w_invol_seps
w_all = (par[:eHH] * wHH + par[:eLL] * wLL + par[:eHL] * wHL + par[:eLH] * wLH) / (1 - par[:u])

println("separation (job value of HL and LH), replacement rate, voluntary wage penalty (-13.2% in data) and separation wage penalty (-16.3% in data)")
println([JHL, JLH, par[:b] / w_all_seps, log(w_vol_seps) - log(w_invol_seps), log(w_all_seps) - log(w_remain)])

# Simulate separations and wage scars
par[:N] = 50000
par[:T] = 5 * 12 + 1
par[:T] = Int(par[:T])  # Ensure par[:T] is an integer which is necessary for loops in Julia


sim_vol = zeros(Int(par[:N]), Int(par[:T]), 5)  # worker type/employed/unemployed indicator/high firm/low firm/wage
sim_vol[:, :, 1] .= 1  # all voluntary separators are high types
sim_vol[:, 2, 2] .= 0  # all start in unemployment initially
sim_vol[:, 2, 3] .= 0  # nobody matched
sim_vol[:, 2, 4] .= 0  # nobody matched
sim_vol[:, 2, 5] .= par[:b]  # unemployment benefit
sim_vol[:, 1, 5] .= wLH  # pre-separation wages
fshocks = rand(Int(par[:N]), Int(par[:T]))
Hshocks = rand(Int(par[:N]), Int(par[:T]))
dshocks = rand(Int(par[:N]), Int(par[:T]))

f_indic = fshocks .<= par[:f]
H_indic = Hshocks .<= par[:piH]
d_indic = dshocks .<= par[:delta]

for t in 3:Int(par[:T])
    sim_vol[:, t, 2] = sim_vol[:, t - 1, 2] .* (sim_vol[:, t - 1, 3] .* (1 .- d_indic[:, t]) .+ sim_vol[:, t - 1, 4] .* 0) .+
                       (1 .- sim_vol[:, t - 1, 2]) .* f_indic[:, t]  # employment indicator
    sim_vol[:, t, 3] = sim_vol[:, t - 1, 2] .* sim_vol[:, t - 1, 3] .+ (1 .- sim_vol[:, t - 1, 2]) .* f_indic[:, t] .* H_indic[:, t]  # high type firm
    sim_vol[:, t, 4] = (1 .- sim_vol[:, t - 1, 2]) .* f_indic[:, t] .* (1 .- H_indic[:, t])  # low type firm
    sim_vol[:, t, 5] = (1 .- sim_vol[:, t, 2]) .* par[:b] .+ sim_vol[:, t, 2] .* (sim_vol[:, t, 3] .* wHH .+ sim_vol[:, t, 4] .* wLH)  # income/wage
end

sim_invol = zeros(Int(par[:N]), Int(par[:T]), 5)
eHLs = par[:eHL] / sep_invol
eHHs = par[:delta] * par[:eHH] / sep_invol
eLLs = par[:delta] * par[:eLL] / sep_invol

Ls = round(Int, (eHLs + eLLs) * par[:N])

sim_invol[1:Ls, :, 1] .= 0  # low types
sim_invol[Ls + 1:end, :, 1] .= 1  # high types
sim_invol[:, 2, 2] .= 0  # all start in unemployment initially
sim_invol[:, 2, 3] .= 0  # nobody matched
sim_invol[:, 2, 4] .= 0  # nobody matched
sim_invol[:, 2, 5] .= par[:b]  # unemployment benefit
sim_invol[1:round(Int, eHLs * par[:N]), 1, 5] .= wHL
sim_invol[round(Int, eHLs * par[:N]) + 1:Ls, 1, 5] .= wLL
sim_invol[Ls + 1:end, 1, 5] .= wHH

for t in 3:Int(par[:T])
    sim_invol[1:Ls, t, 2] = sim_invol[1:Ls, t - 1, 2] .* (sim_invol[1:Ls, t - 1, 3] .* 0 .+ sim_invol[1:Ls, t - 1, 4] .* (1 .- d_indic[1:Ls, t])) .+
                            (1 .- sim_invol[1:Ls, t - 1, 2]) .* f_indic[1:Ls, t]  # employment indicator
    sim_invol[1:Ls, t, 4] = sim_invol[1:Ls, t - 1, 2] .* sim_invol[1:Ls, t - 1, 4] .+ (1 .- sim_invol[1:Ls, t - 1, 2]) .* f_indic[1:Ls, t] .* (1 .- H_indic[1:Ls, t])  # low type firm
    sim_invol[1:Ls, t, 3] = (1 .- sim_invol[1:Ls, t - 1, 2]) .* f_indic[1:Ls, t] .* H_indic[1:Ls, t]  # high type firm
    sim_invol[1:Ls, t, 5] = (1 .- sim_invol[1:Ls, t, 2]) .* par[:b] .+ sim_invol[1:Ls, t, 2] .* (sim_invol[1:Ls, t, 3] .* wHL .+ sim_invol[1:Ls, t, 4] .* wLL)  # income/wage

    sim_invol[Ls + 1:end, t, 2] = sim_invol[Ls + 1:end, t - 1, 2] .* (sim_invol[Ls + 1:end, t - 1, 3] .* (1 .- d_indic[Ls + 1:end, t]) .+ sim_invol[Ls + 1:end, t - 1, 4] .* 0) .+
                                  (1 .- sim_vol[Ls + 1:end, t - 1, 2]) .* f_indic[Ls + 1:end, t]  # employment indicator
    sim_invol[Ls + 1:end, t, 3] = sim_invol[Ls + 1:end, t - 1, 2] .* sim_invol[Ls + 1:end, t - 1, 3] .+ (1 .- sim_invol[Ls + 1:end, t - 1, 2]) .* f_indic[Ls + 1:end, t] .* H_indic[Ls + 1:end, t]  # high type firm
    sim_invol[Ls + 1:end, t, 4] = (1 .- sim_invol[Ls + 1:end, t - 1, 2]) .* f_indic[Ls + 1:end, t] .* (1 .- H_indic[Ls + 1:end, t])  # low type firm
    sim_invol[Ls + 1:end, t, 5] = (1 .- sim_invol[Ls + 1:end, t, 2]) .* par[:b] .+ sim_invol[Ls + 1:end, t, 2] .* (sim_invol[Ls + 1:end, t, 3] .* wHH .+ sim_invol[Ls + 1:end, t, 4] .* wLH)  # income/wage
end

w_vol = mean(sim_vol[:, :, 5], dims=1)  # Average across the first dimension (individuals) - note: second dims is time.
w_invol = mean(sim_invol[:, :, 5], dims=1)  

w_vol = dropdims(w_vol, dims=1)
w_invol = dropdims(w_invol, dims=1)
#=
plot(0:par[:T]-1, (w_vol ./ w_vol[1] .- 1) * 100, label="voluntary", lw=2, color=:black)
plot!(0:par[:T]-1, (w_invol ./ w_invol[1] .- 1) * 100, label="involuntary", lw=2, linestyle=:dash, color=:red)
ylabel!("percent")
xlabel!("months")
title!("scars")
plot!(legend=:topright)

plot(0:par[:T]-2, (w_vol[2:end] ./ w_invol[2:end] .- 1) * 100, lw=2)
ylabel!("percent")
xlabel!("months")
title!("voluntary-involuntary")
=#

w_vol_a = zeros(5)
w_invol_a = zeros(5)

for a in 1:5
    w_vol_a[a] = mean(w_vol[2 + (a - 1) * 12:a * 12 + 1])
    w_invol_a[a] = mean(w_invol[2 + (a - 1) * 12:a * 12 + 1])
end

#=
plot(0:(Int(par[:T])-1)//12-1, (w_vol_a ./ w_vol[1] .- 1) * 100, label="voluntary", lw=2, color=:black)
plot!(0:(Int(par[:T])-1)//12-1, (w_invol_a ./ w_invol[1] .- 1) * 100, label="involuntary", lw=2, linestyle=:dash, color=:red)
ylabel!("percent")
xlabel!("years")
title!("scars")
plot!(legend=:topright)

plot(0:(Int(par[:T])-1)//12-1, (w_vol_a ./ w_invol_a .- 1) * 100, lw=2)
ylabel!("percent")
xlabel!("years")
title!("voluntary-involuntary")
=#

### Show all the plots
#===================================================================#
# First plot
plot(0:par[:T]-1, (w_vol ./ w_vol[1] .- 1) * 100, label="voluntary", lw=2, color=:black)
plot!(0:par[:T]-1, (w_invol ./ w_invol[1] .- 1) * 100, label="involuntary", lw=2, linestyle=:dash, color=:red)
ylabel!("percent")
xlabel!("months")
title!("scars")
plot!(legend=:topright)
display(current())  # Display the first plot

# Second plot
plot(0:par[:T]-2, (w_vol[2:end] ./ w_invol[2:end] .- 1) * 100, lw=2)
ylabel!("percent")
xlabel!("months")
title!("voluntary-involuntary")
display(current())  # Display the second plot

# Third plot
plot(0:(Int(par[:T])-1)//12-1, (w_vol_a ./ w_vol[1] .- 1) * 100, label="voluntary", lw=2, color=:black)
plot!(0:(Int(par[:T])-1)//12-1, (w_invol_a ./ w_invol[1] .- 1) * 100, label="involuntary", lw=2, linestyle=:dash, color=:red)
ylabel!("percent")
xlabel!("years")
title!("scars")
plot!(legend=:topright)
display(current())  # Display the third plot

# Fourth plot
plot(0:(Int(par[:T])-1)//12-1, (w_vol_a ./ w_invol_a .- 1) * 100, lw=2)
ylabel!("percent")
xlabel!("years")
title!("voluntary-involuntary")
display(current())  # Display the fourth plot


#### Save plots
#======================================================#
p1 = plot(0:par[:T]-1, (w_vol ./ w_vol[1] .- 1) * 100, label="voluntary", lw=2, color=:black)
plot!(0:par[:T]-1, (w_invol ./ w_invol[1] .- 1) * 100, label="involuntary", lw=2, linestyle=:dash, color=:red)
ylabel!("percent")
xlabel!("months")
title!("scars")

p2 = plot(0:par[:T]-2, (w_vol[2:end] ./ w_invol[2:end] .- 1) * 100, lw=2)
ylabel!("percent")
xlabel!("months")
title!("voluntary-involuntary")

p3 = plot(0:(Int(par[:T])-1)//12-1, (w_vol_a ./ w_vol[1] .- 1) * 100, label="voluntary", lw=2, color=:black)
plot!(0:(Int(par[:T])-1)//12-1, (w_invol_a ./ w_invol[1] .- 1) * 100, label="involuntary", lw=2, linestyle=:dash, color=:red)
ylabel!("percent")
xlabel!("years")
title!("scars")

p4 = plot(0:(Int(par[:T])-1)//12-1, (w_vol_a ./ w_invol_a .- 1) * 100, lw=2)
ylabel!("percent")
xlabel!("years")
title!("voluntary-involuntary")

# Combine into a 2x2 grid and save as a single PNG file
combined_plot = plot(p1, p2, p3, p4, layout = @layout [a b; c d])
savefig(combined_plot, "combined_plots.png")  # Save the combined grid as "combined_plots.png"
