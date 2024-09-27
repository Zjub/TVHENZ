### Julia replication of Matlab code for value function iteration with an IID income process
## The discrete normal process is defined within the matlab code - so double check that we've interpreted this properly - I think I've messed up the scale.

using Random
using LinearAlgebra
using Roots
using Distributions

## PARAMETERS

# preferences
risk_aver = 2.0
beta = 0.95

# returns
r = 0.03
R = 1 + r

# income risk - iid normal distribution
mu_y = 1 # average earnings
sd_y = 0.2
ny = 5

# asset grids
na = 500
amax = 20
borrow_lim = 0
agrid_par = 1

max_iter = 1000
tol_iter = 1.0e-6
Nsim = 50000
Tsim = 500

# Options
Display = 1
DoSimulate = 1
MakePlots = 1

# Draw random numbers
Random.seed!(1234)
yrand = rand(Nsim, Tsim)

# Set up grids
agrid = range(0, stop=1, length=na) |> collect
agrid = agrid .^ (1.0 ./ agrid_par)
agrid = borrow_lim .+ (amax - borrow_lim) .* agrid

# Function to discretize normal distribution - this is leading to most of the density sitting in the middle of the distribution
#function discrete_normal(ny, mu_y, sd_y, width)
#    dist = Normal(mu_y, sd_y)
#    
#    # Convert the range into a vector
#    ygrid = collect(range(mu_y - width, stop=mu_y + width, length=ny))
#    
#    # Compute probabilities for each grid point
#    ydist = pdf.(dist, ygrid)
#    ydist ./= sum(ydist)  # Normalize to ensure it sums to 1
#    
#    return ygrid, ydist
#end

# Updated function based on the actual matlab code (rather than a rough guess)
function discrete_normal(ny, mu_y, sd_y, width)
    dist = Normal(mu_y, sd_y)

    # Generate grid points
    ygrid = collect(range(mu_y - width * sd_y, stop = mu_y + width * sd_y, length = ny))
    
    # Compute probabilities using CDF differences
    ydist = zeros(ny)
    
    if ny == 2
        ydist .= 0.5  # If there are only two points, assign equal probability
    else
        # First point
        ydist[1] = cdf(dist, ygrid[1] + 0.5 * (ygrid[2] - ygrid[1]))
        
        # Intermediate points
        for i in 2:ny-1
            ydist[i] = cdf(dist, ygrid[i] + 0.5 * (ygrid[i+1] - ygrid[i])) - cdf(dist, ygrid[i] - 0.5 * (ygrid[i] - ygrid[i-1]))
        end
        
        # Last point
        ydist[ny] = 1.0 - sum(ydist[1:ny-1])
    end

    # Normalize probabilities
    ydist ./= sum(ydist)

    return ygrid, ydist
end

#function discrete_normal(ny, mu_y, sd_y)
#    dist = Normal(mu_y, sd_y)
#    
#    # Use quantiles to generate a grid that better captures the tails of the distribution
#    ygrid = quantile(dist, range(0.01, stop=0.99, length=ny))  # Avoid extremes (0, 1) to prevent outliers
#    
#    # Compute probabilities for each grid point
#    ydist = pdf.(dist, ygrid)
#    ydist ./= sum(ydist)  # Normalize to ensure it sums to 1
#    
#    return ygrid, ydist
#end


width = find_zero(x -> sum(discrete_normal(ny, mu_y, sd_y, x)[2]) - 1, 2)
ygrid, ydist = discrete_normal(ny, mu_y, sd_y, width)
ycumdist = cumsum(ydist)

# Utility function
u(c) = risk_aver == 1 ? log(c) : (c^(1 - risk_aver) - 1) / (1 - risk_aver)

# Initialize value function
Vguess = zeros(na, ny)
for iy in 1:ny
    Vguess[:, iy] = u.(r .* agrid .+ ygrid[iy]) ./ (1 - beta)
end

# Iterate on value function
V = Vguess
Vdiff = 1
iter = 0

# I am OVERUSING globals to force assignment here - rewrite this to be more sanitised
while iter <= max_iter && Vdiff > tol_iter
    global iter += 1
    global Vlast = copy(V)
    global V = zeros(na, ny)
    global sav = zeros(na, ny)
    global savind = zeros(na, ny)
    global con = zeros(na, ny)
    
    # Loop over assets
    for ia in 1:na
        # Loop over income
        for iy in 1:ny
            cash = R * agrid[ia] + ygrid[iy]
            Vchoice = u.(max.(cash .- agrid, 1.0e-10)) .+ beta .* (Vlast * ydist)
            V[ia, iy], savind_int = findmax(Vchoice)  # Findmax returns an index as an integer

            # Save the integer index of the optimal choice
            global savind[ia, iy] = savind_int            
        
            # Use the integer index to access agrid
            global sav[ia, iy] = agrid[savind_int]
            global con[ia, iy] = cash - sav[ia, iy]
        end
    end
    
    global Vdiff = maximum(abs.(V - Vlast))
    if Display >= 1
        println("Iteration no. $iter, max val fn diff is $Vdiff")
    end
end

# Vectorised
#if DoSimulate == 1
#    yindsim = zeros(Int, Nsim, Tsim)
#    aindsim = zeros(Int, Nsim, Tsim)
#
#    # Initial assets
#    aindsim[:, 1] .= 1
#
#    # Loop over time periods
#    for it in 1:Tsim
#        if Display >= 1 && it % 100 == 0
#            println("Simulating, time period $it")
#        end
#        
#        # Income realization
#        yindsim[yrand[:, it] .<= ycumdist[1], it] .= 1
#        for iy in 2:ny
#            yindsim[(yrand[:, it] .> ycumdist[iy-1]) .& (yrand[:, it] .<= ycumdist[iy]), it] .= iy
#        end
#        
#        # Asset choice for next period
#        if it < Tsim
#            for iy in 1:ny
#                aindsim[yindsim[:, it] .== iy, it + 1] .= savind[aindsim[yindsim[:, it] .== iy, it], iy]
#            end
#        end
#    end
#
#    # Assign actual asset and income values
#    asim = agrid[aindsim]
#    ysim = ygrid[yindsim]
#end

if DoSimulate == 1
    yindsim = zeros(Int, Nsim, Tsim)
    aindsim = ones(Int, Nsim, Tsim)  # Start with initial asset index of 1

    # Loop over time periods
    for it in 1:Tsim
        if Display >= 1 && it % 100 == 0
            println("Simulating, time period $it")
        end

        # Loop over individuals
        for ind in 1:Nsim
            # Income realization for individual `ind` in period `it`
            rand_val = yrand[ind, it]
            if rand_val <= ycumdist[1]
                yindsim[ind, it] = 1
            else
                for iy in 2:ny
                    if rand_val > ycumdist[iy-1] && rand_val <= ycumdist[iy]
                        yindsim[ind, it] = iy
                    end
                end
            end

            # Asset choice for next period
            if it < Tsim
                iy = yindsim[ind, it]  # Get the income state for this individual
                current_asset_index = aindsim[ind, it]  # Get the current asset index

                # Make sure current_asset_index is a valid integer index
                if current_asset_index > 0 && current_asset_index <= na
                    # Assign the next period's asset index based on the optimal savings decision
                    next_asset_index = savind[current_asset_index, iy]
                    aindsim[ind, it + 1] = next_asset_index
                else
                    println("Warning: Invalid asset index for individual $ind at time $it")
                end
            end
        end
    end

    # Assign actual asset and income values
    asim = agrid[aindsim]
    ysim = ygrid[yindsim]
end


using Plots

if MakePlots == 1
    # Consumption policy function
    p1 = plot(agrid, con[:, 1], label="Lowest income state", linewidth=1, color="blue", grid=true)
    plot!(agrid, con[:, ny], label="Highest income state", linewidth=1, color="red")
    title!("Consumption")
    xlims!(0, amax)

    # Savings policy function
    p2 = plot(agrid, sav[:, 1] .- agrid, label="Lowest income state", linewidth=1, color="blue", grid=true)
    plot!(agrid, sav[:, ny] .- agrid, label="Highest income state", linewidth=1, color="red")
    plot!(agrid, zeros(na), linewidth=0.5, color="black")
    title!("Savings")
    xlims!(0, amax)

    # Consumption policy function (zoomed in)
    p3 = plot(agrid, con[:, 1], label="Lowest income state", linewidth=2, color="blue", grid=true)
    plot!(agrid, con[:, ny], label="Highest income state", linewidth=2, color="red")
    title!("Consumption: Zoomed")
    xlims!(0, 1)

    # Savings policy function (zoomed in)
    p4 = plot(agrid, sav[:, 1] .- agrid, label="Lowest income state", linewidth=2, color="blue", grid=true)
    plot!(agrid, sav[:, ny] .- agrid, label="Highest income state", linewidth=2, color="red")
    plot!(agrid, zeros(na), linewidth=0.5, color="black")
    title!("Savings: Zoomed (a'-a)")
    xlims!(0, 1)

    # Income distribution histogram
    p5 = histogram(ysim[:, Tsim], bins=ygrid, edgecolor=:blue, label="", grid=true)
    title!("Income distribution")
    
    # Asset distribution histogram
    p6 = histogram(asim[:, Tsim], bins=0:0.05:2, edgecolor=:black, label="", grid=true)
    title!("Asset distribution")

    # Mean asset convergence
    #p7 = plot(1:Tsim, mean(asim, dims=1), label="", linewidth=1.5, grid=true)
    #title!("Mean Asset Convergence")
    #ylabel!("Time Period")

    # Display all the plots in a grid layout
    plot(p1, p2, p3, p4, layout=(2, 2))

    # Combine into a 2x2 grid and save as a single PNG file
    combined_plot = plot(p1, p2, p3, p4, layout = @layout [a b; c d])
    savefig(combined_plot, "combined_plots.png")  # Save the combined grid as "combined_plots.png"
    combined_plot2 = plot(p1, p2, p3, p4,p5,p6, layout = @layout [a b; c d; e f])
    savefig(combined_plot2, "combined_plots2.png")  # Save the combined grid as "combined_plots.png"

    # Asset distribution statistics
    aysim = asim[:, Tsim] ./ mean(ysim[:, Tsim])
    println("Mean assets: ", mean(aysim))
    println("Fraction borrowing constrained: ", sum(aysim .== borrow_lim) / Nsim * 100, "%")
    println("10th Percentile: ", quantile(aysim, 0.1))
    println("50th Percentile: ", quantile(aysim, 0.5))
    println("90th Percentile: ", quantile(aysim, 0.9))
    println("99th Percentile: ", quantile(aysim, 0.99))
end




