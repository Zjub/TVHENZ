using LinearAlgebra
using Optim

function get_prods(par, x)
    # Ensure x is correctly unpacked if it is a vector
    pH = exp(x[1])
    a  = exp(x[2])
    # b  = 1 / (1 + exp(x[3]))  # Uncomment if needed
    zH = pH

    # Calculate productivity values
    yHH = 1/2 * (pH + zH)
    yLL = 1/2 * (par[:pL] + par[:zL])
    yHL = 1/2 * (zH + par[:pL]) * exp(-a * (zH - par[:pL])^2)
    yLH = 1/2 * (par[:zL] + pH) * exp(-a * (par[:zL] - pH)^2)

    # Update par with computed values
    par[:pH] = pH
    par[:a] = a
    par[:zH] = zH
    par[:yHH] = yHH
    par[:yLL] = yLL
    par[:yHL] = yHL
    par[:yLH] = yLH

    # Solve for kappas using get_kappas function
    xini = log.([0.1, 0.1])
    resk = optimize(x -> get_kappas(par, x), xini, BFGS()).minimizer
    kappaH = exp(resk[1])
    kappaL = exp(resk[2])

    par[:kappaH] = kappaH
    par[:kappaL] = kappaL

    # Calculate implied wages
    factor = par[:beta] * (1 - par[:delta]) * par[:f] * par[:eta] * par[:theta] * (kappaH - kappaL) / (1 - par[:eta] * par[:beta] * (1 - par[:delta]) * par[:f])

    wHH = par[:eta] * (yHH + kappaH * par[:theta] + (1 - par[:piH]) * factor) + (1 - par[:eta]) * par[:b]
    wLL = par[:eta] * (yLL + kappaL * par[:theta] - par[:piH] * factor) + (1 - par[:eta]) * par[:b]
    wLH = par[:eta] * (yLH + kappaH * par[:theta] + (1 - par[:piH]) * factor) + (1 - par[:eta]) * par[:b]
    wHL = par[:eta] * (yHL + kappaL * par[:theta] - (1 - par[:piH]) * factor) + (1 - par[:eta]) * par[:b]

    # Determine whether to separate
    JHL = yHL - wHL
    JLH = yLH - wLH

    # Check properties
    par[:eHH] = par[:f] * par[:u] * par[:piH]^2 / par[:delta]
    par[:eLL] = par[:f] * par[:u] * (1 - par[:piH])^2 / par[:delta]

    sep_invol = par[:s] * (1 - par[:u]) - par[:eLH]
    remain = (1 - par[:s]) * (1 - par[:u])

    w_vol_seps = wLH
    w_invol_seps = par[:delta] * (par[:eHH] / sep_invol * wHH + par[:eLL] / sep_invol * wLL) + par[:eHL] / sep_invol * wHL
    w_remain = (1 - par[:delta]) / remain * (par[:eHH] * wHH + par[:eLL] * wLL)
    w_all_seps = par[:volsh] * w_vol_seps + (1 - par[:volsh]) * w_invol_seps

    G = [
        10 * (log(w_vol_seps) - log(w_invol_seps) - par[:vol_penalty]),
        log(w_all_seps) - log(w_remain) - par[:sep_penalty]
    ]
    # Uncomment if needed
    # G3 = 100 * (par[:b] / w_all_seps - par[:replacement])

    if max(JHL, JLH) > 0
        return 100.0
    else
        return maximum(abs.(G))
    end
end
