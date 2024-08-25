function F = get_piH_new(par,x)

piH = 1/(1+exp(x(1)));

F = abs(piH*(1-piH) - ((1-par.u)/par.u)*(par.s/par.f)*par.volsh);