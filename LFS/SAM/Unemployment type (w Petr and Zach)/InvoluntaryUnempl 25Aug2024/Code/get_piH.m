function F = get_piH(par,x)

piH = 1/(1+exp(x(1)));

F = abs(par.eHL - par.f*par.u*piH*(1-piH));