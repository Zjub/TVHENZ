function F = get_kappas(par,x)

par.kappaH = exp(x(1));
par.kappaL = exp(x(2));

par.factor = par.beta*(1-par.delta)*par.f*par.eta*par.theta*(par.kappaH-par.kappaL)/(1-par.eta*par.beta*(1-par.delta)*par.f);

wHH = par.eta*(par.yHH + par.kappaH*par.theta + (1-par.piH)*par.factor)+(1-par.eta)*par.b;
wLL = par.eta*(par.yLL + par.kappaL*par.theta - (par.piH)*par.factor)+(1-par.eta)*par.b;
wLH = par.eta*(par.yLH + par.kappaH*par.theta + (1-par.piH)*par.factor)+(1-par.eta)*par.b;
wHL = par.eta*(par.yHL + par.kappaL*par.theta - (1-par.piH)*par.factor)+(1-par.eta)*par.b;

% note that job values anticipate that mismatched jobs get separated
JHL = par.yHL - wHL;
JLH = par.yLH - wLH;
JHH = (par.yHH - wHH)/(1-par.beta*(1-par.delta));
JLL = (par.yLL - wLL)/(1-par.beta*(1-par.delta));

F(1) = par.kappaH - par.beta*(1-par.delta)*par.q*(par.piH*JHH + (1-par.piH)*JHL);
F(2) = par.kappaL - par.beta*(1-par.delta)*par.q*(par.piH*JLH + (1-par.piH)*JLL);

