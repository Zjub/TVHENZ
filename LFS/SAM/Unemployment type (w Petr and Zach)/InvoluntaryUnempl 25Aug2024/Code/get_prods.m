function F = get_prods(par,x)

par.pH = exp(x(1));
par.a  = exp(x(2));
% par.b  = 1/(1+exp(x(3)));
par.zH = par.pH;

par.yHH   = 1/2*(par.pH+par.zH);
par.yLL   = 1/2*(par.pL+par.zL);
par.yHL   = 1/2*(par.zH+par.pL)*exp(-par.a*(par.zH-par.pL)^2);
par.yLH   = 1/2*(par.zL+par.pH)*exp(-par.a*(par.zL-par.pH)^2);

xini      = log([0.1,0.1]);
options     = optimset('MaxIter',10^5,'MaxFunEvals',10E5,'TolX',10E-4,'TolFun',10E-4,'Display','Off');
[resk,valk,flagk] = fsolve(@(x)get_kappas(par,x),xini,options);
par.kappaH  = exp(resk(1));
par.kappaL  = exp(resk(2));

% implied wages
par.factor = par.beta*(1-par.delta)*par.f*par.eta*par.theta*(par.kappaH-par.kappaL)/(1-par.eta*par.beta*(1-par.delta)*par.f);

wHH = par.eta*(par.yHH + par.kappaH*par.theta + (1-par.piH)*par.factor)+(1-par.eta)*par.b;
wLL = par.eta*(par.yLL + par.kappaL*par.theta - (par.piH)*par.factor)+(1-par.eta)*par.b;
wLH = par.eta*(par.yLH + par.kappaH*par.theta + (1-par.piH)*par.factor)+(1-par.eta)*par.b;
wHL = par.eta*(par.yHL + par.kappaL*par.theta - (1-par.piH)*par.factor)+(1-par.eta)*par.b;

% separating?
JHL = par.yHL - wHL;
JLH = par.yLH - wLH;

% check properties

par.eHH = par.f*par.u*par.piH*par.piH/par.delta;
par.eLL = par.f*par.u*(1-par.piH)*(1-par.piH)/par.delta;

sep_invol = par.s*(1-par.u)-(par.eLH);
remain    = (1-par.s)*(1-par.u);

w_vol_seps      = wLH;
w_invol_seps    = par.delta*(par.eHH/sep_invol*wHH + par.eLL/sep_invol*wLL) + par.eHL/sep_invol*wHL;
w_remain        = (1-par.delta)/remain*(par.eHH*wHH + par.eLL*wLL);
w_all_seps      = par.volsh*w_vol_seps+(1-par.volsh)*w_invol_seps;

G(1) = 10*(log(w_vol_seps)-log(w_invol_seps) - par.vol_penalty);
G(2) = log(w_all_seps)-log(w_remain) - par.sep_penalty;
% G(3) = 100*(par.b/w_all_seps - par.replacement);


if max(JHL,JLH)>0
    F = 100;
else
    F    = max(abs(G));
end