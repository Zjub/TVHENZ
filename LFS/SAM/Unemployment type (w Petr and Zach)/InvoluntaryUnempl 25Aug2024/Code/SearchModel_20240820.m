
% (In)voluntary unemployment

clear all
clc
close all
format shortG

%% 0. Parameters/targets
%==========================================================================

% standard choices
par.beta  = 0.996;          % discount factor (monthly frequency)
par.mu    = 0.5;            % elasticity of matching function (estimate)
par.eta   = 0.5;            % bargaining power (Hosios condition)
par.b     = 0.53;           % Australian replacement rate

% production parameters (some are normalizations, others need calibrating)
par.pL  = 1;                % worker productivity, low
par.zL  = par.pL;           % firm productivity, low
par.pH  = 1.5;%1.2568;              % worker productivity, high (calibrate to match pre-separation premium)
par.zH  = par.pH;           % firm productivity, high
par.a   = 10;%5.24;                % strength of mismatch (calibrate s.t. mismatched jobs separate)

% % vacancy posting costs (need calibrating)
% par.kappaL= 1;              % low type vacancy posting (to target LM tightness)
% par.kappaH= 1.2;            % high-type vacancy posting (to target pi_H^v=pi_H^u)

% targets
par.s     = 0.01;           % separation rate 
par.f     = 0.17;           % job filling probability of 17%
par.theta = 0.28;           % labor market tightness
par.volsh = 0.146;          % share of voluntary separations (loose, including temp.job ends)
par.vol_penalty = -0.132;   % wage penalty of voluntary separators (vs involuntary separators)
par.sep_penalty = -0.163;   % wage penalty of separators (vs remainers)
par.replacement = 0.32;     % replacement rate

% implied values of endogenous variables
par.u     = par.s/(par.s+par.f);            % implied unemployment rate
par.q     = par.f/par.theta;                % job filling probability

% compute share of high-types
xini      = log((1-0.99)/0.99);
options   = optimset('MaxIter',10^5,'MaxFunEvals',10E5,'TolX',10E-12,'TolFun',10E-12,'Display','Off');
[res,val,flag] = fsolve(@(x)get_piH_new(par,x),xini,options);
par.piH   = 1/(1+exp(res(1)));

if val > 1e-6
    disp('not solved')
    return
end

par.delta = (par.s-(par.f*par.u*par.piH*(1-par.piH)/(1-par.u)+par.volsh*par.s))/ ...
    (1-(par.f*par.u*par.piH*(1-par.piH)/(1-par.u)+par.volsh*par.s));    % exogenous separations 

par.eHL   = par.f*par.u*(1-par.piH)*par.piH;
par.eLH   = par.eHL;
% par.eLH   = (1-par.u)*par.volsh - par.eHL;
par.eHH   = par.f*par.u*par.piH^2/par.delta;
par.eLL   = par.f*par.u*(1-par.piH)^2/par.delta;

par.uH    = (par.delta*par.eHH + par.eLH)/par.f;
par.uL    = (par.delta*par.eLL + par.eHL)/par.f;

disp('cross-check flows')
disp('-----------------')
disp('eHL+eLH+eLL+eHH=1-u')
disp(par.eHL+par.eLH+par.eHH+par.eLL - (1-par.u))
disp('uH+uL=u')
disp(par.uH+par.uL-par.u)


% implied parameter values
par.m     = par.f/(par.theta^(1-par.mu));   % matching efficiency

% CALIBRATE PRODUCTION FUNCTION PARAMETERS
%===========================================
xini      = [log(par.pH),log(par.a)];%,log((1-par.b)/par.b)];
options     = optimset('MaxIter',2000,'MaxFunEvals',2000,'TolX',10E-16,'TolFun',10E-16,'display','iter','PlotFcns',@optimplotfval);      
[res,val,flag] = fminsearch(@(x)get_prods(par,x),xini,options);
par.pH    = exp(res(1));
par.a     = exp(res(2));
% % par.b     = 1/(1+exp(res(3)));
par.zH    = par.pH;

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
w_all           = (par.eHH*wHH+par.eLL*wLL+par.eHL*wHL+par.eLH*wLH)/(1-par.u);

disp('separation? (job value of HL and LH), replacement rate, voluntary wage penalty (-13.2% in data) and separation wage penalty (-16.3% in data)')
disp([JHL,JLH,par.b/w_all_seps,log(w_vol_seps)-log(w_invol_seps),log(w_all_seps)-log(w_remain)])

% return 

% simulate separations and wage scars

par.N = 50000;
par.T = 5*12+1;

sim_vol = zeros(par.N,par.T,5);   % worke type (1=H)/unemployed indicator (1=employed)/high firm if matched/low firm if matched/wage
sim_vol(:,:,1) = 1;     % all voluntary separators are high types
sim_vol(:,2,2) = 0;     % all start in unemployment initially   % 2nd element indicates employment (1)
sim_vol(:,2,3) = 0;     % nobody matched % 3rd element indicates high type firm (1)
sim_vol(:,2,4) = 0;     % nobody matched % 4th element indicates low type firm (1)
sim_vol(:,2,5) = par.b; % unemployment benefit  % 5th element indicates income (wage or benefit)
sim_vol(:,1,5) = wLH;   % pre-separation wages
fshocks = rand(par.N,par.T);
Hshocks = rand(par.N,par.T);
dshocks = rand(par.N,par.T);

f_indic = fshocks<=par.f;
H_indic = Hshocks<=par.piH;
d_indic = dshocks<=par.delta;

for t = 3:par.T
    sim_vol(:,t,2) = sim_vol(:,t-1,2).*(sim_vol(:,t-1,3).*(1-d_indic(:,t)) + sim_vol(:,t-1,4)*0) ...    % if employed to high type, separate with dshock, if employed with low type, separate for sure
        + (1-sim_vol(:,t-1,2)).*f_indic(:,t);   % if unemployed, find work with f shock
    sim_vol(:,t,3) = sim_vol(:,t-1,2).*sim_vol(:,t-1,3) + (1-sim_vol(:,t-1,2)).*f_indic(:,t).*(H_indic(:,t));     % if previously in high type, stay there, otherwise if unemployed and find work and get high type
    sim_vol(:,t,4) = (1-sim_vol(:,t-1,2)).*f_indic(:,t).*(1-H_indic(:,t));                      % no persistence, if unemployed and found work but not h shock
    sim_vol(:,t,5) = (1-sim_vol(:,t,2)).*par.b + sim_vol(:,t,2).*(sim_vol(:,t,3).*wHH + sim_vol(:,t,4).*wLH);   % if unemployed, get b, if employed, get wHH if high and wLH if low
end

sim_invol= zeros(par.N,par.T,5);
eHLs = par.eHL/sep_invol;
eHHs = par.delta*par.eHH/sep_invol;
eLLs = par.delta*par.eLL/sep_invol;

Ls   = round((eHLs+eLLs)*par.N);

sim_invol(1:Ls,:,1) = 0;      % low types
sim_invol(Ls+1:end,:,1) = 1;  % high types
sim_invol(:,2,2) = 0;     % all start in unemployment initially
sim_invol(:,2,3) = 0;     % nobody matched
sim_invol(:,2,4) = 0;     % nobody matched
sim_invol(:,2,5) = par.b; % unemployment benefit
sim_invol(1:round(eHLs*par.N),1,5) = wHL;
sim_invol(round(eHLs*par.N)+1:Ls,1,5) = wLL;
sim_invol(Ls+1:end,1,5) = wHH;

for t = 3:par.T
    sim_invol(1:Ls,t,2) = sim_invol(1:Ls,t-1,2).*(sim_invol(1:Ls,t-1,3)*0 + sim_invol(1:Ls,t-1,4).*(1-d_indic(1:Ls,t))) ...
        + (1-sim_invol(1:Ls,t-1,2)).*f_indic(1:Ls,t);   % only previously unemployed search and previously low-types get unemployed
    sim_invol(1:Ls,t,4) = sim_invol(1:Ls,t-1,2).*sim_invol(1:Ls,t-1,4) + (1-sim_invol(1:Ls,t-1,2)).*f_indic(1:Ls,t).*(1-H_indic(1:Ls,t));
    sim_invol(1:Ls,t,3) = (1-sim_invol(1:Ls,t-1,2)).*f_indic(1:Ls,t).*(H_indic(1:Ls,t));
    sim_invol(1:Ls,t,5) = (1-sim_invol(1:Ls,t,2)).*par.b + sim_invol(1:Ls,t,2).*(sim_invol(1:Ls,t,3).*wHL + sim_invol(1:Ls,t,4).*wLL);

    sim_invol(Ls+1:end,t,2) = sim_invol(Ls+1:end,t-1,2).*(sim_invol(Ls+1:end,t-1,3).*(1-d_indic(Ls+1:end,t)) + sim_invol(Ls+1:end,t-1,4)*0) + (1-sim_vol(Ls+1:end,t-1,2)).*f_indic(Ls+1:end,t);   % only previously unemployed search and previously low-types get unemployed
    sim_invol(Ls+1:end,t,3) = sim_invol(Ls+1:end,t-1,2).*sim_invol(Ls+1:end,t-1,3) + (1-sim_invol(Ls+1:end,t-1,2)).*f_indic(Ls+1:end,t).*(H_indic(Ls+1:end,t));
    sim_invol(Ls+1:end,t,4) = (1-sim_invol(Ls+1:end,t-1,2)).*f_indic(Ls+1:end,t).*(1-H_indic(Ls+1:end,t));
    sim_invol(Ls+1:end,t,5) = (1-sim_invol(Ls+1:end,t,2)).*par.b + sim_invol(Ls+1:end,t,2).*(sim_invol(Ls+1:end,t,3).*wHH + sim_invol(Ls+1:end,t,4).*wLH);
end


w_vol       = mean(sim_vol(:,:,5));
w_invol     = mean(sim_invol(:,:,5));

figure
subplot(1,2,1)
plot(0:par.T-1,(w_vol./w_vol(1)-1)*100,'k',0:par.T-1,(w_invol./w_invol(1)-1)*100,'--r','LineWidth',2)
legend('voluntary','involuntary'),ylabel('percent'),title('scars'),xlabel('months')
subplot(1,2,2)
plot(0:par.T-2,(w_vol(2:end)./w_invol(2:end)-1)*100,'LineWidth',2)
ylabel('percent'),xlabel('months'),title('voluntary-involuntary')

w_vol_a     = zeros(5,1);
w_invol_a   = zeros(5,1);

for a = 1:5
    w_vol_a(a)  = mean(w_vol(2+(a-1)*12:a*12+1));
    w_invol_a(a)= mean(w_invol(2+(a-1)*12:a*12+1));
end

figure
subplot(1,2,1)
plot(0:(par.T-1)/12-1,(w_vol_a./w_vol(1)-1)*100,'k',0:(par.T-1)/12-1,(w_invol_a./w_invol(1)-1)*100,'--r','LineWidth',2)
legend('voluntary','involuntary'),ylabel('percent'),title('scars'),xlabel('years')
subplot(1,2,2)
plot(0:(par.T-1)/12-1,(w_vol_a./w_invol_a-1)*100,'LineWidth',2)
ylabel('percent'),xlabel('years'),title('voluntary-involuntary')















