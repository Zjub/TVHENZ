
% (In)voluntary unemployment

clear all
clc
format shortG

%% 0. Basic model first
%==========================================================================

% 1.0 "Standard" choices
%==========================================================================

p.yLL   = 1;            % normalization
p.yHH   = 1.1;          % high well matched output
p.yHL   = 0.9;
p.yLH   = p.yHL;        % mismatched output

p.beta  = 0.996;        % discount factor (monthly frequency)
p.s     = 0.01;         % separation rate (CPS quarterly value)
p.mu    = 0.5;          % elasticity of matching function (estimate)
p.eta   = 0.5;          % bargaining power (Hosios condition)
p.b     = 0.32;         % Australian replacement rate

p.kappaL= 1;            % low type vacancy posting (to target LM tightness)
p.kappaH= 1.2;          % high-type vacancy posting (to target pi_H^v=pi_H^u)

% 1.1 Targets
%==========================================================================
t.f     = 0.17;         % job filling probability of 17%
t.theta = 0.28;         % labor market tightness
t.preinc= 1.1;          % voluntary vs involuntary income difference pre separation

% 1.2 Implied values
%==========================================================================
p.u     = p.s/(p.s+t.f);            % implied unemployment rate
p.q     = t.f/t.theta;              % job filling probability
p.m     = t.f/(t.theta^(1-p.mu));   % matching efficiency

% 1.3 Implied job values
%==========================================================================

JHL = (1-p.eta)*(p.yHL-p.b)-p.eta*p.kappaH*t.theta;
JLH = (1-p.eta)*(p.yLH-p.b)-p.eta*p.kappaL*t.theta;

JHH = ((1-p.eta)*(p.yHH-p.b)-p.eta*p.kappaH*t.theta)/(1-p.beta*(1-p.delta));
JLL = ((1-p.eta)*(p.yLL-p.b)-p.eta*p.kappaL*t.theta)/(1-p.beta*(1-p.delta));

















