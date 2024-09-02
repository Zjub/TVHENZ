% Aiyagari model
% Finite difference implicit updating with Poisson income
% Greg Kaplan 2017

clear;
close all;

%% PARAMETERS

% preferences
risk_aver   = 1;
rho         = 0.005; %quarterly

%production
deprec      = 0.025;
capshare    = 0.4;

% income risk: discretized N(mu,sigma^2)
arrivalrate_y   = 0.25; % on average once per year
mu_y            = 1;
sd_y            = 0.2;
ny              = 5;

% asset grids
na          = 50;
amax        = 100;   %multiple of aggregate output
borrow_lim  = 0;    %multiple of aggregate output
agrid_par   = 0.4;    %1 for linear, 0 for L-shaped

% computation
maxiter_hjb = 100;
tol_hjb     = 1.0e-8;
delta_hjb   = 1.0e5;

maxiter_kfe = 1000;
tol_kfe     = 1.0e-8;
delta_kfe   = 1.0e8;

maxiter_KL  = 30;
tol_KL      = 1.0e-5;
step_KL     = 0.005;
rguess      = 1-exp(-rho) - 0.0005; % a bit lower than inverse of discount rate
KLratioguess = ((rguess + deprec)/capshare)^(1/(capshare - 1));

mindV      = 1.0e-10; % for numerical stability

%% OPTIONS
Display     = 1;
MakePlots   = 1;

%% SET UP GRIDS

% assets
agrid = linspace(0,1,na)';
agrid = agrid.^(1./agrid_par);
agrid = borrow_lim + (amax-borrow_lim).*agrid;

% asset grid spacing: for partial derivatives
dagrid  =  diff(agrid);
dagridf = [dagrid; dagrid(na-1)];
dagridb = [dagrid(1); dagrid];

% trapezoidal rule: for KFE and moments
adelta          = zeros(na,1);
adelta(1)       = 0.5*dagrid(1);
adelta(2:na-1)  = 0.5*dagrid(1:na-2) + 0.5*dagrid(2:na-1);
adelta(na)      = 0.5*dagrid(na-1);

% income: disretize normal distribution
if ny>1
    width = fzero(@(x)discrete_normal(ny,mu_y,sd_y,x),2);
    [temp,ygrid,ydist] = discrete_normal(ny,mu_y,sd_y,width);
else
    ygrid = 1;
    ydist = 1;
end

%grids on (a,y) space
aagrid = repmat(agrid,1,ny);
yygrid = repmat(ygrid',na,1);
aydelta = repmat(adelta,1,ny);

% income continuous time transition matrix
ytrans = -arrivalrate_y.*eye(ny) + arrivalrate_y.*repmat(ydist',ny,1);
yytrans = kron(ytrans, speye(na));

%% UTILITY FUNCTION

if risk_aver==1
    u = @(c)log(c);
else    
    u = @(c)(c.^(1-risk_aver)-1)./(1-risk_aver);
end    
u1 = @(c) c.^(-risk_aver);
u1inv = @(u) u.^(-1./risk_aver);



%% ITERATE OVER KL RATIO
KLratio = KLratioguess;

iterKL = 0;
KLdiff = 1;

while iterKL <= maxiter_KL && abs(KLdiff)>tol_KL
    iterKL = iterKL + 1;

    r   = capshare.*KLratio^(capshare-1) - deprec;
    wage= (1-capshare).* KLratio^capshare;

    % rescale efficiency units of labor so that output = 1
    yscale = (KLratio^-capshare)./(ygrid'*ydist);
    
    % initialize value function
    Vguess = zeros(na,ny);
    for iy = 1:ny
        Vguess(:,iy) = u(r.*agrid+wage.*yscale.*ygrid(iy))./rho;
    end

    % solve HJB
    if iterKL==1
        V = Vguess;
    end
    Vvec = reshape(V,na*ny,1);

    Vdiff = 1;
    iter = 0;

    while iter <= maxiter_hjb && Vdiff>tol_hjb
        iter = iter + 1;

        % forward difference
        dVf(1:na-1,:) = (V(2:na,:)-V(1:na-1,:))./dagridf(1:na-1);
        dVf(na,:) = u1(r.*agrid(na) + wage.*yscale.*ygrid); %state constraint

        % backward difference
        dVb(2:na,:) = (V(2:na,:)-V(1:na-1,:))./dagridb(2:na);
        dVb(1,:) = u1(r.*agrid(1) + wage.*yscale.*ygrid); %state constraint

        %consumption and savings with forward difference
        conf = u1inv(max(dVf,mindV));
        savf = r.*aagrid + wage.*yscale.*yygrid - conf;
        Hf = u(conf) + dVf.*savf;

        %consumption and savings with backward difference
        conb = u1inv(max(dVb,mindV));
        savb = r.*aagrid + wage.*yscale.*yygrid - conb;
        Hb = u(conb) + dVb.*savb;

        %consumption and derivative with adot = 0
        con0 = r.*aagrid + wage.*yscale.*yygrid;
        dV0 = u1(con0);
        H0 = u(con0);

        % choice of forward or backward differences based on sign of drift    
        Ineither = (1-(savf>0)) .* (1-(savb<0));
        Iunique = (savb<0).*(1-(savf>0)) + (1-(savb<0)).*(savf>0);
        Iboth = (savb<0).*(savf>0);
        Ib = Iunique.*(savb<0).*(Hb>H0) + Iboth.*(Hb>Hf).*(Hb>H0);
        If = Iunique.*(savf>0).*(Hf>H0) + Iboth.*(Hf>Hb).*(Hf>H0);
        I0 = 1-Ib-If;

        
        %consumption, savings and utility
        con  = conf.*If + conb.*Ib + con0.*I0;
        sav  = savf.*If + savb.*Ib;    
        util = u(con);
        utilvec = reshape(util,na*ny,1);

        %construct A matrix: tri-diagonal elements
        Alowdiag = -Ib.*savb./dagridb;
        Adiag = -If.*savf./dagridf + Ib.*savb./dagridb;
        Aupdiag = If.*savf./dagridf;

        %use spdiags to create A matrix for each income value
        for iy = 1:ny
            Amat{iy} = spdiags(Adiag(:,iy),0,na,na) + ...
                        spdiags(Alowdiag(2:na,iy),-1,na,na) + ...
                        spdiags([0;Aupdiag(1:na-1,iy)],1,na,na);
        end

        %combine to create large sparse A matrix
        if ny > 1
            A  = [Amat{1} sparse(na,na*(ny-1))];
            if ny>2    
                for iy = 2:ny-1
                    A = [A; sparse(na,na*(iy-1)) Amat{iy} sparse(na,na*(ny-iy))];
                end
            end    
            A = [A; sparse(na,na*(ny-1)) Amat{ny}];
        elseif ny==1
            A = Amat{1};
        end

        % add Poisson income switches;
        A = A + yytrans;

        B = (rho + 1./delta_hjb)*speye(na*ny) - A;

        % solve linear system
        Vvecnew = B \ (utilvec + Vvec./delta_hjb);
        Vnew    = reshape(Vvecnew,na,ny);

        Vdiff = max(abs(Vvecnew-Vvec));
        if Display >=2
            disp([' HJB iteration ' int2str(iter), ' diff: ' num2str(Vdiff)]);
        end

        V = Vnew;
        Vvec = Vvecnew;
    end 


    % solve KFE
    gvecadj = [A'; ones(1,na*ny)] \ [zeros(na*ny,1); 1];
    gmatadj = reshape(gvecadj,na,ny);
    gmat    = gmatadj./aydelta;
 
    %asset supply
    gamarg = sum(gmat,2);
    Ea = sum(gamarg.*agrid.*adelta);
    L = yscale.*ygrid'*ydist;
    
    KLrationew = Ea./ L;
    
    KLdiff = KLrationew./KLratio - 1;
    if Display >=1
        disp(['Equm iter ' int2str(iterKL), ', r  = ',num2str(r), ', KL ratio: ',num2str(KLrationew),' KL diff: ' num2str(KLdiff*100) '%']);
    end

    KLratio = (1-step_KL)*KLratio + step_KL*KLrationew;    

end    
    
%% MAKE PLOTS
if MakePlots ==1 
    figure(1);
    
    % consumption policy function
    subplot(2,4,1);
    plot(agrid,con(:,1),'b-',agrid,con(:,ny),'r-','LineWidth',1);
    grid;
    xlim([0 amax]);
    title('Consumption Policy Function');
    legend('Lowest income state','Highest income state');

    % savings policy function
    subplot(2,4,2);
    plot(agrid,sav(:,1),'b-',agrid,sav(:,ny),'r-','LineWidth',1);
    hold on;
    plot(agrid,zeros(na,1),'k','LineWidth',0.5);
    hold off;
    grid;
    xlim([0 amax]);
    title('Savings Policy Function');
    
    % consumption policy function: zoomed in
    subplot(2,4,3);
    plot(agrid,con(:,1),'b-o',agrid,con(:,ny),'r-o','LineWidth',2);
    grid;
    xlim([0 1]);
    title('Consumption: Zoomed');
    
     % savings policy function: zoomed in
    subplot(2,4,4);
    plot(agrid,sav(:,1),'b-o',agrid,sav(:,ny),'r-o','LineWidth',2);
    hold on;
    plot(agrid,zeros(na,1),'k','LineWidth',0.5);
    hold off;
    grid;
    xlim([0 1]);
    title('Savings: Zoomed');
    
    
    %income distribution
    subplot(2,4,5);
    bar(ygrid,ydist);
    h = findobj(gca,'Type','patch');
    set(h,'FaceColor',[0 0.5 0.5],'EdgeColor','blue','LineStyle','-');
    ylabel('')
    title('Income distribution');
    
    
    %asset distribution: CDF
    abin        = [borrow_lim:0.01:amax]'; %multiples of 1% of annual income
    nabin       = length(abin);
    
    interpacdf  = griddedInterpolant(agrid,cumsum(adelta.*gamarg),'pchip');
    amass       = zeros(size(abin));
    amass(1)    = adelta(1)*gamarg(1);
    amass(2:nabin) = interpacdf(abin(2:nabin)) - interpacdf(abin(1:nabin-1));
    
    % asset distribution: inverse CDF
    acdf = cumsum(adelta.*gamarg);
    iacdf1 = find(acdf>0.9999999,1);
    if isempty(iacdf1)
        iacdf1 = na;
    end
    interpainvcdf  = griddedInterpolant(acdf(1:iacdf1),agrid(1:iacdf1),'linear');
    
    %plot asset distribution
    subplot(2,4,6:8);
    bar(abin(2:nabin),amass(2:nabin),'histc');
    sh  = findall(gcf,'marker','*'); delete(sh);
    h = findobj(gca,'Type','patch');
    set(h,'FaceColor',[.7 .7 .7],'EdgeColor',[0.7 0.7 0.7],'LineStyle','-','LineWidth',0.01);
    
    xlim([borrow_lim agrid(min(iacdf1+1,na)) ]);
    
    hold on;
    bar(abin(1),amass(1),0.1,'FaceColor','black','EdgeColor','black','LineWidth',0.001);
    hold off;
    ylabel('')
    grid on;
    title('Asset distribution');
 
    
    disp(['Mean assets: ' num2str(sum(gamarg.*agrid.*adelta))]);
    disp(['Fraction borrowing constrained: ' num2str(gamarg(1).*adelta(1) * 100) '%']);
    disp(['10th Percentile: ' num2str(interpainvcdf(0.1))]);
    disp(['50th Percentile: ' num2str(interpainvcdf(0.5))]);
    disp(['90th Percentile: ' num2str(interpainvcdf(0.9))]);
    disp(['99th Percentile: ' num2str(interpainvcdf(0.99))]);

end

%% FUNCTIONS

function [f,x,p] = discrete_normal(n,mu,sigma,width)
% creates equally spaced approximation to normal distribution
% n is number of points
% mu is mean
% sigma is standard deviation
% width is the multiple of stand deviation for the width of the grid
% f is the error in the approximation
% x gives the location of the points
% p is probabilities


x = linspace(mu-width*sigma,mu+width*sigma,n)';

if n==2
    p = 0.5.*ones(n,1);
elseif n>2    
    p  = zeros(n,1);
    p(1) = normcdf(x(1) + 0.5*(x(2)-x(1)),mu,sigma);
    for i = 2:n-1
        p(i) = normcdf(x(i) + 0.5*(x(i+1)-x(i)),mu,sigma) - normcdf(x(i) - 0.5*(x(i)-x(i-1)),mu,sigma);
    end
    p(n) = 1 - sum(p(1:n-1));
end

Ex = x'*p;
SDx = sqrt((x'.^2)*p - Ex.^2);

f = SDx-sigma;
end