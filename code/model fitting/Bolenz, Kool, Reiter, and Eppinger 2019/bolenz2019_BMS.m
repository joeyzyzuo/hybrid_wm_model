results(1) = importdata('C:/Users/Administrator/Desktop/RLWM����/code/Bolenz, Kool, Reiter, and Eppinger 2019/results_all_bolenz_M0_0.mat');
results(2) = importdata('C:/Users/Administrator/Desktop/RLWM����/code/Bolenz, Kool, Reiter, and Eppinger 2019/results_all_bolenz_M4_4_cf.mat');
results(3) = importdata('C:/Users/Administrator/Desktop/RLWM����/code/Bolenz, Kool, Reiter, and Eppinger 2019/results_all_bolenz_M4_9.mat');

bms_results = mfit_bms(results);
figure;
bar(bms_results.xp); colormap bone;
set(gca,'XTickLabel',{'Model 1' 'Model 2' 'Model 3'},'FontSize',25,'YLim',[0 1.2]);
ylabel('Exceedance probability','FontSize',25);
title('Bayesian model comparison','FontSize',25);

figure;
bar(bms_results.pxp); colormap bone;
set(gca,'XTickLabel',{'Model 1' 'Model 2' 'Model 3'},'FontSize',25,'YLim',[0 1.2]);
ylabel('Protected Exceedance probability','FontSize',25);
title('Bayesian model comparison','FontSize',25);

bms_results.bor

{sum(results(1).aic),sum(results(2).aic),sum(results(3).aic)}

bms_results.pxp