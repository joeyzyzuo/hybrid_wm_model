C = {};

for j = 1:21
    C{j}=num2str(j);
end

results=load('results_Hybrid_rpe.mat')
results.id=C;
% results.id=subdata.id;
t = array2table(results.results.x, 'RowNames', [results.id], 'VariableNames', strrep({results.results.param.name}, ' ', '_'));
t.Properties.DimensionNames(1) = {'id'};
writetable(t, ['params_Hybrid_rpe.csv'], 'WriteRowNames',true,'Delimiter',';');

results=load('results_Hybrid_wm.mat')
results.id=C;
% results.id=subdata.id;
t = array2table(results.results.x, 'RowNames', [results.id], 'VariableNames', strrep({results.results.param.name}, ' ', '_'));
t.Properties.DimensionNames(1) = {'id'};
writetable(t, ['params_Hybrid_wm.csv'], 'WriteRowNames',true,'Delimiter',';');

results=load('results_Hybrid_lwm.mat')
results.id=C;
% results.id=subdata.id;
t = array2table(results.results.x, 'RowNames', [results.id], 'VariableNames', strrep({results.results.param.name}, ' ', '_'));
t.Properties.DimensionNames(1) = {'id'};
writetable(t, ['params_Hybrid_lwm.csv'], 'WriteRowNames',true,'Delimiter',';');