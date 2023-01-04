C = {};

for j = 1:24
    C{j}=num2str(j);
end

results=load('results_raws_M0_0.mat')
results.id=C;
% results.id=subdata.id;
t = array2table(results.results.x, 'RowNames', [results.id], 'VariableNames', strrep({results.results.param.name}, ' ', '_'));
t.Properties.DimensionNames(1) = {'id'};
writetable(t, ['params_raws_M0_0.csv'], 'WriteRowNames',true,'Delimiter',';');

results=load('results_raws_M4_4.mat')
results.id=C;
% results.id=subdata.id;
t = array2table(results.results.x, 'RowNames', [results.id], 'VariableNames', strrep({results.results.param.name}, ' ', '_'));
t.Properties.DimensionNames(1) = {'id'};
writetable(t, ['params_raws_M4_4.csv'], 'WriteRowNames',true,'Delimiter',';');

results=load('results_raws_M4_9.mat')
results.id=C;
% results.id=subdata.id;
t = array2table(results.results.x, 'RowNames', [results.id], 'VariableNames', strrep({results.results.param.name}, ' ', '_'));
t.Properties.DimensionNames(1) = {'id'};
writetable(t, ['params_raws_M4_9.csv'], 'WriteRowNames',true,'Delimiter',';');