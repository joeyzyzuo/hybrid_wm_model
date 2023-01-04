% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <https://www.gnu.org/licenses/>.
function modelfitting_Hybrid_wm
% Original code by Wouter Kool (Kool, Cushman, & Gershman, 2016),Florian
% Bolenz (Bolenz, Kool, Reiter, & Eppinger, 2019)
% Code adapted by Joey Zuo
%% prepare data structure

files = dir('./csvtomat');
% get rid of dictionaries
files = files(~[files.isdir]);

% iterate over all logfiles
for fidx = 1:numel(files)%
% for fidx = 1:1    
    % get file name
    fname = files(fidx).name;
    
    % import logfile
    data = importdata(['./csvtomat/',char(fname)]);
    
    % add logfile to groupdata structure
    if ~exist('groupdata', 'var')
        groupdata = data;
    else
        groupdata = [groupdata, data];
    end
end

%% prepare parameter estimation
opts.model = 1;     % 1 = hybrid model, 2 = model-based 3 = model-free
opts.lambda = 1; % indexes variable eligibility traces
opts.eta = 0;   % indexes variable transition learning rate
opts.kappa = 0; % indexes independent counterfactual transition learning rate
nstarts = 100; % number of starts with different random initializations

%% parameter estimation

rng(102293) % set random seed

% iterate over model types

nrmodels=1;
for m = 1:nrmodels
    disp('Fitting model');
    tic
    [options, params] = set_opts_Hybrid_wm(opts);
    f = @(x,data) Hybrid_wm_rllik(x,data,options);
    res = mfit_optimize_parallel(f,params,groupdata,nstarts);
    res.subID = {groupdata.subID};
    res.nstarts = nstarts;
    res.opts = opts;
    results = res;
    toc
end
save('./results_bolenz_Hybrid_wm.mat', 'results');

for m = 1:nrmodels
    t = array2table(results(m).x, 'RowNames', [results(m).subID], 'VariableNames', strrep({results(m).param.name}, ' ', '_'));
    t.Properties.DimensionNames(1) = {'id'};
    writetable(t, ['./params_bolenz_Hybrid_wm.csv'], 'WriteRowNames',true,'Delimiter',';');

end
end