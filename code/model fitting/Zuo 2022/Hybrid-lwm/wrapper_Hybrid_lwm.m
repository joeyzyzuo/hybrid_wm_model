% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <https://www.gnu.org/licenses/>.
function wrapper_Hybrid_lwm
% Original code by Wouter Kool (Kool, Cushman, & Gershman, 2016),Florian
% Bolenz (Bolenz, Kool, Reiter, & Eppinger, 2019)
% Code adapted by Joey Zuo

%% prepare data structure
rng(0);
files = dir('./csv2mat');
% get rid of dictionaries
files = files(~[files.isdir]);

% iterate over all logfiles
for fidx = 1:numel(files)
% get file name
fname = files(fidx).name;

% import logfile
data = importdata(['./csv2mat/',char(fname)]);
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
opts.eta = 1;   % indexes variable transition learning rate
opts.kappa = 0; % indexes independent counterfactual transition learning rate
opts.fai = 0;
opts.st = 1;
opts.respst = 1;

nstarts = 100; % number of starts with different random initializations

%% parameter estimation

rng(102293) % set random seed

% iterate over model types
nrmodels=1;
for m = 1:nrmodels
    disp('Fitting model');
    tic
    [options, params] = set_opts_Hybrid_lwm(opts);
    f = @(x,data) Hybrid_lwm_rllik(x,data,options);
    res = mfit_optimize_parallel(f,params,groupdata,nstarts);
    res.subID = {groupdata.subID};
    res.nstarts = nstarts;
    res.opts = opts;
    results = res;
    toc
end
save('./results_Hybrid_lwm.mat', 'results');

for m = 1:nrmodels
t = array2table(results(m).x, 'RowNames', [results(m).subID], 'VariableNames', strrep({results(m).param.name}, ' ', '_'));
t.Properties.DimensionNames(1) = {'id'};
writetable(t, ['./params_Hybrid_lwm.csv'], 'WriteRowNames',true,'Delimiter',';');

end
end