% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <https://www.gnu.org/licenses/>.
function results = Hybrid_lwm_wrapper
% Original code by Wouter Kool (Kool, Cushman, & Gershman, 2016),Florian
% Bolenz (Bolenz, Kool, Reiter, & Eppinger, 2019)
% Code adapted by Joey Zuo

files = dir('./spaceship/csv2mat');
% get rid of dictionaries
files = files(~[files.isdir]);

% iterate over all logfiles
for fidx = 1:numel(files)
    
    % get file name
    fname = files(fidx).name;
    
    % import logfile
    data = importdata(['./spaceship/csv2mat/',char(fname)]);
    % add logfile to groupdata structure
    if ~exist('groupdata', 'var')
        groupdata = data;
    else
        groupdata = [groupdata, data];
    end
end

% load groupdata

opts.model = 1; % 1 = hybrid model, 2 = model-based 3 = model-free
opts.st = 1; % indexes presence of stimulus stickiness
opts.respst = 0; % indexes presence of response stickiness
opts = factorial_models(opts);

nrstarts = 100;
nrmodels = length(opts);

data = groupdata;

results = struct;

% run optimization
for m = 1:nrmodels
    
    disp(['Fitting model ',num2str(m)])
    [options, params] = set_opts_Hybrid_lwm(opts(m));
    f = @(x,data) Hybrid_lwm_rllik(x,data,options);
    results = mfit_optimize(f,params,data,nrstarts);
    
    save('./spaceship/results_Hybrid_lwm.mat', 'results');

end

end
