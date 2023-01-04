% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <https://www.gnu.org/licenses/>.
function results = mfit_wrapper_Hybrid_rpe
% Original code by Wouter Kool (Kool, Cushman, & Gershman, 2016),Florian
% Bolenz (Bolenz, Kool, Reiter, & Eppinger, 2019)
% Code adapted by Joey Zuo

load groupdata

opts.model = 1; % 1 = hybrid model, 2 = model-based 3 = model-free
opts.st = 1; % indexes presence of stimulus stickiness
opts.respst = 1; % indexes presence of response stickiness
opts = factorial_models(opts);

nstarts = 100;

nrmodels = length(opts);

data = groupdata.subdata(groupdata.i);

% run optimization
m=1;
disp(['Fitting model']);
[options, params] = set_opts_Hybrid_rpe(opts);
f = @(x,data) Hybrid_rpe_rllik(x,data,options);
results = mfit_optimize(f,params,data,nstarts);

% results(m).opts = opts(model);
save('./results_Hybrid_rpe.mat', 'results');

for j = 1:184
    C{j}=num2str(j);
end

results.id=C;
% results.id=subdata.id;
t = array2table(results.x, 'RowNames', [results.id], 'VariableNames', strrep({results.param.name}, ' ', '_'));
t.Properties.DimensionNames(1) = {'id'};
writetable(t, ['./params_Hybrid_rpe.csv'], 'WriteRowNames',true,'Delimiter',';')
end
