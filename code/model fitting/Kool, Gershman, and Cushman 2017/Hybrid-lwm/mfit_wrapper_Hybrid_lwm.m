% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <https://www.gnu.org/licenses/>.
function results = mfit_wrapper_Hybrid_lwm

% Original code by Wouter Kool (Kool, Cushman, & Gershman, 2016),Florian
% Bolenz (Bolenz, Kool, Reiter, & Eppinger, 2019)
% Code adapted by Joey Zuo

load './groupdata'

% simulation parameters
N = 200;        % number of trials per subject

% data = struct;

nstarts = 100;

for j = 1:length(groupdata.i)
    
    i = groupdata.i(j);
    
    subdata = groupdata.subdata{i};
    
    data(j).points = subdata.points;
    data(j).choice1 = subdata.choice1;
    data(j).state1 = subdata.state1;
    data(j).state2 = subdata.state2;
    data(j).stake = subdata.stake;
    data(j).rt1 = subdata.rt1;
    data(j).rt2 = subdata.rt2;
    data(j).rews = subdata.rews;
    data(j).stim_left = subdata.stim_left;
    data(j).N = N;
    
end

% run optimization
params = set_opts_Hybrid_lwm;
f = @(x,data) Hybrid_lwm_rllik(x,data);
results = mfit_optimize(f,params,data,nstarts);

save('./results_kool2017_Hybrid_lwm.mat', 'results');

C = {};

for j = 1:98
    C{j}=num2str(j);
end

results.id=C;
% results.id=subdata.id;
t = array2table(results.x, 'RowNames', [results.id], 'VariableNames', strrep({results.param.name}, ' ', '_'));
t.Properties.DimensionNames(1) = {'id'};
writetable(t, ['./params_kool2017_Hybrid_lwm.csv'], 'WriteRowNames',true,'Delimiter',';');

end
