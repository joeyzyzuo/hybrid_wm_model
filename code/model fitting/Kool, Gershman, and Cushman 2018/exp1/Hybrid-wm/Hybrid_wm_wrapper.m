% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <https://www.gnu.org/licenses/>.
function results = Hybrid_wm_wrapper
% Original code by Wouter Kool (Kool, Cushman, & Gershman, 2016),Florian
% Bolenz (Bolenz, Kool, Reiter, & Eppinger, 2019)
% Code adapted by Joey Zuo

load '.\groupdata.mat'

% simulation parameters
rng(102293) % set random seed
nstarts = 100;

data = groupdata.subdata(groupdata.i);

% run optimization
f = @(x,data) Hybrid_wm_llik(x,data);
results = mfit_optimize(f,set_Hybrid_wm,data,nstarts);

save('results_Hybrid_wm.mat', 'results');

results.subID=groupdata.id;

t = array2table(results.x, 'RowNames', [results.subID], 'VariableNames', strrep({results.param.name}, ' ', '_'));
t.Properties.DimensionNames(1) = {'id'};
writetable(t, ['param_Hybrid_wm.csv'], 'WriteRowNames',true,'Delimiter',';');

end