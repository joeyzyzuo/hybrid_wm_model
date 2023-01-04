% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <https://www.gnu.org/licenses/>.
function parameter_recovery_Hybrid_wm
% Original code by Wouter Kool (Kool, Cushman, & Gershman, 2016),Florian
% Bolenz (Bolenz, Kool, Reiter, & Eppinger, 2019)
% Code adapted by Joey Zuo
rng(111);
runs = 500;

truep = nan(runs, 12); % true parameters
envs = {importdata('trialseqs/env320_A.mat'), importdata('trialseqs/env320_B.mat'), importdata('trialseqs/env320_C.mat'), importdata('trialseqs/env320_D.mat')};

%% simulate RL agents

for n=1:runs
    % simulate data
    b = unifrnd(0,2);                    % softmax inverse temperature
    lr = unifrnd(0,1);                   % reward learning rate
    lambda = unifrnd(0,1);               % eligibility trace decay
    eta = unifrnd(0,1);                  % transition learning rate
    kappa = eta;                         % counterfactual transition learning rate

    w_WM_low_stable = unifrnd(0,1);       % mixing weight low stakes variable transitions
    w_WM_high_stable = unifrnd(0,1);      % mixing weight high stakes variable transitions
    w_WM_low_variable = unifrnd(0,1);         % mixing weight low stakes stable transitions
    w_WM_high_variable = unifrnd(0,1);        % mixing weight high stakes stable transitions
    
    decay_fai = unifrnd(0,1);             % choice stickiness

    st = unifrnd(-0.5,0.5);                 % stimulus stickiness
    respst = unifrnd(-0.5,0.5);             % response stickiness

    truep(n,:) = [b, lr, lambda, eta, kappa, w_WM_low_stable, w_WM_high_stable, w_WM_low_variable, w_WM_high_variable ,decay_fai,st,respst];

    env = envs{mod(n-1,4)+1}; % loop over task sequences

    data(n) = sim_agent_Hybrid_wm(truep(n,:), env);
end

%% fit data from simulated agents

% estimate parameters
opts.model = 1;     % 1 = hybrid model, 2 = model-based 3 = model-free
opts.lambda = 1; % indexes variable eligibility traces
opts.eta = 1;   % indexes variable transition learning rate
opts.kappa = 0; % indexes independent counterfactual transition learning rate
opts.wwm = 1;    % indexes presence of wm weight parameter
opts.fai = 1; % indexes presence of response stickiness
opts.st = 1;    % indexes presence of wm weight parameter
opts.respst = 1; % indexes presence of response stickiness

nstarts = 100;

[options, params] = set_opts_Hybrid_wm(opts);
f = @(x,data) Hybrid_wm_rllik(x,data,options);
results = mfit_optimize_parallel(f,params,data,nstarts);
%% output results
truep = truep(:,[1:4,6:end]);

cnames1 = strcat(strrep({results.param.name}, ' ', '_'), '_true');
cnames2 = strcat(strrep({results.param.name}, ' ', '_'), '_estimated');
cnames = [cnames1, cnames2];

t = array2table([truep, results.x], 'VariableNames', cnames);
writetable(t, './recovery.csv', 'Delimiter',';')

end