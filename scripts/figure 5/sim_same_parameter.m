% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <https://www.gnu.org/licenses/>.
function sim_same_parameter
rng(111);
% prepares a posterior predictive check by simulating agents on the basis
% of the parameters retrieved from the participants' behavior

% Original code by Wouter Kool (Kool, Cushman, & Gershman, 2016),Florian
% Bolenz (Bolenz, Kool, Reiter, & Eppinger, 2019)
% Code adapted by Joey Zuo

% create output file
fileID_Hybrid_rpe = fopen('./trialdata_bolenz_rational_Hybrid_rpe_sim.csv' ,'w');
% write column names in output file
fprintf(fileID_Hybrid_rpe, 'id;simrun;version;trial;transitions;stake;s1;choice;s2;points;reward1;reward2\n');

fileID_Hybrid_lwm = fopen('./trialdata_bolenz_rational_Hybrid_lwm_sim.csv' ,'w');
% write column names in output file
fprintf(fileID_Hybrid_lwm, 'id;simrun;version;trial;transitions;stake;s1;choice;s2;points;reward1;reward2\n');

fileID_Hybrid_wm = fopen('./trialdata_bolenz_rational_Hybrid_wm_sim.csv' ,'w');
% write column names in output file
fprintf(fileID_Hybrid_wm, 'id;simrun;version;trial;transitions;stake;s1;choice;s2;points;reward1;reward2\n');

envs = {importdata('./trialseqs/env320_A.mat'), importdata('./trialseqs/env320_B.mat'), importdata('./trialseqs/env320_C.mat'), importdata('./trialseqs/env320_D.mat')};

runs=500;
truep_Hybrid_rpe = nan(runs, 11); % true parameters of Hybrid-rpe
truep_Hybrid_lwm = nan(runs, 11); % true parameters of Hybrid-lwm
truep_Hybrid_wm = nan(runs, 12); % true parameters of Hybrid-wm

nruns=10;

for n=1:runs
    b = unifrnd(0,5);                    % softmax inverse temperature
    lr = unifrnd(0,1);                   % reward learning rate
    lambda = unifrnd(0,1);               % eligibility trace decay
    eta = unifrnd(0,1);                  % transition learning rate
    kappa = eta;                         % counterfactual transition learning rate

    w_low_stable = unifrnd(0,1);       % mixing weight low stakes variable transitions
    w_high_stable = unifrnd(0,1);      % mixing weight high stakes variable transitions
    w_low_variable = unifrnd(0,1);         % mixing weight low stakes stable transitions
    w_high_variable = unifrnd(0,1);        % mixing weight high stakes stable transitions

    decay_fai = unifrnd(0,1);             % choice stickiness

    st = unifrnd(0,1);                 % stimulus stickiness
    respst = unifrnd(0,1);             % response stickiness
    
    truep_Hybrid_rpe(n,:) = [b, lr, lambda, eta, kappa, w_low_stable, w_high_stable, w_low_variable, w_high_variable ,st,respst];
    truep_Hybrid_lwm(n,:) = [b, lr, lambda, eta, kappa, w_low_stable, w_high_stable, w_low_variable, w_high_variable ,st,respst];
    truep_Hybrid_wm(n,:) = [b, lr, lambda, eta, kappa, w_low_stable, w_high_stable, w_low_variable, w_high_variable ,decay_fai,st,respst];

end

% iterate over subjects Hybrid_rpe
for s=1:runs
    disp(s);
    
    id = num2str(s);
    
    params = truep_Hybrid_rpe(s,:);

    env = envs{mod(s-1,4)+1};
    
    tversion=num2str(mod(s-1,4)+1);
    for ridx=1:nruns
        sim = sim_agent_Hybrid_rpe(params, env);
        
        for tidx=1:sim.N
            
            if sim.blockCondition(tidx) == 0
                transitions = "stable";
            else
                transitions = "variable";
            end
            
            % write column names in output file
            fprintf(fileID_Hybrid_rpe, '%s;%.0f;%s;%.0f;%s;%.0f;%0.f;%.0f;%.0f;%.0f;%.0f;%.0f\n', id, ridx, tversion, tidx, transitions, sim.stake(tidx), sim.s(tidx,1), sim.choice(tidx), sim.s(tidx,2), sim.points(tidx), env.rews(tidx,1), env.rews(tidx,2));
        end
    end
end

% iterate over subjects Hybrid_lwm
for s=1:runs
    disp(s);
    id = num2str(s);
    
    params = truep_Hybrid_lwm(s,:);

    env = envs{mod(s-1,4)+1};
    
    tversion=num2str(mod(s-1,4)+1);
    for ridx=1:nruns
        sim = sim_agent_Hybrid_lwm(params, env);
        
        for tidx=1:sim.N
            
            if sim.blockCondition(tidx) == 0
                transitions = "stable";
            else
                transitions = "variable";
            end
            
            % write column names in output file
            fprintf(fileID_Hybrid_lwm, '%s;%.0f;%s;%.0f;%s;%.0f;%0.f;%.0f;%.0f;%.0f;%.0f;%.0f\n', id, ridx, tversion, tidx, transitions, sim.stake(tidx), sim.s(tidx,1), sim.choice(tidx), sim.s(tidx,2), sim.points(tidx), env.rews(tidx,1), env.rews(tidx,2));
        end
    end
end

% iterate over subjects Hybrid_wm
for s=1:runs
    disp(s);
    id = num2str(s);
    
    params = truep_Hybrid_wm(s,:);

    env = envs{mod(s-1,4)+1};
    
    tversion=num2str(mod(s-1,4)+1);
    for ridx=1:nruns
        sim = sim_agent_Hybrid_wm(params, env);
        
        for tidx=1:sim.N
            
            if sim.blockCondition(tidx) == 0
                transitions = "stable";
            else
                transitions = "variable";
            end
            
            % write column names in output file
            fprintf(fileID_Hybrid_wm, '%s;%.0f;%s;%.0f;%s;%.0f;%0.f;%.0f;%.0f;%.0f;%.0f;%.0f\n', id, ridx, tversion, tidx, transitions, sim.stake(tidx), sim.s(tidx,1), sim.choice(tidx), sim.s(tidx,2), sim.points(tidx), env.rews(tidx,1), env.rews(tidx,2));
        end
    end
end

fclose(fileID_Hybrid_rpe);
fclose(fileID_Hybrid_lwm);
fclose(fileID_Hybrid_wm);
end