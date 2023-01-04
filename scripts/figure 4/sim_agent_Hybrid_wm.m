% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <https://www.gnu.org/licenses/>.
function  output = sim_agent_Hybrid_wm(x,env)

% simulates behavior in the sequential decision-making task

% Inputs:
% x = model parameters
% env = trial sequence

% Original code by Wouter Kool (Kool, Cushman, & Gershman, 2016),Florian
% Bolenz (Bolenz, Kool, Reiter, & Eppinger, 2019)
% Code adapted by Joey Zuo

% parameters
b = x(1);                   % softmax inverse temperature
lr = x(2);                  % reward learning rate
lambda = x(3);              % eligibility trace decay
eta_variable = x(4);        % transition learning rate
kappa = x(5);               % counterfactual transition learning rate
w_WM_low_stable = x(6);        % mixing weight RL_low_stake
w_WM_high_stable = x(7);       % mixing weight RL_high_stake
w_WM_low_variable = x(8);      % mixing weight WM_stable
w_WM_high_variable = x(9);     % mixing weight WM_variable
decay_fai = x(10);
st = x(11);                 % stimulus stickiness
respst = x(12);             % response stickiness

% initialization
Qmf = ones(2,2)*4.5;
Q2 = ones(2,1)*4.5;            % Q(s,a): state-action value function for Q-learning

W2 = ones(2,1)*4.5;         % Q(s,a): Second-stage WM values

Tm = cell(2,1);
Tm{1} = [0.5 0.5; 0.5 0.5];         % transition matrix, row: to-state, columns: action
Tm{2} = [0.5 0.5; 0.5 0.5];         % transition matrix
M = [0 0; 0 0];                 % last choice structure
R = [0; 0];                     % last choice structure

Tar = [0; 0];                 % last choice structure ,target sticky

N = size(env.rews,1);
output.choice = zeros(N,1);
output.points = zeros(N,1);
output.s = zeros(N,2);
output.stake = zeros(N,1);
output.blockCondition = zeros(N,1);
output.stimuli = zeros(N,2);
output.timeout = zeros(N,2);

% loop through trials
for t = 1:N
    
    if (env.stimuli(t,1) == 2) || (env.stimuli(t,1) == 4)
        R = flipud(R);                                              % arrange R to reflect stimulus mapping
    end
    
    s1 = env.s1(t);
    
    Qmb = Tm{s1}'*Q2;                                       % compute model-based value function
    
    W = Tm{s1}'*W2;               % compute model-based value function
    
    if env.stake(t) == 1 && env.blockCondition(t) == 0
        wwm = w_WM_low_stable;
        
        eta = 1;
    elseif env.stake(t) == 5 && env.blockCondition(t) == 0
        wwm = w_WM_high_stable;

        eta = 1;
    elseif env.stake(t) == 1 && env.blockCondition(t) == 1
        wwm = w_WM_low_variable;

        eta = eta_variable;
    elseif env.stake(t) == 5 && env.blockCondition(t) == 1
        wwm = w_WM_high_variable;

        eta = eta_variable;
    else
        error('Could not determine model-based weight')
    end
    
%     Q = w*Qmb + (1-w)*Qmf(s1,:)' + st.*M(s1,:)' + respst.*R;  % mix TD and model value
%     Q = w*Qmb + wwm*W + (1-w-wwm)*Qmf(s1,:)'  + st.*M(s1,:)' + respst.*R;      % mix TD and model-based value
%     Q = wwm*W + (1-wwm)*Qmf(s1,:)' ;        % mix TD and model-based value
    Q = wwm*W + (1-wwm)*Qmf(s1,:)' + st.*M(s1,:)' + respst.*R;        % mix TD and model-based value
    
    if rand < exp(b*Q(1))/sum(exp(b*Q))                     % make choice using softmax
        a = 1;
    else
        a = 2;
    end
    
    M = zeros(2,2);
    M(s1,a) = 1;                                                    % make the last choice sticky
    
    R = zeros(2,1);
    if a+(s1-1)*2 == env.stimuli(t,1)
        R(1) = 1;                                                   % make the last response sticky
    elseif a+(s1-1)*2 == env.stimuli(t,2)
        R(2) = 1;
    else
        error('Error')
    end 
  
    
   
    
    s2 = env.targets(t, a+2*(s1-1));
    rew = env.rews(t,s2);
    
    %%
    
    Tar = zeros(2,1);
    Tar(s2) = 1;
     %%
    
    dtQ(1) = Q2(s2) - Qmf(s1,a);                            % backup with actual choice (i.e., sarsa)
    Qmf(s1,a) = Qmf(s1,a) + lr*dtQ(1);                      % update TD value function
    
    dtQ(2) = rew - Q2(s2);                           % prediction error (2nd choice)
    
    Q2(s2) = Q2(s2) + lr*dtQ(2);                            % update TD value function
    Qmf(s1,a) = Qmf(s1,a) + lambda*lr*dtQ(2);               % eligibility trace
    
    % refresh wm
    W2(s2) = rew ; 
    
%%
% wm decay
%     dtW2 = ones(2,1)*4.5 - W2; 
    
    if s2==1
        W2(2) = W2(2) + decay_fai * (4.5-W2(2)); 
    else
        W2(1) = W2(1) + decay_fai * (4.5-W2(1));
    end
%%     
    % update transition matrix
    
    spe = 1 - Tm{s1}(s2, a);
    Tm{s1}(s2, a) = Tm{s1}(s2, a) + eta*spe;
    Tm{s1}(abs(s2-3), a) = Tm{s1}(abs(s2-3), a)*(1-eta);
    
    cf_spe = 1 - Tm{s1}(abs(s2-3), abs(a-3));               % counterfactual state prediction error
    Tm{s1}(abs(s2-3), abs(a-3)) =  Tm{s1}(abs(s2-3), abs(a-3)) + kappa*cf_spe;  % update transition
    Tm{s1}(s2, abs(a-3)) = Tm{s1}(s2, abs(a-3))*(1-kappa);  % reduce transition
    
    % store stuff
    output.choice(t,:) = a;
    output.points(t,1) = rew;
    output.s(t,1) = s1;
    output.s(t,2) = s2;
    output.stake(t) = env.stake(t);
    output.blockCondition(t) = env.blockCondition(t);
    output.stimuli(t,:) = env.stimuli(t,:);

end

output.env = env;
output.N = N;

end