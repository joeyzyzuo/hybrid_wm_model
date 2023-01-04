% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <https://www.gnu.org/licenses/>.
function  LL = Hybrid_lwm_llik(x,subdata)
% Original code by Wouter Kool (Kool, Cushman, & Gershman, 2016),Florian
% Bolenz (Bolenz, Kool, Reiter, & Eppinger, 2019)
% Code adapted by Joey Zuo

% parameters
b = x(1);           % softmax inverse temperature
lr = x(2);          % learning rate
lambda = x(3);      % eligibility trace decay
w_low = x(4);           % mixing weight
w_high0 = x(5);           % mixing weight
w_high1 = x(6);           % mixing weight

%%
decay_fai = 0;             % wm decay

%%
% initialization
Qmf_top = ones(1,3)*0.5;
Qmf_middle = ones(6,1)*0.5;
Qmf_terminal = ones(3,1)*0.5;            % Q(s,a): state-action value function for Q-learning

%%
W2 = ones(3,1)*0.5;

LL = 0;

% loop through trials
for t = 1:length(subdata.choice0)
    
    if subdata.missed(t) == 1 || subdata.probe_trial(t) == 1
        continue
    end
    
    %% likelihoods
    if subdata.high_effort(t)==1 % high effort trial
        % level 0
        
        Qmb_middle = zeros(3,2);
        %%
        W_middle = zeros(3,2);
        
        for state = 1:3
            Qmb_middle(state,:) = subdata.Tm_middle(subdata.middle_stims{2}(state,:),:)*Qmf_terminal;   % find model-based values at stage 1
            %%
            W_middle(state,:) = subdata.Tm_middle(subdata.middle_stims{2}(state,:),:)*W2;   % find model-based values at stage 1
        end
                
        Qmb_top = subdata.Tm_top(subdata.stims0(t,:),:)*max(Qmb_middle,[],2);                           % find model-based values at stage 0
        %%
        W_top = subdata.Tm_top(subdata.stims0(t,:),:)*max(W_middle,[],2);                           % find model-based values at stage 0
       %%
        Q_top = w_high0*W_top' + (1-w_high0)*Qmf_top(subdata.stims0(t,:));                            % mix TD and model value
        action = subdata.choice0(t)==subdata.stims0(t,:);
        LL = LL + b*Q_top(action)-logsumexp(b*Q_top);
        
        % level 1
        stims1 = subdata.stims1(t,1:2);
        w = w_high1;
        
    else % low effort trial
        
        stims1 = subdata.stims1(t,:);
        w = w_low;
        
    end
    
    % level 1
    Qmb_middle = subdata.Tm_middle(stims1,:)*Qmf_terminal;                     % find model-based values at stage 0
    %%
    W_middle = subdata.Tm_middle(stims1,:)*W2;                     % find model-based values at stage 0
    %%
    Q_middle = w*W_middle + (1-w)*Qmf_middle(stims1);                        % mix TD and model value
    action = subdata.choice1(t)==stims1;
    LL = LL + b*Q_middle(action)-logsumexp(b*Q_middle);
    
    %% updating
    
    dtQ = zeros(3,1);
    
    if subdata.high_effort(t)==1
        % top level
        dtQ(1) = Qmf_middle(subdata.choice1(t)) - Qmf_top(subdata.choice0(t));
        Qmf_top(subdata.choice0(t)) = Qmf_top(subdata.choice0(t)) + lr*dtQ(1);
    end
    
    %middle level
    dtQ(2) = Qmf_terminal(subdata.state2(t)) - Qmf_middle(subdata.choice1(t));
    Qmf_middle(subdata.choice1(t)) = Qmf_middle(subdata.choice1(t)) + lr*dtQ(2);
    if subdata.high_effort(t)==1
        Qmf_top(subdata.choice0(t)) = Qmf_top(subdata.choice0(t)) + lambda*lr*dtQ(2);
    end
    
    %terminal level
    dtQ(3) = subdata.points(t) - Qmf_terminal(subdata.state2(t));
    Qmf_terminal(subdata.state2(t)) = Qmf_terminal(subdata.state2(t)) + lr*dtQ(3);
    Qmf_middle(subdata.choice1(t)) = Qmf_middle(subdata.choice1(t)) + lambda*lr*dtQ(3);
    if subdata.high_effort(t)==1
        Qmf_top(subdata.choice0(t)) = Qmf_top(subdata.choice0(t)) + (lambda^2)*lr*dtQ(3);
    end
    
    % refresh wm
    W2(subdata.state2(t)) = subdata.points(t) ; 
    
    % wm decay
    if subdata.state2(t)==1
        W2(2) = W2(2) + decay_fai * (0.5-W2(2));
        W2(3) = W2(3) + decay_fai * (0.5-W2(3));
    elseif subdata.state2(t)==2
        W2(1) = W2(1) + decay_fai * (0.5-W2(1));
        W2(3) = W2(3) + decay_fai * (0.5-W2(3));
    else
        W2(1) = W2(1) + decay_fai * (0.5-W2(1));
        W2(2) = W2(2) + decay_fai * (0.5-W2(2));
    end
    
end

end
