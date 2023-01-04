% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <https://www.gnu.org/licenses/>.
function LL = Hybrid_rpe_rllik(x,subdata,opts)
% Original code by Wouter Kool (Kool, Cushman, & Gershman, 2016),Florian
% Bolenz (Bolenz, Kool, Reiter, & Eppinger, 2019)
% Code adapted by Joey Zuo
y = zeros(1,6);
y(opts.ix==1) = x;

switch opts.model
    case 2   
        y(4) = 1;
    case 3
        y(4) = 0;
end
if ~opts.st
    y(5) = 0;
end
if ~opts.respst
    y(6) = 0;
end

% parameters
b = y(1);           % softmax inverse temperature
lr = y(2);          % learning rate
lambda = y(3);      % eligibility trace decay
w = y(4);           % mixing weight

st = y(5);          % stickiness
respst = y(6);      % stickiness

% initialization
Qd = zeros(3,2);            % Q(s,a): state-action value function for Q-learning,step1 and step2

W2 = zeros(2,2); 

Tm = [.5 .5; .5 .5];        % transition matrix
M = [0; 0];                 % last choice structure
R = [0; 0];                 % last response structure

counts = zeros(2,2);        % counting transitions

N = length(subdata.choice1);

LL = 0;

% loop through trials
for t = 1:N

    % Break if trial was missed
    if (subdata.choice1(t) == -1 || subdata.choice2(t) == -1)
        continue
    end
    
    state2 = subdata.state2(t)+1;
    
%     if subdata.stim_1_left(t) == 2
%         R = flipud(R);                                                          % arrange R to reflect stimulus mapping
%     end
    
    maxQ = max(Qd(2:3,:),[],2);                                                 % optimal reward at second step
    Qm = Tm'*maxQ;                                                              % compute model-based value function
    
    
    maxW2 = max(W2,[],2); 
    W1 = Tm'*maxW2;
    
    Q = w*Qm + (1-w)*Qd(1,:)' + st.*M + respst.*R;                              % mix TD and model-based values
        
    LL = LL + b*Q(subdata.choice1(t))-logsumexp(b*Q);                           % update likelihoods
    
    Q2 = w*W2(subdata.state2(t),:)' + (1-w)*Qd(state2,:)';
    
%     LL = LL + b*Q2(subdata.choice2(t)) - logsumexp(b*Q2(:));
    LL = LL + b*Qd(state2,subdata.choice2(t)) - logsumexp(b*Qd(state2,:));

    M = [0; 0];
    M(subdata.choice1(t)) = 1;                                                  % make the last choice sticky
    
%     R = zeros(2,1);
%     if subdata.choice1(t) == subdata.stim_1_left(t)
%         R(1) = 1;                                                               % make the last response sticky
%     else
%         R(2) = 1;
%     end
    
    dtQ(1) = Qd(state2,subdata.choice2(t)) - Qd(1,subdata.choice1(t));          % backup with actual choice (i.e., sarsa)
    Qd(1,subdata.choice1(t)) = Qd(1,subdata.choice1(t)) + lr*dtQ(1);            % update TD value function
     
    dtQ(2) = subdata.win(t) - Qd(state2,subdata.choice2(t));                    % prediction error (2nd choice)
    
    Qd(state2,subdata.choice2(t)) = Qd(state2,subdata.choice2(t)) + lr*dtQ(2);  % update TD value function
    Qd(1,subdata.choice1(t)) = Qd(1,subdata.choice1(t)) + lambda*lr*dtQ(2);     % eligibility trace
    
    fai=0;
    
    W2(1,1)=W2(1,1)+fai*(0.5-W2(1,1));
    W2(1,2)=W2(1,2)+fai*(0.5-W2(1,2));
    W2(2,1)=W2(2,1)+fai*(0.5-W2(2,1));
    W2(1,2)=W2(1,2)+fai*(0.5-W2(1,2));
    
    W2(subdata.state2(t),subdata.choice2(t)) = subdata.win(t);
    
    
    % pick the most likely transition matrix
    counts(subdata.state2(t),subdata.choice1(t)) = counts(subdata.state2(t),subdata.choice1(t))+1;
    
    if sum(diag(counts))>sum(diag(rot90(counts)))
        Tm = [.7 .3; .3 .7];        % transition matrix
    end
    if sum(diag(counts))<sum(diag(rot90(counts)))
        Tm = [.3 .7; .7 .3];        % transition matrix
    end
    if sum(diag(counts))==sum(diag(rot90(counts)))
        Tm = [.5 .5; .5 .5];        % transition matrix
    end
end

end
