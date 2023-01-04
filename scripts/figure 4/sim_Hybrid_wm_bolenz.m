% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <https://www.gnu.org/licenses/>.
function sim_Hybrid_wm_bolenz
rng(111);
% prepares a posterior predictive check by simulating agents on the basis
% of the parameters retrieved from the participants' behavior

% Original code by Wouter Kool (Kool, Cushman, & Gershman, 2016),Florian
% Bolenz (Bolenz, Kool, Reiter, & Eppinger, 2019)
% Code adapted by Joey Zuo

% create output file
fileID = fopen('./trialdata_Hybrid_wm_bolenz_sim.csv' ,'w');
% write column names in output file
fprintf(fileID, 'id;simrun;version;trial;block;transitions;stake;s1;choice;s2;points;reward1;reward2\n');

% load modeling results
results = importdata("./results_bolenz_Hybrid_wm.mat");
x = results.x;
subID = results.subID;

% load trialdata in order to retrieve task versions
[~,txt,~] = xlsread('./trialdata.csv');

% add counterfactual transition learning rate (equal to transition learning
% rate)
x = [x(:,1:4), x(:,4), x(:,5:end)];

nruns = 1; % how often to simulate data for each subject
size_txt=size(txt);

id_cell={};
version_cell={};
for p=2:size_txt
    id_cell{p}=txt{p,1};
end

for p=2:size_txt
    version_cell{p}=txt{p,3};
end

% iterate over subjects
for s=1:size(x,1)
    
    id = subID{s};
    params = x(s,:);
    
    idx2 = find(strcmp(id_cell, id ));
    tversion = version_cell(idx2(1));
    tversion=tversion{1,1};
    
    disp(id);
    disp(tversion);


    % load task version
    switch tversion
        case 'A'
            env = importdata("./trialseqs/env320_A.mat");
        case 'B'
            env = importdata("./trialseqs/env320_B.mat");
        case 'C'
            env = importdata("./trialseqs/env320_C.mat");
        case 'D'
            env = importdata("./trialseqs/env320_D.mat");
        otherwise
            error("Could not load task version")
    end
    
    for ridx=1:nruns
        sim = sim_agent_Hybrid_wm(params, env);
        
        for tidx=1:sim.N
            
            if sim.blockCondition(tidx) == 0
                transitions = "stable";
            else
                transitions = "variable";
            end
            block=repelem([1,2,3,4],80);
            % write column names in output file
            fprintf(fileID, '%s;%.0f;%s;%.0f;%.0f;%s;%.0f;%0.f;%.0f;%.0f;%.0f;%.0f;%.0f\n', id, ridx, tversion, tidx,block(tidx), transitions, sim.stake(tidx), sim.s(tidx,1), sim.choice(tidx), sim.s(tidx,2), sim.points(tidx), env.rews(tidx,1), env.rews(tidx,2));
        end
    end
end

fclose(fileID);
end