% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <https://www.gnu.org/licenses/>.
function [param] = set_opts_Hybrid_rpe
% Original code by Wouter Kool (Kool, Cushman, & Gershman, 2016),Florian
% Bolenz (Bolenz, Kool, Reiter, & Eppinger, 2019)
% Code adapted by Joey Zuo

% create parameter structure
g = [3 0.2];
param(1).name = 'inverse temperature';
param(1).logpdf = @(x) sum(log(gampdf(x,g(1),g(2))));  % log density function for prior
param(1).lb = 0;   % lower bound
param(1).ub = 5;  % upper bound

param(2).name = 'reward learning rate';
param(2).logpdf = @(x) sum(log(betapdf(x,2,2)));
param(2).lb = 0;
param(2).ub = 1;

param(3).name = 'eligibility trace decay';
param(3).logpdf = @(x) sum(log(betapdf(x,2,2)));
param(3).lb = 0;
param(3).ub = 1;

param(4).name = 'low mixing weight';
param(4).logpdf = @(x) sum(log(betapdf(x,2,2)));
param(4).lb = 0;
param(4).ub = 1;


param(5).name = 'high mixing weight';
param(5).logpdf = @(x) sum(log(betapdf(x,2,2)));
param(5).lb = 0;
param(5).ub = 1;

mu = 0; sd = 1;   % parameters of choice stickiness
param(6).name = 'choice stickiness';
param(6).logpdf = @(x) sum(log(normpdf(x,mu,sd)));
param(6).lb = -4;
param(6).ub = 4;

mu = 0; sd = 1;    % parameters of response stickiness
param(7).name = 'response stickiness';
param(7).logpdf = @(x) sum(log(normpdf(x,mu,sd)));
param(7).lb = -4;
param(7).ub = 4;


% g = [4.82 0.88];  % parameters of the gamma prior
% param(1).name = 'inverse temperature';
% param(1).logpdf = @(x) sum(log(gampdf(x,g(1),g(2))));  % log density function for prior
% param(1).lb = 0;   % lower bound
% param(1).ub = 20;  % upper bound
% 
% param(2).name = 'learning rate';
% param(2).logpdf = @(x) 0;
% param(2).lb = 0;
% param(2).ub = 1;
% 
% param(3).name = 'eligibility trace decay';
% param(3).logpdf = @(x) 0;
% param(3).lb = 0;
% param(3).ub = 1;
% 
% param(4).name = 'mixing weight low';
% param(4).logpdf = @(x) 0;
% param(4).lb = 0;
% param(4).ub = 1;
% 
% param(5).name = 'mixing weight high';
% param(5).logpdf = @(x) 0;
% param(5).lb = 0;
% param(5).ub = 1;
% 
% mu = 0.15; sd = 1.42;   % parameters of choice stickiness
% param(6).name = 'choice stickiness';
% param(6).logpdf = @(x) sum(log(normpdf(x,mu,sd)));
% param(6).lb = -20;
% param(6).ub = 20;
% 
% mu = 0.15; sd = 1.42;    % parameters of response stickiness
% param(6).name = 'response stickiness';
% param(6).logpdf = @(x) sum(log(normpdf(x,mu,sd)));
% param(6).lb = -20;
% param(6).ub = 20;

end
