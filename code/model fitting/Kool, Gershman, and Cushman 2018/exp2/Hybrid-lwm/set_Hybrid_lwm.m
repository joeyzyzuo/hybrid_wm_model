% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <https://www.gnu.org/licenses/>.
function [param] = set_Hybrid_lwm
% Original code by Wouter Kool (Kool, Cushman, & Gershman, 2016),Florian
% Bolenz (Bolenz, Kool, Reiter, & Eppinger, 2019)
% Code adapted by Joey Zuo

% create parameter structure
g = [4.82 0.88];  % parameters of the gamma prior

param(1).name = 'inverse temperature';
param(1).logpdf = @(x) sum(log(gampdf(x,g(1),g(2))));  % log density function for prior
param(1).lb = 0;   % lower bound
param(1).ub = 20;  % upper bound

param(2).name = 'learning rate';
param(2).logpdf = @(x) 0;
param(2).lb = 0;
param(2).ub = 1;

param(3).name = 'eligibility trace decay';
param(3).logpdf = @(x) 0;
param(3).lb = 0;
param(3).ub = 1;

param(4).name = 'mixing weight low effort';
param(4).logpdf = @(x) 0;
param(4).lb = 0;
param(4).ub = 1;

param(5).name = 'mixing weight high effort 0';
param(5).logpdf = @(x) 0;
param(5).lb = 0;
param(5).ub = 1;

param(6).name = 'mixing weight high effort 1';
param(6).logpdf = @(x) 0;
param(6).lb = 0;
param(6).ub = 1;

end