# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# analyze results from parameter recovery

# Load the this.path package.
library(this.path)

# Get the directory from the path of the current file.
cur_dir2 = dirname(this.path())

# Set the working directory.
setwd(cur_dir2)

# Check that the working directory has been set as desired.
getwd()

# clear workspace
rm(list=ls())

# load data
d <- read.csv("./recovery.csv", sep=";")

## compute correlations for true and estimated parameter values for each parameter

# model-based weight low stakes, stable transitions
cor(d$mixing_weight_low_stable_true, d$mixing_weight_low_stable_estimated)

# model-based weight high stakes, stable transitions
cor(d$mixing_weight_high_stable_true, d$mixing_weight_high_stable_estimated)

# model-based weight low stakes, variable transitions
cor(d$mixing_weight_low_variable_true, d$mixing_weight_low_variable_estimated)

# model-based weight high stakes, variable transitions
cor(d$mixing_weight_high_variable_true, d$mixing_weight_high_variable_estimated)

# reward learning rate
cor(d$reward_learning_rate_true, d$reward_learning_rate_estimated)

# eligibility trace decay
cor(d$eligibility_trace_decay_true, d$eligibility_trace_decay_estimated)

# transition learning rate
cor(d$transition_learning_rate_true, d$transition_learning_rate_estimated)

# inverse softmax temperature
cor(d$inverse_temperature_true, d$inverse_temperature_estimated)

# fai decay
cor(d$fai_decay_wm_true, d$fai_decay_wm_estimated)

# choice stickiness
cor(d$choice_stickiness_true, d$choice_stickiness_estimated)

# response stickiness
cor(d$response_stickiness_true, d$response_stickiness_estimated)
