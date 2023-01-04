# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

library(ggplot2)
library(ggExtra)
library(ggpmisc)
library(ggpubr)
library(ggsci)
library(mediation)
library(this.path)

# Get the directory from the path of the current file.
cur_dir2 = dirname(this.path())

# Set the working directory.
setwd(cur_dir2)

# Check that the working directory has been set as desired.
getwd()

data <- read.csv('./bolenz_wm_capacity.csv', sep=',')
data$w_mean=mean(data$mixing_weight_low_stable,data$mixing_weight_high_stable,data$mixing_weight_low_variable,data$mixing_weight_high_variable)
data$工作记忆广度=data$digitspan
d3=subset(data, select=c('points_bc','工作记忆广度','w_mean'))
d3=na.omit(d3)
set.seed(1)
b <- lm(w_mean ~ 工作记忆广度, data=d3)
c <- lm(points_bc ~ 工作记忆广度+w_mean, data=d3)
contcont <- mediate(b, c, sims=50, treat="工作记忆广度", mediator="w_mean")
summary(contcont)
plot(contcont)