# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# conducts a posterior predictive check by comparing reward of participants and simulated agents

# load packages
library(dplyr)
library(ggplot2)

# Check the working directory.
getwd()

# # Install the this.path package.
# install.packages("this.path")

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

# import simulated data
trialdata.sim <- read.csv("./trialdata_Hybrid_wm_sim.csv", sep=";")

# factorize
trialdata.sim$simrun <- as.factor(trialdata.sim$simrun)
trialdata.sim$stake <- as.factor(trialdata.sim$stake)
levels(trialdata.sim$stake) <- c("low", "high")
trialdata.sim$s1 <- as.factor(trialdata.sim$s1)
trialdata.sim$choice <- as.factor(trialdata.sim$choice)
trialdata.sim$s2 <- as.factor(trialdata.sim$s2)

# points baseline-correction
trialdata.sim$points_bc <- trialdata.sim$points - (trialdata.sim$reward1+trialdata.sim$reward2)/2

# aggregate simulated data
d.sim <- trialdata.sim %>%
  group_by(id, version, simrun) %>% # 1. for each simulated agents and each run
  summarize(points_bc_sim = mean(points_bc)) %>%
  group_by(id, version) %>% # 2. for each simulated agents across runs
  summarize(points_bc_sim = mean(points_bc_sim))


# import data
d <- read.csv("./trialdata_zuo.csv", sep=",")

# for timeout trials, set missing data to NA
d$choice <- ifelse(d$timeout1==1,NA,d$choice)
d$rt1 <- ifelse(d$timeout1==1,NA,d$rt1)
d$s2 <- ifelse(d$timeout1==1,NA,d$s2)
d$rt2 <- ifelse(d$timeout==1,NA,d$rt2)
d$points <- ifelse(d$timeout==1,NA,d$points)

# factorize
d$stake <- as.factor(d$stake)
levels(d$stake) <- c("low", "high")
d$s1 <- as.factor(d$s1)
d$choice <- as.factor(d$choice)
d$s2 <- as.factor(d$s2)
d$timeout <- as.factor(d$timeout)
d$timeout1 <- as.factor(d$timeout1)
d$timeout2 <- as.factor(d$timeout2)
d$stimulus_left <- as.factor(d$stimulus_left)
d$stimulus_right <- as.factor(d$stimulus_right)
d$destination1 <- as.factor(d$destination1)
d$destination2 <- as.factor(d$destination2)
d$destination3 <- as.factor(d$destination3)
d$destination4 <- as.factor(d$destination4)

# re-order factor levels
d$age_group <- factor(d$age_group, levels=c("YA", "OA"))

# points baseline-correction
d$points_bc <- d$points - (d$reward1+d$reward2)/2

# new transition?
for (i in 1:nrow(d)) {
  if (d[i,]$timeout1=="1") {d[i,"new_transition"] <- NA; next} # skip trials with timeout at first stage
  if (d[i,]$trial==1 && d[i,]$transitions=="variable") {d[i,"new_transition"] <- 1; next} # if experiment starts with variable-transitions blocks, first trial is defined as a new transition
  destinations <- if (d[i,]$s1==1) d[i,c("destination1", "destination2")] else d[i,c("destination3", "destination4")] # store destination planets for choices available in this trial
  prev_trial <- tail(d[d$id==d[i,]$id & d$s1==d[i,]$s1 & d$timeout1=="0" & d$trial<d[i,]$trial,],n=1) # get last trial from same participant and in same starting state that is not a timeout trial
  if (nrow(prev_trial)==0) { d[i,"new_transition"] <- 0; next} # if there is no fitting previous trial, trial is defined as not a new transition
  prev_destinations <- if (d[i,]$s1==1) prev_trial[1,c("destination1", "destination2")] else prev_trial[1,c("destination3", "destination4")] # get destination planets from previous trial
  if (all(prev_destinations==destinations)) d[i,"new_transition"] <- 0 else d[i,"new_transition"] <- 1 # compare destination planets from previous trial to current trial
}
d$new_transition <- as.factor(d$new_transition)

saveRDS(d, file="./trialdata_run.rds")

# load participant data
trialdata.emp <- readRDS("./trialdata_run.rds")

# aggregate participant data
d.emp <- trialdata.emp %>%
  group_by(id, version) %>%
  summarize(points_bc_emp = mean(points_bc, na.rm=T))

colnames(d.emp) <-c("id","version","points_bc_emp")

# merge simulated and participant data
d <- merge(d.emp, d.sim)

# plot participant data against simulated data
ggplot(d, aes(x=points_bc_emp, y=points_bc_sim)) + 
  geom_point(size=3, alpha=.8) + # individual data points
  scale_colour_manual(values=c("#FF7F00")) + # color for age groups
  scale_shape_manual(values=c(16,17)) + # shape for age groups
  geom_abline(slope=1, intercept=0) + coord_equal(ratio = 1, xlim=c(-0.3, 0.9), ylim=c(-0.3, 0.9)) + # identity line, fix ratio
  theme_classic() +
  theme(axis.title.x =element_text(size=16), axis.title.y=element_text(size=16),
        axis.text.x =element_text(size=16), axis.text.y=element_text(size=16))+
  guides(colour = guide_legend(override.aes = list(alpha=1, size=4))) + theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "baseline-corrected reward (observed)", y= "baseline-corrected reward (simulated)", shape="age group", colour="age group")

# correlation for complete sample
cor(d$points_bc_sim, d$points_bc_emp)
cor.test(d$points_bc_sim, d$points_bc_emp)

ggsave(
  filename = "./postpredcheck.tiff", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 6,             # 宽
  height = 6,            # 高
  units = "in",          # 单位
  dpi = 500              # 分辨率DPI
)