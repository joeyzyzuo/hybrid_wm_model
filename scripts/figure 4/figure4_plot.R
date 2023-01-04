# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# load packages
library(brms)
library(dplyr)
library(ggplot2)
library(reshape2)
library(bayesplot)
library(stringr)
library(ggExtra)
library(ggpmisc)
library(ggpubr)
library(ggsci)
library(cowplot)
library(png)
library("gridExtra")
library(tidyverse)
library(gapminder)
library(ggprism)
library(rstatix)
# Load the this.path package.
library(this.path)

# Get the directory from the path of the current file.
cur_dir2 = dirname(this.path())

# Set the working directory.
setwd(cur_dir2)

# Check that the working directory has been set as desired.
getwd()

calc_incr <- function(choiceS2_list, points_list) {
  count_incr = 0
  count_incr_swift = 0
  
  for (i in 2:(length(choiceS2_list)-1)) {
    choice_past1=choiceS2_list[i-1][[1]]#s2(t-1)
    choice_plus1=choiceS2_list[i+1][[1]]#s2(t+1)
    choice_now=choiceS2_list[i][[1]]#s2(t)
    
    if (choice_now == -1) next
    
    if ((choice_now == choice_past1) & ((points_list[i][[1]] - points_list[i-1][[1]]) > 1)) {
      count_incr = count_incr + 1
      if (choice_now == choice_plus1){
        count_incr_swift = count_incr_swift + 1
      }
    }
  }
  return(list(count_incr, count_incr_swift))
}

calc_decr <- function(choiceS2_list, points_list) {
  count_incr = 0
  count_incr_swift = 0
  
  for (i in 2:(length(choiceS2_list)-1)) {
    choice_past1=choiceS2_list[i-1][[1]]#s2(t-1)
    choice_plus1=choiceS2_list[i+1][[1]]#s2(t+1)
    choice_now=choiceS2_list[i][[1]]#s2(t)
    
    if (choice_now == -1) next
    
    if ((choice_now == choice_past1) & ((points_list[i-1][[1]] - points_list[i][[1]]) > 1)) {
      count_incr = count_incr + 1
      if (choice_now == choice_plus1){
        count_incr_swift = count_incr_swift + 1
      }
    }
  }
  return(list(count_incr, count_incr_swift))
}


d = read.csv('./trialdata_zuo.csv',sep=',')

prob = c()
# iterate over subjects
for (s in unique(d$id)) {
  
  count <- c(0, 0) # initial values
  
  # iterate over blocks
  for (t in 1:4) {
    if (d[d$id==s & d$block==t,"transitions"][1] != 'stable') next
    s2 <- d[d$id==s & d$block==t,"s2"]
    rew <- d[d$id==s & d$block==t,"points"]
    
    count_retrun = calc_incr(s2,rew)
    
    count[1] = count[1] + count_retrun[[1]]
    
    count[2] = count[2] + count_retrun[[2]]
    
  }
  if (count[2] != 0){
    prob = append(prob,count[2]/count[1])
  }
}

X = array(0:(length(prob)-1))
source = rep('actual data', times=length(prob))

stay_prob_incr_actual_data = data.frame(
  X = X,
  source = source,
  prob = prob
)

d = read.csv('./trialdata_Hybrid_wm_zuo_sim.csv',sep=';')

prob = c()
# iterate over subjects
for (s in unique(d$id)) {
  
  count <- c(0, 0) # initial values
  
  # iterate over blocks
  for (t in 1:4) {
    if (d[d$id==s & d$block==t,"transitions"][1] != 'stable') next
    s2 <- d[d$id==s & d$block==t,"s2"]
    rew <- d[d$id==s & d$block==t,"points"]
    
    count_retrun = calc_incr(s2,rew)
    
    count[1] = count[1] + count_retrun[[1]]
    
    count[2] = count[2] + count_retrun[[2]]
    
  }
  if (count[2] != 0){
    prob = append(prob,count[2]/count[1])
  }
}

X = array(0:(length(prob)-1))
source = rep('Hybrid-wm', times=length(prob))

stay_prob_incr_wm = data.frame(
  X = X,
  source = source,
  prob = prob
)


d = read.csv('./trialdata_Hybrid_lwm_zuo_sim.csv',sep=';')

prob = c()
# iterate over subjects
for (s in unique(d$id)) {
  
  count <- c(0, 0) # initial values
  
  # iterate over blocks
  for (t in 1:4) {
    if (d[d$id==s & d$block==t,"transitions"][1] != 'stable') next
    s2 <- d[d$id==s & d$block==t,"s2"]
    rew <- d[d$id==s & d$block==t,"points"]
    
    count_retrun = calc_incr(s2,rew)
    
    count[1] = count[1] + count_retrun[[1]]
    
    count[2] = count[2] + count_retrun[[2]]
    
  }
  if (count[2] != 0){
    prob = append(prob,count[2]/count[1])
  }
}

X = array(0:(length(prob)-1))
source = rep('Hybrid-lwm', times=length(prob))

stay_prob_incr_lwm = data.frame(
  X = X,
  source = source,
  prob = prob
)

d = read.csv('./trialdata_Hybrid_rpe_zuo_sim.csv',sep=';')

prob = c()
# iterate over subjects
for (s in unique(d$id)) {
  
  count <- c(0, 0) # initial values
  
  # iterate over blocks
  for (t in 1:4) {
    if (d[d$id==s & d$block==t,"transitions"][1] != 'stable') next
    s2 <- d[d$id==s & d$block==t,"s2"]
    rew <- d[d$id==s & d$block==t,"points"]
    
    count_retrun = calc_incr(s2,rew)
    
    count[1] = count[1] + count_retrun[[1]]
    
    count[2] = count[2] + count_retrun[[2]]
    
  }
  if (count[2] != 0){
    prob = append(prob,count[2]/count[1])
  }
}

X = array(0:(length(prob)-1))
source = rep('Hybrid-rpe', times=length(prob))

stay_prob_incr_rpe = data.frame(
  X = X,
  source = source,
  prob = prob
)

stay_prob_incr=rbind(stay_prob_incr_actual_data,stay_prob_incr_wm,stay_prob_incr_lwm,stay_prob_incr_rpe)

library(stats)
wilcox.test(stay_prob_incr[stay_prob_incr$source == "actual data",]$prob,stay_prob_incr[stay_prob_incr$source == "Hybrid-wm",]$prob, 
            var.equal = TRUE)
wilcox.test(stay_prob_incr[stay_prob_incr$source == "actual data",]$prob,stay_prob_incr[stay_prob_incr$source == "Hybrid-rpe",]$prob, 
            var.equal = TRUE)
wilcox.test(stay_prob_incr[stay_prob_incr$source == "actual data",]$prob,stay_prob_incr[stay_prob_incr$source == "Hybrid-lwm",]$prob, 
            var.equal = TRUE)

stay_prob_incr$source=ordered(stay_prob_incr$source, levels = c("actual data", "Hybrid-wm", "Hybrid-lwm", "Hybrid-rpe")) 
stay_prob_incr$cond='increasing'


d = read.csv('./trialdata_zuo.csv',sep=',')

prob = c()
# iterate over subjects
for (s in unique(d$id)) {
  
  count <- c(0, 0) # initial values
  
  # iterate over blocks
  for (t in 1:4) {
    if (d[d$id==s & d$block==t,"transitions"][1] != 'stable') next
    s2 <- d[d$id==s & d$block==t,"s2"]
    rew <- d[d$id==s & d$block==t,"points"]
    
    count_retrun = calc_decr(s2,rew)
    
    count[1] = count[1] + count_retrun[[1]]
    
    count[2] = count[2] + count_retrun[[2]]
    
  }
  if (count[2] != 0){
    prob = append(prob,count[2]/count[1])
  }
}

X = array(0:(length(prob)-1))
source = rep('actual data', times=length(prob))

stay_prob_decr_actual_data = data.frame(
  X = X,
  source = source,
  prob = prob
)

d = read.csv('./trialdata_Hybrid_wm_zuo_sim.csv',sep=';')

prob = c()
# iterate over subjects
for (s in unique(d$id)) {
  
  count <- c(0, 0) # initial values
  
  # iterate over blocks
  for (t in 1:4) {
    if (d[d$id==s & d$block==t,"transitions"][1] != 'stable') next
    s2 <- d[d$id==s & d$block==t,"s2"]
    rew <- d[d$id==s & d$block==t,"points"]
    
    count_retrun = calc_decr(s2,rew)
    
    count[1] = count[1] + count_retrun[[1]]
    
    count[2] = count[2] + count_retrun[[2]]
    
  }
  if (count[2] != 0){
    prob = append(prob,count[2]/count[1])
  }
}

X = array(0:(length(prob)-1))
source = rep('Hybrid-wm', times=length(prob))

stay_prob_decr_wm = data.frame(
  X = X,
  source = source,
  prob = prob
)


d = read.csv('./trialdata_Hybrid_lwm_zuo_sim.csv',sep=';')

prob = c()
# iterate over subjects
for (s in unique(d$id)) {
  
  count <- c(0, 0) # initial values
  
  # iterate over blocks
  for (t in 1:4) {
    if (d[d$id==s & d$block==t,"transitions"][1] != 'stable') next
    s2 <- d[d$id==s & d$block==t,"s2"]
    rew <- d[d$id==s & d$block==t,"points"]
    
    count_retrun = calc_decr(s2,rew)
    
    count[1] = count[1] + count_retrun[[1]]
    
    count[2] = count[2] + count_retrun[[2]]
    
  }
  if (count[2] != 0){
    prob = append(prob,count[2]/count[1])
  }
}

X = array(0:(length(prob)-1))
source = rep('Hybrid-lwm', times=length(prob))

stay_prob_decr_lwm = data.frame(
  X = X,
  source = source,
  prob = prob
)

d = read.csv('./trialdata_Hybrid_rpe_zuo_sim.csv',sep=';')

prob = c()
# iterate over subjects
for (s in unique(d$id)) {
  
  count <- c(0, 0) # initial values
  
  # iterate over blocks
  for (t in 1:4) {
    if (d[d$id==s & d$block==t,"transitions"][1] != 'stable') next
    s2 <- d[d$id==s & d$block==t,"s2"]
    rew <- d[d$id==s & d$block==t,"points"]
    
    count_retrun = calc_decr(s2,rew)
    
    count[1] = count[1] + count_retrun[[1]]
    
    count[2] = count[2] + count_retrun[[2]]
    
  }
  if (count[2] != 0){
    prob = append(prob,count[2]/count[1])
  }
}

X = array(0:(length(prob)-1))
source = rep('Hybrid-rpe', times=length(prob))

stay_prob_decr_rpe = data.frame(
  X = X,
  source = source,
  prob = prob
)

stay_prob_decr=rbind(stay_prob_decr_actual_data,stay_prob_decr_wm,stay_prob_decr_lwm,stay_prob_decr_rpe)

library(stats)
wilcox.test(stay_prob_decr[stay_prob_decr$source == "actual data",]$prob,stay_prob_decr[stay_prob_decr$source == "Hybrid-wm",]$prob, 
            var.equal = TRUE)
wilcox.test(stay_prob_decr[stay_prob_decr$source == "actual data",]$prob,stay_prob_decr[stay_prob_decr$source == "Hybrid-rpe",]$prob, 
            var.equal = TRUE)
wilcox.test(stay_prob_decr[stay_prob_decr$source == "actual data",]$prob,stay_prob_decr[stay_prob_decr$source == "Hybrid-lwm",]$prob, 
            var.equal = TRUE)

stay_prob_decr$source=ordered(stay_prob_decr$source, levels = c("actual data", "Hybrid-wm", "Hybrid-lwm", "Hybrid-rpe")) 
stay_prob_decr$cond='decreasing'
stay_prob=rbind(stay_prob_incr,stay_prob_decr)

df=stay_prob
df$continent=df$source
df$year=df$cond
df$lifeExp=df$prob

df[df == "increasing"] = 'Reward increase'
df[df == "decreasing"] = 'Reward decrease'

df$year=ordered(df$year, levels = c('Reward increase','Reward decrease')) 

df_p_val1 <- df %>% group_by(continent) %>%
  wilcox_test(lifeExp  ~ year) %>%
  add_significance(p.col = "p") %>% 
  add_xy_position(x = "year", dodge = 0.8) 

p_zuo=df %>%
  ggplot(aes(year,lifeExp,color=year)) +
  stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),width=0.1,color='black')+
  geom_boxplot(position=position_dodge(width =0.2),width=0.4,color='black',outlier.colour = NA)+
  stat_pvalue_manual(df_p_val1,label = "p.signif",label.size=6,hide.ns = T,y.position = 1.1)+
  geom_jitter(shape=16, position=position_jitter(0.1))+
  scale_color_npg()+ 
  labs(x="",y = "Stay probability")+
  facet_wrap(.~continent,nrow=1)+
  theme_prism(base_line_size =0.5)+
  labs(x="",y = "Stay probability")+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),units=,"cm"),
        axis.line = element_line(color = "black",size = 0.4),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(color="black",size=10),
        axis.text.x = element_text(margin = margin(t = -5),color="black",size=10),
        legend.position=c(0.85, 0.09),                        ##设置图例的位置
        legend.text=element_text(face = "bold",size=14),     ##设置图例文本的字体
        
        
        panel.spacing = unit(0,"lines")
  )+
  coord_cartesian()+theme(axis.title.x =element_text(size=16), axis.title.y=element_text(size=20),         axis.text.x =element_text(size=16), axis.text.y=element_text(size=16))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(panel.border = element_blank())+ theme(strip.text.x = element_text(size = 16))+   ## 删去外层边框
  ylim(0.1, 1.15)+ guides(colour = guide_legend(override.aes = list(size=3)))

d = read.csv('./trialdata.csv',sep=',')

prob = c()
# iterate over subjects
for (s in unique(d$id)) {
  
  count <- c(0, 0) # initial values
  
  # iterate over blocks
  for (t in 1:4) {
    if (d[d$id==s & d$block==t,"transitions"][1] != 'stable') next
    s2 <- d[d$id==s & d$block==t,"s2"]
    rew <- d[d$id==s & d$block==t,"points"]
    
    count_retrun = calc_incr(s2,rew)
    
    count[1] = count[1] + count_retrun[[1]]
    
    count[2] = count[2] + count_retrun[[2]]
    
  }
  if (count[2] != 0){
    prob = append(prob,count[2]/count[1])
  }
}

X = array(0:(length(prob)-1))
source = rep('actual data', times=length(prob))

stay_prob_incr_actual_data = data.frame(
  X = X,
  source = source,
  prob = prob
)

d = read.csv('./trialdata_Hybrid_wm_bolenz_sim.csv',sep=';')

prob = c()
# iterate over subjects
for (s in unique(d$id)) {
  
  count <- c(0, 0) # initial values
  
  # iterate over blocks
  for (t in 1:4) {
    if (d[d$id==s & d$block==t,"transitions"][1] != 'stable') next
    s2 <- d[d$id==s & d$block==t,"s2"]
    rew <- d[d$id==s & d$block==t,"points"]
    
    count_retrun = calc_incr(s2,rew)
    
    count[1] = count[1] + count_retrun[[1]]
    
    count[2] = count[2] + count_retrun[[2]]
    
  }
  if (count[2] != 0){
    prob = append(prob,count[2]/count[1])
  }
}

X = array(0:(length(prob)-1))
source = rep('Hybrid-wm', times=length(prob))

stay_prob_incr_wm = data.frame(
  X = X,
  source = source,
  prob = prob
)


d = read.csv('./trialdata_Hybrid_lwm_bolenz_sim.csv',sep=';')

prob = c()
# iterate over subjects
for (s in unique(d$id)) {
  
  count <- c(0, 0) # initial values
  
  # iterate over blocks
  for (t in 1:4) {
    if (d[d$id==s & d$block==t,"transitions"][1] != 'stable') next
    s2 <- d[d$id==s & d$block==t,"s2"]
    rew <- d[d$id==s & d$block==t,"points"]
    
    count_retrun = calc_incr(s2,rew)
    
    count[1] = count[1] + count_retrun[[1]]
    
    count[2] = count[2] + count_retrun[[2]]
    
  }
  if (count[2] != 0){
    prob = append(prob,count[2]/count[1])
  }
}

X = array(0:(length(prob)-1))
source = rep('Hybrid-lwm', times=length(prob))

stay_prob_incr_lwm = data.frame(
  X = X,
  source = source,
  prob = prob
)

d = read.csv('./trialdata_Hybrid_rpe_bolenz_sim.csv',sep=';')

prob = c()
# iterate over subjects
for (s in unique(d$id)) {
  
  count <- c(0, 0) # initial values
  
  # iterate over blocks
  for (t in 1:4) {
    if (d[d$id==s & d$block==t,"transitions"][1] != 'stable') next
    s2 <- d[d$id==s & d$block==t,"s2"]
    rew <- d[d$id==s & d$block==t,"points"]
    
    count_retrun = calc_incr(s2,rew)
    
    count[1] = count[1] + count_retrun[[1]]
    
    count[2] = count[2] + count_retrun[[2]]
    
  }
  if (count[2] != 0){
    prob = append(prob,count[2]/count[1])
  }
}

X = array(0:(length(prob)-1))
source = rep('Hybrid-rpe', times=length(prob))

stay_prob_incr_rpe = data.frame(
  X = X,
  source = source,
  prob = prob
)

stay_prob_incr=rbind(stay_prob_incr_actual_data,stay_prob_incr_wm,stay_prob_incr_lwm,stay_prob_incr_rpe)

library(stats)
wilcox.test(stay_prob_incr[stay_prob_incr$source == "actual data",]$prob,stay_prob_incr[stay_prob_incr$source == "Hybrid-wm",]$prob, 
            var.equal = TRUE)
wilcox.test(stay_prob_incr[stay_prob_incr$source == "actual data",]$prob,stay_prob_incr[stay_prob_incr$source == "Hybrid-rpe",]$prob, 
            var.equal = TRUE)
wilcox.test(stay_prob_incr[stay_prob_incr$source == "actual data",]$prob,stay_prob_incr[stay_prob_incr$source == "Hybrid-lwm",]$prob, 
            var.equal = TRUE)

stay_prob_incr$source=ordered(stay_prob_incr$source, levels = c("actual data", "Hybrid-wm", "Hybrid-lwm", "Hybrid-rpe")) 
stay_prob_incr$cond='increasing'


d = read.csv('./trialdata.csv',sep=',')

prob = c()
# iterate over subjects
for (s in unique(d$id)) {

  count <- c(0, 0) # initial values
  
  # iterate over blocks
  for (t in 1:4) {
    if (d[d$id==s & d$block==t,"transitions"][1] != 'stable') next
    s2 <- d[d$id==s & d$block==t,"s2"]
    rew <- d[d$id==s & d$block==t,"points"]
    
    count_retrun = calc_decr(s2,rew)
    
    count[1] = count[1] + count_retrun[[1]]
    
    count[2] = count[2] + count_retrun[[2]]

  }
  if (count[2] != 0){
    prob = append(prob,count[2]/count[1])
  }
}

X = array(0:(length(prob)-1))
source = rep('actual data', times=length(prob))

stay_prob_decr_actual_data = data.frame(
  X = X,
  source = source,
  prob = prob
)

d = read.csv('./trialdata_Hybrid_wm_bolenz_sim.csv',sep=';')

prob = c()
# iterate over subjects
for (s in unique(d$id)) {
  
  count <- c(0, 0) # initial values
  
  # iterate over blocks
  for (t in 1:4) {
    if (d[d$id==s & d$block==t,"transitions"][1] != 'stable') next
    s2 <- d[d$id==s & d$block==t,"s2"]
    rew <- d[d$id==s & d$block==t,"points"]
    
    count_retrun = calc_decr(s2,rew)
    
    count[1] = count[1] + count_retrun[[1]]
    
    count[2] = count[2] + count_retrun[[2]]
    
  }
  if (count[2] != 0){
    prob = append(prob,count[2]/count[1])
  }
}

X = array(0:(length(prob)-1))
source = rep('Hybrid-wm', times=length(prob))

stay_prob_decr_wm = data.frame(
  X = X,
  source = source,
  prob = prob
)


d = read.csv('./trialdata_Hybrid_lwm_bolenz_sim.csv',sep=';')

prob = c()
# iterate over subjects
for (s in unique(d$id)) {
  
  count <- c(0, 0) # initial values
  
  # iterate over blocks
  for (t in 1:4) {
    if (d[d$id==s & d$block==t,"transitions"][1] != 'stable') next
    s2 <- d[d$id==s & d$block==t,"s2"]
    rew <- d[d$id==s & d$block==t,"points"]
    
    count_retrun = calc_decr(s2,rew)
    
    count[1] = count[1] + count_retrun[[1]]
    
    count[2] = count[2] + count_retrun[[2]]
    
  }
  if (count[2] != 0){
    prob = append(prob,count[2]/count[1])
  }
}

X = array(0:(length(prob)-1))
source = rep('Hybrid-lwm', times=length(prob))

stay_prob_decr_lwm = data.frame(
  X = X,
  source = source,
  prob = prob
)

d = read.csv('./trialdata_Hybrid_rpe_bolenz_sim.csv',sep=';')

prob = c()
# iterate over subjects
for (s in unique(d$id)) {
  
  count <- c(0, 0) # initial values
  
  # iterate over blocks
  for (t in 1:4) {
    if (d[d$id==s & d$block==t,"transitions"][1] != 'stable') next
    s2 <- d[d$id==s & d$block==t,"s2"]
    rew <- d[d$id==s & d$block==t,"points"]
    
    count_retrun = calc_decr(s2,rew)
    
    count[1] = count[1] + count_retrun[[1]]
    
    count[2] = count[2] + count_retrun[[2]]
    
  }
  if (count[2] != 0){
    prob = append(prob,count[2]/count[1])
  }
}

X = array(0:(length(prob)-1))
source = rep('Hybrid-rpe', times=length(prob))

stay_prob_decr_rpe = data.frame(
  X = X,
  source = source,
  prob = prob
)

stay_prob_decr=rbind(stay_prob_decr_actual_data,stay_prob_decr_wm,stay_prob_decr_lwm,stay_prob_decr_rpe)

library(stats)
wilcox.test(stay_prob_decr[stay_prob_decr$source == "actual data",]$prob,stay_prob_decr[stay_prob_decr$source == "Hybrid-wm",]$prob, 
            var.equal = TRUE)
wilcox.test(stay_prob_decr[stay_prob_decr$source == "actual data",]$prob,stay_prob_decr[stay_prob_decr$source == "Hybrid-rpe",]$prob, 
            var.equal = TRUE)
wilcox.test(stay_prob_decr[stay_prob_decr$source == "actual data",]$prob,stay_prob_decr[stay_prob_decr$source == "Hybrid-lwm",]$prob, 
            var.equal = TRUE)

stay_prob_decr$source=ordered(stay_prob_decr$source, levels = c("actual data", "Hybrid-wm", "Hybrid-lwm", "Hybrid-rpe")) 
stay_prob_decr$cond='decreasing'

stay_prob=rbind(stay_prob_incr,stay_prob_decr)

df=stay_prob
df$continent=df$source
df$year=df$cond
df$lifeExp=df$prob

df[df == "increasing"] = 'Reward increase'
df[df == "decreasing"] = 'Reward decrease'

df$year=ordered(df$year, levels = c('Reward increase','Reward decrease')) 

df_p_val1 <- df %>% group_by(continent) %>%
  wilcox_test(lifeExp  ~ year) %>%
  add_significance(p.col = "p") %>% 
  add_xy_position(x = "year", dodge = 0.8) 

p_bolenz=df %>%
  ggplot(aes(year,lifeExp,color=year)) +
  
  stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),width=0.1,color='black')+
  geom_boxplot(position=position_dodge(width =0.2),width=0.4,color='black',outlier.colour = NA)+
  stat_pvalue_manual(df_p_val1,label = "p.signif",label.size=6,hide.ns = T,y.position = 1.1)+
  geom_jitter(shape=16, position=position_jitter(0.1))+
  scale_color_npg()+ 
  labs(x="",y = "Stay probability")+
  facet_wrap(.~continent,nrow=1)+
  theme_prism(base_line_size =0.5)+
  labs(x="",y = "Stay probability")+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),units=,"cm"),
        axis.line = element_line(color = "black",size = 0.4),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(color="black",size=10),
        axis.text.x = element_text(margin = margin(t = -5),color="black",size=10),
        legend.position=c(0.85, 0.09),                        ##设置图例的位置
        legend.text=element_text(face = "bold",size=14),     ##设置图例文本的字体
        panel.spacing = unit(0,"lines")
  )+
  coord_cartesian()+theme(axis.title.x =element_text(size=16), axis.title.y=element_text(size=20),         axis.text.x =element_text(size=16), axis.text.y=element_text(size=16))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  theme(panel.border = element_blank())+ theme(strip.text.x = element_text(size = 16))+   ## 删去外层边框
  ylim(0.1, 1.15)+ guides(colour = guide_legend(override.aes = list(size=3)))

p=ggarrange(p_zuo, p_bolenz, nrow = 2, common.legend=TRUE,legend='top')


p+annotate("text", x =0.53, y = 0.945, label = "Zuo et al.",fontface = "bold",size=5)+
  annotate("text", x =0.53, y = 0.48, label = "Bolenz 2019",fontface = "bold",size=5)

ggsave(
  filename = "./figure_4.tiff", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 12,             # 宽
  height = 8,            # 高
  units = "in",          # 单位
  dpi = 500              # 分辨率DPI
)