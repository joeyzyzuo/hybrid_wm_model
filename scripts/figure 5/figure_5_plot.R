# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

library(dplyr)
library(ggplot2)
library(ggsci)
library(ggprism)
library(ggpubr)
library(reshape2)
library(stats)
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
trialdata.sim <- read.csv("./trialdata_bolenz_rational_Hybrid_rpe_sim.csv", sep=";")

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
d.sim_Hybrid_rpe <- trialdata.sim %>%
  group_by(id, version, simrun) %>% # 1. for each simulated agents and each run
  summarize(points_bc_sim = mean(points_bc)) %>%
  group_by(id, version) %>% # 2. for each simulated agents across runs
  summarize(points_bc_sim = mean(points_bc_sim))

###########################################

trialdata.sim <- read.csv("./trialdata_bolenz_rational_Hybrid_lwm_sim.csv", sep=";")

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
d.sim_Hybrid_lwm <- trialdata.sim %>%
  group_by(id, version, simrun) %>% # 1. for each simulated agents and each run
  summarize(points_bc_sim = mean(points_bc)) %>%
  group_by(id, version) %>% # 2. for each simulated agents across runs
  summarize(points_bc_sim = mean(points_bc_sim))

###########################################

trialdata.sim <- read.csv("./trialdata_bolenz_rational_Hybrid_wm_sim.csv", sep=";")

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
d.sim_Hybrid_wm <- trialdata.sim %>%
  group_by(id, version, simrun) %>% # 1. for each simulated agents and each run
  summarize(points_bc_sim = mean(points_bc)) %>%
  group_by(id, version) %>% # 2. for each simulated agents across runs
  summarize(points_bc_sim = mean(points_bc_sim))

######################################################
# merge simulated and participant data
d <- merge(d.sim_Hybrid_rpe,d.sim_Hybrid_lwm,by = "id")
d <- merge(d,d.sim_Hybrid_wm,by = "id")

df=d
df$Species=df$version
df$Hybrid_rpe=df$points_bc_sim.x
df$Hybrid_wm=df$points_bc_sim
df$Hybrid_lwm=df$points_bc_sim.y

wilcox.test(df$Hybrid_wm,df$Hybrid_rpe, 
            var.equal = TRUE, alternative = "greater")
wilcox.test(df$Hybrid_lwm,df$Hybrid_rpe, 
            var.equal = TRUE, alternative = "greater")
wilcox.test(df$Hybrid_wm,df$Hybrid_lwm, 
            var.equal = TRUE, alternative = "greater")

df<-df [,c(8:11)] #取数据集第1到3列
stringsAsFactors = F
df <- melt(df, id="Species", variable.name="Attribute", value.name = "Size")

mean <- aggregate(df$Size, by=list(df$Species, df$Attribute), FUN=mean)
sd <- aggregate(df$Size, by=list(df$Species, df$Attribute), FUN=sd)
len <- aggregate(df$Size, by=list(df$Species, df$Attribute), FUN=length)
df_res <- data.frame(mean, sd=sd$x, len=len$x)
colnames(df_res) = c("Species", "Attribute", "Mean", "Sd", "Count")
str(df_res)
df_res$Se <- df_res$Sd/sqrt(df_res$Count) ### 计算标准差
df$Attribute <- as.character(df$Attribute)

df[df == "Hybrid_rpe"] = 'Hybrid-rpe'
df[df == "Hybrid_wm"] = 'Hybrid-wm'
df[df == "Hybrid_lwm"] = 'Hybrid-lwm'

df$Attribute <- factor(df$Attribute,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

df$continent=''
df$year=df$Attribute
df$lifeExp=df$Size

df_p_val1 <- df %>% group_by(continent) %>%
  wilcox_test(lifeExp  ~ year) %>%
  adjust_pvalue(p.col = "p", method = "bonferroni") %>%
  add_significance(p.col = "p.adj") %>% 
  add_xy_position(x = "year", dodge = 0.8) 

df %>%
  ggplot(aes(year,lifeExp,color=year)) +
  stat_boxplot(geom="errorbar",position=position_dodge(width=0.2),width=0.1,color='black')+
  geom_boxplot(position=position_dodge(width =0.2),width=0.4,color='black',outlier.colour = NA)+
  geom_jitter(shape=16,size=0.6, position=position_jitter(0.1))+
  geom_signif(comparisons = list(c("Hybrid-wm","Hybrid-rpe")),map_signif_level = TRUE,test = wilcox.test, test.args = c("greater"),y_position = c(1,0.1),tip_length = c(0.02,0.02),color='black')+
  geom_signif(comparisons = list(c("Hybrid-wm","Hybrid-lwm")),map_signif_level = TRUE,test = wilcox.test, test.args = c("greater"),y_position = c(0.9,0.1),tip_length = c(0.02,0.02),color='black')+
  geom_signif(comparisons = list(c("Hybrid-lwm","Hybrid-rpe")),map_signif_level = TRUE,test = wilcox.test, test.args = c("greater"),y_position = c(0.8,0.1),tip_length = c(0.02,0.02),color='black')+
  facet_wrap(.~continent,nrow=1)+
  scale_color_npg()+ 
  labs(x=NULL,y=NULL)+
  labs(x="",y = "baseline-corrected reward")+
  theme_prism(base_line_size =0.5)+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),units=,"cm"),
        axis.line = element_line(color = "black",size = 0.4),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(color="black",size=10),
        axis.text.x = element_text(margin = margin(t = -5),color="black",size=10),
        legend.text=element_text(face = "bold",size=12),     ##设置图例文本的字体
        panel.spacing = unit(0,"lines"))+
  coord_cartesian()+theme(axis.title.x =element_text(size=12), axis.title.y=element_text(size=18),         axis.text.x =element_text(size=16), axis.text.y=element_text(size=12))+
  theme(panel.border = element_blank())+  ## 删去外层边框
  theme(axis.ticks.x= element_blank(),legend.position = 'none')+
  scale_color_manual(values = c("black","black","black")) #自定义颜色

ggsave(
  filename = "./figure_5.tiff", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 6,             # 宽
  height = 6,            # 高
  units = "in",          # 单位
  dpi = 500              # 分辨率DPI
)