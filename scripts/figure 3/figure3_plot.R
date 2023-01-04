# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

library(gg.gap)
library(ggplot2)
library(patchwork)
library(ggpubr)
library(reshape2)
library(brms)
library(bayesplot)
library(psych)
library(BayesFactor)
library(dplyr)
# Load the this.path package.
library(this.path)

# Get the directory from the path of the current file.
cur_dir2 = dirname(this.path())

# Set the working directory.
setwd(cur_dir2)

# Check that the working directory has been set as desired.
getwd()
######################

data1 <- read.csv("../../code/model fitting/Kool, Cushman, and Gershman 2016/params_Hybrid_rpe.csv", sep=";")
data2 <- read.csv("../../code/model fitting/Kool, Cushman, and Gershman 2016/params_Hybrid_wm.csv", sep=";")
data3 <- read.csv("../../code/model fitting/Kool, Cushman, and Gershman 2016/params_Hybrid_lwm.csv", sep=";")

d = as.data.frame(matrix(nrow=184,ncol=0)) #创建一个3列的空对象

d$id=data1$id
d$points_bc_sim.x=data1$reward_learning_rate
d$points_bc_sim=data2$reward_learning_rate
d$points_bc_sim.y=data3$reward_learning_rate

library(reshape2)
df=d
df$Species=df$id
df$MBMF=df$points_bc_sim.x
df$RLWM=df$points_bc_sim
df$RLWMwd=df$points_bc_sim.y

wilcox.test(df$MBMF,df$RLWM, 
            var.equal = TRUE, alternative = "greater")
wilcox.test(df$MBMF,df$RLWMwd, 
            var.equal = TRUE, alternative = "greater")

df<-df [,c(5:8)] #取数据集第1到3列
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

df[df == "MBMF"] = 'Hybrid-rpe'
df[df == "RLWM"] = 'Hybrid-wm'
df[df == "RLWMwd"] = 'Hybrid-lwm'

df$Attribute <- factor(df$Attribute,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

p_D=ggplot(df, aes(x=Attribute, y=Size, fill = Attribute)) + 
  geom_boxplot(show.legend = FALSE,outlier.colour = NA,
               width=0.4)+ylim(0.2, 1.25)+
  labs(x="",y = "Learning rate")+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=c("grey","grey","grey"))+
  
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  geom_signif(comparisons = list(c("Hybrid-rpe","Hybrid-wm")),map_signif_level = TRUE,test = wilcox.test, test.args = c("greater"),y_position = c(1.15,0.1),tip_length = c(0.02,0.02))+
  
  geom_signif(comparisons = list(c("Hybrid-rpe","Hybrid-lwm")),map_signif_level = TRUE,test = wilcox.test, test.args = c("greater"),y_position = c(1.05,0.1),tip_length = c(0.02,0.02))+
  
  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小
p_kool2016=p_D
text <- "Kool 2016"

# Create a text grob
tgrob <- text_grob(text,size = 24)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,2.5, "cm"))

p_kool2016=ggarrange(plot_0,p_kool2016,ncol = 1,nrow = 2,heights = c(1,5))

######################
data1 <- read.csv("../../code/model fitting/Kool, Gershman, and Cushman 2017/params_kool2017_Hybrid_rpe.csv", sep=";")
data2 <- read.csv("../../code/model fitting/Kool, Gershman, and Cushman 2017/params_kool2017_Hybrid_wm.csv", sep=";")
data3 <- read.csv("../../code/model fitting/Kool, Gershman, and Cushman 2017/params_kool2017_Hybrid_lwm.csv", sep=";")

d = as.data.frame(matrix(nrow=98,ncol=0)) #创建一个3列的空对象

d$id=data1$id
d$points_bc_sim.x=data1$reward_learning_rate
d$points_bc_sim=data2$reward_learning_rate
d$points_bc_sim.y=data3$reward_learning_rate

library(reshape2)
df=d
df$Species=df$id

df$MBMF=df$points_bc_sim.x
df$RLWM=df$points_bc_sim
df$RLWMwd=df$points_bc_sim.y

wilcox.test(df$MBMF,df$RLWM, 
            var.equal = TRUE, alternative = "greater")
wilcox.test(df$MBMF,df$RLWMwd, 
            var.equal = TRUE, alternative = "greater")

df<-df [,c(5:8)] #取数据集第1到3列
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

df[df == "MBMF"] = 'Hybrid-rpe'
df[df == "RLWM"] = 'Hybrid-wm'
df[df == "RLWMwd"] = 'Hybrid-lwm'

df$Attribute <- factor(df$Attribute,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

p_D=ggplot(df, aes(x=Attribute, y=Size, fill = Attribute)) + 
  geom_boxplot(show.legend = FALSE,outlier.colour = NA,
               width=0.4)+ylim(0.2, 1.25)+
  labs(x="",y = "Learning rate")+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=c("grey","grey","grey"))+
  
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  geom_signif(comparisons = list(c("Hybrid-rpe","Hybrid-wm")),map_signif_level = TRUE,test = wilcox.test, test.args = c("greater"),y_position = c(1.15,0.1),tip_length = c(0.02,0.02))+
  
  geom_signif(comparisons = list(c("Hybrid-rpe","Hybrid-lwm")),map_signif_level = TRUE,test = wilcox.test, test.args = c("greater"),y_position = c(1.05,0.1),tip_length = c(0.02,0.02))+
  
  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小
p_kool2017=p_D
text <- "Kool 2017"

# Create a text grob
tgrob <- text_grob(text,size = 24)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,2.5, "cm"))

p_kool2017=ggarrange(plot_0,p_kool2017,ncol = 1,nrow = 2,heights = c(1,5))
######################
data1 <- read.csv("../../code/model fitting/Kool, Gershman, and Cushman 2018/exp1/param_Hybrid_rpe.csv", sep=";")
data2 <- read.csv("../../code/model fitting/Kool, Gershman, and Cushman 2018/exp1/param_Hybrid_wm.csv", sep=";")
data3 <- read.csv("../../code/model fitting/Kool, Gershman, and Cushman 2018/exp1/param_Hybrid_lwm.csv", sep=";")

d = as.data.frame(matrix(nrow=98,ncol=0)) #创建一个3列的空对象

d$id=data1$id
d$points_bc_sim.x=data1$learning_rate
d$points_bc_sim=data2$learning_rate
d$points_bc_sim.y=data3$learning_rate

library(reshape2)
df=d
df$Species=df$id

df$MBMF=df$points_bc_sim.x
df$RLWM=df$points_bc_sim
df$RLWMwd=df$points_bc_sim.y

wilcox.test(df$MBMF,df$RLWM, 
            var.equal = TRUE, alternative = "greater")
wilcox.test(df$MBMF,df$RLWMwd, 
            var.equal = TRUE, alternative = "greater")

df<-df [,c(5:8)] #取数据集第1到3列
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

df[df == "MBMF"] = 'Hybrid-rpe'
df[df == "RLWM"] = 'Hybrid-wm'
df[df == "RLWMwd"] = 'Hybrid-lwm'

df$Attribute <- factor(df$Attribute,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

p_D=ggplot(df, aes(x=Attribute, y=Size, fill = Attribute)) + 
  geom_boxplot(show.legend = FALSE,outlier.colour = NA,
               width=0.4)+ylim(0.2, 1.25)+
  labs(x="",y = "Learning rate")+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=c("grey","grey","grey"))+
  
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  geom_signif(comparisons = list(c("Hybrid-rpe","Hybrid-wm")),map_signif_level = TRUE,test = wilcox.test, test.args = c("greater"),y_position = c(1.15,0.1),tip_length = c(0.02,0.02))+
  
  geom_signif(comparisons = list(c("Hybrid-rpe","Hybrid-lwm")),map_signif_level = TRUE,test = wilcox.test, test.args = c("greater"),y_position = c(1.05,0.1),tip_length = c(0.02,0.02))+
  
  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小
p_kool2018_exp1=p_D
text <- "Kool 2018, Exp1"

# Create a text grob
tgrob <- text_grob(text,size = 24)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,2.5, "cm"))

p_kool2018_exp1=ggarrange(plot_0,p_kool2018_exp1,ncol = 1,nrow = 2,heights = c(1,5))
######################
data1 <- read.csv("../../code/model fitting/Kool, Gershman, and Cushman 2018/exp2/param_Hybrid_rpe.csv", sep=";")
data2 <- read.csv("../../code/model fitting/Kool, Gershman, and Cushman 2018/exp2/param_Hybrid_wm.csv", sep=";")
data3 <- read.csv("../../code/model fitting/Kool, Gershman, and Cushman 2018/exp2/param_Hybrid_lwm.csv", sep=";")

d = as.data.frame(matrix(nrow=102,ncol=0)) #创建一个3列的空对象

d$id=data1$id
d$points_bc_sim.x=data1$learning_rate
d$points_bc_sim=data2$learning_rate
d$points_bc_sim.y=data3$learning_rate

library(reshape2)
df=d
df$Species=df$id

df$MBMF=df$points_bc_sim.x
df$RLWM=df$points_bc_sim
df$RLWMwd=df$points_bc_sim.y

wilcox.test(df$MBMF,df$RLWM, 
            var.equal = TRUE, alternative = "greater")
wilcox.test(df$MBMF,df$RLWMwd, 
            var.equal = TRUE, alternative = "greater")

df<-df [,c(5:8)] #取数据集第1到3列
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

df[df == "MBMF"] = 'Hybrid-rpe'
df[df == "RLWM"] = 'Hybrid-wm'
df[df == "RLWMwd"] = 'Hybrid-lwm'

df$Attribute <- factor(df$Attribute,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

p_D=ggplot(df, aes(x=Attribute, y=Size, fill = Attribute)) + 
  geom_boxplot(show.legend = FALSE,outlier.colour = NA,
               width=0.4)+ylim(0.2, 1.25)+
  labs(x="",y = "Learning rate")+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=c("grey","grey","grey"))+
  
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  geom_signif(comparisons = list(c("Hybrid-rpe","Hybrid-wm")),map_signif_level = TRUE,test = wilcox.test, test.args = c("greater"),y_position = c(1.15,0.1),tip_length = c(0.02,0.02))+
  
  geom_signif(comparisons = list(c("Hybrid-rpe","Hybrid-lwm")),map_signif_level = TRUE,test = wilcox.test, test.args = c("greater"),y_position = c(1.05,0.1),tip_length = c(0.02,0.02))+
  
  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小
p_kool2018_exp2=p_D
text <- "Kool 2018, Exp2"

# Create a text grob
tgrob <- text_grob(text,size = 24)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,2.5, "cm"))

p_kool2018_exp2=ggarrange(plot_0,p_kool2018_exp2,ncol = 1,nrow = 2,heights = c(1,5))
######################
data1 <- read.csv("../../code/model fitting/Bolenz, Kool, Reiter, and Eppinger 2019/params_bolenz_Hybrid_rpe.csv", sep=";")
data2 <- read.csv("../../code/model fitting/Bolenz, Kool, Reiter, and Eppinger 2019/params_bolenz_Hybrid_wm.csv", sep=";")
data3 <- read.csv("../../code/model fitting/Bolenz, Kool, Reiter, and Eppinger 2019/params_bolenz_Hybrid_lwm.csv", sep=";")

d = as.data.frame(matrix(nrow=124,ncol=0)) #创建一个3列的空对象

d$id=data1$id
d$points_bc_sim.x=data1$reward_learning_rate
d$points_bc_sim=data2$reward_learning_rate
d$points_bc_sim.y=data3$reward_learning_rate

df=d
df$Species=df$id
df$MBMF=df$points_bc_sim.x
df$RLWM=df$points_bc_sim
df$RLWMwd=df$points_bc_sim.y

wilcox.test(df$MBMF,df$RLWM, 
            var.equal = TRUE, alternative = "greater")
wilcox.test(df$MBMF,df$RLWMwd, 
            var.equal = TRUE, alternative = "greater")

df<-df [,c(5:8)] #取数据集第1到3列
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

df[df == "MBMF"] = 'Hybrid-rpe'
df[df == "RLWM"] = 'Hybrid-wm'
df[df == "RLWMwd"] = 'Hybrid-lwm'

df$Attribute <- factor(df$Attribute,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

p_D=ggplot(df, aes(x=Attribute, y=Size, fill = Attribute)) + 
  geom_boxplot(show.legend = FALSE,outlier.colour = NA,
               width=0.4)+ylim(0.2, 1.25)+
  labs(x="",y = "Learning rate")+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=c("grey","grey","grey"))+
  
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  geom_signif(comparisons = list(c("Hybrid-rpe","Hybrid-wm")),map_signif_level = TRUE,test = wilcox.test, test.args = c("greater"),y_position = c(1.15,0.1),tip_length = c(0.02,0.02))+
  
  geom_signif(comparisons = list(c("Hybrid-rpe","Hybrid-lwm")),map_signif_level = TRUE,test = wilcox.test, test.args = c("greater"),y_position = c(1.05,0.1),tip_length = c(0.02,0.02))+
  
  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小
p_bolenz2019=p_D
text <- "Bolenz 2019"

# Create a text grob
tgrob <- text_grob(text,size = 24)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,2.5, "cm"))

p_bolenz2019=ggarrange(plot_0,p_bolenz2019,ncol = 1,nrow = 2,heights = c(1,5))
######################
# import simulated data
data1 <- read.csv("../../code/model fitting/da Silva and Hare 2020/magic/params_Hybrid_rpe.csv", sep=";")
data2 <- read.csv("../../code/model fitting/da Silva and Hare 2020/magic/params_Hybrid_wm.csv", sep=";")
data3 <- read.csv("../../code/model fitting/da Silva and Hare 2020/magic/params_Hybrid_lwm.csv", sep=";")

d = as.data.frame(matrix(nrow=24,ncol=0)) #创建一个3列的空对象

d$id=data1$id
d$points_bc_sim.x=data1$reward_learning_rate
d$points_bc_sim=data2$reward_learning_rate
d$points_bc_sim.y=data3$reward_learning_rate

df=d
df$Species=df$id
df$MBMF=df$points_bc_sim.x
df$RLWM=df$points_bc_sim
df$RLWMwd=df$points_bc_sim.y

wilcox.test(df$MBMF,df$RLWM, 
            var.equal = TRUE, alternative = "greater")
wilcox.test(df$MBMF,df$RLWMwd, 
            var.equal = TRUE, alternative = "greater")

df<-df [,c(5:8)] #取数据集第1到3列
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

df[df == "MBMF"] = 'Hybrid-rpe'
df[df == "RLWM"] = 'Hybrid-wm'
df[df == "RLWMwd"] = 'Hybrid-lwm'

df$Attribute <- factor(df$Attribute,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

p_D=ggplot(df, aes(x=Attribute, y=Size, fill = Attribute)) + 
  geom_boxplot(show.legend = FALSE,outlier.colour = NA,
               width=0.4)+ylim(0.2, 1.25)+
  labs(x="",y = "Learning rate")+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=c("grey","grey","grey"))+
  
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  geom_signif(comparisons = list(c("Hybrid-rpe","Hybrid-wm")),map_signif_level = TRUE,test = wilcox.test, test.args = c("greater"),y_position = c(1.15,0.1),tip_length = c(0.02,0.02))+
  
  geom_signif(comparisons = list(c("Hybrid-rpe","Hybrid-lwm")),map_signif_level = TRUE,test = wilcox.test, test.args = c("greater"),y_position = c(1.05,0.1),tip_length = c(0.02,0.02))+
  
  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小
p_2020_nhb_magic=p_D
text <- "Hare 2020, magic carpet task"

# Create a text grob
tgrob <- text_grob(text,size = 24)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,2.5, "cm"))

p_2020_nhb_magic=ggarrange(plot_0,p_2020_nhb_magic,ncol = 1,nrow = 2,heights = c(1,5))
######################
data1 <- read.csv("../../code/model fitting/da Silva and Hare 2020/spaceship/params_Hybrid_rpe.csv", sep=";")
data2 <- read.csv("../../code/model fitting/da Silva and Hare 2020/spaceship/params_Hybrid_wm.csv", sep=";")
data3 <- read.csv("../../code/model fitting/da Silva and Hare 2020/spaceship/params_Hybrid_lwm.csv", sep=";")

d = as.data.frame(matrix(nrow=21,ncol=0)) #创建一个3列的空对象

d$id=data1$id
d$points_bc_sim.x=data1$reward_learning_rate
d$points_bc_sim=data2$reward_learning_rate
d$points_bc_sim.y=data3$reward_learning_rate

library(reshape2)
df=d
df$Species=df$id

df$MBMF=df$points_bc_sim.x
df$RLWM=df$points_bc_sim
df$RLWMwd=df$points_bc_sim.y

wilcox.test(df$MBMF,df$RLWM, 
            var.equal = TRUE, alternative = "greater")
wilcox.test(df$MBMF,df$RLWMwd, 
            var.equal = TRUE, alternative = "greater")

df<-df [,c(5:8)] #取数据集第1到3列
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

df[df == "MBMF"] = 'Hybrid-rpe'
df[df == "RLWM"] = 'Hybrid-wm'
df[df == "RLWMwd"] = 'Hybrid-lwm'

df$Attribute <- factor(df$Attribute,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

p_D=ggplot(df, aes(x=Attribute, y=Size, fill = Attribute)) + 
  geom_boxplot(show.legend = FALSE,outlier.colour = NA,
               width=0.4)+ylim(0.2, 1.25)+
  labs(x="",y = "Learning rate")+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=c("grey","grey","grey"))+
  
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  geom_signif(comparisons = list(c("Hybrid-rpe","Hybrid-wm")),map_signif_level = TRUE,test = wilcox.test, test.args = c("greater"),y_position = c(1.15,0.1),tip_length = c(0.02,0.02))+
  
  geom_signif(comparisons = list(c("Hybrid-rpe","Hybrid-lwm")),map_signif_level = TRUE,test = wilcox.test, test.args = c("greater"),y_position = c(1.05,0.1),tip_length = c(0.02,0.02))+
  
  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小
p_2020_nhb_spaceship=p_D
text <- "Hare 2020, spaceship task"

# Create a text grob
tgrob <- text_grob(text,size = 24)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,2.5, "cm"))

p_2020_nhb_spaceship=ggarrange(plot_0,p_2020_nhb_spaceship,ncol = 1,nrow = 2,heights = c(1,5))
######################

# import simulated data
data1 <- read.csv("../../code/model fitting/Bolenz and Eppinger 2022/params_bolenz_Hybrid_rpe.csv", sep=";")
data2 <- read.csv("../../code/model fitting/Bolenz and Eppinger 2022/params_bolenz_Hybrid_wm.csv", sep=";")
data3 <- read.csv("../../code/model fitting/Bolenz and Eppinger 2022/params_bolenz_Hybrid_lwm.csv", sep=";")

d = as.data.frame(matrix(nrow=201,ncol=0)) #创建一个3列的空对象

d$id=data1$id
d$points_bc_sim.x=data1$reward_learning_rate
d$points_bc_sim=data2$reward_learning_rate
d$points_bc_sim.y=data3$reward_learning_rate

df=d
df$Species=df$id

df$MBMF=df$points_bc_sim.x
df$RLWM=df$points_bc_sim
df$RLWMwd=df$points_bc_sim.y

wilcox.test(df$MBMF,df$RLWM, 
            var.equal = TRUE, alternative = "greater")
wilcox.test(df$MBMF,df$RLWMwd, 
            var.equal = TRUE, alternative = "greater")

df<-df [,c(5:8)] #取数据集第1到3列
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

df[df == "MBMF"] = 'Hybrid-rpe'
df[df == "RLWM"] = 'Hybrid-wm'
df[df == "RLWMwd"] = 'Hybrid-lwm'

df$Attribute <- factor(df$Attribute,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

p_D=ggplot(df, aes(x=Attribute, y=Size, fill = Attribute)) + 
  geom_boxplot(show.legend = FALSE,outlier.colour = NA,
               width=0.4)+ylim(0.2, 1.25)+
  labs(x="",y = "Learning rate")+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=c("grey","grey","grey"))+
  
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  geom_signif(comparisons = list(c("Hybrid-rpe","Hybrid-wm")),map_signif_level = TRUE,test = wilcox.test, test.args = c("greater"),y_position = c(1.15,0.1),tip_length = c(0.02,0.02))+
  
  geom_signif(comparisons = list(c("Hybrid-rpe","Hybrid-lwm")),map_signif_level = TRUE,test = wilcox.test, test.args = c("greater"),y_position = c(1.05,0.1),tip_length = c(0.02,0.02))+
  
  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小
p_bolenz2021=p_D
text <- "Bolenz 2021"

# Create a text grob
tgrob <- text_grob(text,size = 24)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,2.5, "cm"))

p_bolenz2021=ggarrange(plot_0,p_bolenz2021,ncol = 1,nrow = 2,heights = c(1,5))
######################
# import simulated data
data1 <- read.csv("../../code/model fitting/Zuo 2022/params_Hybrid_rpe.csv", sep=";")
data2 <- read.csv("../../code/model fitting/Zuo 2022/params_Hybrid_wm.csv", sep=";")
data3 <- read.csv("../../code/model fitting/Zuo 2022/params_Hybrid_lwm.csv", sep=";")


d = as.data.frame(matrix(nrow=60,ncol=0)) #创建一个3列的空对象

d$id=data1$id
d$points_bc_sim.x=data1$reward_learning_rate
d$points_bc_sim=data2$reward_learning_rate
d$points_bc_sim.y=data3$reward_learning_rate

df=d
df$Species=df$id

df$MBMF=df$points_bc_sim.x
df$RLWM=df$points_bc_sim
df$RLWMwd=df$points_bc_sim.y

wilcox.test(df$MBMF,df$RLWM, 
            var.equal = TRUE, alternative = "greater")
wilcox.test(df$MBMF,df$RLWMwd, 
            var.equal = TRUE, alternative = "greater")

df<-df [,c(5:8)] #取数据集第1到3列
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

df[df == "MBMF"] = 'Hybrid-rpe'
df[df == "RLWM"] = 'Hybrid-wm'
df[df == "RLWMwd"] = 'Hybrid-lwm'

df$Attribute <- factor(df$Attribute,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

p_D=ggplot(df, aes(x=Attribute, y=Size, fill = Attribute)) + 
  geom_boxplot(show.legend = FALSE,outlier.colour = NA,
               width=0.4)+ylim(0.2, 1.25)+
  labs(x="",y = "Learning rate")+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=c("grey","grey","grey"))+
  
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  geom_signif(comparisons = list(c("Hybrid-rpe","Hybrid-wm")),map_signif_level = TRUE,test = wilcox.test, test.args = c("greater"),y_position = c(1.15,0.1),tip_length = c(0.02,0.02))+
  
  geom_signif(comparisons = list(c("Hybrid-rpe","Hybrid-lwm")),map_signif_level = TRUE,test = wilcox.test, test.args = c("greater"),y_position = c(1.05,0.1),tip_length = c(0.02,0.02))+
  
  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小
p_zuo2022=p_D
text <- "Zuo et al."

# Create a text grob
tgrob <- text_grob(text,size = 24)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,2.5, "cm"))

p_zuo2022=ggarrange(plot_0,p_zuo2022,ncol = 1,nrow = 2,heights = c(1,5))
p_zuo2022

##################################
p9 <- ggpubr::ggarrange(p_kool2016, p_kool2017,  p_kool2018_exp1,p_kool2018_exp2, p_bolenz2019,p_2020_nhb_magic,p_2020_nhb_spaceship,p_bolenz2021,p_zuo2022,nrow = 3, ncol = 3, labels = c('a', 'b', 'c', 'd','e', 'f','g', 'h', 'i'),font.label = list(color = 'Black',size = 20))
p9

ggsave(
  filename = "./figure_3.tiff", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 18,             # 宽
  height = 12,            # 高
  units = "in",          # 单位
  dpi = 500              # 分辨率DPI
)