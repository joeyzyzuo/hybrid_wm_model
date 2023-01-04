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
library(this.path)

# Get the directory from the path of the current file.
cur_dir2 = dirname(this.path())

# Set the working directory.
setwd(cur_dir2)

# Check that the working directory has been set as desired.
getwd()
# clear workspace
rm(list=ls())

set.seed(1234)

# add column to subject data
d.subject <- read.csv('./group_aic2016.csv', sep=',')

data = d.subject

data$model <- factor(data$model,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

subids <- unique(data$id)
for(i in c(1:length(subids))){
  id_s <- subids[i]
  print(id_s)
  id_RLWM_aic=data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic
  print(data[which(data$id==id_s & data$model=="Hybrid-wm"),])
  data[which(data$id==id_s & data$model=="Hybrid-rpe"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-rpe"),]$aic-id_RLWM_aic
  data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic-id_RLWM_aic
  data[which(data$id==id_s & data$model=="Hybrid-lwm"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-lwm"),]$aic-id_RLWM_aic
  
}
count(data, model)

p2=ggplot(data, aes(x=model, y=aic)) +
  geom_bar(data=data%>%group_by(model)%>%summarise(aic=mean(aic)), stat="identity", aes(fill=model), show.legend=F,width = 0.6) +
  geom_errorbar(data=data%>%group_by(model)%>%summarise(se=sd(aic)/sqrt(n()), aic=mean(aic)), aes(x=model, ymin=aic-se, ymax=aic+se), width=.2) +
  scale_fill_manual(values=c("grey","grey","grey")) +
  labs(x="",y = "ΔAIC")+
  theme_classic() + theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))

p_kool2016_aic=p2+  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+ ylim(0,3)+
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=24),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小

data <- data.frame(x = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'),y=c(0.3463   ,0.3250  ,   0.3286))
data$x <- factor(data$x,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

#画图
p1 = ggplot(data, aes(x = x, y = y, fill = x)) +
  geom_bar(stat = 'identity', position = position_dodge(),show.legend = FALSE,width=0.5) +
  theme_classic() +
  scale_fill_manual(values=c("grey","grey","grey"))+theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+   ylim(0, 1)+
  labs(x="Model",y = "PEP")+theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))

p_kool2016_BMS=p1+
  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=24),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小

text <- "Kool 2016"

# Create a text grob
tgrob <- text_grob(text,size = 24)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,2, "cm"))

p_kool2016=ggarrange(plot_0,p_kool2016_aic,nrow = 2,heights = c(1,4))

########################################################################################

# add column to subject data
d.subject <- read.csv('./group_aic2017.csv', sep=',')

data = d.subject

data$model <- factor(data$model,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

subids <- unique(data$id)
for(i in c(1:length(subids))){
  id_s <- subids[i]
  print(id_s)
  id_RLWM_aic=data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic
  print(data[which(data$id==id_s & data$model=="Hybrid-wm"),])
  data[which(data$id==id_s & data$model=="Hybrid-rpe"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-rpe"),]$aic-id_RLWM_aic
  data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic-id_RLWM_aic
  data[which(data$id==id_s & data$model=="Hybrid-lwm"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-lwm"),]$aic-id_RLWM_aic
  
}
count(data, model)

p2=ggplot(data, aes(x=model, y=aic)) +
  geom_bar(data=data%>%group_by(model)%>%summarise(aic=mean(aic)), stat="identity", aes(fill=model), show.legend=F,width = 0.6) +
  geom_errorbar(data=data%>%group_by(model)%>%summarise(se=sd(aic)/sqrt(n()), aic=mean(aic)), aes(x=model, ymin=aic-se, ymax=aic+se), width=.2) +
  scale_fill_manual(values=c("grey","grey","grey")) +
  labs(x="",y = "ΔAIC")+
  theme_classic() + theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))

p_kool2017_aic=p2+  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=24),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小

data <- data.frame(x = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'),y=c(0.9950  ,0.0023 ,     0.0026))
data$x <- factor(data$x,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

#画图
p1 = ggplot(data, aes(x = x, y = y, fill = x)) +
  geom_bar(stat = 'identity', position = position_dodge(),show.legend = FALSE,width=0.5) +
  theme_classic() +
  scale_fill_manual(values=c("grey","grey","grey"))+theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+   ylim(0, 1)+
  labs(x="Model",y = "PEP")+theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))

p_kool2017_BMS=p1+
  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=24),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小

text <- "Kool 2017"

# Create a text grob
tgrob <- text_grob(text,size = 24)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,2, "cm"))

p_kool2017=ggarrange(plot_0,p_kool2017_aic,nrow = 2,heights = c(1,4))

####################################################################################################
# add column to subject data
d.subject <- read.csv('./group_aic2018_exp1.csv', sep=',')

data = d.subject

data$model <- factor(data$model,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

subids <- unique(data$id)
for(i in c(1:length(subids))){
  id_s <- subids[i]
  print(id_s)
  id_RLWM_aic=data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic
  print(data[which(data$id==id_s & data$model=="Hybrid-wm"),])
  data[which(data$id==id_s & data$model=="Hybrid-rpe"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-rpe"),]$aic-id_RLWM_aic
  data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic-id_RLWM_aic
  data[which(data$id==id_s & data$model=="Hybrid-lwm"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-lwm"),]$aic-id_RLWM_aic
  
}
count(data, model)

p2=ggplot(data, aes(x=model, y=aic)) +
  geom_bar(data=data%>%group_by(model)%>%summarise(aic=mean(aic)), stat="identity", aes(fill=model), show.legend=F,width = 0.6) +
  geom_errorbar(data=data%>%group_by(model)%>%summarise(se=sd(aic)/sqrt(n()), aic=mean(aic)), aes(x=model, ymin=aic-se, ymax=aic+se), width=.2) +
  scale_fill_manual(values=c("grey","grey","grey")) +
  labs(x="",y = "ΔAIC")+
  theme_classic() + theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))

p_kool2018_exp1_aic=p2+  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=24),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小

data <- data.frame(x = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'),y=c(0.9125 ,0.0500 ,      0.0375))
data$x <- factor(data$x,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

#画图
p1 = ggplot(data, aes(x = x, y = y, fill = x)) +
  geom_bar(stat = 'identity', position = position_dodge(),show.legend = FALSE,width=0.5) +
  theme_classic() +
  scale_fill_manual(values=c("grey","grey","grey"))+theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+   ylim(0, 1)+
  labs(x="Model",y = "PEP")+theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))

p_kool2018_exp1_BMS=p1+
  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=24),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小

text <- "Kool 2018, Exp1"

# Create a text grob
tgrob <- text_grob(text,size = 24)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,2, "cm"))

p_kool2018_exp1=ggarrange(plot_0,p_kool2018_exp1_aic,nrow = 2,heights = c(1,4))

################################################################
# add column to subject data
d.subject <- read.csv('./group_aic2018_exp2.csv', sep=',')

data = d.subject

data$model <- factor(data$model,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

subids <- unique(data$id)
for(i in c(1:length(subids))){
  id_s <- subids[i]
  print(id_s)
  id_RLWM_aic=data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic
  print(data[which(data$id==id_s & data$model=="Hybrid-wm"),])
  data[which(data$id==id_s & data$model=="Hybrid-rpe"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-rpe"),]$aic-id_RLWM_aic
  data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic-id_RLWM_aic
  data[which(data$id==id_s & data$model=="Hybrid-lwm"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-lwm"),]$aic-id_RLWM_aic
  }
count(data, model)

p2=ggplot(data, aes(x=model, y=aic)) +
  geom_bar(data=data%>%group_by(model)%>%summarise(aic=mean(aic)), stat="identity", aes(fill=model), show.legend=F,width = 0.6) +
  geom_errorbar(data=data%>%group_by(model)%>%summarise(se=sd(aic)/sqrt(n()), aic=mean(aic)), aes(x=model, ymin=aic-se, ymax=aic+se), width=.2) +
  scale_fill_manual(values=c("grey","grey","grey")) +
  labs(x="",y = "ΔAIC")+
  theme_classic() + theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))

p_kool2018_exp2_aic=p2+  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+ ylim(0,30)+
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=24),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小

data <- data.frame(x = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'),y=c(1,0,    0))
data$x <- factor(data$x,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

#画图
p1 = ggplot(data, aes(x = x, y = y, fill = x)) +
  geom_bar(stat = 'identity', position = position_dodge(),show.legend = FALSE,width=0.5) +
  theme_classic() +
  scale_fill_manual(values=c("grey","grey","grey"))+theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+   ylim(0, 1)+
  labs(x="Model",y = "PEP")+theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))


p_kool2018_exp2_BMS=p1+
  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=24),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小

text <- "Kool 2018, Exp2"

# Create a text grob
tgrob <- text_grob(text,size = 24)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,2, "cm"))

p_kool2018_exp2=ggarrange(plot_0,p_kool2018_exp2_aic,nrow = 2,heights = c(1,4))

#######################################################################
# add column to subject data
d.subject <- read.csv('./group_aic2019.csv', sep=',')

data = d.subject

data$model <- factor(data$model,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

subids <- unique(data$id)
for(i in c(1:length(subids))){
  id_s <- subids[i]
  print(id_s)
  id_RLWM_aic=data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic
  print(data[which(data$id==id_s & data$model=="Hybrid-wm"),])
  data[which(data$id==id_s & data$model=="Hybrid-rpe"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-rpe"),]$aic-id_RLWM_aic
  data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic-id_RLWM_aic
  data[which(data$id==id_s & data$model=="Hybrid-lwm"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-lwm"),]$aic-id_RLWM_aic
  }
count(data, model)

p2=ggplot(data, aes(x=model, y=aic)) +
  geom_bar(data=data%>%group_by(model)%>%summarise(aic=mean(aic)), stat="identity", aes(fill=model), show.legend=F,width = 0.6) +
  geom_errorbar(data=data%>%group_by(model)%>%summarise(se=sd(aic)/sqrt(n()), aic=mean(aic)), aes(x=model, ymin=aic-se, ymax=aic+se), width=.2) +
  scale_fill_manual(values=c("grey","grey","grey")) +
  labs(x="",y = "ΔAIC")+
  theme_classic() + theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))

p_bolenz2019_aic=p2+  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+ ylim(0,30)+
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=24),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小

data <- data.frame(x = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'),y=c(1,0,    0))
data$x <- factor(data$x,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

#画图
p1 = ggplot(data, aes(x = x, y = y, fill = x)) +
  geom_bar(stat = 'identity', position = position_dodge(),show.legend = FALSE,width=0.5) +
  theme_classic() +
  scale_fill_manual(values=c("grey","grey","grey"))+theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+   ylim(0, 1)+
  labs(x="Model",y = "PEP")+theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))

p_bolenz2019_BMS=p1+
  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=24),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小

text <- "Bolenz 2019"

# Create a text grob
tgrob <- text_grob(text,size = 24)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,2, "cm"))

p_bolenz2019=ggarrange(plot_0,p_bolenz2019_aic,nrow = 2,heights = c(1,4))

##################################################################
# add column to subject data
d.subject <- read.csv('./group_aic2020_nhb_magic.csv', sep=',')

data = d.subject

data$model <- factor(data$model,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

subids <- unique(data$id)
for(i in c(1:length(subids))){
  id_s <- subids[i]
  print(id_s)
  id_RLWM_aic=data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic
  print(data[which(data$id==id_s & data$model=="Hybrid-wm"),])
  data[which(data$id==id_s & data$model=="Hybrid-rpe"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-rpe"),]$aic-id_RLWM_aic
  data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic-id_RLWM_aic
  data[which(data$id==id_s & data$model=="Hybrid-lwm"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-lwm"),]$aic-id_RLWM_aic
  }
count(data, model)

p2=ggplot(data, aes(x=model, y=aic)) +
  geom_bar(data=data%>%group_by(model)%>%summarise(aic=mean(aic)), stat="identity", aes(fill=model), show.legend=F,width = 0.6) +
  geom_errorbar(data=data%>%group_by(model)%>%summarise(se=sd(aic)/sqrt(n()), aic=mean(aic)), aes(x=model, ymin=aic-se, ymax=aic+se), width=.2) +
  scale_fill_manual(values=c("grey","grey","grey")) +
  labs(x="",y = "ΔAIC")+
  theme_classic() + theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))

p_2020_nhb_magic_aic=p2+  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=24),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小

data <- data.frame(x = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'),y=c(0.4035 ,0.2646  ,     0.3320))
data$x <- factor(data$x,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

#画图
p1 = ggplot(data, aes(x = x, y = y, fill = x)) +
  geom_bar(stat = 'identity', position = position_dodge(),show.legend = FALSE,width=0.5) +
  theme_classic() +
  scale_fill_manual(values=c("grey","grey","grey"))+theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+   ylim(0, 1)+
  labs(x="Model",y = "PEP")+theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))

p_2020_nhb_magic_BMS=p1+
  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=24),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小

text <- "Hare 2020, magic carpet task"

# Create a text grob
tgrob <- text_grob(text,size = 24)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,2, "cm"))

p_2020_nhb_magic=ggarrange(plot_0,p_2020_nhb_magic_aic,nrow = 2,heights = c(1,4))

##################################################################
# add column to subject data
d.subject <- read.csv('./group_aic2020_nhb_spaceship.csv', sep=',')

data = d.subject

data$model <- factor(data$model,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

subids <- unique(data$id)
for(i in c(1:length(subids))){
  id_s <- subids[i]
  print(id_s)
  id_RLWM_aic=data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic
  print(data[which(data$id==id_s & data$model=="Hybrid-wm"),])
  data[which(data$id==id_s & data$model=="Hybrid-rpe"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-rpe"),]$aic-id_RLWM_aic
  data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic-id_RLWM_aic
  data[which(data$id==id_s & data$model=="Hybrid-lwm"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-lwm"),]$aic-id_RLWM_aic
}
count(data, model)

p2=ggplot(data, aes(x=model, y=aic)) +
  geom_bar(data=data%>%group_by(model)%>%summarise(aic=mean(aic)), stat="identity", aes(fill=model), show.legend=F,width = 0.6) +
  geom_errorbar(data=data%>%group_by(model)%>%summarise(se=sd(aic)/sqrt(n()), aic=mean(aic)), aes(x=model, ymin=aic-se, ymax=aic+se), width=.2) +
  scale_fill_manual(values=c("grey","grey","grey")) +
  labs(x="",y = "ΔAIC")+
  theme_classic() + theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))

p_2020_nhb_spaceship_aic=p2+  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=24),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小

data <- data.frame(x = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'),y=c(0.4077  ,0.2542 ,     0.3381))
data$x <- factor(data$x,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

#画图
p1 = ggplot(data, aes(x = x, y = y, fill = x)) +
  geom_bar(stat = 'identity', position = position_dodge(),show.legend = FALSE,width=0.5) +
  theme_classic() +
  scale_fill_manual(values=c("grey","grey","grey"))+theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+   ylim(0, 1)+
  labs(x="Model",y = "PEP")+theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))

p_2020_nhb_spaceship_BMS=p1+
  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=24),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小

text <- "Hare 2020, spaceship task"

# Create a text grob
tgrob <- text_grob(text,size = 24)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,2, "cm"))

p_2020_nhb_spaceship=ggarrange(plot_0,p_2020_nhb_spaceship_aic,nrow = 2,heights = c(1,4))

##################################################################
# add column to subject data
d.subject <- read.csv('./group_aic2021.csv', sep=',')

data = d.subject

data$model <- factor(data$model,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

subids <- unique(data$id)
for(i in c(1:length(subids))){
  id_s <- subids[i]
  print(id_s)
  id_RLWM_aic=data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic
  print(data[which(data$id==id_s & data$model=="Hybrid-wm"),])
  data[which(data$id==id_s & data$model=="Hybrid-rpe"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-rpe"),]$aic-id_RLWM_aic
  data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic-id_RLWM_aic
  data[which(data$id==id_s & data$model=="Hybrid-lwm"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-lwm"),]$aic-id_RLWM_aic
  
}
count(data, model)

p2=ggplot(data, aes(x=model, y=aic)) +
  geom_bar(data=data%>%group_by(model)%>%summarise(aic=mean(aic)), stat="identity", aes(fill=model), show.legend=F,width = 0.6) +
  geom_errorbar(data=data%>%group_by(model)%>%summarise(se=sd(aic)/sqrt(n()), aic=mean(aic)), aes(x=model, ymin=aic-se, ymax=aic+se), width=.2) +
  scale_fill_manual(values=c("grey","grey","grey")) +
  labs(x="",y = "ΔAIC")+
  theme_classic() + theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))

p_bolenz2021_aic=p2+  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=24),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小

data <- data.frame(x = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'),y=c(1,0,    0))
data$x <- factor(data$x,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

#画图
p1 = ggplot(data, aes(x = x, y = y, fill = x)) +
  geom_bar(stat = 'identity', position = position_dodge(),show.legend = FALSE,width=0.5) +
  theme_classic() +
  scale_fill_manual(values=c("grey","grey","grey"))+theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+   ylim(0, 1)+
  labs(x="Model",y = "PEP")+theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))


p_bolenz2021_BMS=p1+
  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=24),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小

text <- "Bolenz 2021"

# Create a text grob
tgrob <- text_grob(text,size = 24)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,2, "cm"))

p_bolenz2021=ggarrange(plot_0,p_bolenz2021_aic,nrow = 2,heights = c(1,4))

########################################################################

# add column to subject data
d.subject <- read.csv('./group_aic_zuo.csv', sep=',')

data = d.subject

data$model <- factor(data$model,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

subids <- unique(data$id)
for(i in c(1:length(subids))){
  id_s <- subids[i]
  print(id_s)
  id_RLWM_aic=data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic
  print(data[which(data$id==id_s & data$model=="Hybrid-wm"),])
  data[which(data$id==id_s & data$model=="Hybrid-rpe"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-rpe"),]$aic-id_RLWM_aic
  data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-wm"),]$aic-id_RLWM_aic
  data[which(data$id==id_s & data$model=="Hybrid-lwm"),]$aic <- data[which(data$id==id_s & data$model=="Hybrid-lwm"),]$aic-id_RLWM_aic
  
}
count(data, model)

p2=ggplot(data, aes(x=model, y=aic)) +
  geom_bar(data=data%>%group_by(model)%>%summarise(aic=mean(aic)), stat="identity", aes(fill=model), show.legend=F,width = 0.6) +
  geom_errorbar(data=data%>%group_by(model)%>%summarise(se=sd(aic)/sqrt(n()), aic=mean(aic)), aes(x=model, ymin=aic-se, ymax=aic+se), width=.2) +
  scale_fill_manual(values=c("grey","grey","grey")) +
  labs(x="",y = "ΔAIC")+
  theme_classic() + theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))

p_zuo2022_aic=p2+  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=24),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小

data <- data.frame(x = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'),y=c(0.6488  ,0.1738  ,    0.1774))
data$x <- factor(data$x,levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))

#画图
p1 = ggplot(data, aes(x = x, y = y, fill = x)) +
  geom_bar(stat = 'identity', position = position_dodge(),show.legend = FALSE,width=0.5) +
  theme_classic() +
  scale_fill_manual(values=c("grey","grey","grey"))+theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+   ylim(0, 1)+
  
  labs(x="Model",y = "PEP")+theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))

p_zuo2022_BMS=p1+
  xlab(NULL)+scale_x_discrete(guide = guide_axis(n.dodge=2))+
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=24),
        axis.text.x =element_text(size=24), axis.text.y=element_text(size=21.5))+
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小

text <- "Zuo et al."

# Create a text grob
tgrob <- text_grob(text,size = 24)
# Draw the text
plot_0 <- as_ggplot(tgrob) + theme(plot.margin = margin(0,0,0,2, "cm"))

p_zuo2022=ggarrange(plot_0,p_zuo2022_aic,nrow = 2,heights = c(1,4))
p9 <- ggpubr::ggarrange(p_kool2016, p_kool2017,  p_kool2018_exp1,p_kool2018_exp2, p_bolenz2019,p_2020_nhb_magic,p_2020_nhb_spaceship,p_bolenz2021,p_zuo2022,nrow = 3, ncol = 3, labels = c('a', 'b', 'c', 'd','e', 'f','g', 'h', 'i'),font.label = list(color = 'Black',size = 20))
p9

ggsave(
  filename = "./figure_2.tiff", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 18,             # 宽
  height = 12,            # 高
  units = "in",          # 单位
  dpi = 500              # 分辨率DPI
)