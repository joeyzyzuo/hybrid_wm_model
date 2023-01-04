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
library(cowplot)
library(png)
library(this.path)

# Get the directory from the path of the current file.
cur_dir2 = dirname(this.path())

# Set the working directory.
setwd(cur_dir2)

# Check that the working directory has been set as desired.
getwd()


data <- read.csv('./zuo_wm_capacity.csv', sep=',')
data$w_mean=(data$mixing_weight_low_stable+data$mixing_weight_low_variable+data$mixing_weight_high_stable+data$mixing_weight_high_variable)/4

cor(data$工作记忆广度, data$w_mean)
cor.test(data$工作记忆广度, data$w_mean)
cor.test(data$工作记忆广度, data$points_bc)

cor.test(data$工作记忆广度, data$fai_decay_wm)

p_4=ggscatter(data,alpha = 0.3, x = "工作记忆广度", y = "w_mean",
              add = "reg.line",  # 添加回归线
              cor.coef = FALSE, # 添加相关系数
              cor.coeff.args = list(method = "pearson", label.x = 10.25,label.y = 0.7, label.sep = "\n"),#选择Pearson相关
              cor.coeff.size = 20
)+scale_fill_npg()+ labs(x="WM capacity", y = "Model-based weight (average)")+
  theme(axis.title.x =element_text(size=15), axis.title.y=element_text(size=15))+
  annotate('text',x=13.7,y=0.42,
           label='r = 0.29',
           size=6,color='black')+
  annotate('text',x=14,y=0.37,
           label='p = 0.027',
           size=6,color='black')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=20), axis.title.y=element_text(size=20),
        axis.text.x =element_text(size=16), axis.text.y=element_text(size=16))+
  theme(legend.title = element_text(size = 14))+#图例标题字体大小
  theme(legend.text = element_text(size = 12))+#图例文字字体大小
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小

p_4

img1B <- png::readPNG('./zuo_mediation.png')

p2 <- ggdraw() + draw_image(img1B)
plot_left=plot_grid(p_4, p2,ncol = 1, nrow = 2,rel_heights=c(1.25,1),
                    labels = c('a', 'c'),label_size = 18)

title <- ggdraw() + 
  draw_label(
    "Zuo et al.",
    fontface = 'bold',
    size = 20,
    x = 0,
    hjust = 0
  ) +
  theme(
    # 添加边距，使标题左对齐边缘
    plot.margin = margin(1, 0, 0,7, "cm")
  )
p_zuo=plot_grid(
  title, plot_left,
  ncol = 1,
  # 控制标题与图形的高度比例
  rel_heights = c(0.1, 1)
)
p_zuo

data <- read.csv('./bolenz_wm_capacity.csv', sep=',')

data$w_mean=(data$mixing_weight_low_stable+data$mixing_weight_low_variable+data$mixing_weight_high_stable+data$mixing_weight_high_variable)/4

cor(data$digitspan, data$w_mean)
cor.test(data$digitspan, data$w_mean)
cor.test(data$digitspan, data$points_bc)

cor.test(data$digitspan, data$fai_decay_wm)
p_1=ggscatter(data,alpha = 0.3, x = "digitspan", y = "w_mean",
              add = "reg.line",  # 添加回归线
              cor.coef = FALSE, # 添加相关系数
              cor.coeff.args = list(method = "pearson", label.x = 10.25,label.y = 0.7, label.sep = "\n"),#选择Pearson相关
              cor.coeff.size = 40)+scale_fill_npg()+ labs(x="WM capacity", y = "Model-based weight (average)")+
  theme(axis.title.x =element_text(size=15), axis.title.y=element_text(size=15))+
  annotate('text',x=11.45,y=0.3,
           label='r = 0.24',
           size=6,color='black')+
  annotate('text',x=12,y=0.24,
           label='p = 0.0068',
           size=6,color='black')+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=20), axis.title.y=element_text(size=20),
        axis.text.x =element_text(size=16), axis.text.y=element_text(size=16))+
  theme(plot.title = element_text(size = 18, face = "bold"))#标题字体大小

img1B <- png::readPNG('./bolenz_mediation.png')

p2 <- ggdraw() + draw_image(img1B)
plot_right =plot_grid(p_1, p2,ncol = 1, nrow = 2,rel_heights=c(1.25,1),
                      labels = c('b', 'd'),label_size = 18)

title <- ggdraw() + 
  draw_label(
    "Bolenz 2019",
    fontface = 'bold',
    size = 20,
    x = 0,
    hjust = 0
  ) +
  theme(
    # 添加边距，使标题左对齐边缘
    plot.margin = margin(1, 0, 0,6, "cm")
  )
p_bolenz=plot_grid(
  title, plot_right,
  ncol = 1,
  # 控制标题与图形的高度比例
  rel_heights = c(0.1, 1)
)
p_bolenz

p=plot_grid(
  p_zuo, p_bolenz
)
ggsave(
  filename = "./figure_6.tiff", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 12,             # 宽
  height = 12,            # 高
  units = "in",          # 单位
  dpi = 500              # 分辨率DPI
)