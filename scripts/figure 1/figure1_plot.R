# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

library(ggplot2)
library(cowplot)
library(png)
library(ggpubr)
library(this.path)

# Get the directory from the path of the current file.
cur_dir2 = dirname(this.path())

# Set the working directory.
setwd(cur_dir2)

# Check that the working directory has been set as desired.
getwd()

img1A <- png::readPNG('./Fig1A.png')
pA <- ggdraw() + draw_image(img1A)
img_model_stru <- png::readPNG('./model_stru.png')
pB <- ggdraw() + draw_image(img_model_stru)

web.data<-data.frame(gene =c("Hybrid-wm","Hybrid-wm","Hybrid-wm","Hybrid-wm","Hybrid-wm","Hybrid-wm",
                             "Hybrid-lwm","Hybrid-lwm","Hybrid-lwm","Hybrid-lwm","Hybrid-lwm","Hybrid-lwm",
                             "Hybrid-rpe","Hybrid-rpe","Hybrid-rpe","Hybrid-rpe","Hybrid-rpe","Hybrid-rpe"),
                     ave=c(2,9,7.2,6.12,5.472,5.0832,
                           2,9,9,9,9,9,
                           2,6.9,6.9,6.9,6.9,6.9
                     ),
                     treat=c('T-1','T','T+1','T+2','T+3','T+4',
                             'T-1','T','T+1','T+2','T+3','T+4',
                             'T-1','T','T+1','T+2','T+3','T+4'
                     )
)
web.data$treat=ordered(web.data$treat, levels = c('T-1','T','T+1','T+2','T+3','T+4')) 
web.data$gene=ordered(web.data$gene, levels = c('Hybrid-wm','Hybrid-lwm','Hybrid-rpe'))
legend_title=''
pD=ggplot(web.data,aes(treat,ave,group=gene,color=gene,shape=gene))+
  scale_color_manual(legend_title,values=c("#3C5488B2",
                                           
                                           "#F39B7FB2",
                                           
                                           "#00A087B2"))+theme_classic()+
  guides(shape='none')+
  geom_point(size=-4+4)+
  geom_line(position = position_dodge(0.1),cex=1.3)+
  ylim(0,10)+
  labs(x="Trial",y = "state-action value") + 
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.title.x=element_blank()) +
  theme(axis.title.x =element_text(size=22), axis.title.y=element_text(size=22),
        axis.text.x =element_text(size=16), axis.text.y=element_text(size=16))+
  theme(plot.title = element_text(size= 22, face = "bold"))+#标题字体大小
  theme(legend.position = c(0.65,0.2))+ theme(legend.text = element_text(size = 18))+
  geom_hline(yintercept = 4.5, linetype="dotted")+ annotate("text", x = 5, y = 4.2, label = "average reward", size=6)+
  annotate("label", x = 2, y = 9.8,size=6,
           label = "got reward:9")+
  annotate("segment", x = 2, xend = 2, y = 9.4, yend = 9)

pD

ggarrange(pA,                                               # 第一行为散点图
          ggarrange(pB,pD, nrow = 1, labels = c("b", "c"),widths =  c(1,1)), # 第二行为箱线图和点图
          nrow = 2,
          labels = c("a","")
)

ggsave(
  filename = "./figure_1.tiff", # 保存的文件名称。通过后缀来决定生成什么格式的图片
  width = 12,             # 宽
  height = 8,            # 高
  units = "in",          # 单位
  dpi = 500              # 分辨率DPI
)