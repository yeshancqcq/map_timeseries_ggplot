#需要先安装下面的library
library(ggplot2)
library(ggmap)
library(maps)
library(readr)

#设定working directory
setwd("~/Documents/github/map_timeseries_ggplot")

#导入数据
data <- read_csv("data.csv")

#从第一列数据循环到最后一列数据。前三列是城市名和经纬度，所以从第四列开始。
for(i in 4:(ncol(data))){
  #添加底图
  base <- map_data("state")
  #创建每一张图的临时数据，包括经纬度（数据的2、3列）和当前列i的数据，组成3列数据
  gg_data <- data.frame(lon=data[,2], lat=data[,3], time=data[,i])
  
  #数据的表头是y2020格式，如果要提取2020当做label，就从第2个字符开始提取
  label = substring(names(data)[i],2)
  
  #创建底图
  map <- ggplot() + 
    #绘制美国本土的底图
    geom_polygon(data = base, aes(x=long, y = lat, group = group), 
                 fill = "white", color = "black", size = 0.05) +
    #设置经纬度范围以及相对比例尺
    coord_fixed(xlim = c(-130, -65),  ylim = c(25, 50), ratio = 1.4)+
    #绘制气泡，大小就是数据大小，即临时数据的第三列，设定颜色和透明度
    geom_point(data = gg_data, aes(x = lon, y = lat, size = gg_data[,3]),colour="red", alpha = 0.22) +
    #设置数据范围（10-40）及其对应的气泡大小（2-20）
    scale_size_continuous(limits=c(10,40),range = c(2, 20))+
    #其他美化，还有很多选项，参见 https://yeshancqcq.github.io/document/ggplot2_tutorial.pdf
    theme(
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      legend.position = "bottom"
    )+
    #如果要添加中文，需要把字体设为楷书
    theme(text = element_text(family = 'Kai'))+
    #设置图例
    labs(size= "Size of the label")+
    #可以添加标题或其他标注
    annotate("text", x = -70, y = 30, label = "Some Labels, 也可以是中文", size = 5, colour = "black",family = 'Kai')+
    annotate("text", x = -125, y = 28, label = paste("Year: ", as.character(label)), size = 5, colour = "black",family = 'Kai')+
    annotate("text", x = -100, y = 50, label = paste("Some Titles, 也可以是中文"), size = 8, colour = "black",family = 'Kai')
  
  #保存本张地图到working directory之下的map文件夹，设置画幅大小以及dpi分辨率，名字为[i].jpg
  ggsave(paste(as.character(i), ".jpg"), plot = last_plot(), device = NULL, path = "map",
         width = 30, height = 20, units = "cm",
         dpi = 300)
  
  #输出本轮的完成信息，如果没有出错，应该从4一直输出到最后的列数
  cat("processing", i, "\n")
}

#完成之后，去map文件夹里就能找到图
