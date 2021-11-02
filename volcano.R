library(ggpubr)
library(ggthemes)

# 读取数据
deg.data <- read.table("ET1-normal.DEG.csv", header = T, sep = ",")
# 显示文件前六�?
head(deg.data)

# 对差异p（adj.P.Val一列）进行log10转换
deg.data$logP <- -log10(deg.data$adj.P.Val)

# 将adj.P.Val大于0.05，logFC大于1的基因设为显著上调基�?
# 将adj.P.Val小于0.05，logFC小于-1的基因设为显著下调基�?
deg.data$Group = "no-significant"
deg.data$Group[which((deg.data$adj.P.Val < 0.05) & (deg.data$logFC > 1))] <- "up-regulated"
deg.data$Group[which((deg.data$adj.P.Val < 0.05) & (deg.data$logFC < -1))] <- "down-regulated"
# 查看上调和下调基因数�?
table(deg.data$Group)

# 绘制新的火山�?
ggscatter(deg.data, 
          x = "logFC", 
          y = "logP", 
          color = "Group") + theme_base()

# 改变火山图颜色（palette）和点的大小（size�?
ggscatter(deg.data, 
          x = "logFC", 
          y = "logP",
          color = "Group", 
          palette = c("green","gray","red"), 
          size = 1) + 
  theme_base()

# 为火山图添加logP分界线（geom_hline）和logFC分界线（geom_vline�?
ggscatter(deg.data, 
          x = "logFC", 
          y = "logP", 
          color = "Group", 
          palette = c("green","gray","red"),
          size = 1) + 
  theme_base() + 
  geom_hline(yintercept = 1.30, 
             linetype = "dashed") + 
  geom_vline(xintercept = c(-1,1), 
             linetype = "dashed")

# 新加一列Label
deg.data$Label  = ""
# 对差异表达基因的p值进行从大到小排�?
deg.data <- deg.data[order(deg.data$adj.P.Val),]
# 高表达的基因中，选择adj.P.Val最小的10�?
up.genes <- head(deg.data$X[which(deg.data$Group == "up-regulated")], 10)
# 低表达的adj.P.Val最小的10�?
down.genes <- head(deg.data$X[which(deg.data$Group == "down-regulated")],10)
# 将up.genes和down.genes合并，并加入到Label�?
deg.top10.genes <- c(as.character(up.genes), as.character(down.genes))
deg.data$Label[match(deg.top10.genes,deg.data$X)] <- deg.top10.genes

# 为火山图添加显著差异表达前十名的基因
ggscatter(deg.data, 
          x = "logFC", 
          y = "logP", 
          color = "Group", 
          palette = c("green","gray","red"),
          size = 1,
          label = deg.data$Label, 
          font.label = 8, 
          repel= T) + 
  theme_base() + 
  geom_hline(yintercept = 1.30, 
             linetype = "dashed") + 
  geom_vline(xintercept = c(-1,1), 
             linetype = "dashed")

# 改变火山图的颜色和坐标轴标注，是图片更美�?
ggscatter(deg.data, 
          x = "logFC", 
          y = "logP", 
          color = "Group", 
          palette = c("#2f5688","#BBBBBB","#cc0000"),
          size = 1, 
          label = deg.data$Label, 
          font.label = 8, 
          repel = T, 
          xlab = "log2FoldChange", 
          ylab = "-log10(Adjust P-value)") + 
  theme_base() + 
  geom_hline(yintercept = 1.30, 
             linetype = "dashed") +
  geom_vline(xintercept = c(-1,1) , 
             linetype = "dashed")
