theme_Publication <- function(base_size=14, base_family="helvetica") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = "black", size=1),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            # axis.line = element_line(colour="black"),
            axis.line = element_blank(),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour = "gray", linetype = "dashed"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(1.3, "cm"),
            legend.spacing = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}




df <- read.csv('p2.csv', header=T)

library(dplyr)
library(reshape2)
library(ggplot2)

sub1 <- df %>% filter(Type=='Loss')
sub2 <- df %>% filter(Type=='Acc')

df1 <- sub1 %>% distinct(Epoch)

df1$mean <- aggregate(sub1$Value, list(sub1$Epoch), mean)$x
df1$min <- aggregate(sub1$Value, list(sub1$Epoch), min)$x
df1$max <- aggregate(sub1$Value, list(sub1$Epoch), max)$x

df2 <- sub2 %>% distinct(Epoch)

df2$mean <- aggregate(sub2$Value, list(sub2$Epoch), mean)$x
df2$min <- aggregate(sub2$Value, list(sub2$Epoch), min)$x
df2$max <- aggregate(sub2$Value, list(sub2$Epoch), max)$x

data1 <- melt(df1, id=c("Epoch"))
data2 <- melt(df2, id=c("Epoch"))

ends1 <- data1 %>%
  group_by(variable) %>%
  top_n(1, Epoch) %>%
  pull(value)

ends2 <- data2 %>%
  group_by(variable) %>%
  top_n(1, Epoch) %>%
  pull(value)


p1 <- ggplot(data1, aes(x=Epoch, y=value, group=variable, linetype=variable)) +
  geom_line(size=1) +
  theme_Publication() +
  theme(legend.position = "none") +
  ggtitle("Validation Loss") + 
  labs(x = "Epochs", y = "Loss") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(sec.axis = sec_axis(~ ., breaks = ends1, labels = c("Mean", "Min", "Max"))) +
  scale_linetype_manual(values = c(1, 2, 2))

p2 <- ggplot(data2, aes(x=Epoch, y=value, group=variable, linetype=variable)) +
  geom_line(size=1) +
  theme_Publication() +
  theme(legend.position = "none") +
  ggtitle("Validation Accuracy") + 
  labs(x = "Epochs", y = "Accuracy") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(sec.axis = sec_axis(~ ., breaks = ends2, labels = c("Mean", "Min", "Max"))) +
  scale_linetype_manual(values = c(1, 2, 2))

p1
p2

