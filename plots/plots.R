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




df <- read.csv('plots.csv', header=T)

library(dplyr)
library(ggplot2)

df1 <- df %>% filter(Type=='Loss')
df2 <- df %>% filter(Type=='Acc')

d_ends1 <- df1 %>%
  group_by(Label) %>%
  top_n(1, Epoch) %>%
  pull(Value)

d_labs1 <- df1 %>%
  group_by(Label) %>%
  top_n(1, Epoch) %>%
  pull(Label)

d_ends2 <- df2 %>%
  group_by(Label) %>%
  top_n(1, Epoch) %>%
  pull(Value)

d_labs2 <- df2 %>%
  group_by(Label) %>%
  top_n(1, Epoch) %>%
  pull(Label)

p1 <- ggplot(df1, aes(x=Epoch, Value, group=Label, linetype=Label)) +
  geom_line(size=1) +
  theme_Publication() +
  theme(legend.title = element_blank()) +
  ggtitle("Validation Loss") + 
  labs(x = "Epochs", y = "Loss") +
  scale_linetype_manual(values = c(3, 8, 1, 4), labels = c("Adjectives", "Domains", "Full set", "Stop words"))


p2 <- ggplot(df2, aes(x=Epoch, Value, group=Label, linetype=Label)) +
  geom_line(size=1) +
  theme_Publication() +
  theme(legend.title = element_blank()) +
  ggtitle("Validation Accuracy") + 
  labs(x = "Epochs", y = "Accuracy") +
  scale_linetype_manual(values = c(3, 8, 1, 4), labels = c("Adjectives", "Domains", "Full set", "Stop words"))

p1
p2


# require(gridExtra)

# grid.arrange(p1, p2, ncol=2)