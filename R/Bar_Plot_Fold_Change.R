library (data.table)
library(ggplot2)
df <- fread ("/Volumes/ATUL_6TB/Work/Projects/Nicholas_Ashton/Bar_Plot_Data.txt")
p <- ggplot(data=df, aes(x=reorder(Protein, -(-log (P))), y=Effect, width = Width/50)) +
  geom_bar(stat="identity", fill="#D55E00")
p <- p + scale_y_continuous(breaks = round (seq (-3, 4, by = 0.5), 1))
p <- p + xlab ("Differentially Expressed Proteins") + ylab ("Effect Size") + theme_classic()
p <- p +
  theme(plot.title = element_text(family = "serif", size=17, face = "bold", hjust = 0.5),
        axis.title.x = element_text(family = "serif", size=16),
        axis.title.y = element_text(family = "serif", size=16),
        axis.text.x = element_text(family = "serif", size=10, angle = 90),
        axis.text.y = element_text(family = "serif", size=10),
        legend.title = element_text(family = "serif", size=16),
        legend.text = element_text(family = "serif", size=16),
        panel.background = element_blank()) + labs(title=expression("Top 50 differentially expressed Proteins: SAA- CUI vs SAA+ LBD"))
p

