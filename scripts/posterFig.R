library(ggplot2)
library(Cairo)
library(RColorBrewer)
library(tidyr)
library(dplyr)

load("data/poster.RData")
path<-"results/posterfig/"
# fig1.1 KSA generation (DONE)

Cairo(file=paste(path, "generation.png", sep=""), width=90, height=80, units="mm", dpi=300, type="png")
ggplot(pgg, aes(y, p)) + geom_line() + labs(title="Peak Power Demand Forecast [1]", y="GW", x="Year")+ theme_minimal(base_size = 10, base_family = "Helvetica")
dev.off()
# ggplot(pgg, aes(factor(y),p)) + geom_bar(stat="identity")

# fig1.2 Oil consumption production(DONE)
Cairo(file=paste(path, "oil.png", sep=""), width=90, height=80, units="mm", dpi=300, type="png")
ggplot(ksaOil, aes(x=y, fill=Oil)) + geom_ribbon(aes(ymin=0,ymax=p, fill="Production")) + geom_ribbon(aes(ymin=0, ymax=c, fill="Consumption")) + theme_minimal(base_size = 10, base_family = "Helvetica") + labs(title="Oil Production vs Consumption [2]", x="Year", y="Million Barrels Daily", color="Oil") + theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# fig1.3 PIE (ISSUES!)
Cairo(file=paste(path, "mix.png", sep=""), width=90, height=80, units="mm", dpi=300, type="png")

mix %>%
  ggplot(aes(x=type,y = amount, fill = type)) +
  geom_bar(width = 1, stat = "identity") +
  scale_fill_manual(values = brewer.pal(7,"RdYlBu")) +
  labs(title = "Power Generation Goal by 2032 [3]", x="", y="GW") +
  theme_minimal(base_size = 10,base_family = "Helvetica") +
  theme(legend.position="NULL", axis.text.x = element_text(angle = 45, hjust = 1))

# pie(mix$amount..GW, paste(mix$type, " ", mix$amount..GW, "GW", sep=""), col=brewer.pal(7,"RdYlBu"), cex=2, title="Electricity Generation by 2032 [3]")
dev.off()

# fig1.4 small facts about offsore DRAWN IN ID


# fig2.1 process diagram DRAWN IN illustrator
# fig2.2 legend DRAWN IN ID

# fig3.1 diurnal
#quick fix of names
# library(plyr)
# ts$site<-revalue(ts$site, c("aqaba"="Aqaba", "yanbu"="Yanbu", "jeddah"="Jeddah"))

Cairo(file=paste(path,"diurnal.png",sep=""),width=90, height=80, units="mm", type="png", dpi=300)
ts %>%
  group_by(site, H) %>%
  summarize(cf = mean(P80)/3600) %>%
  ggplot(aes(factor((H+3)%%24), cf, color=factor(site)))+ geom_point()+geom_line(aes(group=(site))) + labs(title = "Diurnal Variation of Mean CF", x="Hour" , y="mean CF", color="Region") + theme_minimal(base_size = 10, base_family = "Helvetica") + theme(legend.position="bottom")
dev.off()

# fig3.2: monthly
Cairo(file=paste(path,"seasonal.png",sep=""),width=90, height=80, units="mm", type="png", dpi=300)
ts %>%
  group_by(site, m) %>%
  summarize(cf = mean(P80)/3600) %>%
  ggplot(aes(factor(m), cf, color=factor(site)))+ geom_point()+geom_line(aes(group=(site))) + labs(title = "Seasonal Variation of Mean CF", x="Month" , y="mean CF", color="Region") + theme_minimal(base_size = 10, base_family = "Helvetica") + theme(legend.position="bottom")
dev.off()

