## Conceptual figure visualization

library(tidyverse)


## All ice

allIce <- data.frame(lakeType = rep(c("Permanent","Seasonal","Intermittent", "Ice-free"),each=2),
                     ice= rep(c("On","Off"), 4),
                     duration = c(365,0,120,265,60,285,0,365))

allIce <- allIce %>% group_by(lakeType) %>% mutate(fraction = duration/sum(duration), ymax = cumsum(fraction))
allIce[,"ymin"] <- c(0,1,0,0.312,0,0.174,0,0)

ggplot(allIce, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=ice)) +
  geom_rect() + facet_grid(~lakeType) + 
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2,4)) + theme_void() + 
  scale_fill_manual(values=c("#9cd3db","#0000f2"))