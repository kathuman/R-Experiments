rm(list=ls())

source("waterfall.r")

## Load the data and set correct column names
df <- read.csv("data.csv", stringsAsFactors=FALSE)

## ----further-prep--------------------------------------------------------
## Tidy the levels
df$category <- factor(df$category, levels=unique(df$category))
levels(df$category) <- gsub("flows ", "flows \n", levels(df$category))
levels(df$category) <- gsub(" emissions", "\nemissions", levels(df$category))
df$sector <- factor(df$sector, levels=c("UK", "EU", "Annex 1", "Non-Annex 1",
                                   "China", "Other non-Annex 1"))
## ----prepare-plot, echo=TRUE---------------------------------------------

## Determines the spacing between columns in the waterfall chart
offset <- 0.3
gg <- waterfall(df, offset=offset) +
    coord_cartesian(ylim=c(600, 900)) +
    scale_fill_manual(guide="none", values=c(rgb(81, 34, 112, max=255),
                                        rgb(125, 96, 153, max=255),
                                        rgb(116, 173, 226, max=255),
                                        rgb(17, 135, 146, max=255),
                                        rgb(69, 171, 183, max=255),
                                        rgb(17, 135, 146, max=255))) +
    labs(x="Components of the Flow", y="Mt CO2", 
         title="UK embodied emissions balance (import and export) with major regions, 2004") +
    theme_light() +
    annotate("text", x=6, y=838, label="China", colour="white") +
    annotate("text", x=8 + offset, y=900,
             hjust=1, vjust=1,
             size=3,
             label="Data source: Carbon Trust (2011)",
             fontface="italic") +
    theme(plot.title=element_text(face="bold", hjust=0, vjust=2))


## ----plot, dev='png', fig.width=12, fig.height=7.5-----------------------
print(gg)


