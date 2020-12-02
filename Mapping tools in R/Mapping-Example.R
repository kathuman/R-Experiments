library(foreign)
counties <- read.spss("U.S. Religion Census Religious Congregations and Membership Study, 2010 (County File).SAV", to.data.frame = TRUE)

install.packages("choroplethr")
library(choroplethr)
install.packages("choroplethrMaps")
library(choroplethrMaps)

library(ggplot2)
install.packages("viridis")
library(viridis)
counties <- counties[!is.na(counties$TOTRATE),]
counties$region <- counties$FIPS
counties$value <- counties$TOTRATE
counties[(counties$value > 1000),'value'] <- 1000.

choro = CountyChoropleth$new(counties)
choro$title = "Total Religious Adherents by County"
choro$set_num_colors(1)
choro$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro$ggplot_scale = scale_fill_gradientn(name = "Adherence raten(per 1000 population)", colours = viridis(32), limits = c(0, 1000))
choro$render()

choro = CountyChoropleth$new(counties)
choro$title = "Total Religious Adherents in Iowa"
choro$set_num_colors(1)
choro$set_zoom("iowa")
choro$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro$ggplot_scale = scale_fill_gradientn(name = "Adherence raten(per 1000 population)", colours = viridis(32))
choro$render()

counties$value <- counties$EVANRATE
choro = CountyChoropleth$new(counties)
choro$title = "Evangelical Protestants in Iowa"
choro$set_num_colors(1)
choro$set_zoom("iowa")
choro$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro$ggplot_scale = scale_fill_gradientn(name = "Adherence raten(per 1000 population)", colours = viridis(32))
choro$render()

library(gridExtra)
counties[is.na(counties)] <- 0
counties$value <- counties$MPRTRATE
choro1 = CountyChoropleth$new(counties)
choro1$set_zoom("iowa")
choro1$title = "Mainline Protestants in Iowa"
choro1$set_num_colors(1)
choro1$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro1$ggplot_scale = scale_fill_gradientn(name = "Adherence raten(per 1000 population)", colours = viridis(32))
counties$value <- counties$CATHRATE
choro2 = CountyChoropleth$new(counties)
choro2$set_zoom("iowa")
choro2$title = "Catholics in Iowa"
choro2$set_num_colors(1)
choro2$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro2$ggplot_scale = scale_fill_gradientn(name = "Adherence raten(per 1000 population)", colours = viridis(32))

grid.arrange(choro1$render(), choro2$render(), ncol = 2)

counties$value <- counties$EVANRATE
choro = CountyChoropleth$new(counties)
choro$title = "Evangelical Protestants in Iowa and Neighbors"
choro$set_num_colors(1)
choro$set_zoom(c("iowa", "missouri", "illinois", "wisconsin", "minnesota",
                 "south dakota", "nebraska", "kansas"))
choro$ggplot_polygon = geom_polygon(aes(fill = value), color = NA)
choro$ggplot_scale = scale_fill_gradientn(name = "Adherence raten(per 1000 population)", colours = viridis(32))
choro$render()

