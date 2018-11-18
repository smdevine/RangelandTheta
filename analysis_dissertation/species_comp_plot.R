library(ggplot2)
species_comp <- read.csv('C:/Users/smdevine/Desktop/rangeland project/clip_plots/Camatta_species_comp_2017.csv', stringsAsFactors = FALSE)
species_comp$Composition_perc <- species_comp$Composition_perc * 100
species_comp
tapply(species_comp$Composition_perc, list(species_comp$Date, species_comp$Site_class), sum)
tapply(species_comp$Common_Name, list(species_comp$Date, species_comp$Site_class), length)

species_by_date <- as.data.frame(tapply(species_comp$Composition_perc, list(species_comp$Date, species_comp$Common_Name), mean))
species_by_date[1, order(species_by_date[1,])]
species_by_date[2, order(species_by_date[2,])]
species_by_date[3, order(species_by_date[3,])]
tapply(species_comp$Composition_perc, species_comp$Common_Name, mean)
unique(species_comp$Genus)
unique(species_comp$Species)
legend_labels <- species_comp[1:9,]
legend_labels <- legend_labels[order(legend_labels$Dummy_var),]
legend_labels <- paste(legend_labels$Genus, legend_labels$Species)
png(file = file.path(resultsFigures, 'species.comp', paste0('species.comp.by.date.landscape.png')), family = 'Book Antiqua', width = 1200, height = 1000, units = 'px', res=100)
ggplot(data=species_comp) +
  geom_col(aes(y = Composition_perc, x = Site_class, fill = as.character(Dummy_var)), position='stack') +
  labs(title= 'Species composition in 2017 by date and landscape position', x = 'Landscape position', y = 'Species composition (%)') +
  scale_fill_manual(values=c('forestgreen', 'springgreen', 'lightgreen', 'thistle', 'gold1', 'goldenrod2', 'lemonchiffon', 'rosybrown', 'brown'), name ='Species', labels=legend_labels) +
  theme_classic(base_size = 15) +
  theme(legend.position = 'bottom') +
  facet_grid( ~ Date)
dev.off()

tiff(file = file.path(resultsFigures, 'species.comp', paste0('species.comp.by.date.landscape.tif')), family = 'Times New Roman', width = 6.5, height = 5.5, units = 'in', pointsize = 10, res=150)
ggplot(data=species_comp) +
  geom_col(aes(y = Composition_perc, x = Site_class, fill = as.character(Dummy_var)), position='stack') +
  labs(x = 'Landscape position', y = 'Species composition (%)') + #title= 'Species composition in 2017 by date and landscape position'
  scale_fill_manual(values=c('forestgreen', 'springgreen', 'lightgreen', 'thistle', 'gold1', 'goldenrod2', 'lemonchiffon', 'rosybrown', 'brown'), name ='Species', labels=legend_labels) +
  theme_classic(base_size = 11) +
  theme(legend.position = 'bottom')  +
  #theme(legend.margin = unit(c(0.25, 0.5, 0.25, 0.25), 'in')) +
  facet_grid( ~ Date) +
  theme(plot.margin = unit(c(0.1, 0.75, 0.1, 0.3), 'in'))
dev.off()
