library(ggplot2)
library(jpeg)
library(grid)

# 4 # Shot Charts
### Shot charts of each player
court_file <- '~/Dropbox/19_1/stat133/workout/workout01/images/nba-court.jpg'
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, 'npc'),
  height = unit(1, "npc")
)

pdf(file = '~/Dropbox/19_1/stat133/workout/workout01/images/andre-iguodala-shot-chart.pdf', width = 6.5, height = 5)
iguodala_shot_chart <-  ggplot(data = iguodala, aes(x = x, y = y, color = shot_made_flag)) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point() +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Andre Iguodala (2016 season)') +
  theme_minimal()
iguodala_shot_chart
dev.off()

pdf(file = '~/Dropbox/19_1/stat133/workout/workout01/images/draymond-green-shot-chart.pdf', width = 6.5, height = 5)
green_shot_chart <-  ggplot(data = green, aes(x = x, y = y, color = shot_made_flag)) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point() +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Draymond Green (2016 season)') +
  theme_minimal()
green_shot_chart
dev.off()

pdf(file = '~/Dropbox/19_1/stat133/workout/workout01/images/kevin-durant-shot-chart.pdf', width = 6.5, height = 5)
durant_shot_chart <-  ggplot(data = durant, aes(x = x, y = y, color = shot_made_flag)) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point() +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Kevin Durant (2016 season)') +
  theme_minimal()
durant_shot_chart
dev.off()

pdf(file = '~/Dropbox/19_1/stat133/workout/workout01/images/klay-thompson-shot-chart.pdf', width = 6.5, height = 5)
thompson_shot_chart <-  ggplot(data = thompson, aes(x = x, y = y, color = shot_made_flag)) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point() +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Klay Thompson (2016 season)') +
  theme_minimal()
thompson_shot_chart
dev.off()

pdf(file = '~/Dropbox/19_1/stat133/workout/workout01/images/stephen-curry-shot-chart.pdf', width = 6.5, height = 5)
curry_shot_chart <-  ggplot(data = curry, aes(x = x, y = y, color = shot_made_flag)) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point() +
  ylim(-50, 420) +
  ggtitle('Shot Chart: Stephen Curry (2016 season)') +
  theme_minimal()
curry_shot_chart
dev.off()

### Facetted Shot Chart
pdf(file = '~/Dropbox/19_1/stat133/workout/workout01/images/gsw-shot-chart.pdf', width = 8, height = 7)
gsw_shot_charts <- ggplot(data = df, aes(x = x, y = y, color = shot_made_flag)) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point() +
  ylim(-50, 420) +
  facet_grid(~name) +
  ggtitle('Shot Charts:GSW(2016 season)')
gsw_shot_charts
dev.off()

png(file = '~/Dropbox/19_1/stat133/workout/workout01/images/gsw-shot-chart.png', width = 8, height = 7)
gsw_shot_charts <- ggplot(data = df, aes(x = x, y = y, color = shot_made_flag)) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point() +
  ylim(-50, 420) +
  facet_grid(~name) +
  ggtitle('Shot Charts:GSW(2016 season)')
gsw_shot_charts
dev.off()

gsw_shot_charts
  