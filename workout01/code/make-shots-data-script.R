library(dplyr)

# 3 # Data Preparation
### read files
classes <- c('character', 'character','character','integer','integer','integer','character','character','character','integer','character','integer','integer')
iguodala <- read.csv('~/Dropbox/19_1/stat133/workout/workout01/data/andre-iguodala.csv', stringsAsFactors = FALSE, colClasses = classes)
green <- read.csv('~/Dropbox/19_1/stat133/workout/workout01/data/draymond-green.csv', stringsAsFactors = FALSE, colClasses = classes)
durant <-  read.csv('~/Dropbox/19_1/stat133/workout/workout01/data/kevin-durant.csv', stringsAsFactors = FALSE, colClasses = classes)
thompson <-  read.csv('~/Dropbox/19_1/stat133/workout/workout01/data/klay-thompson.csv', stringsAsFactors = FALSE, colClasses = classes)
curry <-  read.csv('~/Dropbox/19_1/stat133/workout/workout01/data/stephen-curry.csv', stringsAsFactors = FALSE, colClasses = classes)

### shot_made_flag
# iguodala
boolen_y <- iguodala$shot_made_flag == 'y'
boolen_n <- iguodala$shot_made_flag == 'n'

iguodala$shot_made_flag[boolen_y] <- 'shot_yes'
iguodala$shot_made_flag[boolen_n] <- 'shot_no'

unique(iguodala$shot_made_flag)

# green
boolen_y <- green$shot_made_flag == 'y'
boolen_n <- green$shot_made_flag == 'n'

green$shot_made_flag[boolen_y] <- 'shot_yes'
green$shot_made_flag[boolen_n] <- 'shot_no'

unique(green$shot_made_flag)

# durant
boolen_y <- durant$shot_made_flag == 'y'
boolen_n <- durant$shot_made_flag == 'n'

durant$shot_made_flag[boolen_y] <- 'shot_yes'
durant$shot_made_flag[boolen_n] <- 'shot_no'

unique(durant$shot_made_flag)

# thompson
boolen_y <- thompson$shot_made_flag == 'y'
boolen_n <- thompson$shot_made_flag == 'n'

thompson$shot_made_flag[boolen_y] <- 'shot_yes'
thompson$shot_made_flag[boolen_n] <- 'shot_no'

unique(thompson$shot_made_flag)

# curry
boolen_y <- curry$shot_made_flag == 'y'
boolen_n <- curry$shot_made_flag == 'n'

curry$shot_made_flag[boolen_y] <- 'shot_yes'
curry$shot_made_flag[boolen_n] <- 'shot_no'

unique(curry$shot_made_flag)

### minute
iguodala <- mutate(iguodala, minute = 12*iguodala$period - iguodala$minutes_remaining)
green <- mutate(green, minute = 12*green$period - green$minutes_remaining)
durant <- mutate(durant, minute = 12*durant$period - durant$minutes_remaining)
thompson <- mutate(thompson, minute = 12*thompson$period - thompson$minutes_remaining)
curry <- mutate(curry, minute = 12*curry$period - curry$minutes_remaining)

### summary
sink(file = '~/Dropbox/19_1/stat133/workout/workout01/output/andre-iguodala-summary.txt')
summary(iguodala)
sink()

sink(file = '~/Dropbox/19_1/stat133/workout/workout01/output/draymond-green-summary.txt')
summary(green)
sink()

sink(file = '~/Dropbox/19_1/stat133/workout/workout01/output/kevin-durant-summary.txt')
summary(durant)
sink()

sink(file = '~/Dropbox/19_1/stat133/workout/workout01/output/klay-thompson-summary.txt')
summary(thompson)
sink()

sink(file = '~/Dropbox/19_1/stat133/workout/workout01/output/stephen-curry-summary.txt')
summary(curry)
sink()

### rbind
iguodala$name <- 'andre-iguodala'
green$name <- 'draymond-green'
durant$name <- 'kevin-durant'
thompson$name <- 'klay-thompson'
curry$name <- 'stephen-curry'

df <- rbind(iguodala, green, durant, thompson, curry)

write.csv(
  x = df,
  file = '~/Dropbox/19_1/stat133/workout/workout01/data/shots-data.csv'
)

sink(file = '~/Dropbox/19_1/stat133/workout/workout01/output/shots-data-summary.txt')
summary(df)
sink()