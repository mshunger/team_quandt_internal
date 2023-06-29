### Cleaning ###

# real data is usually quite messy or consists of multiple datasets!
# we have to handle missing data standardize data etc.

# a messier Bands dataset:

name <- c(
  'Wardruna',
  'Skinflower',
  'Behemoth',
  'Konvent',
  'Ultha',
  'Chthonic',
  'Jinjer'
)
country <- c(
  'Norway',
  'Netherlands',
  'Poland',
  'Denmark',
  'Germany',
  'Taiwan',
  'Ukraine'
)
albums <- c(5, NaN, 12, 2, 4, 11, 4)
singles_ep <- c(3, NaN, 12, 0, 4, 14, 3)
splits <- c(1, NaN, 1, 0, 3, 2, NaN)
formed <- c(2003, NaN, 1991, 2015, 2014, 1995, 2009)

bands <- dplyr::tibble(name, country, albums, singles_ep, splits, formed)
# calculate age and productivity
bands <- bands %>% 
  mutate(age = 2023 - formed) %>% 
  mutate(productivity = albums / age)

albums <- read_csv('/Users/ungers/git/team_quandt_internal/01-intro/local/albums.csv')

bands %>% head()
albums %>% head()

# before combining the dataset: delete columns n_reviews & r_percent
albums <- albums %>% select(-n_reviews, -r_percent)

# joining by a common identifier -> bandname
# we mainly use a 'left join', taking one dataframe as the "main" reference
# other joins are 'inner', 'outer' and 'right'
# left_join(a, b, by='cid'): take a, take everything from b that exists in a
# and add it to a where a common id exists, dropping nonfitting info from b

both <- dplyr::left_join(bands, albums, by=join_by(name==band))
both %>% view()
both <- dplyr::rename(both, albumname=name.y)

# check for missing values:
both %>%
  summary()

both %>% 
  filter(is.na(albums) | is.na(formed) | is.na(published))

# exclude these specific bands
both <- both %>% 
  filter(!name %in% c('Skinflower', 'Chthonic'))

# more indiscriminate alternative:
#both %>% 
#  filter(complete.cases(.)) %>% 
#  view()

### simple visualization: light intro to ggplot2 ###

# tidy + ggplot = <3 -> we can pipe both packages together!
both %>% ggplot()

# some more background -> aes = aesthetics
# we only do the basiscs here, but aes alone has extensive options
bands %>% ggplot(
  aes(
    x = formed,
    y = productivity
  )
)

# adding the actual data; scatterplot: geom_point()
bands %>% ggplot(
  aes(
    x = formed,
    y = productivity
  )
) + 
geom_point()

# cumulative line plot of albums over time
both %>% 
  group_by(name) %>% 
  arrange(published) %>% 
  mutate(n_albums = row_number()) %>% 
  ggplot(
    aes(x=published, y=n_albums, color=name)
  ) + geom_line()