#utilizing the office dataset to create data visualization

#Theofficedataset
# side-by-side boxplots for episode IMDB rating
boxplot_ratings <- theoffice |> 
  ggplot(aes( x= as.factor (season), y = imdb_rating, fill = as.factor(season))) +
  geom_boxplot() +
  labs(x = "Season", y = "IMDb Rating", title = "IMDb Ratings by Season") +
  scale_color_manual(values=c('skyblue4','orange','pink2', 'purple', 'blue','green','yellow','red'))

boxplot_ratings

#showing the average number of lines per episode Dwight has per season,
#Filter the dataset for all of Dwight’s lines
dwight_lines <- theoffice |>
  filter(character == "Dwight") |>
  group_by(season, episode) |>
  summarize(lines = n()) |>  
  group_by(season) |>
  summarize(avg_lines = mean(lines, na.rm = TRUE))

#Step 2 create the bar chart
barplot_dwight <- dwight_lines |>
  ggplot(aes(x = reorder(as.factor(season), avg_lines), y = avg_lines, fill = as.factor(season)))+
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Season", y = "Average Lines per Episode", title = "Average Number of Lines for Dwight") +
  scale_color_manual(values=c('skyblue4','orange','pink2', 'purple', 'blue','green','yellow','red'))

#Display chart
barplot_dwight

# display the maximum imdb_rating per season
# Summarizing the ratings data
max_ratings <- theoffice |>
  group_by(season) |>
  summarize(max_rating = max(imdb_rating, na.rm = TRUE))

#Step2 Plotting
barplot_max_rating <- max_ratings |>
  ggplot(aes(x = reorder(as.factor(season), max_rating), 
             y = max_rating, fill = as.factor(season))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Season", y = "Maximum IMDb Rating", title = "Maximum IMDb Rating per Season")+
  scale_color_manual(values=c('skyblue4','orange','pink2', 'purple', 'blue','green','yellow','red'))

#Displaying the chart
barplot_max_rating


#combining all the plots into one page using patchwork
#Step1 clean up the graphs
#Removed the title and removed the legend
boxplot_ratings <- theoffice |> 
  ggplot(aes( x= as.factor (season), y = imdb_rating, fill = as.factor(season))) +
  geom_boxplot() +
  labs(x = "Season", y = "IMDb Rating") +
  scale_color_manual(values=c('skyblue4','orange','pink2', 'purple', 'blue','green','yellow','red'))

boxplot_ratings

barplot_dwight <- dwight_lines |>
  ggplot(aes(x = reorder(as.factor(season), avg_lines), y = avg_lines, fill = as.factor(season)))+
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Season", y = "Average Lines per Episode") +
  scale_color_manual(values=c('skyblue4','orange','pink2', 'purple', 'blue','green','yellow','red'))

barplot_max_rating <- max_ratings |>
  ggplot(aes(x = reorder(as.factor(season), max_rating), 
             y = max_rating, fill = as.factor(season))) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Season", y = "Maximum IMDb Rating")+
  scale_color_manual(values=c('skyblue4','orange','pink2', 'purple', 'blue','green','yellow','red'))

boxplot_ratings <- boxplot_ratings + guides(fill='none')
boxplot_ratings

barplot_dwight <- barplot_dwight + guides(fill='none')
barplot_dwight

barplot_max_rating <- barplot_max_rating + guides(fill='none')
barplot_max_rating
combined_plot <- (boxplot_ratings / (barplot_dwight | barplot_max_rating))
combined_plot + plot_layout(guides = 'collect') & theme(legend.position = 'top')

combined_plot <- (boxplot_ratings / (barplot_dwight | barplot_max_rating)) +
  plot_annotation(
    title='Visualization of all 3 plots',
    tag_levels = "A",
    tag_prefix = "(",
    tag_suffix = ")"
  ) & 
  theme(legend.position = "top")


# Displaying the plot
combined_plot

#Creating an animated plot

animated_plot <- minor_characters |>
  ggplot(aes(x = reorder(character, avg_lines), y = avg_lines, fill = character)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Character", 
       y = "Average Lines per Episode", 
       title = "Average Number of Lines per Episode by Character: Season {closest_state}") +
  theme_minimal() +
  transition_states(season, transition_length = 2, state_length = 1) +  
  ease_aes('cubic-in-out')

#Creating the animated gif
animate(animated_plot, nframes = 150, fps = 10, width = 600, height = 400, renderer = gifski_renderer("minor_characters_lines.gif"))