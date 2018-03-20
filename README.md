# spotify_top_songs

---
title: "Analysis of Top Songs of 2017 on Spotify"
author: "Cihan Oklap"
output: 
 html_document:
  toc: true
  code_folding: hide
---

# Introduction

In this kernel, I'll take a look at the audio features of the tracks in **Spotify's Top Songs of 2017** playlist and try to highlight the common patterns behind the audio features of these songs. 

# Read In and Explore the Data
First off, let’s import our data and get an idea of what we’re working with.

```{r read, echo=TRUE, message=FALSE, warning=FALSE}
library(readr)
library(plyr)
library(dplyr)
library(ggplot2)
spotify_data <- read_csv('../input/featuresdf.csv')
```

```{r structure, echo=TRUE, message=FALSE, warning=FALSE}
glimpse(spotify_data)
```

```{r summary, echo=TRUE}
summary(spotify_data)
```

I understand that the data is a tidy one and doesn't contain any NA's. However, for the simplicity, I'll convert `duration_ms` variable in seconds rather than miliseconds; and turn `loudness` from negative to positive since the less the `loudness` level quantitatively means the more loud a song is.

```{r , echo=TRUE, message = FALSE}
spotify_data$duration_ms <- round(spotify_data$duration_ms / 1000)
colnames(spotify_data)[15] <- "duration"
spotify_data$loudness <- - spotify_data$loudness
```

Now, I can start the analysis.

# Data Analysis

## Artists dominating the Top List

Let's determine the artists who have more than one song on the Top 100 Songs List  


```{r , echo=TRUE, message = FALSE}
top_artists <- spotify_data %>%
    group_by(artists)  %>%
    summarise(n_apperance = n()) %>%
    filter(n_apperance > 1) %>%
    arrange(desc(n_apperance))

top_artists$artists <- factor(top_artists$artists, levels = top_artists$artists[order(top_artists$n_apperance)])

ggplot(top_artists, aes(x = artists, y = n_apperance)) +
    geom_bar(stat = "identity",  fill = "tomato2", width = 0.6 ) + 
    labs(title = "Top Artists of 2017", x = "Artists", y = "Number of Apperance on the Top 100") +
    theme(plot.title = element_text(size=15,hjust=-.3,face = "bold"), axis.title = element_text(size=12)) +
    geom_text(aes(label=n_apperance), hjust = 2, size = 3, color = 'white') +
    coord_flip()
```

So, It seems like **Ed Sheeran** and **The Chainsmokers** rocked the 2017 with 4 different songs on the Top list, while **Drake**  and **Martin Garrix** are following them with 3 different songs.

## Correlation between variables
In order to understand the correlation between variables, I'll use `corrplot` function, which is one of the base data visualization functions.

```{r , echo=TRUE, message = FALSE}
library(corrplot)
spotify_data_num <- spotify_data[,-(1:3)]
mtCor <- cor(spotify_data_num)
corrplot(mtCor, method = "ellipse", type = "upper", tl.srt = 45)
```

It seems like `speechiness` and `loudness` are positively correlated with each other. 

Also, `valence` is positively correlated with `danceability` and `energy`. Considering happy songs make people energetic and want to dance, the correlation make a lot sense. 

Interestingly, `energy` and `loudness` are negatively correlated with a really low level. 


## Common features of Top Songs
Let's figure out the variables that are common among the Top Songs List.

### Common Keys

Let's determine the most common keys among Top 100 Songs.

Rather than using the numeric quantities, I'll convert values of `keys` into their original symbols.  

```{r , echo=TRUE, message = FALSE}
spotify_data$key <- as.character(spotify_data$key)
spotify_data$key <- revalue(spotify_data$key, c("0" = "C", "1" = "C♯,D♭", "2" = "D", "3" = "D♯,E♭", "4" = "E", "5" =  "F", "6" = "F♯,G♭","7" = "G","8" = "G♯,A♭","9" = "A","10" = "A♯,B♭","11" = "B"))

song_keys <- spotify_data %>%
    group_by(key) %>%
    summarise(n_key = n()) %>%
    arrange(desc(n_key))
    
song_keys$key <- factor(song_keys$key, levels = song_keys$key[order(song_keys$n_key)])

ggplot(song_keys, aes(x = reorder(key,-n_key), y = n_key, fill = reorder(key,-n_key))) +
    geom_bar(stat = "identity") +
    labs(title = "Distribution of the Keys of Top Songs", x = "Keys", y = "Count of Keys on the Top 100") +
    geom_text(aes(label=n_key), position = position_stack(vjust = 0.8)) +
    theme(plot.title = element_text(size=15,face = "bold"), axis.title = element_text(size=12)) +
    theme(legend.title=element_blank())
```

So, it seems like the most common key among top tracks is **C♯,D♭**; while **D♯,E♭** is the least preferred in the Top Songs list.

### Density Plots of Correlated Variables

We've already determined that `energy`,`valence` and `danceability` are positively correlated; but this time, let's see how these variables are distributed over 100 songs.

```{r , echo=TRUE, message = FALSE}
correlated_density <- ggplot(spotify_data) +
    geom_density(aes(energy, fill ="energy", alpha = 0.1)) + 
    geom_density(aes(valence, fill ="valence", alpha = 0.1)) + 
    geom_density(aes(danceability, fill ="danceability", alpha = 0.1)) + 
    scale_x_continuous(name = "Energy, Valence and Danceability") +
    scale_y_continuous(name = "Density") +
    ggtitle("Density plot of Energy, Valence and Danceability") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          text = element_text(size = 12)) +
    theme(legend.title=element_blank()) +
    scale_fill_brewer(palette="Accent")

correlated_density
```

As it can be seen on the graph, since these variables are positively correlated and have limited between **(0,1)**, the distribution of these variables look similar to each other.

### The More Loud the Less Popular

After we acknowledge the density of `energy` variable, we can guess the density of `loudness` must be low, with the help of Correlation Table above. Let's see how loud the Top 100 Songs of 2017 are.

```{r , echo = TRUE, message = FALSE}
loudness_density <- ggplot(spotify_data) +
    geom_density(aes(loudness, fill ="loudness")) + 
    scale_x_continuous(name = "Loudness") +
    scale_y_continuous(name = "Density") +
    ggtitle("Density plot of Loudness") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face = "bold"),
            text = element_text(size = 12)) +
    theme(legend.title=element_blank()) +
    scale_fill_brewer(palette="Paired")

print(loudness_density)
```

As we've guessed, the Top 100 Songs are mostly not so loud.

## Understanding the Most Important Factor
In order to understand what makes Number 1 song better than the Number 100, I'll add a "standings" column to the dataset, and look into the features that differentiates the best of the top songs lists from the rest of the list.

```{r , echo=TRUE}
library(rpart)
library(rpart.plot)
spotify_data_num$standing <- c(1:100)
tree_model <- rpart(standing ~ ., data = spotify_data_num)
rpart.plot(tree_model, box.palette = "GnBu")
```

That means, the songs of which the key values are less than 3.5 **("C","C♯,D♭","D","D♯,E♭"**), `duration` is more than **204** seconds and `valence` is more than **0.68** have highest chance to be around the top of the Top 100 list; while the songs of which the key values are more than 3.5, `valence` is less than **0.73**, `duration` is less than **244** seconds and `speechiness` is less than **0.05** have the highest chance to be around the bottom of the Top 100 list.


***This is my first analysis and I'm still trying to make it better. That's why, please feel free to give any feedback!***
