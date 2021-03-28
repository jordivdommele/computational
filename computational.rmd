---
title: "Computational Musicology"
author: "Jordi van Dommele"
date: "2/12/2021"
output: 
  flexdashboard::flex_dashboard:
    storyboard: true
    theme: flatly
---

```{r, eval = FALSE}
remotes::install_github('jaburgoyne/compmus')
```

```{r setup}
library(tidyverse)
library(ggdendro)
library(heatmaply)
library(spotifyr)
library(plotly)
library(ggplot2)
library(compmus)
library(knitr)
library(fmsb)
library(dplyr)
Sys.setenv(SPOTIFY_CLIENT_ID = 'f78108b902694ef28f04812d3847084a')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'a4d9fd978a2d42078c67522ddacf7771')

```


### Classification & Clustering

```{r}
metallica__all<-get_playlist_audio_features("metallicaofficial","1asCHnChQHsZ3es6eUomon")


ggplot(data = metallica__all, aes(x = valence, y = energy, color=mode )) +
  geom_jitter() +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  annotate('text', 0.25 / 2, 0.95, label = "Angry", fontface =
             "bold") +
  annotate('text', 1.75 / 2, 0.95, label = "Happy", fontface = "bold") +
  annotate('text', 1.75 / 2, 0.05, label = "Peaceful", fontface =
             "bold") +
  annotate('text', 0.25 / 2, 0.05, label = "Sad", fontface =
             "bold")


```

-------------------------------------

This shows how metallicas music should interact with emotions

### Rhythmical analysis of the working from home playlists; is there correlation?
```{r}
Metallica_Master<- get_playlist_audio_features("1126158837", "0jxWWZBdHiWEHZgmpXikyr")
Metallica_BLack_album <- get_playlist_audio_features("1126158837", "1ozYFVIwA3JFZShCiqn606")
MEtallica_average <-
  Metallica_Master %>%
  mutate(country = "First Album") %>%
  bind_rows(Metallica_BLack_album %>% mutate(country = "Most Popular")) %>%
  mutate(
    country = fct_relevel(country,  "Master of Puppets ", " Black Album")
  )


february_dip <-
  MEtallica_average %>%
  ggplot(                          # Set up the plot.
    aes(
      x = valence,
      y = energy,
      size = track.popularity,
      colour = danceability,
      label = track.name           # Labels will be interactively visible.
    )
  ) +
  geom_point() +                   # Scatter plot.
  geom_rug(size = 0.1) +           # Add 'fringes' to show data distribution.
  facet_wrap(~country) +           # Separate charts per country.
  scale_x_continuous(              # Fine-tune the x axis.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),        # Use grid-lines for quadrants only.
    minor_breaks = NULL            # Remove 'minor' grid-lines.
  ) +
  scale_y_continuous(              # Fine-tune the y axis in the same way.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),
    minor_breaks = NULL
  ) +
  scale_colour_viridis_c(          # Use the cividis palette
    option = "E",                  # Qualitative set.
    alpha = 0.8,                   # Include some transparency
    guide = "none"
  ) +
  scale_size_continuous(           # Fine-tune the sizes of each point.
    guide =                  # Remove the legend for size.
  ) +
  theme_light() +                  # Use a simpler theme.
  labs(                            # Make the titles nice.
    x = "Valence",
    y = "Energy"
  )
ggplotly(february_dip)
```
###  tempo 

```{r}
 Metallica_Master<- get_playlist_audio_features("1126158837", "0jxWWZBdHiWEHZgmpXikyr")
Metallica_BLack_album <- get_playlist_audio_features("1126158837", "1ozYFVIwA3JFZShCiqn606")
awards <-
  bind_rows(
    Metallica_Master %>% mutate(category= "master" ),
    Metallica_BLack_album%>% mutate(category = "Black"))
awards %>%
  ggplot(aes(x = category, y = tempo))+
  geom_boxplot()     
                     
Master <-
  get_playlist_audio_features(
    "1126158837",
    "0jxWWZBdHiWEHZgmpXikyr"
  ) %>%
  slice(1:30) %>%
  add_audio_analysis()
Black  <-
  get_playlist_audio_features(
    "1126158837",
    "1ozYFVIwA3JFZShCiqn606"
  ) %>%
  slice(1:30) %>%
  add_audio_analysis()
Metallica <-
  Master %>%
  mutate(genre = "Master") %>%
  bind_rows(Black %>% mutate(genre = "Black"))

Metallica %>%
  mutate(
    sections =
      map(
        sections,                                    # sections or segments
        summarise_at,
        vars(tempo, loudness, duration),             # features of interest
        list(section_mean = mean, section_sd = sd)   # aggregation functions
      )
  ) %>%
  unnest(sections) %>%
  ggplot(
    aes(
      x = tempo,
      y = tempo_section_sd,
      colour = genre,
      alpha = loudness
    )
  ) +
  geom_point(aes(size = duration / 60)) +
  geom_rug() +
  theme_minimal() +
  ylim(0, 5) +
  labs(
    x = "Mean Tempo (bpm)",
    y = "SD Tempo",
    colour = "Genre",
    size = "Duration (min)",
    alpha = "Volume (dBFS)"
  )

         
```




### 2020: What is the best music to be listening to, working from home?
```{r}

MoP <-
  get_tidy_audio_analysis("2MuWTIM3b0YEAskbeeFE1i") %>% # Change URI.
  compmus_align(bars, segments) %>%                     # Change 
  select(bars) %>%                                      #   in all three
  unnest(bars) %>%                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
        compmus_summarise, pitches,
        method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) %>%
  mutate(
    timbre =
      map(segments,
        compmus_summarise, timbre,
        method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  )
MoP %>%
  compmus_gather_timbre() %>%
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = basis,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  scale_fill_viridis_c() +                              
  theme_classic()

MoP %>%
  compmus_self_similarity(timbre, "manhattan") %>% 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(x = "", y = "")

ES <-
  get_tidy_audio_analysis("5sICkBXVmaCQk5aISGR3x1") %>% # Change URI.
  compmus_align(bars, segments) %>%                     # Change 
  select(bars) %>%                                      #   in all three
  unnest(bars) %>%                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
        compmus_summarise, pitches,
        method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) %>%
  mutate(
    timbre =
      map(segments,
        compmus_summarise, timbre,
        method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  )
ES %>%
  compmus_gather_timbre() %>%
  ggplot(
    aes(
      x = start + duration / 2,
      width = duration,
      y = basis,
      fill = value
    )
  ) +
  geom_tile() +
  labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
  scale_fill_viridis_c() +                              
  theme_classic()

ES %>%
  compmus_self_similarity(timbre, "manhattan") %>% 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  labs(x = "", y = "")





``` 

### What does working from home mean for Spotify? 
```{r}
TTN<- get_tidy_audio_analysis("2VAQuXC01B2eJAEnkjIj7z")


TTN%>%
  tempogram(window_size = 8, hop_size = 1, cyclic = TRUE) %>%
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)", title = "Tempogram Through the Never") +
  theme_classic()







BTY<- get_tidy_audio_analysis("6UB9mShVLbMm0W4e6vud4C")


BTY%>%
  tempogram(window_size = 8, hop_size = 1, cyclic = TRUE) %>%
  ggplot(aes(x = time, y = bpm, fill = power)) +
  geom_raster() +
  scale_fill_viridis_c(guide = "none") +
  labs(x = "Time (s)", y = "Tempo (BPM)", title = "Tempogram Battery") +
  theme_classic()












```

### Tempo throughout the ages 
```{r}

circshift <- function(v, n) {
  if (n == 0) v else c(tail(v, n), head(v, -n))
}

#      C     C#    D     Eb    E     F     F#    G     Ab    A     Bb    B
major_chord <-
  c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    0,    0)
minor_chord <-
  c(   1,    0,    0,    1,    0,    0,    0,    1,    0,    0,    0,    0)
seventh_chord <-
  c(   1,    0,    0,    0,    1,    0,    0,    1,    0,    0,    1,    0)

major_key <-
  c(6.35, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
minor_key <-
  c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)

chord_templates <-
  tribble(
    ~name, ~template,
    "Gb:7", circshift(seventh_chord, 6),
    "Gb:maj", circshift(major_chord, 6),
    "Bb:min", circshift(minor_chord, 10),
    "Db:maj", circshift(major_chord, 1),
    "F:min", circshift(minor_chord, 5),
    "Ab:7", circshift(seventh_chord, 8),
    "Ab:maj", circshift(major_chord, 8),
    "C:min", circshift(minor_chord, 0),
    "Eb:7", circshift(seventh_chord, 3),
    "Eb:maj", circshift(major_chord, 3),
    "G:min", circshift(minor_chord, 7),
    "Bb:7", circshift(seventh_chord, 10),
    "Bb:maj", circshift(major_chord, 10),
    "D:min", circshift(minor_chord, 2),
    "F:7", circshift(seventh_chord, 5),
    "F:maj", circshift(major_chord, 5),
    "A:min", circshift(minor_chord, 9),
    "C:7", circshift(seventh_chord, 0),
    "C:maj", circshift(major_chord, 0),
    "E:min", circshift(minor_chord, 4),
    "G:7", circshift(seventh_chord, 7),
    "G:maj", circshift(major_chord, 7),
    "B:min", circshift(minor_chord, 11),
    "D:7", circshift(seventh_chord, 2),
    "D:maj", circshift(major_chord, 2),
    "F#:min", circshift(minor_chord, 6),
    "A:7", circshift(seventh_chord, 9),
    "A:maj", circshift(major_chord, 9),
    "C#:min", circshift(minor_chord, 1),
    "E:7", circshift(seventh_chord, 4),
    "E:maj", circshift(major_chord, 4),
    "G#:min", circshift(minor_chord, 8),
    "B:7", circshift(seventh_chord, 11),
    "B:maj", circshift(major_chord, 11),
    "D#:min", circshift(minor_chord, 3)
  )

key_templates <-
  tribble(
    ~name, ~template,
    "Gb:maj", circshift(major_key, 6),
    "Bb:min", circshift(minor_key, 10),
    "Db:maj", circshift(major_key, 1),
    "F:min", circshift(minor_key, 5),
    "Ab:maj", circshift(major_key, 8),
    "C:min", circshift(minor_key, 0),
    "Eb:maj", circshift(major_key, 3),
    "G:min", circshift(minor_key, 7),
    "Bb:maj", circshift(major_key, 10),
    "D:min", circshift(minor_key, 2),
    "F:maj", circshift(major_key, 5),
    "A:min", circshift(minor_key, 9),
    "C:maj", circshift(major_key, 0),
    "E:min", circshift(minor_key, 4),
    "G:maj", circshift(major_key, 7),
    "B:min", circshift(minor_key, 11),
    "D:maj", circshift(major_key, 2),
    "F#:min", circshift(minor_key, 6),
    "A:maj", circshift(major_key, 9),
    "C#:min", circshift(minor_key, 1),
    "E:maj", circshift(major_key, 4),
    "G#:min", circshift(minor_key, 8),
    "B:maj", circshift(major_key, 11),
    "D#:min", circshift(minor_key, 3)
  )



Orion<-
  get_tidy_audio_analysis("2HD5BWFthrNf2uFyEhi53d") %>%
  compmus_align(sections, segments) %>%
  select(sections) %>%
  unnest(sections) %>%
  mutate(
    pitches =
      map(segments,
        compmus_summarise, pitches,
        method = "mean", norm = "manhattan"
      )
  )
  Orion %>%
  compmus_match_pitch_template(
    chord_templates,       # Change to chord_templates if descired
    method = "euclidean",  # Try different distance metrics
    norm = "manhattan"     # Try different norms
  ) %>%
  ggplot(
    aes(x = start + duration / 2, width = duration, y = name, fill = d)
  ) +
  geom_tile() +
  scale_fill_viridis_c(guide = "none") +
  theme_minimal() +
  labs(x = "Time (s)", y = "")
  
  MF<-
  get_tidy_audio_analysis("6YwqziI3H71IMishKRTHHg") %>%
  compmus_align(sections, segments) %>%
  select(sections) %>%
  unnest(sections) %>%
  mutate(
    pitches =
      map(segments,
        compmus_summarise, pitches,
        method = "mean", norm = "manhattan"
      )
  )
  MF%>% 
  compmus_match_pitch_template(
chord_templates,        # Change to chord_templates if descired
    method = "euclidean",  # Try different distance metrics
    norm = "manhattan"     # Try different norms
  ) %>%
  ggplot(
    aes(x = start + duration / 2, width = duration, y = name, fill = d)
  ) +
  geom_tile() +
  scale_fill_viridis_c(guide = "none") +
  theme_minimal() +
  labs(x = "Time (s)", y = "")
  
  
  

```

### nog een deel 

