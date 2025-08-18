library(shiny)
library(tidyverse)
library(DT)
library(shinydashboard)
library(shinyjs)
library(shinythemes)
library(RPostgreSQL)
library(shinyWidgets)
library(wildrtrax)

drv<-dbDriver("PostgreSQL")
pw<-"IronKnee4321"
con<-dbConnect(drv, dbname="wildtrax", host="prod.wildtrax.ca", port="5432", user="abmiviewer", password=pw)

Sys.setenv(WT_USERNAME = "agmacpha@ualberta.ca", WT_PASSWORD = "k1ttyco0zoobad00!!")
wt_auth(force = T)

recs <- RPostgreSQL::dbGetQuery(
  conn = con,
  statement = 'SELECT recording_id, recording_insert_date
               FROM aru.recording') |>
  as_tibble()

imgs <- RPostgreSQL::dbGetQuery(
  conn = con,
  statement = 'SELECT
    date(image_added_on) AS day,
    COUNT(*) AS image_count
FROM camera.image
WHERE image_added_on IS NOT NULL
GROUP BY date(image_added_on)
ORDER BY day') |>
  as_tibble()

rc <- recs %>%
  filter(!is.null(recording_insert_date), !is.na(recording_insert_date)) %>%
  mutate(day = as.Date(recording_insert_date)) %>%
  group_by(day) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(day) %>%                                # ensure chronological order
  mutate(cumulative_count = cumsum(count))        # running total

images <- imgs %>%
group_by(day) %>%
  summarise(count = image_count, .groups = "drop") %>%
  arrange(day) %>%                                # ensure chronological order
  mutate(cumulative_count = cumsum(count))

library(scales)

scale_factor <- 0.01  # adjust to fit camera data to audio y-axis scale

p <- ggplot() +
  geom_line(data = rc, aes(x = day, y = cumulative_count), color = "#6E6E6E", linewidth = 2) +
  geom_point(data = rc, aes(x = day, y = cumulative_count), color = "#6E6E6E", size = 4) +
  geom_line(data = images, aes(x = day, y = cumulative_count * scale_factor), color = "#C42E2E", linewidth = 2) +
  geom_point(data = images, aes(x = day, y = cumulative_count * scale_factor), color = "#C42E2E", size = 4) +
  scale_y_continuous(
    name = "Audio cumulative count (Millions of recordings)",
    labels = label_number(scale = 1e-6, suffix = "M", accuracy = 1),
    sec.axis = sec_axis(~ . / scale_factor, name = "Image cumulative count (Millions of images)", labels = label_number(scale = 1e-6, suffix = "M", accuracy = 1))
  ) +
  labs(x = "Date") +
  theme_bw() +
  theme(
    axis.title.y.left = element_text(color = "#6E6E6E", size = 16, face = "bold"),
    axis.title.y.right = element_text(color = "#C42E2E", size = 16, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 14)
  ) +
  transition_reveal(day)

anim <- animate(p, fps = 20, duration = 10, width = 550, height = 600, renderer = gifski_renderer())
anim_save("wildtrax_cumulative_growth.gif", animation = anim)

