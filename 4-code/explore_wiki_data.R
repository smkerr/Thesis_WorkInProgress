# Explore Wikipedia data -------------------------------------------------------

# waxer package ----------------------------------------------------------------
#remotes::install_github("wikimedia/waxer@main")
pacman::p_load(
  dplyr, 
  ggplot2, 
  purrr,
  RccpRoll,
  waxer
)

# page views
pageviews <- wx_page_views(
  project = "en.wikipedia", # TODO: try different languages
  page_name = c("New Year's Eve", "New Year's Day"), # TODO: complete list of page names
  access_method = "all",
  agent_type = "user",
  granularity = "daily",
  start_date = "20220101", # 1 Jan 2022
  end_date = "20231031", # 31 Oct 2023 
  include_redirects = TRUE
)

# page edits
page_edits <- wx_page_edits(
  project = "en.wikipedia", # TODO: try different languages
  page_name = c("New Year's Eve", "New Year's Day"), # TODO: complete list of page names
  start_date = "20220101", # 1 Jan 2022
  end_date = "20221231" # 31 Dec 2022
)

# plot edits
ggplot(page_edits) +
  geom_line(aes(x = date, y = edits, color = page_name)) +
  labs(
    title = "Edits made to English Wikipedia articles on coronavirus",
    x = "Date", y = "Edits per day", color = "Article"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# editing activity
daily_edits <- map_dfr(
  projects,
  wx_project_edits,
  editor_type = "all", page_type = "content",
  start_date = "20200101", end_date = "20201001",
  granularity = "daily",
  .id = "language"
)
daily_editors <- map_dfr(
  projects,
  wx_active_editors,
  editor_type = "all", page_type = "content",
  start_date = "20200101", end_date = "20201001",
  granularity = "daily",
  .id = "language"
)
editing_activity <- daily_edits %>%
  left_join(daily_editors, by = c("project", "language", "date")) %>%
  mutate(edits_per_editor = edits / editors) %>%
  arrange(language, date)

# plot editing activity
editing_activity %>%
  group_by(language) %>%
  mutate(
    rolling_avg = c(
      rep(NA, 3), # first 3 days
      RcppRoll::roll_mean(edits_per_editor, n = 7),
      rep(NA, 3) # last 3 days
    )
  ) %>%
  ungroup %>%
  ggplot(aes(x = date, color = language)) +
  geom_line(aes(y = edits_per_editor), alpha = 0.25) +
  geom_line(aes(y = rolling_avg)) +
  scale_y_continuous(minor_breaks = NULL) +
  scale_x_date(date_labels = "%d %b\n%Y", date_breaks = "2 weeks", minor_breaks = NULL) +
  labs(
    title = "Average article edits per editor",
    x = "Date", y = "Average edits per editor", color = "Wikipedia"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_line(color = "gray90", size = 0.2),
    panel.grid.major.x = element_line(color = "gray90", size = 0.5),
    legend.position = "bottom"
  )

# define projects
projects <- c( # TODO: complete list of languages
  "English" = "en.wikipedia",
  "French" = "fr.wikipedia",
  "Italian" = "it.wikipedia",
  "Spanish" = "es.wikipedia"
)

# page views for multiple projects
project_views <- map_dfr(
  projects, 
  wx_project_views,
  access_method = "desktop", 
  agent_type = "user",
  granularity = "monthly", 
  start_date = "20220101", 
  end_date = "20231031",
  .id = "language"
)

# plot page views for multiple projects
ggplot(project_views) +
  geom_vline(aes(xintercept = as.Date("2022-01-01")), linetype = "dashed") +
  geom_line(aes(x = date, y = views, color = language), size = 0.8) +
  geom_text(
    aes(
      x = as.Date("2022-01-01"), y = 0,
      label = "Automated traffic detection",
      vjust = "bottom", hjust = "left"
    ),
    angle = 90, nudge_x = -10
  ) +
  scale_y_continuous(
    minor_breaks = NULL,
    labels = scales::label_number(scale = 1e-6, suffix = "M")
  ) +
  scale_x_date(date_labels = "%b\n%Y", date_breaks = "3 month", minor_breaks = NULL) +
  labs(
    title = "Monthly Wikipedia user (non-bot) traffic, by language",
    subtitle = "To desktop website",
    x = "Month", y = "Pageviews", color = "Language"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "gray90", size = 0.2),
    panel.grid.major.y = element_line(color = "gray70", size = 0.5),
    legend.position = "bottom"
  )
