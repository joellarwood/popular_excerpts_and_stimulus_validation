
#################################################################
##                 Analysis of 1000ms excerpts                 ##
##         Target Emotions: Happy, Sad, Angry, Relaxed         ##
##                    Analysis: Joel Larwood                   ##
#################################################################


# Load Packages  ----------------------------------------------------------

library(tidyverse)
library(sjPlot)
library(ggeffects)

# Load Processed Data -----------------------------------------------------

data <- here::here(
  "data", 
  "1000ms_processed_data.csv"
) %>% 
  read_csv() %>% 
  rename(
    target_emotion = target_song,
    selected_emotion = emotion
  ) %>% 
  mutate(
    stimulus_song = str_extract(
      string = stimulus_song, pattern = "[[:digit:]]_(.*)"
    ),
    correct_fct = as.factor(
      case_when(
        correct == TRUE ~ "TRUE",
        correct == FALSE ~ "FALSE"
      )
    )
  )

glimpse(data) 


# Get descriptives --------------------------------------------------------

# Proportion correct 

correct_prop <- function(x){
  sum(x, na.rm = TRUE)/nrow(x)
}

v_a_desc <- data %>% 
  group_by(target_emotion) %>% 
  summarise(
    across(
      arousal:valence, 
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE)
    )
  )
  )

correct_desc <- data %>% 
  group_by(target_emotion) %>% 
  summarise(
    proportion_correct = sum(correct)/n()
  )

left_join(
  v_a_desc,
  correct_desc
) %>% 
  ggplot2::ggplot(
    aes(
      x = valence_mean,
      y = arousal_mean,
      label = glue::glue("{stringr::str_to_upper(target_emotion)} \n Correctly identified in {round(proportion_correct * 100, 2)}% of presentations")
    )
  ) +
  geom_point() +
  ggrepel::geom_label_repel(point.padding =  1) +
  scale_y_continuous(name = "Mean Arousal Rating", limits = c(1, 7),  breaks = seq(1, 7, by = 1)) +
  scale_x_continuous(name = "Mean Valence Rating", limits = c(1, 7),  breaks = seq(1, 7, by = 1)) +
  theme_classic()


# Model Valence as GLMM ---------------------------------------------------

valence_glmm <- lmerTest::lmer(valence ~ target_emotion + (target_emotion | pid) + (1 | stimulus_song),
                               data = data)

anova(valence_glmm, type = 3)

valence_pairs <- emmeans::emmeans(valence_glmm, pairwise ~ target_emotion, infer = TRUE)

valence_pairs
# Model Arousal as GLMM ---------------------------------------------------

arousal_glmm <- lmerTest::lmer(arousal ~ target_emotion + (target_emotion | pid) + (1 | stimulus_song),
                               data = data)

anova(arousal_glmm, type = 3)

arousal_pairs <- emmeans::emmeans(arousal_glmm, pairwise ~ target_emotion, infer = TRUE)

arousal_pairs



# Get confusion matrix ----------------------------------------------------

confusion <- table(data$stimulus_song, data$correct)
confusion

#What predicts a relaxing song being cirrectly identifed


relax_model <- lme4::glmer(correct_fct~valence * arousal + (1 |pid) + (1 |stimulus_song),
                           data = filter(data, target_emotion == "relaxed"), 
                           family = "binomial")

summary(relax_model)

