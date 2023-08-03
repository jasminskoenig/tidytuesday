# libraries

library(tidyverse)
library(hrbrthemes)
library(ggtext)
library(wesanderson)
library(waffle)
library(Cairo)
library(showtext)

# data

detectors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-18/detectors.csv') |> 
  mutate(correct = if_else(kind == .pred_class, 1, 0)) 


detectors |> 
  group_by(detector, model, correct) |> 
  count() |> 
  filter(model == "GPT4" & correct == 1) |> 
  arrange(desc(n)) |> 
  pull(detector) ->
  detector_ordered

models_ordered <- c("GPT4", "GPT3", "Human")

detectors |> 
  arrange(correct) |> 
  mutate(best = case_when(
    model == "GPT4" & detector == "OriginalityAI" ~ 1,
    model == "GPT3" & detector == "Sapling" ~ 1,
    model == "Human" & detector == "ZeroGPT" ~ 1,
    TRUE ~ 0
  )) |> 
  group_by(detector, model, correct, best) |> 
  count() |> 
  mutate(detector = factor(detector, detector_ordered),
         model = factor(model, models_ordered),
         correct = paste0(correct, best)) |> 
  arrange(desc(correct)) ->
  detectors_plot

# theme
theme_jass <- theme_void() +
  theme(
    strip.text.y = element_text(size = 11, 
                              family = "rajdhani",
                              hjust = 1),
    strip.text.x = element_text(size = 11, 
                                family = "rajdhani",
                                hjust = 0.5),
    plot.title = element_markdown(size = 15, 
                              family = "rajdhani",
                              margin = margin(5, 0, 5, 0),
                              hjust = -0.007),
    plot.subtitle = element_markdown(size = 12, 
                                     family = "rajdhani",
                                     hjust = 0.01,
                                     margin = margin(0, 0, 5, 0)),
    plot.caption = element_text(size = 9, 
                                family = "rajdhani", 
                                face = "plain", 
                                hjust = 0.01,
                                vjust = 200),
    legend.position = "none",
    legend.key.size = unit(0.5, "cm"),
    plot.margin = grid::unit(c(5, 5, 5, 5), "mm"),
    plot.background = element_rect(fill = "#FBFAF6"),
    text = element_text(family = "rajdhani")
  )

theme_set(theme_jass)

font_add_google(family = "rajdhani", name = "Rajdhani")
showtext_auto()

detectors_plot |> 
  ggplot(aes(fill = correct, 
             values = n,
             alpha = best)) +
  geom_waffle(
    n_rows = 10, 
    size = 1,
    color = "#FBFAF6", 
    make_proportional = TRUE,
    na.rm = TRUE,
    flip = TRUE
  ) +
  facet_grid(model ~ detector,
             switch = "y") +
  
  labs(title = "<span style='color: #263f3f; font-weight: 600;'> **Can Texts Written by AI  be Detected?** </span>",
       subtitle = '<span style="color: #263f3f;"> Each detector was given texts either written by GPT4, GPT3, and humans. The visualization<br> shows the share of texts that each AI detector classified <span style="color:#F1BB7B; font-weight: 600;">**correctly**</span> and <span style="color:#5B1A18;  font-weight: 600;">**wrongly**</span> for each<br>author category. The detector that performed best in classifying the text as humand or AI written is emphasized<br>for each author category. OriginalityAI did the best job detecting texts written by one of the<br>most advanced language models, GPT4, classifying <span style=" font-weight: 600;">42%</span> correctly.</span>',
       caption = 'data: detectors R package | graphics: Jasmin Sarah Koenig')+
  scale_fill_manual(
    values = c(alpha("#F1BB7B", 1/3), "#F1BB7B", alpha("#5B1A18", 1/3), "#5B1A18")
  ) +
  coord_equal() +
  theme_enhance_waffle()

ggsave("plots/2023-07-17.pdf", width = width, height = 13, units = "cm", device = cairo_pdf)




detectors |> 
  ggplot() +
  geom_histogram(aes(x = .pred_AI, 
                     fill =  as.factor(correct),
                     group = correct))

detectors |> 
  group_by(detector) |> 
  count()

detectors |> 
  ggplot() +
  geom_bar(aes(x = detector, fill = as.factor(correct)))

detectors |> 
  ggplot() +
  geom_bar(aes(x = model, fill = as.factor(correct)))

pal <- wes_palette("GrandBudapest1", 100, type = "continuous")

detectors |> 
  group_by(model, detector, correct) |> 
  count() |> 
  ungroup() |> 
  group_by(model, detector) |> 
  mutate(sum = sum(n),
         share = n / sum) ->
  detectors_heat

detectors_heat |> 
  filter(correct == 0) 
  filter(model != "Human") |> 
  ggplot() +
  geom_tile(aes(x = model, 
                y = fct_reorder(detector, 
                                share, 
                                .desc = TRUE), 
                fill = share),
            width = 0.5) +
  scale_fill_gradientn(
    colours = c("#F1BB7B", "#5B1A18"),
    limits = c(0,1)
  ) +
  labs(y = "",
       x = "",
       fill = "Share of correctly predicted texts")

detectors_heat |> 
  filter(correct == 0) |> 
  filter(model == "Human") |> 
  ggplot() +
  geom_tile(aes(x = model, y = fct_reorder(detector, share, .desc = TRUE), fill = share)) +
  scale_fill_gradientn(
    colours = c("#F1BB7B", "#5B1A18"),
    limits = c(0,1)
  ) +
  labs(y = "",
       x = "",
       fill = "Share of correctly predicted texts")


detectors_heat |> 
  mutate(share = share * 100,
         share = ceiling(share)) |> 
  ggplot(aes(fill = correct, values = share)) +
  geom_waffle(
    n_rows = 10,
    size = 1,
    color = "white", 
    flip = TRUE, 
    make_proportional = F
  ) +
  labs(fill = str_wrap("Was the origin of the text correctly predicted?", 30)) +
  coord_equal() +
  theme_void() +
  theme_enhance_waffle()


detectors_plot |> 
  ggplot(aes(fill = correct, 
             values = n,
             alpha = best)) +
  geom_waffle(
    n_rows = 10, 
    size = 1,
    color = "white", 
    make_proportional = TRUE,
    na.rm = TRUE,
    flip = TRUE
  ) +
  facet_grid(model ~ detector,
             switch = "y") +
  labs(title = "<span style='color: #5B3C18; font-weight: 3000'> Can Texts Written by AI  be Detected? </span>",
       subtitle = 'Each detector was given texts either written by GPT4, GPT3, and humans. The visualization shows the share of texts that each AI detector classified <span style="color:#F1BB7B">correctly</span> <br> and <span style="color:#5B1A18">wrongly</span> for each author category. The detector that performed best in classifying the text as humand or AI written is emphasized for each author model.OriginalityAI did the best job detecting texts written by one of the most advanced language models, GPT4, classifying 42% correctly.')+
  scale_fill_manual(
    values = c(alpha("#F1BB7B", 1/3), "#F1BB7B", alpha("#5B1A18", 1/3), "#5B1A18")
    ) +
  coord_equal() +
  theme_enhance_waffle()

ggsave("plots/2023-07-17.pdf", height = 10, width = 15, device = cairo_pdf)
