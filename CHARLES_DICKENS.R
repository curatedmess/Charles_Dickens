# Charles Dickens - Top 4 downloaded books on Gutenberg (last 100 Days List)
# 4x4 - #1 A Tale of Two Cities 2. Great Expectations 3. A Christmas Carol 4. Oliver Twist
# Portraits created from sample book text from their most popular downloaded book

# load libraries ----------------------------------------------------------
library(gutenbergr)
library(tidyverse)
library(tidytext)
library(imager)
library(showtext)
library(ggnewscale)

# -------------------------------------------------------------------------

# PREPARE FONTS

# -------------------------------------------------------------------------

# add font ----------------------------------------------------------------
font_add_google(name = "Bitter", family = "Bitter")
font_add_google(name = "Rubik", family = "Rubik")

# turn on showtext --------------------------------------------------------
showtext_auto()

# -------------------------------------------------------------------------

# IMAGE DATA FOR CHARLES DICKENS PIC FROM CREATIVE COMMONS

# -------------------------------------------------------------------------

# load image and change to grayscale --------------------------------------
dickens_raw_pic <- load.image("dickens_800x800.png")

dickens_raw_pic_gray <- grayscale(rm.alpha(dickens_raw_pic))

# reduce the number of points
dickens_pic_df <- dickens_raw_pic_gray %>% 
  as.data.frame() %>%
  filter(value > 0) %>% 
  mutate(x = cut(x, round(dim(dickens_raw_pic_gray)[1]/10, 0), labels = FALSE),
         y = cut(y, round(dim(dickens_raw_pic_gray)[2]/10, 0), labels = FALSE)) %>%
  group_by(x, y) %>%
  summarise(value = mean(value)) %>% 
  arrange(y,x)


# replicate 4 times for the 4x4 image -------------------------------------
df_four <- data.frame(sapply(dickens_pic_df, rep.int, times = 4))

# -------------------------------------------------------------------------

# TEXT DATA FOR FOUR CHARLES DICKENS BOOKS

# -------------------------------------------------------------------------

# STEP 1: get book text from project gutenberg ----------------------------
# gutenberg_metadata %>%
#   filter(title == "")

tale_of_two_cities_data <- gutenberg_download(98) #538 downloads
great_expectations_data <- gutenberg_download(1400) #340 downloads
a_christmas_carol_data <- gutenberg_download(46) #217 downloads
oliver_twist_data <- gutenberg_download(74) #201 downloads

# STEP 2: processing steps to clean books ----------------------------------
ttc_clean <- tale_of_two_cities_data %>%
  slice(80:n()) %>% #remove nonsense from the beginning
  filter(nzchar(text)) %>% 
  filter(!grepl('CHAPTER', text))

ge_clean <- great_expectations_data %>%
  slice(83:n()) %>% #remove nonsense from the beginning
  filter(nzchar(text)) %>% 
  filter(!grepl('CHAPTER', text))

cc_clean <- a_christmas_carol_data %>%
  slice(37:n()) %>% #remove nonsense from the beginning
  filter(nzchar(text)) %>% 
  filter(!grepl('STAVE', text))

ot_clean <- oliver_twist_data %>%
  slice(119:n()) %>% #remove nonsense from the beginning
  filter(nzchar(text)) %>% 
  filter(!grepl('CHAPTER', text))
         
# STEP 3: pull sample of text using sentiment analysis   ------------------
# a bit random and unnecessary, but oh well...

# load lexicon ------------------------------------------------------------
lexicon <- get_sentiments("bing")

# combine clean data frames -----------------------------------------------
clean_df <- rbind(ttc_clean, ge_clean, cc_clean, ot_clean)

# get bing sentiment scores -----------------------------------------------
bing_scores_df <- clean_df %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  inner_join(lexicon) %>%
  count(gutenberg_id, sentiment) %>%
  spread(key = sentiment, value = n)

# create start and end using negative and positive text sentiment ---------
ttc_start <- bing_scores_df %>% 
  filter(gutenberg_id == "98") %>% 
  pull(positive)

ttc_end <- bing_scores_df %>% 
  filter(gutenberg_id == "46") %>% 
  pull(negative)

ge_start <- bing_scores_df %>% 
  filter(gutenberg_id == "1400") %>% 
  pull(positive)

ge_end <- bing_scores_df %>% 
  filter(gutenberg_id == "1400") %>% 
  pull(negative)

cc_start <- bing_scores_df %>% 
  filter(gutenberg_id == "46") %>% 
  pull(positive)

cc_end <- bing_scores_df %>% 
  filter(gutenberg_id == "46") %>% 
  pull(negative)

ot_start <- bing_scores_df %>% 
  filter(gutenberg_id == "74") %>% 
  pull(positive)

ot_end <- bing_scores_df %>% 
  filter(gutenberg_id == "74") %>% 
  pull(negative)


# slices ------------------------------------------------------------------
ttc_random <- ttc_clean %>%
  slice(ttc_start:ttc_end)

ge_random <- ge_clean %>%
  slice(ge_start:ge_end)

cc_random <- cc_clean %>%
  slice(cc_start:cc_end)

ot_random <- ot_clean %>%
  slice(ot_start:ot_end)

# STEP 4: create string from sample of text  ------------------------------
ttc_string<- data.frame(string = paste0(ttc_random$text, collapse = ''))

ge_string<- data.frame(string = paste0(ge_random$text, collapse = ''))

cc_string<- data.frame(string = paste0(cc_random$text, collapse = ''))

ot_string<- data.frame(string = paste0(ot_random$text, collapse = ''))


# STEP 5: split up string into letters ------------------------------------
ttc_letters <- str_split(ttc_string$string, "") 
ttc_df <- data.frame(letters = Reduce(rbind, ttc_letters)) %>% 
  slice(1:3910) %>% 
  mutate(book = paste("98")) %>% 
  mutate(downloads = paste(538)) %>% 
  mutate(title = paste("A Tale of Two Cities"))

ge_letters <- str_split(ge_string$string, "") 
ge_df <- data.frame(letters = Reduce(rbind, ge_letters)) %>% 
  slice(1:3910) %>% 
  mutate(book = paste("1400")) %>% 
  mutate(downloads = paste(340)) %>% 
  mutate(title = paste("Great Expectations"))

cc_letters <- str_split(cc_string$string, "") 
cc_df <- data.frame(letters = Reduce(rbind, cc_letters)) %>% 
  slice(1:3910) %>% 
  mutate(book = paste("46")) %>% 
  mutate(downloads = paste(217)) %>% 
  mutate(title = paste("A Christmas Carol"))

ot_letters <- str_split(ot_string$string, "") 
ot_df <- data.frame(letters = Reduce(rbind, ot_letters)) %>% 
  slice(1:3910) %>% 
  mutate(book = paste("74")) %>% 
  mutate(downloads = paste(201)) %>% 
  mutate(title = paste("Oliver Twist"))

# STEP 6: combine text data frames ----------------------------------------
books_df <- rbind(ttc_df, ge_df, cc_df, ot_df) 

# STEP 7: combine text and pic data frame ---------------------------------
df <- cbind(df_four, books_df)

# -------------------------------------------------------------------------

# PLOT

# -------------------------------------------------------------------------

# create factors for desired order on plot
df$book <- factor(df$book, levels = c("98", "1400", "46", "74"))

# -------------------------------------------------------------------------

# SOME PARAMETERS

# -------------------------------------------------------------------------

# fonts -------------------------------------------------------------------
font <- "Bitter"
font2 <- "Rubik"


# geom_rect backgrounds ---------------------------------------------------
one <- "#1B9CFC"
two <- "#EAB543"
three <- "#58B19F"
four <- "#F97F51"

# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x = x, y = y)) +
  geom_rect(aes(fill = book), xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  scale_fill_manual(values = c(one, two, three, four)) +
  geom_text(aes(label = letters, size = value, color = value), filter(df, book == "98"), family = font) +
  scale_color_continuous(low =  "#2C3A47", high = "#25CCF7") +
  new_scale_color() + #reset color scale image text
  geom_text(aes(label = letters, size = value, color = value), filter(df, book == "1400"), family = font) +
  scale_color_continuous(low = "#2C3A47", high = "#F8EFBA") +
  new_scale_color() + #reset color scale image text
  geom_text(aes(label = letters, size = value, color = value), filter(df, book == "46"), family = font) +
  scale_color_continuous(low = "#2C3A47", high = "#55E6C1") +
  new_scale_color() + #reset color scale image text
  geom_text(aes(label = letters, size = value, color = value), filter(df, book == "74"), family = font) +
  scale_color_continuous(low = "#2C3A47", high = "#FEA47F") +
  geom_text(aes(label = title, x = 3, y = 3), angle = 90, hjust = 1, family = font, color = "#F2F2F2") +
  scale_x_continuous(expand = c(0,0)) +
  scale_size_continuous(range = c(4, 1.5)) +
  scale_y_continuous(expand = c(0,0), trans=scales::reverse_trans()) +
  facet_wrap(~ book, ncol = 2) + 
  coord_cartesian(xlim =c(0, 80), ylim = c(80, -1)) +
  theme_void() +
  theme(text = element_text(size = 12, family = font),
        plot.title = element_text(family = font, size = 68, hjust = 0.5, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font2, size = 12, hjust = 0.5),
        plot.caption = element_text(hjust = 0.5, size = 12, family = font2),
        strip.text = element_blank(),
        panel.spacing = unit(0.25, "cm"),
        plot.margin = unit(c(0.75, 0.75, 0.75, 0.75), "in"),
        legend.position = "none",
        plot.background = element_rect(fill = "#f2f2f2", color = NA),
        panel.background = element_rect(fill = NA, color = NA)) +
  labs(title = "CHARLES DICKENS",
       subtitle = "Text samples from the top four downloaded eBooks over the last 100 days on Project Gutenberg as of 8.21.22\n\n",
       caption = "\n\nImage: Wikimedia Commons • Text: Project Gutenberg • Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("CHARLES_DICKENS_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 10, height = 10, units = "in")



