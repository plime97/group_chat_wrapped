# Custom inputs:

folder_name_with_htmls <- "boys chat 2024"

# Note - the below is only needed if the amassing of names is incomplete -
# this will occur in chats with many participants only, or I believe also when
# chat members have left the chat during the period. Please check the
# 'individual_names' variable to see if additional names need to be added.

names_additional <- c("Joseph Smith")

# Nickname coding - while the program is made to ignore capitalisation by parsing
# all data in lowercase and only look for first name usage, it does need extra 
# help  to detect nicknames. This can only be done manually, based on understood 
# nicknames within yor chat. E.g. we call Nicolas 'nick' and so that is one of 
# the nickname codes. Use the full name as appearing on Facebook / left-hand of
# the names_lookup frame for proper attribution.
  
nickname_coding <- c("joe" = "Joseph Smith",
                     "katie" = "Kate Bloggs")


# Relative weighings of mentions? Depending on how you view it, a mention 
# (someone mentioning another person's name in chat) can be worth more than a
# react on their comment within chat. If you want this weighting, then weight
# mentions higher than 1 (e.g. 2 or more). At its base, we keep mentions and
# reacts at equal weighting in how we determine the 'centre' of the chat.

mentions_weighting <- 1

# Wrapped year? Which year do we want to keep data to? Keep empty if you want all
# html data that you've dropped into the folder to be analysed.

wrapped_year <- 2024

# Exclusions - any people who are in chat but don't use it and we don't want to
# present results for.

exclusions <- c("Jimmy Neutron")

# PART 1: NETWORK ANALYSIS ---- ################################################

# Package installation

if(!require("pacman")) install.packages("pacman")

p_load(tidyverse, rvest, xml2, igraph, visNetwork, lubridate, snakecase, 
       tidytext, textdata, openxlsx)

extract_nodes <- function(pg, css) {
  pg %>%
    html_nodes(css) %>%
    html_text()
}

files <- list.files(folder_name_with_htmls)


# First amass all potential names:

for(i in 1:length(files)){
  
  doc <-  paste0(folder_name_with_htmls,"/", files[i])
  
  pg <- doc %>% read_html()
  
  names <- extract_nodes(pg, ":nth-child(3) ._a6-h")
  
  individual_names <- names %>% str_remove("Participants: ") %>% str_split(", ")
  
  individual_names <- individual_names[[1]] %>% str_split(" and ")
  
  individual_names <- unlist(individual_names)
  
  if(i == 1){
    
    individual_names_list <- individual_names
    
  }else{
    
    individual_names_list <- c(individual_names_list,individual_names)
    
  }
  
}

individual_names_list <- c(individual_names, names_additional)

individual_names_list <- individual_names_list[!(individual_names_list %in% exclusions)]


# Then amass all potential likes:

for(i in 1:length(files)){
  
  doc <-  paste0(folder_name_with_htmls,"/", files[i])
  
  pg <- doc %>% read_html()
  
  likes_to_names <- pg %>%
    extract_nodes("._a6-i , li, ._a72d") %>% 
    tibble()
  
  colnames(likes_to_names)[1] <- "names"
  
  likes_to_names <- likes_to_names %>%
    mutate(date = as_date(names, format = "%b %d, %Y %I:%M:%S %p")) %>%
    mutate(year = year(date)) %>%
    fill(year, .direction = "up") %>%
    filter(is.na(date))
  
  
  likes_to_names <- likes_to_names %>%
    mutate(react = ifelse(str_detect(names,"[^[:ascii:]]"),1,0))
  
  likes_to_names <- likes_to_names %>%
    mutate(names_poster = ifelse(react == 0, names, NA)) %>%
    fill(names_poster, .direction = "down")
  
  likes_to_names <- likes_to_names %>%
    mutate(names_reacter = ifelse(react == 1,
                                  str_remove(names,"[^[:ascii:]]"),
                                  0)) %>%
    mutate(names_reacter = str_remove(names_reacter," \\(.+\\)"))
  
  react_frame <- likes_to_names %>%
    filter(react == 1) %>%
    group_by(year, names_reacter, names_poster) %>%
    summarise(number = n())
  
  react_frame <- react_frame %>%
    filter(names_reacter %in% individual_names_list)
  
  if(i == 1){
    
    react_frame_all <- react_frame
    
  }else if(nrow(react_frame) >= 1){
    
    react_frame_all <- react_frame_all %>% bind_rows(react_frame)
    
  }
  
}

react_frame_all <- react_frame_all %>%
  group_by(year, names_reacter, names_poster) %>%
  summarise(number = sum(number))


react_frame_all <- react_frame_all %>%
  ungroup() %>%
  rename(names_to = names_poster, names_from = names_reacter)





# Then amass all potential name drops:

names_lookup <- tibble(names_full = individual_names_list, 
                       names_shortened = individual_names_list %>% str_to_lower() %>%
                         str_extract("\\w*"))

nickname_coding <- data.frame(
  names_full = nickname_coding
) %>%
  rownames_to_column(var = "names_shortened")

names_lookup <- names_lookup %>%
  bind_rows(nickname_coding)


for(i in 1:length(files)){
  
  doc <-  paste0(folder_name_with_htmls,"/", files[i])
  
  pg <- doc %>% read_html()
  
  mentions_to_names_data <- pg %>%
    extract_nodes("._a6-p div:nth-child(2) , ._a6-i, ._a72d") %>%
    tibble()
  
  colnames(mentions_to_names_data)[1] <- "names"
  
  mentions_to_names_data <- mentions_to_names_data %>%
    mutate(date = as_date(names, format = "%b %d, %Y %I:%M:%S %p")) %>%
    mutate(year = year(date)) %>%
    fill(year, .direction = "up") %>%
    filter(is.na(date))
  
  mentions_to_names_data <- mentions_to_names_data %>%
    mutate(names_poster = ifelse(names %in% individual_names_list, names, NA),
           poster_flag = ifelse(!is.na(names_poster),1,0)) %>%
    fill(names_poster, .direction = "down")
  
  mentions_to_names_data <- mentions_to_names_data %>%
    filter(poster_flag == 0)
  
  mentions_to_names_data <- mentions_to_names_data %>%
    mutate(names = str_to_lower(names))
  
  # Note we're careful to only include @'s, isolated names, and names at the start of a sentence.
  
  mentions_to_names_detected <- mentions_to_names_data %>%
    mutate(detection_1 = str_extract_all(names, str_c("\\@",names_lookup$names_shortened, collapse = "|")),
           detection_2 = str_extract_all(names, str_c(" ",names_lookup$names_shortened, ",", collapse = "|")),
           detection_3 = str_extract_all(names, str_c(" ",names_lookup$names_shortened, "$", collapse = "|")),
           detection_4 = str_extract_all(names, str_c("^",names_lookup$names_shortened, " ", collapse = "|")),
           detection_5 = str_extract_all(names, str_c(" ",names_lookup$names_shortened, " ", collapse = "|")))
  
  mentions_to_names_detected <- mentions_to_names_detected %>%
    pivot_longer(detection_1:detection_5, names_to = "method", values_to = "names_shortened")
  
  # UNNEST THE EXTRACTED NAME MENTIONS
  
  mentions_to_names_detected <- mentions_to_names_detected %>%
    unnest(names_shortened)
  
  mentions_to_names_detected <- mentions_to_names_detected %>%
    mutate(names_shortened = to_snake_case(names_shortened))
  
  # COMPILE AND SUMMARISE, BY YEAR
  
  mentions_to_names_detected <- mentions_to_names_detected %>%
    left_join(names_lookup) %>%
    transmute(year, names_to = names_full, names_from = names_poster) %>%
    group_by(year, names_from, names_to) %>%
    summarise(number = n())
  
  
  if(i == 1){
    
    mentions_frame_all <- mentions_to_names_detected
    
  }else if(nrow(mentions_to_names_detected) >= 1){
    
    mentions_frame_all <- mentions_frame_all %>% bind_rows(mentions_to_names_detected)
    
  }
  
}


mentions_frame_all <- mentions_frame_all %>%
  group_by(year, names_from, names_to) %>%
  summarise(number = sum(number))


# Bringing it all together:

mentions_frame_all <- mentions_frame_all %>%
  filter(names_to != names_from)

react_frame_all <- react_frame_all %>%
  filter(names_to != names_from)


full_results <- mentions_frame_all %>%
  mutate(number = number*mentions_weighting) %>%
  bind_rows(react_frame_all) %>%
  filter(year == wrapped_year) %>%
  group_by(names_from, names_to) %>%
  summarise(number = sum(number))  


full_results <- full_results %>%
  mutate(names_from = str_extract(names_from,"^.+(?= )"),
         names_to = str_extract(names_to,"^.+(?= )"))
  

# And then creating the network graph - image

g <- graph_from_data_frame(full_results %>%
                             rename(weight = number),
                           directed = TRUE)


E(g)$weight

ranks <- page_rank(g, weights = E(g)$weight)$vector


png(file = "Outputs/Network plot.png", width = 1200, height = 1200)
plot(g,
     edge.width = 10*E(g)$weight / max(E(g)$weight),
     vertex.size = sqrt(ranks*3000),
     vertex.label.cex = 2,
     vertex.label.family = "sans",
     vertex.color = "steelblue",
     vertex.label.color = "black",
     edge.arrow.size = 0.1)
dev.off()

# And find the network score!

network_score <- data.frame(
  eigen_network_score = eigen_centrality(g)$vector) %>%
  rownames_to_column(var = "names") %>%
  arrange(desc(eigen_network_score))


# Most reactive:

number_reacts_given <- react_frame_all %>%
  filter(year == wrapped_year) %>%
  group_by(names_from) %>%
  summarise(react_count = sum(number)) %>%
  arrange(desc(react_count))


# Most reacts received:

number_reacts_received <- react_frame_all %>%
  filter(year == wrapped_year) %>%
  group_by(names_to) %>%
  summarise(react_count = sum(number)) %>%
  arrange(desc(react_count))

# Most call outs:

number_mentions <- mentions_frame_all %>%
  filter(year == wrapped_year) %>%
  group_by(names_from, names_to) %>%
  summarise(mentions_count = sum(number)) %>%
  arrange(desc(mentions_count))


# PART 2: SENTIMENT ANALYSIS ---- ##############################################


# Amass all words:

for(i in 1:length(files)){
  
  doc <-  paste0(folder_name_with_htmls,"/", files[i])
  
  pg <- doc %>% read_html()
  
  words_said <- pg %>%
    extract_nodes("._a6-i , ._a72d, ._a6-p div:nth-child(2)") %>% 
    tibble()
  
  colnames(words_said)[1] <- "chat"
  
  words_said <- words_said %>%
    filter(!str_detect(chat, "to your message"))
  
  words_said <- words_said %>%
    mutate(date = as_date(chat, format = "%b %d, %Y %I:%M:%S %p")) %>%
    mutate(year = year(date),
           month = month(date)) %>%
    fill(year, .direction = "up") %>%
    fill(month, .direction = "up") %>%
    filter(is.na(date))
  
  words_said <- words_said %>%
    mutate(name_flag = ifelse(chat %in% individual_names_list, 1, 0),
           name_poster = ifelse(name_flag == 1, chat, NA)) %>%
    fill(name_poster, .direction = "down")
  
  words_said <- words_said %>%
    filter(name_flag == 0)
  
  words_said <- words_said %>%
    filter(!is.na(name_poster)) %>%
    select(chat, year, month, name_poster)
  
  if(i == 1){
    
    words_said_all <- words_said
    
  }else if(nrow(react_frame) >= 1){
    
    words_said_all <- words_said_all %>% bind_rows(words_said)
    
  }
  
}

words_said_all <- words_said_all %>%
  group_by(year, month, name_poster) %>%
  unnest_tokens(word, chat, token = "words")

words_said_all <- words_said_all %>% 
  filter(year == wrapped_year)

# Number of words said

number_words_said <- words_said_all %>%
  group_by(name_poster) %>%
  count() %>%
  arrange(desc(n))

total_words_said <- words_said_all %>%
  ungroup() %>%
  count() %>%
  transmute(number_words_total = n,
            number_da_vinci_codes = n/144330)

# Most likes per word said.

words_per_like_received <- number_words_said %>%
  left_join(number_reacts_received, by = c("name_poster" = "names_to")) %>%
  mutate(words_per_each_like_received = n/react_count) %>%
  arrange(words_per_each_like_received)


# Sentiment analysis - although I recommend only looking at results for those 
# with at least 500 or more words said (as identified in the 'number_words_said'
# frame above).

number_words_said_sentiments <- words_said_all %>%
  group_by(name_poster) %>%
  inner_join(get_sentiments("nrc"))

sentiment_proportions <- number_words_said_sentiments %>%
  group_by(name_poster, sentiment) %>%
  count() %>%
  group_by(name_poster) %>%
  filter(!(sentiment %in% c("negative", "positive"))) %>%
  mutate(proportion_sentiment = n/sum(n)) %>%
  select(everything(), -n) %>%
  pivot_wider(names_from = sentiment, values_from = proportion_sentiment)
  

# Chat sentiment through the year:

sentiment_over_time <- number_words_said_sentiments %>%
  group_by(month, sentiment) %>%
  count() %>%
  group_by(month) %>%
  filter(sentiment %in% c("negative", "positive")) %>%
  group_by(month, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n) %>%
  mutate(sentiment_index = positive/negative-1)

sentiment_over_time %>% ggplot(aes(x = month, y = sentiment_index)) +
  geom_col(fill = "darkgreen")


write.xlsx(list( "Words said per person" = number_words_said,
                 "Total words said" = total_words_said,
                 "Network centrality scores" = network_score,
                 "Reacts given per person" = number_reacts_given,
                 "Reacts received per person" = number_reacts_received,
                 "Most person to person mentions" = number_mentions,
                 "Words per like by person" = words_per_like_received,
                 "Sentiment by person" = sentiment_proportions,
                 "Sentiment by month" = sentiment_over_time),
           "Outputs/results_tables.xlsx")
