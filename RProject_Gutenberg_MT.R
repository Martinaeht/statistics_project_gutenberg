library(gutenbergr)
library(tidytext)
library(dplyr)
library(vcd)

### Task 1-2 - import text, count words and bigrams

my_mirror <- "http://mirrors.xmission.com/gutenberg/"

df <-gutenberg_metadata

unique(df$author)[startsWith(unique(df$author), "Martin")]

# gutenberg id 24120 The Hour and the Man, An Historical Romance by Harriet Martineau
# https://www.gutenberg.org/ebooks/24120

gutenberg_works(author == "Martineau, Harriet")

# Project Book:

HourAndMan <- gutenberg_download(24120, mirror = my_mirror)

words_HourAndMan <- unnest_tokens(HourAndMan, words, text)

wordcounter_HourAndMan <- count(words_HourAndMan, words, sort = TRUE)

bigrams_HourAndMan <-unnest_tokens(HourAndMan,words,text, token = "ngrams" , n=2)

bigramcounter_HourAndMan <- count(bigrams_HourAndMan, words, sort = TRUE)


### Task 3-4 - dependence of bigram words

bigramcounter <- bigramcounter_HourAndMan[-1,] # deletes empty row counter NA 

bigramcounter[startsWith(bigramcounter$words,"saint domingo"),]
bigramcounter[startsWith(bigramcounter$words,"saint "),]
bigramcounter[endsWith(bigramcounter$words," domingo"),]

s.d<-bigramcounter[startsWith(bigramcounter$words,"saint domingo"),]$n
s.notd <- sum(bigramcounter[startsWith(bigramcounter$words,"saint "),]$n) - s.d
nots.d <- sum(bigramcounter[endsWith(bigramcounter$words," domingo"),]$n) - s.d
nots.notd<-sum(bigramcounter$n) - s.notd - nots.d - s.d

freq<-matrix(c(s.d, s.notd,nots.d,nots.notd),ncol = 2, byrow = T)

mosaicplot(freq, main = "Mosaic Plot for 'saint domingo'") 

chisq.test(freq)

### additional: test values to visualize data better

s.d_value <- 400  
s.notd_value <- 150 
nots.d_value <- 30 
nots.notd_value <- 5000 

freq_value<-matrix(c(s.d_value, s.notd_value,nots.d_value,nots.notd_value),ncol = 2, byrow = T)

mosaicplot(freq_value, main = "TEST Mosaic Plot for Numerical Values")

chisq.test(freq_value)


### Task 5-6 -  entropy of 1000-word parts 

entropy<-c()
for(i in 0:172)
{
  entr<-words_HourAndMan[(i*1000+1):(i*1000+1000),2]
  char<-unnest_tokens(entr,token,words, token="characters")
  df.char<-as.data.frame(count(char,token,sort=TRUE))
  df.char$relfreq<-df.char$n/sum(df.char$n)
  df.char$ent<-df.char$relfreq*log2(df.char$relfreq)
  entropy<- c(entropy,- sum(df.char$ent))
}

entropy

plot(entropy)


### additional: see how many outliers there are based on estimated values

outliers <- c() 
for (i in entropy) 
{if (i < 4.1 | i >= 4.18) 
  {outliers <- c(outliers, i)}}
outliers


### Task 7-8 - calculate 95 % confidence interval for entropy values 

mean_entropy<- mean(entropy)
sd_entropy <- sd(entropy)

#compute standard error of mean (SEM)

n<- length(entropy)
sem<-sd_entropy/sqrt(n)

#critical value for t 95% confidence interval

t_value<-qt(0.975, df=n-1) # 0.975 for two tailed

margin_of_error<- t_value*sem

# confidence interval 

lower_bound<-mean_entropy - margin_of_error
upper_bound<-mean_entropy + margin_of_error

confidence_interval <- c(lower_bound, upper_bound)
print(paste("95% Confidence Interval: [", lower_bound, ", ", upper_bound, "]"))


### TASK 9 Naive Bayes

# divide text into 4 parts and compute NB for a sentence

19019/4

HourAndMan1<- HourAndMan[1:4754,]
HourAndMan2<- HourAndMan[4755:(2*4754),]
HourAndMan3<- HourAndMan[(2*4754+1):(3*4754),]
HourAndMan4<- HourAndMan[(3*4754+1):19019,]

HourAndMan1_tok<-unnest_tokens(HourAndMan1, words, text)
HourAndMan2_tok<-unnest_tokens(HourAndMan2, words, text)
HourAndMan3_tok<-unnest_tokens(HourAndMan3, words, text)
HourAndMan4_tok<-unnest_tokens(HourAndMan4, words, text)

# chosen sentence from HourAndMan3, "He bowed, and left the room with Monsieur Pascal."

sentence <- "He bowed, and left the room with Monsieur Pascal."
df.sentence <- data.frame(text = sentence)

words_sentence <- unnest_tokens(df.sentence, words, text)

word_counts_sentence <- count(words_sentence, words)


count_word_frequency <- function(words, section_data) {
  word_counts_sentence <- words %>%
    left_join(section_data %>% count(words), by = "words") %>%
    mutate(count = ifelse(is.na(n), 0, n)) %>%
    select(words, count)
  
  return(word_counts_sentence)
}

# word frequencies/section

freq_section1 <- count_word_frequency(words_sentence, HourAndMan1_tok)
freq_section2 <- count_word_frequency(words_sentence, HourAndMan2_tok)
freq_section3 <- count_word_frequency(words_sentence, HourAndMan3_tok)
freq_section4 <- count_word_frequency(words_sentence, HourAndMan4_tok)

# merge frequencies

all_freq <- full_join(freq_section1, freq_section2, by = "words", suffix = c("_1", "_2")) %>%
  full_join(freq_section3, by = "words", suffix = c("", "_3")) %>%
  full_join(freq_section4, by = "words", suffix = c("", "_4"))

all_freq

### Naive Bayes

compute_probabilities <- function(word_counts, section_data, section_length) {
  total_words <- sum(section_data %>% count(words) %>% pull(n))
  probabilities <- word_counts %>%
    mutate(prob = (count + 1) / (total_words + section_length)) %>%  # Laplace smoothing
    pull(prob)
  
  return(prod(probabilities))
}


section_lengths <- c(nrow(HourAndMan1), nrow(HourAndMan2), nrow(HourAndMan3), nrow(HourAndMan4))

# probabilities/section

prob_section1 <- compute_probabilities(freq_section1, HourAndMan1_tok, section_lengths[1])
prob_section2 <- compute_probabilities(freq_section2, HourAndMan2_tok, section_lengths[2])
prob_section3 <- compute_probabilities(freq_section3, HourAndMan3_tok, section_lengths[3])
prob_section4 <- compute_probabilities(freq_section4, HourAndMan4_tok, section_lengths[4])

# combine

probabilities <- data.frame(
  Section = c("HourAndMan1", "HourAndMan2", "HourAndMan3", "HourAndMan4"),
  Probability = c(prob_section1, prob_section2, prob_section3, prob_section4)
)

probabilities


classified_section <- probabilities %>%
  arrange(desc(Probability)) %>%
  slice(1) %>%
  pull(Section)

print(paste("The sentence is most likely in:", classified_section))


### additional: NB with bigrams: 

HourAndMan1_tok2 <- unnest_tokens(HourAndMan1, bigrams, text, token = "ngrams", n = 2)
HourAndMan2_tok2 <- unnest_tokens(HourAndMan2, bigrams, text, token = "ngrams", n = 2)
HourAndMan3_tok2 <- unnest_tokens(HourAndMan3, bigrams, text, token = "ngrams", n = 2)
HourAndMan4_tok2 <- unnest_tokens(HourAndMan4, bigrams, text, token = "ngrams", n = 2)

# Sentence with bigrams

words_sentence2 <- unnest_tokens(df.sentence, bigrams, text, token = "ngrams", n = 2)

word_counts_sentence2 <- count(words_sentence2, bigrams)

count_bigram_frequency <- function(bigrams, section_data) {
  word_counts_sentence2 <- bigrams %>%
    left_join(section_data %>% count(bigrams), by = "bigrams") %>%
    mutate(count = ifelse(is.na(n), 0, n)) %>%
    select(bigrams, count)
  
  return(word_counts_sentence2)
}


freq_bi_section1 <- count_bigram_frequency(words_sentence2, HourAndMan1_tok2)
freq_bi_section2 <- count_bigram_frequency(words_sentence2, HourAndMan2_tok2)
freq_bi_section3 <- count_bigram_frequency(words_sentence2, HourAndMan3_tok2)
freq_bi_section4 <- count_bigram_frequency(words_sentence2, HourAndMan4_tok2)

all_freq2 <- full_join(freq_bi_section1, freq_bi_section2, by = "bigrams", suffix = c("_1", "_2")) %>%
  full_join(freq_bi_section3, by = "bigrams", suffix = c("", "_3")) %>%
  full_join(freq_bi_section4, by = "bigrams", suffix = c("", "_4"))

all_freq2


compute_bigram_probabilities <- function(word_counts, section_data, section_length) {
  total_words <- sum(section_data %>% count(bigrams) %>% pull(n))
  probabilities2 <- word_counts %>%
    mutate(prob = (count + 1) / (total_words + section_length)) %>%  # Laplace smoothing
    pull(prob)
  
  return(prod(probabilities2))
}


section_lengths2 <- c(nrow(HourAndMan1_tok2), nrow(HourAndMan2_tok2), nrow(HourAndMan3_tok2), nrow(HourAndMan4_tok2))

prob_bi_section1 <- compute_bigram_probabilities(freq_bi_section1, HourAndMan1_tok2, section_lengths2[1])
prob_bi_section2 <- compute_bigram_probabilities(freq_bi_section2, HourAndMan2_tok2, section_lengths2[2])
prob_bi_section3 <- compute_bigram_probabilities(freq_bi_section3, HourAndMan3_tok2, section_lengths2[3])
prob_bi_section4 <- compute_bigram_probabilities(freq_bi_section4, HourAndMan4_tok2, section_lengths2[4])

probabilities2 <- data.frame(
  Section = c("HourAndMan1", "HourAndMan2", "HourAndMan3", "HourAndMan4"),
  Probability = c(prob_bi_section1, prob_bi_section2, prob_bi_section3, prob_bi_section4)
)

probabilities2

classified_section2 <- probabilities2 %>%
  arrange(desc(Probability)) %>%
  slice(1) %>%
  pull(Section)

print(paste("Using bigrams, the sentence is most likely in:", classified_section2))
