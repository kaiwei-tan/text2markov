library(dplyr)
library(stringr)
library(textclean)

# Split text input into separate sentences
split.sentences <- function(documents,
                            sentence_end=TRUE, sentence_mid=FALSE,
                            quotes=FALSE, remove_parenthesis=TRUE,
                            lower_case=FALSE) {
  
  document_split <- documents
  
  # regular, square, or curly brackets
  if (remove_parenthesis) {
    document_split <-
      document_split %>%
      gsub('\\(.*\\)', '', .) %>% # regular brackets
      gsub('\\[.*\\]', '', .) %>% # square brackets
      gsub('\\{.*\\}', '', .) %>% # curly brackets
      unlist() %>%
      str_squish()
  }
  
  # quotation marks that end with a comma, full stop, exclamation, or question
  if (quotes) {
    document_quotes <-
      document_split %>%
      str_extract_all('\\".*\\"') %>%
      unlist() %>%
      str_squish()
    
    document_split <-
      document_split %>%
      strsplit('\\".*\\"', perl=TRUE) %>%
      unlist() %>%
      str_squish()
    
    document_split <- c(document_split, document_quotes)
  }
  
  # full stop, exclamation, question, ellipsis (must be followed by a space)
  if (sentence_end) {
    document_split <-
      document_split %>%
      gsub('\\.{2,}', '.', .) %>% # ellipsis to full stop
      strsplit('(?<=.[.!?][[:space:]])', perl=TRUE) %>%
      unlist() %>%
      str_squish()
  }
  
  # comma, colon, semicolon, dash
  if (sentence_mid) {
    document_split <-
      document_split %>%
      strsplit('(?<=.[,:;-][[:space:]])', perl=TRUE) %>%
      unlist() %>%
      gsub('\\"', '', .) %>%
      str_squish()
  }
  
  return(document_split)
}

split.sentences(test_cases_cleaned[3], sentence_mid=TRUE)

# Separate any punctuation used to end a sentence/phrase
split.end.punctuation <- function(document) {
  return(list(punct = gsub('.*(?=[[:punct:]]$)', '', document, perl=TRUE),
              remainder = gsub('[[:punct:]]$', '', document, perl=TRUE)
  )
  )
}

# Get all possible n-gram to n-gram transition in documents
# sentence_end = 0 : Do not split the text by .!?
# sentence_end = 1 : Split the text by .!? but do not consider them as absorbing states
# sentence_end = 2 : Split the text by .!? and consider them as absorbing states
# sentence_end = 3 : Split the text by .!? but all !? is converted to . and treated as that absorbing state
# sentence_mid = 0 : Do not split the text by ,:;-
# sentence_mid = 1 : Split the text by ,:;- but do not consider them as absorbing states
# sentence_mid = 2 : Split the text by ,:;- and consider them as absorbing states
get.all.transitions <- function(documents, n,
                                sentence_end=2, sentence_mid=0,
                                quotes=FALSE, remove_parenthesis=TRUE,
                                lower_case=FALSE) {
  
  sentence_end_bool = FALSE
  sentence_mid_bool = FALSE
  
  if (sentence_end > 0) {
    sentence_end_bool = TRUE
  }
  
  if (sentence_mid > 0) {
    sentence_mid_bool = TRUE
  }
  
  document_split <-
    documents %>%
    split.sentences(sentence_end=sentence_end_bool, sentence_mid=sentence_mid_bool,
                    quotes=quotes, remove_parenthesis=remove_parenthesis,
                    lower_case=lower_case) # split text according to parameters
  
  documents_states <- c()
  absorbing_states <- c()
  punctuation <- c()
  
  if (sentence_end > 1 | sentence_mid > 1) {
    if (sentence_end > 1) {
      punctuation <- c(punctuation, '.', '!', '?')
    } 
    if (sentence_mid > 1) {
      punctuation <- c(punctuation, ',', ':', ';', '-')
    } 
    
    for (i in 1:length(document_split)) {
      punct <- split.end.punctuation(document_split[i])$punct
      
      if (punct %in% punctuation) {
        absorbing_states[i] <- punct
      }
    }
    
    if (sentence_end == 3) {
      absorbing_states <- absorbing_states %>% gsub('[!?]', '.', .)
    }
  }
  
  documents_states <-
    document_split %>%
    gsub('[.!?,:;-]', '', .) %>%
    str_squish() %>%
    strsplit(' ')
  
  ngram_combinations <- list(start_state=c(), end_state=c(), transition=c())
  
  for (i in 1:length(documents_states)) {
    transitions_count <- length(documents_states[[i]]) - n
    
    # state transitions
    if (transitions_count > 0) {
      for (j in 1:transitions_count) {
        ngram_combinations$start_state <-
          c(ngram_combinations$start_state, paste(documents_states[[i]][j:(j+n-1)], collapse=' '))
        ngram_combinations$end_state <-
          c(ngram_combinations$end_state, paste(documents_states[[i]][(j+1):(j+n)], collapse=' '))
        ngram_combinations$transition <-
          c(ngram_combinations$transition, paste(documents_states[[i]][j:(j+n)], collapse=' '))
      }
    }
    
    # absorbing state for end of sentence/phrase
    # last state in sentence/phrase leads to final punctuation, which is an absorbing state
    if (!is.null(absorbing_states[i])) {
      ngram_last <- tail(documents_states[[i]], n) %>% paste(collapse=' ')
      ngram_combinations$start_state <-
        c(ngram_combinations$start_state, ngram_last)
      ngram_combinations$end_state <-
        c(ngram_combinations$end_state, absorbing_states[i])
      ngram_combinations$transition <-
        c(ngram_combinations$transition, paste(ngram_last, absorbing_states[i], sep=''))
    }
  }
  
  # returns list (start_state, end_state, transition)
  return(ngram_combinations)
}

# Get all possible n-grams in documents
# punct = TRUE : Output punctuation, if considered as states
# punct = FALSE : Do not output punctuation, even if they are considered as states
# sort = TRUE: Sort output in ascending alphabetical order
# sort = FALSE: Do not sort output
# sentence_end = 0 : Do not split the text by .!?
# sentence_end = 1 : Split the text by .!? but do not consider them as absorbing states
# sentence_end = 2 : Split the text by .!? and consider them as absorbing states
# sentence_end = 3 : Split the text by .!? but all !? is converted to . and treated as that absorbing state
# sentence_mid = 0 : Do not split the text by ,:;-
# sentence_mid = 1 : Split the text by ,:;- but do not consider them as absorbing states
# sentence_mid = 2 : Split the text by ,:;- and consider them as absorbing states
get.all.ngrams <- function(documents, n, sort=TRUE,                                
                           sentence_end=2, sentence_mid=0,
                           quotes=FALSE, remove_parenthesis=TRUE,
                           lower_case=FALSE) {
  
  document_split <-
    documents %>%
    split.sentences(sentence_end=sentence_end, sentence_mid=sentence_mid,
                    quotes=quotes, remove_parenthesis=remove_parenthesis,
                    lower_case=lower_case) # split text according to parameters
  
  
  punctuation <- c()
  absorbing_states <- c()
  
  if (sentence_end > 1) {
    punctuation <- c(punctuation, '.', '!', '?')
  }
  
  if (sentence_mid > 1) {
    punctuation <- c(punctuation, ',', ':', ';', '-')
  }
  
  # check for punctuation (which would become absorbing states)
  for (symbol in punctuation) {
    for (document in document_split) {
      if (grepl(paste('\\', symbol, sep=''), document)) {
        absorbing_states <- c(absorbing_states, symbol)
        break
      }
    }
  }
  
  if (sentence_end == 3) {
    absorbing_states <- absorbing_states %>% gsub('[!?]', '.', .)
  }
  
  document_split <- 
    document_split %>%
    gsub('(?<![a-zA-Z])[[:punct:]]|[[:punct:]](?![a-zA-Z])', '', ., perl=TRUE) %>%
    strsplit(' ')
  
  all_ngrams <- vector()
  
  for (i in 1:length(document_split)) {
    if (length(document_split[[i]]) >= n) {
      for (j in 1:(length(document_split[[i]])-n+1)) {
        all_ngrams <- c(all_ngrams, paste(document_split[[i]][j:(j+n-1)], collapse=' '))
      }
    }
  }
  
  
  all_ngrams <- c(all_ngrams, absorbing_states) %>% unique()
  
  if (sort) {
    all_ngrams <- all_ngrams %>% sort()
  }
  
  return(all_ngrams)
}

# Create 'long' version of transition matrix (easier for later processing)
get.transition.matrix.long <- function(documents, n, round=FALSE, sort=TRUE,
                                       sentence_end=2, sentence_mid=0,
                                       quotes=FALSE, remove_parenthesis=TRUE,
                                       lower_case=FALSE) {
  
  all_ngrams <-
    get.all.ngrams(documents, n, sort=sort,
                   sentence_end=sentence_end, sentence_mid=sentence_mid,
                   quotes=quotes, remove_parenthesis=remove_parenthesis,
                   lower_case=lower_case)
  
  all_transitions <-
    get.all.transitions(documents, n,
                        sentence_end=sentence_end, sentence_mid=sentence_mid,
                        quotes=quotes, remove_parenthesis=remove_parenthesis,
                        lower_case=lower_case)
  
  transitions_end <-
    all_transitions %>%
    as.data.frame() %>%
    group_by(start_state, end_state) %>%
    summarise(count=n(), .groups='drop')
  
  transitions_start <-
    all_transitions %>%
    as.data.frame() %>%
    group_by(start_state) %>%
    summarise(total=n(), .groups='drop')
  
  transitions_prob <- full_join(transitions_end, transitions_start, by='start_state')
  
  if (round == TRUE) {
    transitions_prob <-
      transitions_prob %>%
      mutate(prob=format(round(count/total, 2), nsmall=2)) %>%
      select('start_state', 'end_state', 'prob')
  } else {
    transitions_prob <-
      transitions_prob %>%
      mutate(prob=count/total) %>%
      select('start_state', 'end_state', 'prob')
  }
  
  # add full stop as absorbing state
  #transitions_prob <- rbind(transitions_prob, data.frame(start_state='.', end_state='.', prob=1))
  
  # add start states that were not accounted for (leads to nowhere, hence absorbing state)
  ngrams_start_missing <- setdiff(all_ngrams, transitions_prob$start_state)
  if (length(ngrams_start_missing) > 0) {
    transitions_prob <- rbind(transitions_prob,
                              data.frame(start_state = ngrams_start_missing,
                                         end_state = ngrams_start_missing,
                                         prob = 1))
  }
  
  # add end states that were not accounted for (no state leads to them, hence absorbing state)
  ngrams_end_missing <- setdiff(all_ngrams, transitions_prob$end_state)
  if (length(ngrams_end_missing) > 0) {
    transitions_prob <- rbind(transitions_prob,
                              data.frame(start_state = rep(ngrams_end_missing),
                                         end_state = ngrams_end_missing,
                                         prob = 0))
  }
  
  return(transitions_prob)
}

get.transition.matrix <- function(documents, n, output_type='data.frame',
                                  round=FALSE, sort=TRUE,
                                  sentence_end=2, sentence_mid=0,
                                  quotes=FALSE, remove_parenthesis=TRUE,
                                  lower_case=FALSE) {
  
  transitions_prob_matrix_df <-
    documents %>%
    get.transition.matrix.long(n, round=round, sort=sort,
                               sentence_end=sentence_end, sentence_mid=sentence_mid,
                               quotes=quotes, remove_parenthesis=remove_parenthesis,
                               lower_case=lower_case) %>%
    reshape2::dcast(start_state ~ end_state, value.var='prob') %>%
    arrange(start_state)
  
  # will generate NAs where zeroes are supposed to be, hence replace them
  transitions_prob_matrix_df[is.na(transitions_prob_matrix_df)] <- 0
  
  if (output_type == 'data.frame') {
    return(transitions_prob_matrix_df)
  }
  
  if (output_type == 'matrix') {
    transitions_prob_matrix <- transitions_prob_matrix_df %>% select(-'start_state') %>% data.matrix()
    rownames(transitions_prob_matrix) <- transitions_prob_matrix_df$start_state
    return(transitions_prob_matrix)
  }
}