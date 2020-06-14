#' @title A more intuitive take on R's try function.
#'
#' @description Combines the benefits of try and else into one easy-to-use function.
#' @param expr The expression to be attempted.
#' @param el The return if expr fails. Can be a value or alternate function.
#' @param return_error Should the error message of a failed expr be returned? Defaults to FALSE.
#' @keywords try, else, ifelse
#' @export
#' @examples
#' bad_fun <- function() log("potato")
#' tryel(bad_fun(), 100)
#' good_fun <- function() log(10)
#' tryel(bad_fun(), good_fun())
tryel <- function(expr, el, return_error = FALSE){
  expr <- try(expr, silent = TRUE)
  if(inherits(expr, "try-error")){
    if(return_error){
      err_attr <- attributes(expr)$condition
      return(list("ret" = el, "error" = err_attr))
    } else{ return(el) }
  } else{ return(expr) }
}











#' @title Returns an iterator. Used by prepare_dtm() function.
#'
#' @description Creates iterator object to be passed to make_vectorizer(). Ultimately used by prepare_dtm() to construct a DTM.
#' @param dat_in The data.frame or tibble input.
#' @param text_var_name The name of the column in dat_in containing the text source.
#' @param id_var_name The name of the column in dat_in containing the id source.
#' @param see_verbose Akin to verbose. Defaults to TRUE.
#' @keywords prepare_dtm, make_vectorizer
#' @export
#' @examples
#' iter_ret <- product_review_df %>% make_iterator("product_review", "product_id")
make_iterator <- function(dat_in, text_var_name, id_var_name, see_verbose = TRUE){
  if(see_verbose){
    cat("\nCreating Iterator...\n")
  }
  iter_out <- dat_in[[text_var_name]] %>%
    stringr::str_trim() %>%
    tolower() %>%
    text2vec::word_tokenizer() %>%
    text2vec::itoken(ids = dat_in[[id_var_name]])
  return(iter_out)
}










#' @title Returns a vectorizer. Used by prepare_dtm() function.
#'
#' @description Takes in iterator from make_iterator() and returns a vectorizer. Ultimately used by prepare_dtm() to construct a DTM.
#' @param iterator_in The iterator output from make_iterator().
#' @param stop_word_list A list of stopwords.
#' @param n_gram_min Minimum n-gram for vocabulary.
#' @param n_gram_max Maximum n-gram for vocabulary.
#' @param see_verbose Akin to verbose. Defaults to TRUE.
#' @keywords prepare_dtm, make_iterator
#' @export
#' @examples
#' vectorizer_ret <- iter_ret %>% make_vectorizer()
#' vectorizer_ret <- iter_ret %>% make_vectorizer(stop_word_list = my_stopword_list)
make_vectorizer <- function(iterator_in, stop_word_list, n_gram_min, n_gram_max, see_verbose = TRUE){
  if(see_verbose){
    cat("\nCreating Vectorizer...\n")
  }
  vocab <- text2vec::create_vocabulary(
    it = iterator_in,
    ngram = c(ngram_min = n_gram_min,
              ngram_max = n_gram_max),
    stopwords = stop_word_list
  )
  vectorizer <- text2vec::vocab_vectorizer(vocab)
  return(vectorizer)
}










#' @title Create a document-term-matrix (DTM) for NLP analysis and modeling.
#'
#' @description Allows for creation of a document-term-matrix (DTM), returned in sparse matrix format.
#' @param dat_in The data.frame or tibble input data.
#' @param text_col_name The name of the column in dat_in containing the text source.
#' @param return_vectorizer Should the created vectorizer be returned? Only set this to TRUE if input data is TRAIN data, otherwise keep FALSE (default).
#' @param use_vectorizer The vectorizer to be used. Only provide arg if input data is TEST data, otherwise keep NULL (default).
#' @param stopword_list A list of stopwords.
#' @param vect_n_gram_min Minimum n-gram for vocabulary.
#' @param vect_n_gram_max Maximum n-gram for vocabulary.
#' @param see_verbose Akin to verbose. Defaults to TRUE.
#' @keywords make_iterator, make_vectorizer
#' @export
#' @examples
#' dtm_train_vect <- train_ %>% prepare_dtm("product_review", return_vectorizer = TRUE)
#' dtm_train <- dtm_train_vect$"dtm_obj"
#' dtm_test <- test_ %>% prepare_dtm("product_review", use_vectorizer = dtm_train_vect$"vectorizer_out")
prepare_dtm <- function(dat_in, text_col_name,
                        return_vectorizer = FALSE, use_vectorizer = NULL,
                        stopword_list = c("the", "and", "etc"),
                        vect_n_gram_min = 1, vect_n_gram_max = 1,
                        see_verbose = TRUE){
  #*# Dep:
  `%ni%` <- function(x, table){
    !match(x, table, nomatch = 0) > 0
  }
  #*# DATA CHECKS:
  if(text_col_name %ni% colnames(dat_in)){
    stop("Please provide an existing Text Column Name to arg 'text_col_name'",
         call. = FALSE)
  }
  if(is.null(stopword_list)){
    stopword_list <- c("")
  }
  ### Create 'id' variable and keep only necessary columns:
  dat_in$"id" <- 1:nrow(dat_in)
  dat_tmp <- dat_in[, c("id", text_col_name)]
  ### Iterator and Vectorizer:
  ## Iterator:
  iter_ret <- dat_tmp %>%
    make_iterator(
      text_var_name = text_col_name,
      id_var_name = "id"
    )
  ## Vectorizer:
  if(is.null(use_vectorizer)){
    ## If no vectorizer is provided ('use_vectorizer = NULL'), creates
    ## a vectorizer to be used and/or returned. This is the case when
    ## creating a DTM from TRAINING data.
    if(see_verbose){
      cat("No vectorizer provided (this is most likely Training data).\n")
    }
    vectorizer_out <- iter_ret %>%
      make_vectorizer(
        stop_word_list = stopword_list,
        n_gram_min = vect_n_gram_min,
        n_gram_max = vect_n_gram_max
      )
  } else{
    ## If vectorizer is provided, uses the provided vectorizer and
    ## does not create a new one. This is the mandatory case when
    ## creating a DTM from TEST data.
    if(see_verbose){
      cat("Using provided vectorizer (this is most likely Test data).\n")
    }
    vectorizer_out <- use_vectorizer
  }
  ### DTM object:
  if(see_verbose){
    cat("Creating DTM object...\n")
  }
  dtm_obj <- text2vec::create_dtm(it = iter_ret, vectorizer = vectorizer_out)
  ### RETURN:
  if(return_vectorizer){
    if(see_verbose){
      cat("\nReturning dtm object and vectorizer.\n")
    }
    return(list("dtm_obj" = dtm_obj, "vectorizer_out" = vectorizer_out))
  } else{
    if(see_verbose){
      cat("\nReturning dtm object only.\n")
    }
    return(dtm_obj)
  }
}










#' @title NLP modeling using xgboost.
#'
#' @description Allows for modeling of Natural Language Processing (NLP) data using the xgboost package. Default param values are optimized for NLP.
#' @param train_data The NON-DTM training input data.
#' @param dtm_train_data The DTM training input data. Returned from prepare_dtm().
#' @param response_label_name The name of the column in dat_in containing the text source.
#' @param xgb_objective The xgboost parameter for objective (see xgboost documentation). One of "reg:linear", "binary:logistic", etc.
#' @param xgb_nrounds The xgboost parameter for nrounds (see xgboost documentation). Defaults to 100.
#' @param xgb_max_depth The xgboost parameter for max_depth (see xgboost documentation). Defaults to 20.
#' @param xgb_eta The xgboost parameter for eta (see xgboost documentation). Defaults to 0.1.
#' @param xgb_nthread The xgboost parameter for nthreads (see xgboost documentation). Defaults to 4.
#' @param xgb_weight The xgboost parameter for weight (see xgboost documentation). Defaults to NULL.
#' @param xgb_missing The xgboost parameter for missing (see xgboost documentation). Defaults to NA.
#' @keywords prepare_dtm
#' @export
#' @examples
#' xgb_mod <- prepare_xgboost_model(train_, dtm_train, "product_review", xgb_objective = "binary:logistic")
prepare_xgboost_model <- function(train_data, dtm_train_data, response_label_name, xgb_objective,
                                  xgb_nrounds = 100, xgb_max_depth = 20, xgb_eta = 0.1, xgb_nthread = 4,
                                  xgb_weight = NULL, xgb_missing = NA){
  xgb_mod_ret <- xgboost::xgboost(
    data = dtm_train_data,
    label = unlist(train_data[response_label_name]),
    missing = xgb_missing,
    weight = xgb_weight,
    params = list(
      max_depth = xgb_max_depth, eta = xgb_eta,
      nthread = xgb_nthread, objective = xgb_objective
    ),
    nrounds = xgb_nrounds,
    early_stopping_rounds = xgb_nrounds/2
  )
  return(xgb_mod_ret)
}










#' @title Tokenization and sentiment analysis.
#'
#' @description Allows for creating one-word tokens, with options for sentiment and stop-word application. A wrapper for tokenizers::tokenize_words().
#' @param df An input data.frame or tibble.
#' @param text_col_name The name of the column containing text to be tokenized.
#' @param stop_word_src The stop-word source. Either a vector of custom stopwords, or one of pre-built "snowball", "stopwords-iso", "misc", or "smart".
#' @param sentiment_src The sentiment source. Ex: "nrc".
#' @param to_lower Should tokens be forced to lowercase? Defaults to TRUE.
#' @param to_strip_punct Should punctuation be removed prior to tokenization? Defaults to TRUE.
#' @param to_strip_numeric Should numeric values be removed prior to tokenization? Defaults to FALSE.
#' @param remove_empty_tokens Should empty-space tokens be removed after tokenization? Defaults to TRUE.
#' @param show_verbose Should verbose be applied? Defaults to FALSE.
#' @keywords tokenize, sentiment
#' @export
#' @examples
#' ori_dat <- data.frame(doc_main = rep(c("Book_A", "Book_B", "Book_C"), each = 10), doc_sub = rep(c("Chp_1", "Chp_2"), each = 5), doc_line = rep(1:10, 3), doc_text = stringr::sentences[1:30], stringsAsFactors = FALSE)
#' ret_nsns <- ori_dat %>% token_eyes("doc_text")   # no stopwords, no sentiment
#' ret_ysns <- ori_dat %>% token_eyes(text_col_name = "doc_text", stop_word_src = "smart" )   # yes stopwords, no sentiment
#' ret_ysys <- ori_dat %>% token_eyes(text_col_name = "doc_text", stop_word_src = "smart", sentiment_src = "nrc" )   # yes stopwords, yes sentiment
token_eyes <- function(df, text_col_name = NULL, stop_word_src = NULL, sentiment_src = NULL,
                       to_lower = TRUE, to_strip_punct = TRUE, to_strip_numeric = FALSE,
                       remove_empty_tokens = TRUE, show_verbose = FALSE){
  ### Checks:
  if(is.null(text_col_name)){
    stop("A text column name must be provided.", call. = FALSE)
  }
  ### Verbose:
  if(show_verbose){
    if(is.null(stop_word_src)){
      cat("Tokens created without a stopword list (stop_word_src = NULL)\n")
    } else{
      cat("Tokens created with provided stopword source\n")
    }
    if(is.null(sentiment_src)){
      cat("Sentiment will not be performed (sentiment_src = NULL)\n")
    } else{
      cat("Sentiment will be performed\n")
    }
    if(remove_empty_tokens){
      cat("Empty tokens will be removed from output (remove_empty_tokens = TRUE)\n")
    } else{
      cat("Empty tokens will not be removed from output (remove_empty_tokens = FALSE)\n")
    }
  }
  ### Tokenize each obs of text via lapply loop:
  ret_lapply <- lapply(1:nrow(df), function(r){
    tmp_df <- df[r, ]
    tmp_txt <- unname(unlist(tmp_df[, text_col_name]))
    word <- tokenizers::tokenize_words(
      x = tmp_txt, lowercase = to_lower,
      strip_punct = to_strip_punct,
      strip_numeric = to_strip_numeric,
      simplify = TRUE
    )
    if(identical(word, character(0))){ word = "" }
    tmp_df[, text_col_name] <- NULL
    suppressWarnings(data.frame(cbind(tmp_df, word, stringsAsFactors = FALSE)))
  })
  ### Combine all with plyr::rbind.fill()' (faster):
  ret_docall_rbind <- plyr::rbind.fill(ret_lapply)
  ret <- data.frame(ret_docall_rbind, stringsAsFactors = FALSE)
  ### Removing empty tokens (optional):
  if(remove_empty_tokens){
    ret <- ret[which(ret$"word" != ""), ]
  }
  ### Stopwords (NULL by default):
  if(!is.null(stop_word_src)){
    if(length(stop_word_src) == 1 & stop_word_src %in% c("snowball", "stopwords-iso", "misc", "smart")){
      ## Using pre-built stopword list from tidytext::get_stopwords() function:
      stpwrd_df <- tidytext::get_stopwords(source = stop_word_src)
    } else{
      ## Using custom stopword vector list:
      stpwrd_df <- data.frame(word = stop_word_src, stringsAsFactors = FALSE)
    }
    ret <- dplyr::anti_join(ret, stpwrd_df, by = "word")
  }
  ### Sentiments (NULL by default):
  if(!is.null(sentiment_src)){
    ret <- dplyr::left_join(ret, tidytext::get_sentiments(sentiment_src), by = "word")
  }
  ### Return:
  return(ret)
}










#' @title Create a term-by-term matrix.
#'
#' @description Takes in two documents or two lists of words and creates a matrix of all possible word-by-word combinations. Used primarily by 'leven_dist' function.
#' @param text_1 The first list of words or document input.
#' @param text_2 The second list of words or document input.
#' @param stop_words A list of stop words to be used. Defaults to no stop words used (NULL).
#' @param make_lower Should text be converted to lower case? Defaults to TRUE.
#' @param rem_punct Should punctuation be removed? Defaults to TRUE.
#' @param rem_num Should numerics be removed? Defaults to TRUE.
#' @keywords leven_dist
#' @export
#' @examples
#' expand_matrix_text(c("duck", "pig", "cat"), c("dog", "cat"))
#' expand_matrix_text("HEllo I'm me", "hI tHeRe")
expand_matrix_text <- function(text_1, text_2, stop_words = NULL,
                               make_lower = TRUE, rem_punct = TRUE,
                               rem_num = FALSE){
  ## tokenize:
  text_1 <- unlist(
    tokenizers::tokenize_words(
      x = text_1,
      lowercase = make_lower,
      stopwords = stop_words,
      strip_punct = rem_punct,
      strip_numeric = rem_num,
      simplify = TRUE
    )
  )
  text_2 <- unlist(
    tokenizers::tokenize_words(
      x = text_2,
      lowercase = make_lower,
      stopwords = stop_words,
      strip_punct = rem_punct,
      strip_numeric = rem_num,
      simplify = TRUE
    )
  )
  ## return matrix:
  return(as.matrix(expand.grid(
    text_1 = text_1, text_2 = text_2,
    stringsAsFactors = FALSE
  )))
}










#' @title Levenshtein distance.
#'
#' @description Takes in output from the 'expand_matrix_text' function and returns the Levenshtein distance between each word-by-word combination.
#' @param mat_in The output from the 'expand_matrix_text' function.
#' @param col_num_1 The numerical column index for the first column of words.
#' @param col_num_2 The numerical column index for the second column of words.
#' @keywords expand_matrix_text
#' @export
#' @examples
#' leven_dist(expand_matrix_text(c("duck", "pig", "cat"), c("dog", "cat")))
#' leven_dist(expand_matrix_text("HEllo I'm me", "hI tHeRe"))
leven_dist <- function(mat_in, col_num_1 = 1, col_num_2 = 2){
  .adist_mat <- function(mat, r, c1, c2){
    drop(attr(adist(mat[r, c1], mat[r, c2], counts = "TRUE"), "counts"))
  }
  ret <- do.call(rbind, lapply(1:nrow(mat_in), function(nr){
    .adist_mat(mat = mat_in, r = nr, c1 = col_num_1, c2 = col_num_2)
  }))
  ret <- data.frame(cbind(mat_in, ret), stringsAsFactors = FALSE)
  ret[, 3:5] <- sapply(3:5, function(c) as.numeric(ret[, c]))
  return(ret)
}










#' @title Show the difference between two vectors of words.
#'
#' @description Given two vectors of words, this function uses the diffr lib and displays a comparison between them.
#' @param word_vec_1 The first vector of words to be compared.
#' @param word_vec_2 The second vector of words to be compared.
#' @param title_1 The title given to the first vector of words. Defaults to "Text 1".
#' @param title_2 The title given to the second vector of words. Defaults to "Text 2".
#' @keywords diffr
#' @export
#' @examples
#' show_diffr(c("cat", "dog", "puppy"), c("cat", "dog", "Puppy", "kitten"))
show_diffr <- function(word_vec_1, word_vec_2, title_1 = "Text 1", title_2 = "Text 2"){
  file1 = tempfile()
  writeLines(paste(word_vec_1, collapse = "\n"), con = file1)
  file2 = tempfile()
  writeLines(paste(word_vec_2, collapse = "\n"), con = file2)
  ret <- diffr::diffr(
    file1, file2,
    before = title_1, after = title_2,
    minJumpSize = 1e6
  )
  unlink(file1)
  unlink(file2)
  rm(file1, file2)
  return(ret)
}
