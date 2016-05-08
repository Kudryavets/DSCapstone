# Tasks to accomplish
# Obtaining the data - Can you download the data and load/manipulate it in R?
# Familiarizing yourself with NLP and text mining - Learn about the basics of natural language processing and how it relates to the data science process you have learned in the Data Science Specialization.
# Questions to consider
# What do the data look like?
# Where do the data come from?
# What are some common issues in the analysis of text data?
# What is the relationship between NLP and the concepts you have learned in the Specialization?
# Exploratory analysis - perform a thorough exploratory analysis of the data,
# What are the common steps in natural language processing?
# Exploratory analysis - understanding the distribution of words and relationship between the words in the corpora. 
# Understand frequencies of words and word pairs - build figures and tables to understand variation in the frequencies of words and word pairs in the data.
# Tokenization - identifying appropriate tokens such as words, punctuation, and numbers. Writing a function that takes a file as input and returns a tokenized version of it.
# Some words are more frequent than others - what are the distributions of word frequencies? 
# What are the frequencies of 2-grams and 3-grams in the dataset? 
# How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%? 
# Profanity filtering - removing profanity and other words you do not want to predict.
# How do you evaluate how many of the words come from foreign languages?
# Build basic n-gram model - using the exploratory analysis you performed, build a basic n-gram model for predicting the next word based on the previous 1, 2, or 3 words.
# How can you use backoff models to estimate the probability of unobserved n-grams?
# Can you think of simple ways to "smooth" the probabilities (think about giving all n-grams a non-zero probability even if they aren't observed in the data) ?
# Build a predictive model based on the previous data modeling steps - you may combine the models in any way you think is appropriate.

Can you think of any other data sources that might help you in this project?  WSJ kaggle
Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?
How can you efficiently store an n-gram model (think Markov Chains)?
How can you use the knowledge about word frequencies to make your model smaller and more efficient?
How many parameters do you need (i.e. how big is n in your n-gram model)?
Build a model to handle unseen n-grams - in some cases people will want to type a combination of words that does not appear in the corpora. Build a model to handle cases where a particular n-gram isn't observed.

# Tasks to accomplish
Evaluate the model for efficiency and accuracy - use timing software to evaluate the computational complexity of your model. Evaluate the model accuracy using different metrics like perplexity, accuracy at the first word, second word, and third word.

# Questions to consider
How does the model perform for different choices of the parameters and size of the model? 
How much does the model slow down for the performance you gain?
Does perplexity correlate with the other measures of accuracy?
Can you reduce the size of the model (number of parameters) without reducing performance?

# Tasks to accomplish
Explore new models and data to improve your predictive model.
Evaluate your new predictions on both accuracy and efficiency. 

# Questions to consider
What are ways in which the n-gram model may be inefficient?
What are the most commonly missed n-grams? Can you think of a reason why they would be missed and fix that? 
What are some other things that other people have tried to improve their model? 
Can you estimate how uncertain you are about the words you are predicting? 

# Tasks to accomplish
Create a data product to show off your prediction algorithm You should create a Shiny app that accepts an n-gram and predicts the next word.

# Questions to consider
What are the most interesting ways you could show off your algorithm?
Are there any data visualizations you think might be helpful (look at the Swiftkey data dashboard if you have it loaded on your phone)?
How should you document the use of your data product (separately from how you created it) so that others can rapidly deploy your algorithm?

# Tasks to accomplish
Create a slide deck promoting your product. Write 5 slides using RStudio Presenter explaining your product and why it is awesome!
    
    # Questions to consider
    How can you briefly explain how your predictive model works?
How can you succinctly quantitatively summarize the performance of your prediction algorithm?
How can you show the user how the product works?