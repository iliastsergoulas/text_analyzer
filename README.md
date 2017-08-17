# text_analyzer
This application has been created as a tool examining statistics, correlation and trends in agricultural texts, 
as they have been drawn from speeches, interviews and articles of personas and organization active in Greek agriculture.

The application is pretty simple: at first, it gets data from the PostgreSQL database installed on the server. The data 
is the text itself as well metadata upon the text such as author and date. Then, ingoring all the stopwords (like prepositions 
and adverbs), the frequency of each word is measured and a word cloud is created based on these frequencies.
