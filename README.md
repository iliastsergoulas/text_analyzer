# text_analyzer
This application has been created as a tool examining statistics, correlation and trends in agricultural texts, 
as they have been drawn from speeches, interviews and articles of personas and organization active in Greek agriculture.
![Alt text](screenshot.png?raw=true "Text analyzer - agristats.eu")

The application is pretty simple: 

1. At first, it gets data from the PostgreSQL database installed on the server. The data is the text itself as well as metadata upon the text, such as author and date. 
2. The user may filter the to-be-analyzed text, according to the person/organization and the selected date interval.
3. Then, ingoring all the stopwords (like prepositions and adverbs), the frequency of each word is measured and a word cloud is created based on these frequencies.
4. There is an extra functionality of creating a dendrogram with the most common words within the text.

The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/agri_subsidies/blob/master/LICENSE.