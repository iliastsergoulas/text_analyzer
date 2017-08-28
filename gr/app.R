# This R script is created as a Shiny application to analyze texts such as speeches, press releases, interviews etc
# regarding the agricultural sector.
# The code is available under MIT license, as stipulated in https://github.com/iliastsergoulas/text_analyzer/blob/master/LICENSE.
# Author: Ilias Tsergoulas, Website: www.agristats.eu

library(shiny)
library(shinythemes)
library(wordcloud)
library(tm)
library(base64enc)
library(SnowballC)
library(lubridate)
library(RWeka)
library(rJava)
library(RWekajars)
library(shinydashboard)
library(RPostgreSQL)
library(dendextend)

get_data_from_db<-function(x){ # Retrieving data from database 
    drv <- dbDriver("PostgreSQL") # Loading the PostgreSQL driver
    con <- dbConnect(drv, dbname = as.character(credentials$database), # Creating a connection to the postgres database
                 host = '88.99.13.199', port = as.character(credentials$port), 
                 user = as.character(credentials$user), password = as.character(credentials$password))
    query<-paste0("SELECT * from agriculture.agritexts WHERE person_gr='",x,"'")
    mydata <- dbGetQuery(con, query) # Get data
    dbDisconnect(con)
    dbUnloadDriver(drv)
    mydata$date <- dmy(mydata$date) # Converting to date
    return(mydata)}

credentials<-read.csv("/home/iliastsergoulas/dbcredentials.csv") # Reading credentials from csv file
drv <- dbDriver("PostgreSQL") # Loading the PostgreSQL driver
con <- dbConnect(drv, dbname = as.character(credentials$database), # Creating a connection to the postgres database
                 host = '88.99.13.199', port = as.character(credentials$port), 
                 user = as.character(credentials$user), password = as.character(credentials$password))
mylist <- dbGetQuery(con, "SELECT person_gr FROM agriculture.agritexts") # Get data
dbDisconnect(con)
dbUnloadDriver(drv)

mystopwords<-c("agronewsgr","για","και","από","των", "οι", "...","ώστε","μέσα", "αυτά","περίπου",
               "την","στις","της","του","τον","τους", "τα","να", "τέλος","στιγμή", "ούτε", "μία", "ακόμη","παράδειγμα",
               "τις","στους","αύριο","στην","προς", "θα", "ως", "ευρώ","κάτι", "είπε","ενώ","πως","έχω","λένε","κάποια",
               "που","στα","κάθε","λέει","στο","στη", "σε", "agrenda","όσο", "πάνω", "δούμε", "κάνει","εμείς","απόφαση",
               "ζωντανά","αγρότες","αγροτικής","μήνα", "τη", "φεκ","όχι","μπορεί","εκεί","βεβαίως","καμία","μπορούμε",
               "ημέρες","μέρες","στον", "έως", "λόγω", "εκατ","ότι","ήδη", "όλες", "έχω","ακόμα","αυτή","αυτές",
               "αγροτικό","ζητά","αλλά","χωρίς", "προ", "ύψους","όλα","όπου", "τσιρώνης", "ίδια", "εγώ","υπάρχουν",
               "αγροτικού", "δείτε", "πριν", "πού", "με", "το","πάλι","σημείωσε", "οποία", "κύριο","εάν","χώρο","άνθρωποι",
               "πιο", "όλοι", "φωτό","νέα", "δισ", "δεν", "να","μια","άλλων","μόνο","ήταν","αφορά","μόνο","χώρα", 
               "ειδήσεις","αγροτικές", "μέχρι","μετά","γίνει","έχουν","χώρας","δήλωσε","news","λοιπόν","έτσι","αποτελεί",
               "είναι","ανά","νέο","αγροτική","αγροτών","κιλό","https","δόθηκαν","επίσης","μεγάλο","κάνουν","μεγάλη",
               "σας","μας","αυτό","έχει","όπως","θέλουν","καθώς","ένα","ελλάδα","είμαστε","δημοσιογράφος","όμως",
               "έχουμε","κάνουμε","θέμα","επιπλέον","μην","μου","είτε","στοιχεία","μάλιστα","πρώτη","πρέπει","ένας",
               "κατά", "υπό", "πώς", "δις", "όταν", "αποστόλου", "υπουργός", "τότε", "διότι","βάση","είχαμε","δίνει",
               "αυτήν","μέσω","χώρου","κυρίως", "μεταξύ","αυτοί","δηλαδή", "ξέρουν","άλλη","σχετικά","οποίες","τρόπο",
               "πρώτα","όλη","τώρα","τόνισε","πολύ","οποίο","γιατί","μπορούν","περιοχές","δύο","μιλάμε","κάποιες",
               "τόσο","σήμερα","λέω","πληρώσει","ώρα","εδώ","yoleni","μέρα","άλλες","πάρει","εεδ","γιάννης","θέματα",
               "γίνεται","άρα","νέας","όλους","ποια","πρώτο","άλλα","θέλω","υπουργέ","συριζα", "θέλω","θέλει","υπάρξει",
               "περίοδο","χρόνο", "υπάρχει","τι","αν","κι","σημασία","λπ","περίπτωση","-","yoleni's", "ββ", "ξένοι",
               "τομέα","κόσμος","αναπληρωτής","ελληνική","κόσμος","αυτών","πλέον","χρόνια","έρχονται","σχέση","τομέας",
               "κάποιος","βλέπουμε","υποστηρίζουμε","δώσουμε","μεγάλες","δε","πάει","κλπ","γι'","κάτω","πίσω","παρά",
               "επομένως","αυτός","κόσμο","κύριε","πω","μεγάλα","άλλο","πιστεύω","πάρουν","ας","έναν","είπα","ίδιο","καλά",
               "ιδιαίτερα","πούμε","πολλές","μη","πει","μαζί","επειδή","έγινε","γι'","αυτούς","πολλά","πάρα", "αυτούς",
               "θέλουμε","κάναμε","θυμίζω","φορά","βοηθήσουμε","είχαν","προκειμένου","πάνε","έχετε","τέτοια","λίγο","είχαν",
               "προσπαθούμε","είχε","δημοσιογράφοσ","υπουργόσ","σειρά","αυτού","πάμε","πασεγεσ","όσα","σχεδόν","όλων",
               "εκ","κύριος","ενώ","άλλωστε","υπουργείο","διαδικασία","δουλειά","δυο","πραγματικά","υποστηρίξουμε",
               "άρθρο","πλαίσιο","καν","σύμφωνα","παράγραφος","υπόψη","στοιχείο","εν","αριθ","αναφέρονται","αφορούν","€",
               "σχετικές","βάσει","κανονισμού","εε","σημείο","παα","μέτρου","μέτρων","",
               "βλέπε","σύνολο","πίνακας","άρθρου","όσον","|","εντός",             
               "agronewsgr", "https", "amp", "food", "read", "https","without","bring","usda", "agency","gov","navigation",
               "thanks", "look", "looking", "see", "people", "way","fms","hemp","due","information","press","release","releases",
               "gaid", "agchattruth", "foodtruth","agchat","want","list","make","range","site","commission","europa","european",
               "agriculture","will", "talk", "now", "new", "forget","like","using","ghg","member","policy","search","also",
               "farm", "thank", "book", "year", "week", "photo","today","farmers","states","tools","div","gpt","count",
               "add", "can", "truths", "life", "news", "every","lelyknowhow","farming","fao","said","wfp","office","america",
               "time", "action", "know", "share", "just", "found","always","day","farms","graziano","silva","communities",
               "latest", "find", "get", "great", "table", "join", "word", "countries","across","director","program",
               "thunderclap", "excited", "better", "voice", "video","use","asa","ahdb","vilsack","secretary","programs",               "paper", "getting", "blogs", "help", "one","unfao","via","...","_",
               "best", "animal", "need", "joy", "many", "awesome", "support", "first","million","national","service",
               "futureofcap","sacfarmclimate","offers","shows","play","-","means","hort","percent","|","l","state",
               "allow","provides","nfutweets","never","century","address","say","since","conservation","opportunity",
               "assistance","general","world",
               "almost","philhoganeu","farmwildlifeuk","min","nyc","around","devex","xhnews","review")

ui <- fluidPage( # Creating shiny app's UI
    theme = shinytheme("spacelab"),
    sidebarPanel(
        selectInput('person', 'Πρόσωπο/Οργανισμός', choices = unique(mylist$person_gr), 
                    selected='Βαγγέλης Αποστόλου - Υπουργός Αγροτικής Ανάπτυξης και Τροφίμων', multiple=FALSE),
        dateRangeInput("mydate", "Ημερομηνία:",min='01/01/2000', max=Sys.Date(), 
                                        start='01/01/2000',end=Sys.Date(), sep="")
    ),
    mainPanel(
        tabsetPanel(
            tabPanel("Wordcloud", plotOutput("view")),
            tabPanel("Δενδρόγραμμα", plotOutput("hclust"))
    ))
)

server <- function(input, output) {
    mytext <- reactive({ # Adding reactive data information
        mydata<-get_data_from_db(input$person)
        mydata<-mydata[which(mydata$date>=input$mydate[1] & mydata$date<=input$mydate[2]),]
        mydata.text<-paste(unlist(mydata$text), collapse =" ")
        myCorpus <- Corpus(VectorSource(mydata.text)) # Building a corpus
        # Creating matrix of texts after "cleaning" them from anything unnecessary
        myDtm <- TermDocumentMatrix(myCorpus, control = list(removePunctuation = TRUE, 
             stopwords = c(mystopwords, stopwords("english")),
             removeNumbers = TRUE, tolower = TRUE))
        m <- as.matrix(myDtm) # Converting to matrix
        v <- sort(rowSums(m), decreasing=TRUE) # Calculating frequency of words
        myNames <- names(v) # Getting words
        mytext <- data.frame(word=myNames, freq=v) # Creating dataframe with each word and its frequency
    })
    output$hclust <- renderPlot({
        mydata<-get_data_from_db(input$person)
        mydata<-mydata[which(mydata$date>=input$mydate[1] & mydata$date<=input$mydate[2]),]
        mydata.text<-paste(unlist(mydata$text), collapse =" ")
        myCorpus <- Corpus(VectorSource(mydata.text)) # Building a corpus
        # Creating matrix of texts after "cleaning" them from anything unnecessary
        myDtm <- TermDocumentMatrix(myCorpus, control = list(removePunctuation = TRUE, 
                                                             stopwords = c(mystopwords, stopwords("english")),
                                                             removeNumbers = TRUE, tolower = TRUE))
        mydf <- as.data.frame(inspect(myDtm))
        mydf.scale <- scale(mydf)
        d <- dist(mydf.scale,method="euclidean")
        fit <- as.dendrogram(hclust(d, method="ward.D2"))
        d2=color_branches(fit,k=5)
        plot(d2)
    })
    corrplot <- reactive({
        print(input$person)
        mydata<-get_data_from_db(as.character(input$person))
        mydata<-mydata[which(mydata$date>=input$mydate1[1] & mydata$date<=input$mydate1[2]),]
        corpus <- TextReuseCorpus(text=mydata, tokenizer = tokenize_ngrams, n = 5,
                                  progress = FALSE)
        comparisons <- pairwise_compare(corpus, jaccard_similarity, progress = FALSE)
        comparisons[1:4, 1:4]
        print(comparisons)
    })
    output$view <- renderPlot({ # Creating wordcloud
        wordcloud(mytext()$word, mytext()$freq, min.freq=5, max.words=100, scale=c(3,.5), 
                  rot.per=.5,colors=brewer.pal(8, "Dark2"))
    })
}
shinyApp(ui, server)