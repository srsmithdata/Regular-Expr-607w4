# Packages
install.packages("shape")
install.packages("diagram")
require(shape)
require(diagram)

as.vector(phonelist[,1]) -> strvect


# Q3
#
# Text about decision tree
# The number of spaces give us the total variables, the presence of a coma gives us order.

as.vector(VectFullNames)

parse_Allname <- function(VectFullNames) {
  # counting spaces and finding comma and period location
  SpaceCt <- c(str_count(trimws(VectFullNames), ' '))
  CommLocal <- unlist(str_locate(trimws(VectFullNames), ','))
  PerLocal <- unlist(str_locate(VectFullNames, '\\.'))
  # Extract each word
  #
  w1 <- trimws(str_extract(trimws(VectFullNames), '[[:alpha:]]{1,}\\.?'))
  w2 <- trimws(str_extract(trimws(VectFullNames), ' [[:alpha:]]{1,}\\.?'))
  w3 <- trimws(str_match(trimws(VectFullNames), '[[:alpha:]]{1,}\\.?\\,? [[:alpha:]]{1,}\\.?\\,? ([[:alpha:]]{1,}\\.?\\,?)'))
  w3 <- unlist(w3[,2])
  w4 <- trimws(str_match(trimws(VectFullNames), '[[:alpha:]]{1,}\\.?\\,? [[:alpha:]]{1,}\\.?\\,? [[:alpha:]]{1,}\\.?\\,? ([[:alpha:]]{1,}\\.?\\,?)'))
  w4 <- unlist(w4[,2])

  w1L <- str_length(w1)
  w2L <- str_length(w2)
  w3L <- str_length(w3)
  w4L <- str_length(w4)

  w1posn <- str_locate(string = VectFullNames, pattern = w1)
  w2posn <- str_locate(string = VectFullNames, pattern = w2)
  w3posn <- str_locate(string = VectFullNames, pattern = w3)
  w4posn <- str_locate(string = VectFullNames, pattern = w4)

  # Creates a word matrix to which we can add attributes
    NmDets <- data.frame( SpaceCt, CommLocal, PerLocal[,1], w1, w2 , w3 , w4 , w1L, w2L, w3L, w4L, w1posn, w2posn, w3posn, w4posn, stringsAsFactors = F)
    NmDets[is.na(NmDets)] <- 0
    NmDets <- data.frame(NmDets[,-3])
    colnames(NmDets) <- c("SpaceCt", "CommaLoc", "PerLocal", "w1", "w2", "w3", "w4", "w1L", "w2L", "w3L", "w4L", "w1s", "w1e", "w2s", "w2e", "w3s", "w3e", "w4s", "w4e" )


#    TestName <- ifelse( NmDets$SpaceCt == 3, ifelse( NmDets$start > 0, '1.1', '1.2' ), ifelse( NmDets[,1] == 1, ifelse( NmDets[,2] > 0, '2.1.1', '2.1.2'), ifelse( NmDets[,2] > 0, ifelse( NmDets$w2e == NmDets$PerLocal, ifelse( NmDets$w2L > 2, '2.2.1.1.1', '2.2.1.1.2'  ), '2.2.1.2' ), ifelse( ( NmDets$w1e == NmDets$PerLocal & NmDets$w1L > 2), '2.2.2.1', '2.2.2.2' ) ) ) )
    FName <- ifelse( NmDets$SpaceCt == 3, ifelse( NmDets$start > 0, w3, w1 ), ifelse( NmDets[,1] == 1, ifelse( NmDets[,2] > 0, w2, w1), ifelse( NmDets[,2] > 0, ifelse( NmDets$w2e == NmDets$PerLocal, ifelse( NmDets$w2L > 2, w3, w2), w2 ), ifelse( ( NmDets$w1e == NmDets$PerLocal & NmDets$w1L > 2), w2, w1 ) ) ) )

    TName <- ifelse( NmDets$SpaceCt == 3, ifelse( NmDets$start > 0, w2, w1 ), ifelse( NmDets[,1] == 1, ifelse( NmDets[,2] > 0, ' ', ' '), ifelse( NmDets[,2] > 0, ifelse( NmDets$w2e == NmDets$PerLocal, ifelse( NmDets$w2L > 2, NmDets$w2, ' '), ' ' ), ifelse( ( NmDets$w1e == NmDets$PerLocal & NmDets$w1L > 2), w1, ' ' ) ) ) )

    MName <- ifelse( NmDets$SpaceCt == 3, ifelse( NmDets$start > 0, w4, w3 ), ifelse( NmDets[,1] == 1, ifelse( NmDets[,2] > 0, ' ', ' '), ifelse( NmDets[,2] > 0, ifelse( NmDets$w2e == NmDets$PerLocal, ifelse( NmDets$w2L > 2, ' ', NmDets$w3 ), NmDets$w3 ), ifelse( ( NmDets$w1e == NmDets$PerLocal & NmDets$w1L > 2), ' ', w2 ) ) ) )

    LName <- ifelse( NmDets$SpaceCt == 3, ifelse( NmDets$start > 0, NmDets$w1, NmDets$w4 ), ifelse( NmDets[,1] == 1, ifelse( NmDets[,2] > 0, NmDets$w1, NmDets$w2), ifelse( NmDets[,2] > 0, ifelse( NmDets$w2e == NmDets$PerLocal, ifelse( NmDets$w2L > 2, NmDets$w1, NmDets$w1  ), NmDets$w1 ), ifelse( ( NmDets$w1e == NmDets$PerLocal & NmDets$w1L > 2), NmDets$w3, NmDets$w3 ) ) ) )

DetFName <- data.frame( stringsAsFactors = F, VectFullNames ,TName, FName, MName, LName )
names(DetFName) <- c('Full Name', 'Title', 'First Name', 'Middle Name', 'Last Name')

return(DetFName)

}


#4 words means all possible vars present. no comma merans title, s
          # No comma == first then lastcommalocal testing = 1, str_extract(namevector , '' ) == 3,
           ifelse( str_detect(namevector, ','), str_extract(namevector,
                                                            [[:alpha:]]{1,}\\.?\\,?
w3 <- str_match(trimws(phonelist[,1]), '[[:alpha:]]{1,}\\.?\\,? [[:alpha:]]{1,}\\.?\\,? ([[:alpha:]]{1,}\\.?\\,?)')
w3 <- unlist(w3[,2])

w1L <- length(w1)
w2L <- length(w2)
w3L <- length(w3)
phonelist[,3] <- c(str_count(trimws(phonelist[,1]), ' '))
colnames(phonelist)[3] <- 'SpaceCt'

phonelist[,4] <- unlist(str_locate(trimws(phonelist[,1]), ','))
colnames(phonelist)[4] <- 'CommLocal'
str(phonelist)

w1 <- str_extract(trimws(phonelist[,1]), '[[:alpha:]]{1,}\\.?')
w2 <- str_extract(trimws(phonelist[,1]), ' [[:alpha:]]{1,}\\.?')
w3 <- str_match(trimws(phonelist[,1]), '[[:alpha:]]{1,}\\.?\\,? [[:alpha:]]{1,}\\.?\\,? ([[:alpha:]]{1,}\\.?\\,?)')
w3 <- unlist(w3[,2])
w4 <- str_match(trimws(phonelist[,1]), '[[:alpha:]]{1,}\\.?\\,? [[:alpha:]]{1,}\\.?\\,? [[:alpha:]]{1,}\\.?\\,? ([[:alpha:]]{1,}\\.?\\,?)')
w4 <- unlist(w4[,2])

w1L <- length(w1)
w2L <- length(w2)
w3L <- length(w3)
w4L <- length(w4)
, w2, w2L, w2posn, w3, w3L, w3posn, w4, w4L, w4posn,


#
# Q7
#
cnn <- '<title>+++BREAKING NEWS+++</title>'
In data mining and extraction, the web is a great resource with vast amounts of data. As such, many cases arise in which we will want to pull data from webpages. To do so, we must be able to handle the "unstructured" data found there, reading in the standard web page in HTML format, and extracting the valuable information while leaving the rest. The very coding and structure of an HTML page can help us know where to tell R to look for the information we want.

Using regular expressions can be a powerful way to achieve this goal. However, we must be careful since many characters used in HTML are *special* *characters* in the regex engine.

Take the considering string as an example:
cnn <- '<title>+++BREAKING NEWS+++</title>'

If you want to extract the first HTML tag here, you might think to write:


The string of the HTML tag


chtwebpg <- htmlParse(