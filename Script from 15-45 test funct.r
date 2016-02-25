# R script from this afternoon 15-45, not from Rmd doc
# Init packages needed for subsequent code - please remove '#' before the install line if you need to install that referenced package

# install.packages('devtools',repos = "http://lib.stat.cmu.edu/R/CRAN/")
require(devtools)
# install_github('rstudio/rmarkdown')
# install.packages('knitr', repos = c('http://rforge.net', 'http://cran.rstudio.org'), type = 'source')
# install_github('jimhester/knitrBootstrap')
# install.packages("shape")
# install.packages("diagram")
# install.packages("tau")
require(rmarkdown)
require(knitr)
require(knitrBootstrap)
require(xtable)
require(stringr)
require(XML)
require(RCurl)
require(tau)
require(graphics)
require(shape)
require(diagram)

raw.data <- "555-1239Moe Szyslak(636) 555-0113Burns, C. Montgomery555-6542Rev. Timothy Lovejoy555 8904Ned Flanders636-555-3226Simpson, Homer5543642Dr. Julius Hibbert"

name <- unlist(str_extract_all(raw.data, "[[:alpha:]., ]{2,}"))
phone <- unlist(str_extract_all(raw.data, "\\(?(\\d{3})?\\)?(-| )?\\d{3}(-| )?\\d{4}"))
phonelist <- data.frame(name, phone)
names(phonelist) <- c('Full Name', 'Phone No.')

# TestName <- ifelse( NmDets$SpaceCt == 3, ifelse( NmDets$start > 0, ' 1.1 ', ' 1.2 ' ), ifelse( NmDets[,1] == 1, ifelse( NmDets[,2] > 0, ' 2.1 .1 ', ' 2.1 .2 '), ifelse( NmDets[,2] > 0, ifelse( NmDets$w2e == NmDets$PerLocal, ifelse( NmDets$w2L > 2, ' 2.2 .1 .1 .1 ', ' 2.2 .1 .1 .2 '  ), ' 2.2 .1 .2 ' ), ifelse( ( NmDets$w1e == NmDets$PerLocal & NmDets$w1L > 2), ' 2.2 .2 .1 ', ' 2.2 .2 .2 ' ) ) ) )`



as.vector(phonelist[, 1]) -> strvect


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
    w3 <- unlist(w3[, 2])
    w4 <- trimws(str_match(trimws(VectFullNames), '[[:alpha:]]{1,}\\.?\\,? [[:alpha:]]{1,}\\.?\\,? [[:alpha:]]{1,}\\.?\\,? ([[:alpha:]]{1,}\\.?\\,?)'))
    w4 <- unlist(w4[, 2])

    w1L <- str_length(w1)
    w2L <- str_length(w2)
    w3L <- str_length(w3)
    w4L <- str_length(w4)

    w1posn <- str_locate(string = VectFullNames, pattern = w1)
    w2posn <- str_locate(string = VectFullNames, pattern = w2)
    w3posn <- str_locate(string = VectFullNames, pattern = w3)
    w4posn <- str_locate(string = VectFullNames, pattern = w4)

    # Creates a word matrix to which we can add attributes
    NmDets <- data.frame(SpaceCt, CommLocal, PerLocal[, 1], w1, w2, w3, w4, w1L, w2L, w3L, w4L, w1posn, w2posn, w3posn, w4posn, stringsAsFactors = F)
    NmDets[is.na(NmDets)] <- 0
    NmDets <- data.frame(NmDets[, -3])
    colnames(NmDets) <- c("SpaceCt", "CommaLoc", "PerLocal", "w1", "w2", "w3", "w4", "w1L", "w2L", "w3L", "w4L", "w1s", "w1e", "w2s", "w2e", "w3s", "w3e", "w4s", "w4e")


    #    TestName <- ifelse( NmDets$SpaceCt == 3, ifelse( NmDets$start > 0, '1.1', '1.2' ), ifelse( NmDets[,1] == 1, ifelse( NmDets[,2] > 0, '2.1.1', '2.1.2'), ifelse( NmDets[,2] > 0, ifelse( NmDets$w2e == NmDets$PerLocal, ifelse( NmDets$w2L > 2, '2.2.1.1.1', '2.2.1.1.2'  ), '2.2.1.2' ), ifelse( ( NmDets$w1e == NmDets$PerLocal & NmDets$w1L > 2), '2.2.2.1', '2.2.2.2' ) ) ) )
    FName <- ifelse(NmDets$SpaceCt == 3, ifelse(NmDets$start > 0, w3, w1), ifelse(NmDets[, 1] == 1, ifelse(NmDets[, 2] > 0, w2, w1), ifelse(NmDets[, 2] > 0, ifelse(NmDets$w2e == NmDets$PerLocal, ifelse(NmDets$w2L > 2, w3, w2), w2), ifelse((NmDets$w1e == NmDets$PerLocal & NmDets$w1L > 2), w2, w1))))

    TName <- ifelse(NmDets$SpaceCt == 3, ifelse(NmDets$start > 0, w2, w1), ifelse(NmDets[, 1] == 1, ifelse(NmDets[, 2] > 0, ' ', ' '), ifelse(NmDets[, 2] > 0, ifelse(NmDets$w2e == NmDets$PerLocal, ifelse(NmDets$w2L > 2, NmDets$w2, ' '), ' '), ifelse((NmDets$w1e == NmDets$PerLocal & NmDets$w1L > 2), w1, ' '))))

    MName <- ifelse(NmDets$SpaceCt == 3, ifelse(NmDets$start > 0, w4, w3), ifelse(NmDets[, 1] == 1, ifelse(NmDets[, 2] > 0, ' ', ' '), ifelse(NmDets[, 2] > 0, ifelse(NmDets$w2e == NmDets$PerLocal, ifelse(NmDets$w2L > 2, ' ', NmDets$w3), NmDets$w3), ifelse((NmDets$w1e == NmDets$PerLocal & NmDets$w1L > 2), ' ', w2))))

    LName <- ifelse(NmDets$SpaceCt == 3, ifelse(NmDets$start > 0, NmDets$w1, NmDets$w4), ifelse(NmDets[, 1] == 1, ifelse(NmDets[, 2] > 0, NmDets$w1, NmDets$w2), ifelse(NmDets[, 2] > 0, ifelse(NmDets$w2e == NmDets$PerLocal, ifelse(NmDets$w2L > 2, NmDets$w1, NmDets$w1), NmDets$w1), ifelse((NmDets$w1e == NmDets$PerLocal & NmDets$w1L > 2), NmDets$w3, NmDets$w3))))

    DetFName <- data.frame(stringsAsFactors = F, VectFullNames, TName, FName, MName, LName)
    names(DetFName) <- c('Full Name', 'Title', 'First Name', 'Middle Name', 'Last Name')

    return(DetFName)

}

