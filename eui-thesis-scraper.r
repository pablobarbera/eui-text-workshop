setwd("~/Dropbox/teaching/text")
options(stringsAsFactors=F)
library(rvest)

# scraping list of thesis URLs
urls <- paste0('http://cadmus.eui.eu/handle/1814/4857/browse?order=ASC&rpp=100',
	'&sort_by=2&etal=-1&offset=', c(0, 100, 200, 300, 400, 500, 600), '&type=title')
theses <- c()
for (url in urls){
	message(url)
	d <- read_html(url) # reading the HTML code
	links <- html_nodes(d, ".content:nth-child(2)") # identify the CSS selector
	links <- gsub('.*(handle/[0-9]{4}/[0-9]{4,5}).*', links, repl="\\1") # extract URL
	theses <- c(theses, links) # add to character vector
}

theses <- paste0("http://cadmus.eui.eu/", theses)
writeLines(theses, con=file('eui-theses/urls-list.txt'))

# downloading URLs
theses <- readLines('eui-theses/urls-list.txt')
results <- list()
i <- 1

for (url in theses){

	message(i, ': ', url)
	d <- readLines(url) # reading the HTML code

	title <- d[grep("Title: ", d)]
	title <- gsub(".*Title: </span><span>(.*)</span>", title, repl="\\1")

	date <- d[grep("bold.*Date: ", d)]
	date <- gsub(".*Date: </span><span>(.*)</span>", date, repl="\\1")

	author <- d[grep("Author: ", d)]
	author <- gsub(".*\">(.*)</a></span>", author, repl="\\1")

	subject.line <- grep("LC Subject Heading: ", d)
	if (length(subject.line)>0){
		subject <- d[subject.line]
		subject <- gsub(".*Heading: </span><span>(.*)</span>", subject, repl="\\1")
		subject <- gsub("<span>|</span>", "", subject)
	} else { subject <- NA }
	abstract.line <- grep("Abstract: ", d)
	if (length(abstract.line)>0){
		abstract <- d[abstract.line+1]
		abstract <- gsub("<div>|</div>", "", abstract)
	} else { abstract <- NA }

	results[[i]] <- data.frame(
		url = url,
		title = title,
		date = date,
		author = author,
		subject = subject,
		abstract = abstract)
	i <- i + 1
	Sys.sleep(runif(1, 1, 2))
}

results <- do.call(rbind, results)
write.csv(results, file="eui-theses/eui-theses-data.csv", row.names=FALSE)

# running topic models
d <- read.csv("eui-theses/eui-theses-data.csv")

# deleting empty
d <- d[!is.na(d$abstract),]

# install.packages("topicmodels")
library(topicmodels)
# reading data and preparing corpus object
library(quanteda)
theses <- corpus(d$abstract)
euidfm <- dfm(theses, ngrams=c(1,3), stem=TRUE,
	ignoredFeatures=stopwords("english"))

# we now export to a format that we can run the topic model with
dtm <- convert(euidfm, to="topicmodels")

# estimate LDA with K topics
K <- 50
lda <- LDA(dtm, k = K, method = "Gibbs", 
                control = list(verbose=25L, seed = 123, burnin = 100, iter = 500))

terms <- get_terms(lda, 15)
terms[,1:4]
topics <- get_topics(lda, 1)
head(topics)

# Topic 44
paste(terms[,44], collapse=", ")
sample(d$abstract[topics==44], 2)

topic <- 49
paste(terms[,topic], collapse=", ")
# add probability to df
d$prob_topic <- lda@gamma[,topic]
# now aggregate at the year level
agg <- aggregate(d$prob_topic, by=list(year=d$date), FUN=mean)
# and plot it
plot(agg$year, agg$x, type="l", xlab="Year", ylab="Avg. prob. of article about topic",
     main="Estimated proportion of articles about selected topic")




