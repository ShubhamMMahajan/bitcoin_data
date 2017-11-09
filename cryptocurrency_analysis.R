todays_date = format(Sys.Date(), "%Y%m%d")
formatted_date = format(Sys.Date(), "%b-%d-%Y")

#enter your working directory here
wd <- "C:\\Users\\shubh\\Documents\\GitHub\\bitcoin_data"

setwd(wd)
dir.create(formatted_date)



txt_list <- unlist(strsplit(readLines("https://coinmarketcap.com/currencies/bitcoin/historical-data/?start=20130428&end=20171105"), "\n"))
start_of_data <- grep("<tbody>", txt_list) + 3
end_of_data <- grep("/tbody", txt_list)


find_date <- function(txt_list) {
    list_of_dates = list()
    library(qdapRegex)
    for(i in seq(from=start_of_data, to=end_of_data, by=10)){
        
        
        list_of_dates = c(list_of_dates, rm_between(txt_list[i], "t\">", "</", extract=TRUE)[[1]])          
    }
    return(unlist(list_of_dates))
}

#list_dates = find_date(txt_list)

find_open <- function(txt_list) {
    list_of_open_price = list()
    library(qdapRegex)
    for(i in seq(from=start_of_data + 1, to=end_of_data, by=10)){
        list_of_open_price = c(list_of_open_price, as.double(rm_between(txt_list[i], "<td>", "</td>", extract=TRUE)[[1]]))          
    }
    return(unlist(list_of_open_price))
    
}

#list_open_price = find_open()

find_high <- function(txt_list) {
    list_of_high_price = list()
    library(qdapRegex)
    for(i in seq(from=start_of_data + 2, to=end_of_data, by=10)){
        list_of_high_price = c(list_of_high_price, as.double(rm_between(txt_list[i], "<td>", "</td>", extract=TRUE)[[1]]))          
    }
    return(unlist(list_of_high_price))
    
}

#list_high_price = find_high()

find_low <- function(txt_list) {
    list_of_low_price = list()
    library(qdapRegex)
    for(i in seq(from=start_of_data + 3, to=end_of_data, by=10)){
        list_of_low_price = c(list_of_low_price, as.double(rm_between(txt_list[i], "<td>", "</td>", extract=TRUE)[[1]]))          
    }
    return(unlist(list_of_low_price))
    
}

#list_low_price = find_low()

find_close <- function(txt_list) {
    list_of_close_price = list()
    library(qdapRegex)
    for(i in seq(from=start_of_data + 4, to=end_of_data, by=10)){
        list_of_close_price = c(list_of_close_price, as.double(rm_between(txt_list[i], "<td>", "</td>", extract=TRUE)[[1]]))          
    }
    return(unlist(list_of_close_price))
    
}

#list_close_price = find_close()

find_volume <- function(txt_list) {
    list_of_volume_price = list()
    library(qdapRegex)
    for(i in seq(from=start_of_data + 5, to=end_of_data, by=10)){
        list_of_volume_price = c(list_of_volume_price, as.double(gsub(",", "", rm_between(txt_list[i], "<td>", "</td>", extract=TRUE)[[1]])))          
    }
    return(unlist(list_of_volume_price))
    
}

#list_volume_price = find_volume(txt_list)

find_market <- function(txt_list) {
    list_of_market_price = list()
    library(qdapRegex)
    for(i in seq(from=start_of_data + 6, to=end_of_data, by=10)){
        list_of_market_price = c(list_of_market_price, as.double(gsub(",", "", rm_between(txt_list[i], "<td>", "</td>", extract=TRUE)[[1]])))          
    }
    return(unlist(list_of_market_price))
    
}

#list_market_price = find_market()

# cryptocurrencies <- function() {
#     website <- "https://coinmarketcap.com/all/views/all/"
#     html_text <- readLines(website)
#     crypto_list <- unlist(strsplit(html_text, "\n"))
#     write(html_text, "crypto.txt")
#     return(crypto_list)
#     
# }
# 
# get_currencies <- function() {
#     crypto_list <- cryptocurrencies()
#     crypto_list <- unlist(crypto_list)
#     library(qdapRegex)
#     list_of_currencies = list()
#     start <- which(grepl("bitcoin", crypto_list),arr.ind = TRUE)[4]
#     for (i in seq(from=start, to=(start+ 400) - 1, by=40)){
#         list_of_currencies <- c(list_of_currencies, rm_between(crypto_list[i], "\">", "</a>", extract = TRUE)[[1]])
#     }
#     print(list_of_currencies)
#     return(unlist(list_of_currencies))
# }

list_of_currencies <- c("Bitcoin", "Ethereum", "Dash", "Bitcoin-Cash", "Ethereum-Classic", "LiteCoin", "Monero", "Ripple", "NEM", "NEO")

get_df <- function(website){
    thepage <- readLines(website)
    txt_list <- unlist(strsplit(thepage, "\n"))
    start_of_data <- grep("<tbody>", txt_list) + 3
    end_of_data <- grep("/tbody", txt_list)
    list_dates <- find_date(txt_list)
    list_open_price <- find_open(txt_list)
    list_high_price<- find_high(txt_list)
    list_low_price <- find_low(txt_list)
    list_close_price <- find_close(txt_list)
    list_volume_price <- find_volume(txt_list)
    list_market_price <- find_market(txt_list)
    df <- data.frame("Date" = list_dates, "Open" = list_open_price, "High" = list_high_price,
                     "Low" = list_low_price, "Close" = list_close_price,
                     "Volume" = list_volume_price, "Market Cap" = list_market_price)
    return(df)
    
}

df <- get_df(input)


write_csv <- function() {
    #list_of_currencies <- get_currencies()
    todays_date = format(Sys.Date(), "%Y%m%d")
    formatted_date = format(Sys.Date(), "%b-%d-%Y")
    setwd(wd)
    #dir.create(formatted_date)
    setwd(paste(wd, "\\", formatted_date, sep = ""))
    for (i in seq(from = 1, to = length(list_of_currencies), by = 1)){
        list_of_currencies[i] <- gsub(" ", "-", list_of_currencies[i])
        csv_name <- paste(list_of_currencies[i], ".csv", sep = "")
        print(paste("Creating",csv_name))
        website <- paste("https://coinmarketcap.com/currencies/", list_of_currencies[i],"/historical-data/?start=20130428&end=", toString(todays_date), sep = "")
        df <- get_df(website)
        
        write.csv(df, csv_name)
    }
    
}
write_csv()

setwd(paste(wd, "\\", formatted_date, sep = ""))
bitcoin_price <- read.csv("Bitcoin.csv")


bitcoin_price$Date<- as.Date(bitcoin_price$Date, format = "%b %d, %Y")

library(plotly)
p <- plot_ly(bitcoin_price, x = ~bitcoin_price$Date, y = ~bitcoin_price$Open, name = 'Price in $', type = 'scatter', mode = 'lines')
p