library(plyr)

btc_price <- read.csv('BTC_USD Bitfinex Historical Data.csv',stringsAsFactors = F,header = T)
sp500 <- read.csv('SP500.csv',stringsAsFactors = F,header = T)
gold <- read.csv('GOLDAMGBD228NLBM.csv',stringsAsFactors = F,header = T)
texas_oil <- read.csv('DCOILWTICO.csv',stringsAsFactors = F,header = T)
usdeu_conv <- read.csv('DEXUSEU.csv',stringsAsFactors = F,header = T)

btc_price$Date <- as.Date(btc_price$Date,format = '%b %d, %Y')
sp500$DATE <- as.Date(sp500$DATE)
texas_oil$DATE <- as.Date(texas_oil$DATE)
usdeu_conv$DATE <- as.Date(usdeu_conv$DATE)
gold$DATE <- as.Date(gold$DATE)
sp500$SP500 <- as.numeric(sp500$SP500)
gold$GOLDAMGBD228NLBM <- as.numeric(gold$GOLDAMGBD228NLBM)
texas_oil$DCOILWTICO <- as.numeric(texas_oil$DCOILWTICO)
usdeu_conv$DEXUSEU <- as.numeric(usdeu_conv$DEXUSEU)
btc_price$Price <- as.numeric(gsub(',','',btc_price$Price))

colnames(btc_price)[1] <- 'DATE'
btc_price <- btc_price[c(1:2)]

btc <- join_all(list(btc_price,sp500,gold,texas_oil,usdeu_conv), by = 'DATE', type = 'full')
btcf <- na.omit(btc)

btcf <- btcf[order(btcf$DATE),]
colnames(btcf) <- c('date','btc_price','sp500','gold','texas_oil','usd_eu_conv')

write.csv(btcf,'btc_data_final.csv',row.names = F)
