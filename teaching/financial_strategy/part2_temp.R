
















## 10.1 Risk & Return: Insights from History {.smaller background="#f2f5ce"} 

Ibov vs CDI vs Selic in Brazil 2002-2007.

```{r}
#| warning: false
#| message: false
#| fig-align: center
#| echo: true
#| output-location: default
#| code-fold: true
#| code-summary: "R"
#| code-line-numbers: true
#| eval: true
#### QUANDL API
# Step to create your key
# 1. Go to https://www.quandl.com/
# 2. Create an account
# 3. Get your key
# 4. Add your key below (I can't give you mine)
api_KEY <- "Your QUANDL API KEY here"
#Ibov
stock<-'^BVSP' 
start<-'2002-01-01' 
end<-'2007-01-01' 
ibov <- yf_get(tickers = stock, 
               first_date = start,
               last_date = end)
ibov<- ibov[order(as.numeric(ibov$ref_date)),]
# Cumulative return Ibov
ibov$Ibov_return <- ibov$cumret_adjusted_prices -1
#Selic
selic <- get_Quandl_series(id_in = c('Selic' = 'BCB/11'),
                           api_key = api_key, 
                           first_date = start,
                           last_date = end)
selic<- selic[order(as.numeric(selic$ref_date)),]
selic$value <- selic$value / 100
# Cumulative return Selic
return_selic <- data.frame(nrow(selic):1)
colnames(return_selic)<- "selic_return"

for(i in (2:nrow(selic))) {
  return_selic[i,1] <- Return.cumulative( selic$value[1:i] )
}
# CDI
cdi <- get_Quandl_series(id_in =  c('CDI' = 'BCB/12'), 
                         api_key = api_key , 
                         first_date = start ,
                         last_date = end)
cdi<- cdi[order(as.numeric(cdi$ref_date)),]
cdi$value <- cdi$value / 100
# Cumulative return CDI
return_CDI <- data.frame(nrow(cdi):1)
colnames(return_CDI)<- "CDI_return"
for(i in (2:nrow(cdi))) {
  return_CDI[i,1] <- Return.cumulative( cdi$value[1:i] )
}
# Merging dataframes
selic <- cbind(selic, return_selic)
cdi <- cbind(cdi, return_CDI)
inf <- cbind(inf, return_Inf)
df <- merge(cdi ,selic, by=c("ref_date"))
df <- merge(df ,ibov, by=c("ref_date"))
df2 <- merge(df ,inf, by=c("ref_date"))
df$selic_return[1] <- NA
df$CDI_return[1] <- NA
df$Ibov_return[1] <- NA
df2$Inflation_return[1] <- NA
p<-ggplot(df, aes(ref_date)) + 
  geom_line(aes(y = CDI_return, colour = "CDI")) + 
  geom_line(aes(y = Ibov_return, colour = "Ibov")) +
  geom_line(aes(y = selic_return, colour = "Selic")) +
  labs(y='Cumulative return (daily)') +   theme_solarized() +
  ggtitle("Cumulative Returns CDI, Ibov, and Selic (2002-2007)")
ggplotly(p)
```








## 10.1 Risk & Return: Insights from History {.smaller background="#f2f5ce"} 

Ibov vs CDI vs Selic in Brazil 2002-2007.

```{python}
#| warning: false
#| message: false
#| fig-align: center
#| echo: true
#| results: false
#| output: true
#| code-fold: true
#| code-line-numbers: true	
#| output-location: default
#| code-summary: "Python"
#| collapse: true
#| eval: true
# Define API key and other parameters
api_KEY = "Your QUANDL API KEY here"
stock = "^BVSP"
start = "2002-01-01"
end =   "2007-01-01"
# Get data for IBOV
ibov = yf.download(stock, start=start, end=end)
ibov = ibov.reset_index()
ibov = ibov.sort_values("Date")
ibov = ibov.rename(columns={'Date': 'ref_date', 'Value': 'Ibovespa'})
ibov["return"] = ibov["Adj Close"].pct_change()
ibov['Ibov_return'] = (1 + ibov['return']).cumprod() -1
ibov = ibov[['ref_date','Ibov_return']]
# Get data for Selic
selic = quandl.get('BCB/11', start_date=start, end_date=end)
selic = selic.reset_index()
selic = selic.rename(columns={'Date': 'ref_date', 'Value': 'Selic'})
selic['Selic'] = selic['Selic'] / 100
selic['selic_return'] = (1 + selic['Selic']).cumprod() - 1
# Get data for CDI
cdi = quandl.get('BCB/12', start_date=start, end_date=end)
cdi = cdi.reset_index()
cdi = cdi.rename(columns={'Date': 'ref_date', 'Value': 'CDI'})
cdi['CDI'] = cdi['CDI'] / (100)
cdi['cdi_return'] = (1 + cdi['CDI']).cumprod() - 1
# Get data for inflation
inf = quandl.get('BCB/433', start_date=start, end_date=end)
inf = inf.reset_index()
inf = inf.rename(columns={'Date': 'ref_date', 'Value': 'Inflation'})
inf['Inflation'] = inf['Inflation'] / 100
inf['inf_return'] = (1 + inf['Inflation']).cumprod() - 1
# Merge dataframes
df = pd.merge(cdi, selic, on="ref_date")
df = pd.merge(df, ibov, on="ref_date")
df = pd.merge(df, inf, on="ref_date")
# Plot data
plt.close()
fig, ax = plt.subplots(figsize=(10,5))
ax.plot(df["ref_date"], df["cdi_return"], label='CDI')
ax.plot(df["ref_date"], df["Ibov_return"], label='Ibov')
ax.plot(df["ref_date"], df["selic_return"], label='Selic')
plt.ylabel('Cumulative return (daily)', fontsize=13)
plt.title('Cumulative Returns  CDI, Ibov, and Selic (2002-2007)', fontsize=15)
plt.xlabel('Date', fontsize=13)
plt.legend(loc='upper left')
plt.show()
```













## 10.1 Risk & Return: Insights from History {.smaller background="#f2f5ce"} 

Ibov vs CDI vs Selic in Brazil 2010-2019.

```{r}
#| warning: false
#| message: false
#| fig-align: center
#| echo: true
#| output-location: default
#| code-fold: true
#| code-summary: "R"
#| code-line-numbers: true
#| eval: true
#### QUANDL API
# Step to create your key
# 1. Go to https://www.quandl.com/
# 2. Create an account
# 3. Get your key
# 4. Add your key below (I can't give you mine)
api_KEY <- "Your QUANDL API KEY here"
#Ibov
stock<-'^BVSP' 
start<-'2010-01-01' 
end<-'2019-01-01' 
ibov <- yf_get(tickers = stock, 
               first_date = start,
               last_date = end)
ibov<- ibov[order(as.numeric(ibov$ref_date)),]
# Cumulative return Ibov
ibov$Ibov_return <- ibov$cumret_adjusted_prices -1
#Selic
selic <- get_Quandl_series(id_in = c('Selic' = 'BCB/11'),
                           api_key = api_key, 
                           first_date = start,
                           last_date = end)
selic<- selic[order(as.numeric(selic$ref_date)),]
selic$value <- selic$value / 100
# Cumulative return Selic
return_selic <- data.frame(nrow(selic):1)
colnames(return_selic)<- "selic_return"
for(i in (2:nrow(selic))) {
  return_selic[i,1] <- Return.cumulative( selic$value[1:i] )
}
# CDI
cdi <- get_Quandl_series(id_in =  c('CDI' = 'BCB/12'), 
                         api_key = api_key , 
                         first_date = start ,
                         last_date = end)
cdi<- cdi[order(as.numeric(cdi$ref_date)),]
cdi$value <- cdi$value / 100
# Cumulative return CDI
return_CDI <- data.frame(nrow(cdi):1)
colnames(return_CDI)<- "CDI_return"
for(i in (2:nrow(cdi))) {
  return_CDI[i,1] <- Return.cumulative( cdi$value[1:i] )
}
#Inflation Brazil
inf <- get_Quandl_series(id_in =  c('Inflation' = 'BCB/433'),
                         api_key = api_key, 
                         first_date = start,
                         last_date = end)
inf<- inf[order(as.numeric(inf$ref_date)),]
inf$value <- inf$value / 100
# Cumulative return Inflation
return_Inf <- data.frame(nrow(inf):1)
colnames(return_Inf)<- "Inflation_return"
for(i in (2:nrow(inf))) {
  return_Inf[i,1] <- Return.cumulative( inf$value[1:i] )
}
# Merging dataframes
selic <- cbind(selic, return_selic)
cdi <- cbind(cdi, return_CDI)
inf <- cbind(inf, return_Inf)
df <- merge(cdi ,selic, by=c("ref_date"))
df <- merge(df ,ibov, by=c("ref_date"))
df2 <- merge(df ,inf, by=c("ref_date"))
df$selic_return[1] <- NA
df$CDI_return[1] <- NA
df$Ibov_return[1] <- NA
df2$Inflation_return[1] <- NA
# Graph cumulated return CDI and IBOV
p<-ggplot(df, aes(ref_date)) + 
  geom_line(aes(y = CDI_return, colour = "CDI")) + 
  geom_line(aes(y = Ibov_return, colour = "Ibov")) +
  geom_line(aes(y = selic_return, colour = "Selic")) +
  labs(y='Cumulative return (daily)') +   theme_solarized() +
  ggtitle("Cumulative Returns CDI, Ibov, and Selic (2010-2019)")
ggplotly(p)
```













## 10.1 Risk & Return: Insights from History {.smaller background="#f2f5ce"} 

```{python}
#| warning: false
#| message: false
#| fig-align: center
#| echo: true
#| results: false
#| output: true
#| code-fold: true
#| code-line-numbers: true	
#| output-location: default
#| code-summary: "Python"
#| collapse: true
#| eval: true
# Define API key and other parameters
api_KEY = "Your QUANDL API KEY here"
stock = "^BVSP"
start = "2010-01-01"
end =   "2019-01-01"
# Get data for IBOV
ibov = yf.download(stock, start=start, end=end)
ibov = ibov.reset_index()
ibov = ibov.sort_values("Date")
ibov = ibov.rename(columns={'Date': 'ref_date', 'Value': 'Ibovespa'})
ibov["return"] = ibov["Adj Close"].pct_change()
ibov['Ibov_return'] = (1 + ibov['return']).cumprod() -1
ibov = ibov[['ref_date','Ibov_return']]
# Get data for Selic
selic = quandl.get('BCB/11', start_date=start, end_date=end)
selic = selic.reset_index()
selic = selic.rename(columns={'Date': 'ref_date', 'Value': 'Selic'})
selic['Selic'] = selic['Selic'] / 100
selic['selic_return'] = (1 + selic['Selic']).cumprod() - 1
# Get data for CDI
cdi = quandl.get('BCB/12', start_date=start, end_date=end)
cdi = cdi.reset_index()
cdi = cdi.rename(columns={'Date': 'ref_date', 'Value': 'CDI'})
cdi['CDI'] = cdi['CDI'] / (100)
cdi['cdi_return'] = (1 + cdi['CDI']).cumprod() - 1
# Get data for inflation
inf = quandl.get('BCB/433', start_date=start, end_date=end)
inf = inf.reset_index()
inf = inf.rename(columns={'Date': 'ref_date', 'Value': 'Inflation'})
inf['Inflation'] = inf['Inflation'] / 100
inf['inf_return'] = (1 + inf['Inflation']).cumprod() - 1
# Merge dataframes
df = pd.merge(cdi, selic, on="ref_date")
df = pd.merge(df, ibov, on="ref_date")
df = pd.merge(df, inf, on="ref_date")
# Plot data
plt.close()
fig, ax = plt.subplots(figsize=(10,5))
ax.plot(df["ref_date"], df["cdi_return"], label='CDI')
ax.plot(df["ref_date"], df["Ibov_return"], label='Ibov')
ax.plot(df["ref_date"], df["selic_return"], label='Selic')
plt.ylabel('Cumulative return (daily)', fontsize=13)
plt.title('Cumulative Returns  CDI, Ibov, and Selic (2010-2019)', fontsize=15)
plt.xlabel('Date', fontsize=13)
plt.legend(loc='upper left')
plt.show()
```


















## 10.1 Risk & Return: Insights from History {.smaller background="#f2f5ce"} 

And USD?
  
  ```{r}
#| warning: false
#| message: false
#| fig-align: center
#| echo: true
#| output-location: default
#| code-fold: true
#| code-summary: "R"
#| code-line-numbers: true
#| eval: true
#### QUANDL API
# Step to create your key
# 1. Go to https://www.quandl.com/
# 2. Create an account
# 3. Get your key
# 4. Add your key below (I can't give you mine)
api_KEY <- "Your QUANDL API KEY here"
#Ibov
stock<-'^BVSP' 
start<-'2010-01-01' 
end   <- Sys.Date() 
ibov <- yf_get(tickers = stock, 
               first_date = start,
               last_date = end)
ibov<- ibov[order(as.numeric(ibov$ref_date)),]
# Cumulative return Ibov
ibov$Ibov_return <- ibov$cumret_adjusted_prices -1
#Selic
selic <- get_Quandl_series(id_in = c('Selic' = 'BCB/11'),
                           api_key = api_key, 
                           first_date = start,
                           last_date = end)
selic<- selic[order(as.numeric(selic$ref_date)),]
selic$value <- selic$value / 100
# Cumulative return Selic
return_selic <- data.frame(nrow(selic):1)
colnames(return_selic)<- "selic_return"

for(i in (2:nrow(selic))) {
  return_selic[i,1] <- Return.cumulative( selic$value[1:i] )
}
# CDI
cdi <- get_Quandl_series(id_in =  c('CDI' = 'BCB/12'), 
                         api_key = api_key , 
                         first_date = start ,
                         last_date = end)
cdi<- cdi[order(as.numeric(cdi$ref_date)),]
cdi$value <- cdi$value / 100
# Cumulative return CDI
return_CDI <- data.frame(nrow(cdi):1)
colnames(return_CDI)<- "CDI_return"
for(i in (2:nrow(cdi))) {
  return_CDI[i,1] <- Return.cumulative( cdi$value[1:i] )
}
#Inflation Brazil
inf <- get_Quandl_series(id_in =  c('Inflation' = 'BCB/433'),
                         api_key = api_key, 
                         first_date = start,
                         last_date = end)
inf<- inf[order(as.numeric(inf$ref_date)),]
inf$value <- inf$value / 100
# Cumulative return Inflation
return_Inf <- data.frame(nrow(inf):1)
colnames(return_Inf)<- "Inflation_return"

for(i in (2:nrow(inf))) {
  return_Inf[i,1] <- Return.cumulative( inf$value[1:i] )
}
#USD/BRL
library(GetBCBData)
dolar <- gbcbd_get_series(id = 1, 
                          first.date = start,
                          last.date = end)
require(quantmod)
dolar$return <-Delt(dolar$value)
return_dol <- data.frame(nrow(dolar):1)
for(i in (2:nrow(dolar))) {
  return_dol[i,1] <- Return.cumulative( dolar$return[1:i] )
}
colnames(return_dol)<- "Dolar_return"
# Merging dataframes
selic <- cbind(selic, return_selic)
cdi <- cbind(cdi, return_CDI)
inf <- cbind(inf, return_Inf)
dolar <- cbind(dolar, return_dol)
df <- merge(cdi ,selic, by=c("ref_date"))
df <- merge(df ,ibov, by=c("ref_date"))
df2 <- merge(df ,inf, by=c("ref_date"))
colnames(dolar)[1] <- 'ref_date'
df2 <- merge(df ,dolar, by=c("ref_date"))
df2$selic_return[1] <- NA
df2$CDI_return[1] <- NA
df2$Ibov_return[1] <- NA
df2$Inflation_return[1] <- NA
df2$Dolar_return[1] <- NA
p<-ggplot(df2, aes(ref_date)) + 
  geom_line(aes(y = CDI_return, colour = "CDI")) + 
  geom_line(aes(y = Ibov_return, colour = "Ibov")) +
  geom_line(aes(y = selic_return, colour = "Selic")) +
  geom_line(aes(y = Dolar_return, colour = "Dolar")) +
  labs(y='Cumulative return (daily)') +   theme_solarized() +
  ggtitle("Cumulative Returns CDI, Ibov, Selic, and USD (2010-2023)")
ggplotly(p)
```











## 10.1 Risk & Return: Insights from History {.smaller background="#f2f5ce"} 

```{python}
#| warning: false
#| message: false
#| fig-align: center
#| echo: true
#| results: false
#| output: true
#| code-fold: true
#| code-line-numbers: true	
#| output-location: default
#| code-summary: "Python"
#| collapse: true
#| eval: true
# Define API key and other parameters
api_KEY = "Your QUANDL API KEY here"
stock = "^BVSP"
start = "2010-01-01"
end = pd.Timestamp.now()
# Get data for IBOV
ibov = yf.download(stock, start=start, end=end)
ibov = ibov.reset_index()
ibov = ibov.sort_values("Date")
ibov = ibov.rename(columns={'Date': 'ref_date', 'Value': 'Ibovespa'})
ibov["return"] = ibov["Adj Close"].pct_change()
ibov['Ibov_return'] = (1 + ibov['return']).cumprod() -1
ibov = ibov[['ref_date','Ibov_return']]
# Get data for Selic
selic = quandl.get('BCB/11', start_date=start, end_date=end)
selic = selic.reset_index()
selic = selic.rename(columns={'Date': 'ref_date', 'Value': 'Selic'})
selic['Selic'] = selic['Selic'] / 100
selic['selic_return'] = (1 + selic['Selic']).cumprod() - 1
# Get data for CDI
cdi = quandl.get('BCB/12', start_date=start, end_date=end)
cdi = cdi.reset_index()
cdi = cdi.rename(columns={'Date': 'ref_date', 'Value': 'CDI'})
cdi['CDI'] = cdi['CDI'] / (100)
cdi['cdi_return'] = (1 + cdi['CDI']).cumprod() - 1
# Get data for inflation
inf = quandl.get('BCB/433', start_date=start, end_date=end)
inf = inf.reset_index()
inf = inf.rename(columns={'Date': 'ref_date', 'Value': 'Inflation'})
inf['Inflation'] = inf['Inflation'] / 100
inf['inf_return'] = (1 + inf['Inflation']).cumprod() - 1
# Get data for USD/BRL exchange rate
usd_brl = quandl.get('BCB/10813', start_date=start, end_date=end)
usd_brl = usd_brl.reset_index()
usd_brl = usd_brl.rename(columns={'Date': 'ref_date', 'Value': 'USD_BRL'})
usd_brl['usd_brl_return'] = (1 + usd_brl['USD_BRL'].pct_change()).cumprod() - 1
# Merge dataframes
df = pd.merge(cdi, selic, on="ref_date")
df = pd.merge(df, ibov, on="ref_date")
df = pd.merge(df, inf, on="ref_date")
df = pd.merge(df, usd_brl, on="ref_date")
# Plot data
plt.close()
fig, ax = plt.subplots(figsize=(10,5))
ax.plot(df["ref_date"], df["cdi_return"], label='CDI')
ax.plot(df["ref_date"], df["Ibov_return"], label='Ibov')
ax.plot(df["ref_date"], df["selic_return"], label='Selic')
ax.plot(df["ref_date"], df["usd_brl_return"], label='USD',color='darkred')
plt.ylabel('Cumulative return (daily)', fontsize=13)
plt.title('Cumulative Returns  CDI, Ibov, and Selic (2010-2023)', fontsize=15)
plt.xlabel('Date', fontsize=13)
plt.legend(loc='upper left')
plt.show()
```







