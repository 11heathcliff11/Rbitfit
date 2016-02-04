library(httr)

key <- '78a2a677ef62755e08d3cf4197e8042b'
secret <- 'f4bcfad2bd8afed9d9d2eae89f83f291'
clientID = "229WRY"


tokenURL <- 'http://api.fitbit.com/oauth/request_token'
accessTokenURL <- 'https://api.fitbit.com/oauth2/token'
authorizeURL <- 'https://www.fitbit.com/oauth2/authorize'
scope <- 'activity nutrition heartrate location nutrition profile settings sleep social weight'
request_url <- 'https://api.fitbit.com/1/user/-/activities/date/2015-12-12.json'

oauthString <- paste0(authorizeURL ,
                      "?response_type=token" ,
                      "&client_id=",
                       clientID,
                      "&redirect_uri=http%3A%2F%2Flocalhost%3A1410",
                      "&scope=",
                      scope,
                      "&expires_in=604800"
                      )
print(oauthString)


t <- GET(oauthString)

## Take the oauthString. Type in Browser and get the token from the redirected URL. Put the token String into the token field below


token <- 'Bearer eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE0NTUxOTM0NzAsInNjb3BlcyI6Indwcm8gd2xvYyB3bnV0IHdzbGUgd3NldCB3aHIgd3dlaSB3YWN0IHdzb2MiLCJzdWIiOiIzTUc0Q1kiLCJhdWQiOiIyMjlXUlkiLCJpc3MiOiJGaXRiaXQiLCJ0eXAiOiJhY2Nlc3NfdG9rZW4iLCJpYXQiOjE0NTQ1ODg2NzB9.S9bksE8wmqXW2tfb3rckIyMvprFICUZj4vEE5pjg1xA'

request <- GET(request_url,
               add_headers("Authorization"= token))

bin <- content(request , as = "text")
writeBin(bin, "myfile.json")

str(content(request))
headers(request)




