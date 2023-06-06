
# 1230 games in a season so start with 
library(httr)
library(rjson)


game = "14620"


request_url = paste0(base_url, game,"?key=",key)

## display the string by evaluating it
request_url

s = GET(request_url)
status_code(s)

game1_id = 10941
last_game_id = 12171 #Need to make sure not off by one (actually 12379?!?!?!)
  # Need to do some analysis of some sort to see if there were duplicate
  # games or if the all star game or something got counted.

  #' For some reason there are a bunch of cancelled games in the middle
  #' that are adding to the count. Thankfully these have SeasonType of 3 so
  #' I can just filter based on that and get those out because they don't
  #' have any data that I'm interested in.


# THIS IS THE PART WHERE WE ACTUALLY USE THE API
base_url = "https://api.sportsdata.io/v3/nba/pbp/json/PlayByPlay/"
key = "2ec12610fd164f9589bbfad218d354b2"

for (i in 10941:12379){
  request_url = paste0(base_url,i,"?key=",key)
  s = GET(request_url)
  as_json <- toJSON(httr::content(s))
  if (status_code(s) != 200){
    print(i)
  }
  write(as_json,paste0("~/COURSES/QAC241 Network Analysis/NBA Game Data/",i,".json"))
}

# I think I ran out of quieries by the time I hit the limit on the old one
key = "8e1c82fdb4454cf8b2d7c014186bd7c3"
for (i in 11686:12379){
  request_url = paste0(base_url,i,"?key=",key)
  s = GET(request_url)
  as_json <- toJSON(httr::content(s))
  if (status_code(s) != 200){
    print(i)
  }
  write(as_json,paste0("~/COURSES/QAC241 Network Analysis/NBA Game Data/",i,".json"))
}