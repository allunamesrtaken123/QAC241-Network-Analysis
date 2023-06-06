
library(readr)
library(dplyr)
library(igraph)
library(ggplot2)
library(httr)
library(jsonlite)
library(gridExtra)

#' It would be cool (and much easier to interpret the data) to get the names
#' of the players rather than ID's. Maybe for each team, just get the names
#' of the top 7 players from each team because I have limited queries
#' 
#' If you aren't receiving + giving more than 50 assists in a season, you 
#' are probably not an important member of the team.
#' 
#' Analysis to Do:
#' (1) It would be interesting to look at in-degree versus out-degree to
#' see if it varies greatly for each team and if so, see if it varies
#' consistently.
#' 
#' (2) Next, identifying what the distribution of in/out-degree looks like for 
#' winning teams/losing teams looks like
#' 
#' (3) A really key metric that is integral to this analysis is to calculate
#' the proportion of assists received/assists made for each player out of the 
#' entire team. One could calculate this for all players and scrape the names
#' for the top bunch. 
#' 
#' (4) Next, you could see who assists way more than gets assisted or 
#' vice versa. I'd predict that spot up shooters like Kyle Korver would
#' get assited like crazy but not have many assists and true PGs like Chris
#' Paul would be on the other end of the spectrum. It would be interesting
#' to see if there are any surprises in any of these statistics though.
#' 
#' For using the remaining queries, it may be smart to use them to put
#' names to numbers of leaders in the statistics that I create, as this would
#' help to illustrate them, especially to a knowledgeable basketball fan.

data <- read_csv("~/COURSES/QAC241 Network Analysis/full_assists.csv") %>%
    filter(!is.na(assisted))

#' Below I want to go through and for each team, I want to calculate the 
#' proportion of assists of the entire team each player contributed (assistor).
#' I'll then pack that into a data frame and look at the top assistors.

abrs <- unique(data[['team']])
prop_assistor_df = tibble(team = c(),player_id = c(), prop_assistor = c())
for (teams in abrs){
  t_data <- data %>% filter(team == teams) %>% select(assistor,assisted)
  t_data_g <- graph_from_data_frame(t_data,directed = T)
  t_dgr <- sort(degree(t_data_g, mode = "out"),decreasing = T)
  props <- sort(t_dgr/sum(t_dgr),decreasing = T)
  prop_assistor_df <- prop_assistor_df %>% bind_rows(tibble(team = teams,
                                        player_id = names(props),
                                        prop_assistor = unname(props)))
}
  #' I have two thoughts on how I want to extract the data here. I could take
  #' two/three players from every team and get their names, or I could take
  #' the top 75 in the league and see which teams have more people and which
  #' have less. I think I like the second option
top_assistors <- prop_assistor_df %>% 
  arrange(desc(prop_assistor)) %>% 
  slice_head(n=75)
top_assistors <- transform(top_assistors, player_id = as.numeric(player_id))



#' Now I want to get the actual names of these players. To do this, I'm going
#' to use the Player Details by Player endpoint and extract the first name
#' and last names column, matching it with the corresponding player ID.

base_url = "https://api.sportsdata.io/v3/nba/scores/json/Player/"
key = "15e0c11ca4ea434c8bc9dc5444d28a5b"

#for (i in top_assistors$player_id){
#  request_url = paste0(base_url,i,"?key=",key)
#  s = GET(request_url)
#  as_json <- toJSON(httr::content(s))
#  if (status_code(s) != 200){
#    print(i)
#  }
#  write(as_json,paste0("~/COURSES/QAC241 Network Analysis/Assistor ID Name/",i,".json"))
#}

names_ids <- read_csv("~/COURSES/QAC241 Network Analysis/assistor.csv")
top_assistors <- inner_join(top_assistors,names_ids,by = "player_id")


#'*-----------------------------------------------------------------------*
#'* Now to do the same analysis but for players that get assisted the most*
#'*-----------------------------------------------------------------------*


  #'* Data Frame Creation*
abrs <- unique(data[['team']])
prop_assisted_df = tibble(team = c(),player_id = c(), prop_assisted = c())
for (teams in abrs){
  t_data <- data %>% filter(team == teams) %>% select(assistor,assisted)
  t_data_g <- graph_from_data_frame(t_data,directed = T)
  t_dgr <- sort(degree(t_data_g, mode = "in"),decreasing = T)
  props <- sort(t_dgr/sum(t_dgr),decreasing = T)
  prop_assisted_df <- prop_assisted_df %>% bind_rows(tibble(team = teams,
                                                            player_id = names(props),
                                                            prop_assisted = unname(props)))
}
#' I have two thoughts on how I want to extract the data here. I could take
#' two/three players from every team and get their names, or I could take
#' the top 75 in the league and see which teams have more people and which
#' have less. I think I like the second option
top_assisted <- prop_assisted_df %>% 
  arrange(desc(prop_assisted)) %>% 
  slice_head(n=75)
top_assisted <- transform(top_assisted, player_id = as.numeric(player_id))
    

  #'* Scrape Time!*
base_url = "https://api.sportsdata.io/v3/nba/scores/json/Player/"

#for (i in top_assisted$player_id){
#  request_url = paste0(base_url,i,"?key=",key)
#  s = GET(request_url)
#  as_json <- toJSON(httr::content(s))
#  if (status_code(s) != 200){
#    print(i)
#  }
#  write(as_json,paste0("~/COURSES/QAC241 Network Analysis/Assisted ID Name/",i,".json"))
#}

assisted_ids <- read_csv("~/COURSES/QAC241 Network Analysis/assisted.csv")
top_assisted <- inner_join(top_assisted,assisted_ids,by = "player_id")


#'*-----------------------------------------------------------------------*
#'* Finding Players with Large Discrepancies Between Assistor and Assisted*
#'*-----------------------------------------------------------------------*

g <- graph_from_data_frame(data %>% select(assistor,assisted)
                           , directed = T)
# First, calculate the in degree (assisted) for each player
in_degree <- degree(g,mode = "in")
assisted_players <- names(in_degree)
in_degree_df <- tibble(assisted_players,in_degree)

# Next, calculate the out degree (assistor) for each player
out_degree <- degree(g,mode = "out")
assistor_players <- names(out_degree)
out_degree_df <- tibble(assistor_players,out_degree)

# Then, calculate the difference for each player and sort the columns
relevant_players <- unique(c(assisted_players,assistor_players))

# The difference is in-degree - out-degree. This means that a player will
# have a positive value if they receive more assists than they dish out. On
# the other hand, a player will have a negative score if they make more
# assists than they receive.
  # I would expect ball heavy players to have negative scores while spot-up
  # shooters like Kyle Korver would have positive scores, as they are on the
  # receiving end of many more assists than the facilitator.
degree_df <- inner_join(in_degree_df,out_degree_df,
                        by = c("assisted_players"="assistor_players")) %>%
  mutate(dif = in_degree - out_degree)

most_assistors <- degree_df %>% arrange(dif) %>% slice_head(n=25)
most_assisted <- degree_df %>% arrange(desc(dif)) %>% slice_head(n=25)

# Lastly, I want to scrape the unique names of players in most_assistors and/or
# most_assisted.

  # Get the union of the player_ids
dif_player_ids <- unique(c(most_assistors$assisted_players,
                           most_assisted$assisted_players))
  # Now, iterate over this list of names and scrape their actual name and
  # save the output in a json file=
base_url = "https://api.sportsdata.io/v3/nba/scores/json/Player/"

#for (i in dif_player_ids){
#  request_url = paste0(base_url,i,"?key=",key)
#  s = GET(request_url)
#  as_json <- toJSON(httr::content(s))
#  if (status_code(s) != 200){
#    print(i)
#  }
#  write(as_json,paste0("~/COURSES/QAC241 Network Analysis/dif_players/",i,".json"))
#}

diff_names <- read_csv("~/COURSES/QAC241 Network Analysis/diffs.csv")
diff_names <- transform(diff_names, player_id = as.character(player_id))
most_assistors <- left_join(most_assistors,diff_names, 
                            by = c("assisted_players" = "player_id"))
most_assisted <- left_join(most_assisted,diff_names,
                          by = c("assisted_players"="player_id"))



#'*----------------------------------------------------------------------------*
#'*ANALYSIS*
#'*PART    *
#'*----------------------------------------------------------------------------*

data <- read_csv("~/COURSES/QAC241 Network Analysis/full_assists.csv") %>%
  filter(!is.na(assisted))

#' Part 1: *Visuals + Graphs*
  #' I want to plot the node degree distribution for each of the two top teams
  #' in each conference and then for the two worst teams in each conference.
  #' If they show any kinds of differences (try in/out/all), I might include 
  #' them in my presentation after making them prettier. The top teams in the 
  #' 2018-19 regular season were the Bucks, Raptors, Warriors, and Nuggets.
  #'

  # For the good teams
top_team_degree_df <- tibble(team = c(),in_degree = c(),out_degree = c())
top_teams <- c("TOR","BOS","HOU","GS")
for (teams in top_teams){
  d <- data %>% filter(team == teams) %>% select(assistor,assisted)
  dg <- graph_from_data_frame(d,directed = T)
  od <- degree(dg, mode = "out")
  id <- degree(dg, mode = "in")
  top_team_degree_df <- rbind(top_team_degree_df,
    tibble(team = teams,in_degree = id,out_degree = od) %>%
      filter(in_degree > 25 & out_degree > 25))
}

  # Now for the bad ones
bad_team_degree_df <- tibble(team = c(),in_degree = c(),out_degree = c())
bad_teams <- c("ORL","ATL","MEM","PHO")
for (teams in bad_teams){
  d <- data %>% filter(team == teams) %>% select(assistor,assisted)
  dg <- graph_from_data_frame(d,directed = T)
  od <- degree(dg, mode = "out")
  id <- degree(dg, mode = "in")
  bad_team_degree_df <- rbind(bad_team_degree_df,
                              tibble(team = teams,in_degree = id,out_degree = od) %>%
                                filter(in_degree > 25 & out_degree > 25))
}

# Can't figure out how to get this cleaned up for the life of me! I feel
# like it'd be possible to have a conditional and then just a for loop but I
# can't get it to work.

# Either way, this function plots data from data1 and data2 (depending on in_out)
# with xmax and ymax as the scales and saves them to path1 and path2.
plot_save <- function(path1,path2,data1,data2,xmax,ymax,in_out){
  if (in_out == "out"){
    # 1 will be top
    top_od <- ggplot(data = data1) + 
      geom_freqpoly(aes(x = out_degree,color = team)) + 
      theme_classic() + 
      labs(x = "Outdegree",y = "Frequency") +
      theme(legend.position = "none") + 
      facet_wrap(~team) + xlim(0,xmax) + ylim(0,ymax)
    plot(top_od)
    ggsave(path1,plot = top_od, dpi = 500, limitsize = F)
    # 2 will be bad
    bad_od <- ggplot(data = data2) + 
      geom_freqpoly(aes(x = out_degree,color = team)) + 
      theme_classic() + 
      labs(x = "Outdegree",y = "Frequency") +
      theme(legend.position = "none") + 
      facet_wrap(~team) + xlim(0,xmax) + ylim(0,ymax)
    plot(bad_od)
    ggsave(path2,plot = bad_od, dpi = 500, limitsize = F)
  }else{
    # 1 will be top
    top_id <- ggplot(data = data1) + 
      geom_freqpoly(aes(x = in_degree,color = team)) + 
      theme_classic() + 
      labs(x = "Indegree",y = "Frequency") +
      theme(legend.position = "none") + 
      facet_wrap(~team) + xlim(0,xmax) + ylim(0,ymax)
    plot(top_id)
    ggsave(path1,plot = top_id, dpi = 500, limitsize = F)
    # 2 will be bad
    bad_id <- ggplot(data = data2) + 
      geom_freqpoly(aes(x = in_degree,color = team)) + 
      theme_classic() + 
      labs(x = "Indegree",y = "Frequency") +
      theme(legend.position = "none") + 
      facet_wrap(~team) + xlim(0,xmax) + ylim(0,ymax)
    plot(bad_id)
    ggsave(path2,plot = bad_id, dpi = 500, limitsize = F)
  }
}

# For outdegree I want x to be [0,700] and y to be [0,5]
#plot_save("~/COURSES/QAC241 Network Analysis/Visuals/Top Outdegree.png",
          #"~/COURSES/QAC241 Network Analysis/Visuals/Bad Outdegree.png",
          #top_team_degree_df, bad_team_degree_df,700,5,"out")

# For indegree I want x to be [0,600] and y to be [0,4]
#plot_save("~/COURSES/QAC241 Network Analysis/Visuals/Top Indegree.png",
          #"~/COURSES/QAC241 Network Analysis/Visuals/Bad Indegree.png",
          #top_team_degree_df, bad_team_degree_df,600,4,"in")


#' Part 2: *Diving Deeper Into My Statistics*
#' I also want to put up a table with the names of the top players in 
#' each category
#' * For the out_degree proportion category, I would like to compare this with
#'   the generally accepted and widespread assists stat and explain why I like
#'   mine better, explain the utility of it


  #'*Proportion of Team Assists Received*
  # The table is a bit funky in that it has their names as one element lists,
  # plus there is no need for the player_id in the visual but it's nice to
  # have in the actual data, so I'm going to make a nicer table to screenshot
  # and include

par_vis <- tibble('Prop. Assists Received' = c(.215,.214,.189,.173,.172,
                                              .171,.168,.166,.163,.160),
                  'Player' = c("Karl-Anthony Towns","Anthony Davis",
                               "Jusuf Nurkic", "Paul George",
                               "Klay Thompson", "Clint Capela",
                               "Giannis Antetokounmpo","Khris Middleton",
                               "Lamarcus Aldridge","Marc Gasol"),
                  'Team' = c("MIN","NO","POR","OKC","GS",
                             "HOU","MIL","MIL","SA","MEM"))

#pdf("~/COURSES/QAC241 Network Analysis/assisted_vis.pdf")
#grid.table(par_vis)
#dev.off()

pai_vis <- tibble('Prop. Assist Initiated' = c(.324,.284,.251,.206,.202,
                                               .190,.189,.196,.183,.183),
                  'Player' = c("Russell Westbrook","Lebron James",
                               "James Harden", "Ben Simmons",
                               "Damian Lillard","Kyle Lowry",
                               "Spencer Dinwiddie","Jeff Teague",
                               "Chris Paul","Rajon Rondo"),
                  'Team' = c("OKC","CLE","HOU","PHI","POR",
                             "TOR","BKN","MIN","HOU","NO"))
#pdf("~/COURSES/QAC241 Network Analysis/assistor_vis.pdf")
#grid.table(pai_vis)
#dev.off()

mad_vis <- tibble('Indegree' = c(545,418,347,409,571,528,360,375,383,440),
            'Outdegree' = c(240,161,100,183,355,315,155,170,179,250),
            'Difference' = c(305,257,247,226,216,213,205,205,204,190),
            'Name' = c("Klay Thompson","Clint Capela","Serge Ibaka",
                       "TJ Warren","Anthony Davis","Karl-Anthony Towns",
                       "Dirk Nowitzki","Taj Gibson","Derrick Favors", "Jusuf Nurkic"))
#pdf("~/COURSES/QAC241 Network Analysis/mad_vis.pdf")
#grid.table(mad_vis)
#dev.off()

mar_vis <- tibble('Indegree' = c(316,232,386,319,110,180,140,168,258,257),
                  'Outdegree' = c(1000,838,939,819,537,586,542,564,627,576),
                  'Difference' = c(-684,-606,-553,-500,-427,-406,-402,-396,-369,-319),
                  'Name' = c("Russell Westbrook","James Harden",
                             "Lebron James","Ben Simmons",
                             "Rajon Rondo","Spencer Dinwiddie",
                             "Chris Paul","Jeff Teague",
                             "Damian Lilliard", "Kemba Walker"))
#pdf("~/COURSES/QAC241 Network Analysis/mar_vis.pdf")
#grid.table(mar_vis)
#dev.off()