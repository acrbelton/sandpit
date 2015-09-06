# load required libraries

library(shiny)
library(igraph)

# load external code

source("globals.R")                # global variables etc.
source("create_community_data.R")  # creates dummy community data
source("database.R")               # functions to liaise with the database

# build a dummy community

community_data <- create_random_community()

# create a list of twitter ids labelled by the corresponding handles

community_names <- as.list(community_data$twitter_id)
names(community_names) <- lapply(community_data$twitter_id, function(x) getTwitterHandle(x))

# create initial interactions for the community

interaction_data <- create_random_interactions(community_data)

# build an initial igraph

g <- graph_from_data_frame(interaction_data[,c("twitter_id_A", "twitter_id_B")],
                           directed = FALSE,
                           vertices = community_data[,"twitter_id"])

# label vertices with twitter ids

V(g)$label <- unlist(community_names)

# shape vertices by segment

V(g)$shape <- sapply(community_data$segment,
                     function(x) switch( as.character(x),
                                         "Politicians" = "circle",
                                         "Media" = "square",
                                         "3rd Parties" = "square",
                                         "Advisors" = "circle",
                                         "rectangle"))

# specify x and y coordinates for each vertex at random

community_size <- nrow(community_data)
Xstep <- floor(XDIM / community_size)
Ystep <- floor(YDIM / community_size)

# Conservatives go on the right, members of other parties on the left

xCoords <- sapply(community_data$party, function(x) switch(as.character(x),
                                                           "Conservative" = XDIM,
                                                           0))
xCoords <- xCoords - (Xstep * sample(1:community_size, community_size, replace = FALSE))

# Media and 3rd Parties go at the bottom, others (Politicians and Advisors) at the top

yCoords <- - sapply(community_data$segment, function(x) switch(as.character(x),
                                                               "Media" = YDIM,
                                                               "3rd Parties" = YDIM,
                                                               0))
yCoords <- yCoords + (Ystep * sample(1:community_size, community_size, replace = FALSE))

# this gives the layout for plot.igraph

graphLayout <- cbind(xCoords / (1.25 * XDIM), yCoords / (1.25 * YDIM))

# define server logic

shinyServer(function(input, output, session) {

  # output the community data, for debugging
  
  output$communityData <-
    renderDataTable(community_data, options = list(paging = FALSE, searching = FALSE))
  
  # output the community dropdown
  
  output$dropdown <- renderUI({
    memberList <- list(0)
    names(memberList) <- NOSELECTION
    memberList <- c(memberList, community_names[order(names(community_names))])
    selectInput("memberSelect",
                label = h4("Community member"), 
                choices = memberList,
                selected = 1)
  })
  
  # output the community graph
  
  output$communityGraph <-
    renderPlot({
      
      # check for new interactions and update accordingly
      # newInts has columns (active_id, passive_id, is_RT)
      
      newInts <- newInteractions(session)
      
      if (!is.null(newInts)){

        # update no_of_RTs (passive because must be RTed by someone else)
        
        community_data$no_of_RTs <-
          community_data$no_of_RTs + sapply(community_data$twitter_id,
                                            function(x) sum((newInts$passive_id == x) & (newInts$is_RT == TRUE)))
        
        # update no_of_tweets (active because must be tweeting)
        
        community_data$no_of_tweets <-
          community_data$no_of_tweets + sapply(community_data$twitter_id,
                                               function(x) sum((newInts$active_id == x) & (newInts$is_RT == FALSE)))
        
        # update interaction scores
        # increase score if interaction already present
        # otherwise add new row and edge
        
        # each column of intPairs is of the form (twitter_id_A, twitter_id_B, score_increment)
        
        intPairs <- apply(newInts[,c("active_id","passive_id")], 1, function(x) x[order(x)])
        intPairs <- as.data.frame(rbind(intPairs,
                                        apply(intPairs,
                                              2,
                                              function(x) with(interaction_data,
                                                          sum((twitter_id_A == x[1]) & (twitter_id_B == x[2]))
                                 ))))
        
        # increase the score of existing rows by score_increment
        
        addScores <- intPairs[which(intPairs[3,] != 0)]
        addScoreRows <- apply(addScores,
                              2,
                              function(x)
                              which((interaction_data$twitter_id_A == x[1]) & (interaction_data$twitter_id_B == x[2])))
        incScores <- sapply(seq_len(nrow(interaction_data)),
                            function(x) if(x %in% addScoreRows) addScores[addScoreRows == x][3,] else 0)
        interaction_data$score <- interaction_data$score + incScores
        
        # add rows to interaction_data and edges to g where score_increment = 0
        
        newRows <- t(intPairs[which(intPairs[3,] == 0)][1:2,])
        
        newEdges <- lapply(seq_len(nrow(newRows)), function(x) as.character(newRows[x,]))
        
        newRows <- cbind(newRows, rep(1, nrow(newRows)))
        colnames(newRows) <- c("twitter_id_A", "twitter_id_B", "score")
        interaction_data <- rbind(interaction_data, newRows)
        
        g <- add.edges(g, unlist(newEdges))
                        
        # update locations - TO DO
        
        layoutDelta <- Reduce('+',
                              lapply(newScoreRows,
                                     function(x) {
                                       aRow <- which(community_data$twitter_id == interaction_data[x, "twitter_id_A"])
                                       bRow <- which(community_data$twitter_id == interaction_data[x, "twitter_id_B"])

                                       aCoords <- graphLayout[aRow,]
                                       bCoords <- graphLayout[bRow,]
                                       
                                       mult <- PMULTIPLIER^incScores[x]
                                       
                                       delta <- mat.or.vec(nrow(graphLayout), 2)
                                       delta[aRow,] <- (aCoords * mult) + (bCoords * (1 - mult))
                                       delta[bRow,] <- (aCoords * (1 - mult)) + (bCoords * mult)
                                       
                                       return(delta)
                                     }))
      }
      
      # maximum number of tweets of any community member
      
      tweetmax <-max(community_data$no_of_tweets)

      # maximum number of RTs of any community member
      
      RTmax <- max(community_data$no_of_RTs)
      
      # maximum interaction score
      
      scoremax <- max(interaction_data$score)

      # colour vertices accordinng to party, with intensity related to number of RTs
      
      V(g)$color <- apply(community_data,
                          1,
                          function(x) {
                            partyColour <- switch(x["party"],
                                                  "Conservative" = CONCOL,
                                                  "Labour" = LABCOL,
                                                  "Liberal Democrat" = LIBDEMCOL,
                                                  "UKIP" = UKIPCOL,
                                                  UNKNOWNCOL)
                            rgbColour <- paste(partyColour,
                                               as.hexmode(128 + floor(128 * as.integer(x["no_of_RTs"]) / (RTmax + 1))),
                                               sep="")
                          })

      # scale vertices according to the number of tweets produced
      
      V(g)$size <- VERTEXSIZE * (1 + community_data$no_of_tweets / (tweetmax + 1))
      
      # color the edges
      
      E(g)$color <- EDGECOLOUR
      
      # highlight the vertex selected from the dropdown (if any) (volatile)
      # and highlight nearest neighbours if checkbox ticked
      
      if (!is.null(input$memberSelect) && input$memberSelect != 0) {
        
        if (input$neighbours) {
          
          nearNbs <- as.vector(neighborhood(g, 1, V(g)[V(g)$label == input$memberSelect])[[1]])
          
          V(g)[nearNbs]$color <- NEIGHBOURHIGHLIGHT
          E(g)[from(V(g)$label == input$memberSelect)]$color <- EDGEHIGHLIGHT
          
        }
        
        V(g)$color[V(g)$label == input$memberSelect] <- VERTEXHIGHLIGHT
        
      }
      
      # make the widths of edges related to interaction scores
      
      E(g)$width <- EDGESIZE * (1 + interaction_data$score / (scoremax + 1)) * log(input$scale)

      # draw the graph
      
      plot(g,
           layout = graphLayout,
           rescale = FALSE,
           xlim = range(graphlayout[,1]),
           ylim = range(graphlayout[,2]),
           frame = TRUE)
    })
  
  # text output for debugging purposes
  
   # output$text1 <- renderText(as.character(community_data$no_of_RTs))
})