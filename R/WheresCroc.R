#'
#'
#'


#' getNodeProbability
#'
#'
getNodeProbability = function(node, observations, prev_f, probs, neighbors) {
  #Compute the probability of Croc reaching the waterhole considering what we know of the previous state
  moving = 0
  for (n in length(neighbors[node])) {
    neighbor = neighbors[[node]][n]
    moving = moving + (1.0 / length(neighbors[[neighbor]])) * prev_f[neighbor]
  }
  # calculate the emission probability
  e = dnorm(observations[["s"]], probs[["salinity"]][node, 1], probs[["salinity"]][node, 2])
  e = e * dnorm(observations[["p"]], probs[["phosphate"]][node, 1], probs[["phosphate"]][node, 2])
  e = e * dnorm(observations[["n"]], probs[["nitrogen"]][node, 1], probs[["nitrogen"]][node, 2])
  # return new_f = sum(prev_f * T) * e
  return(e * moving)
}

#' getProbabilities
#'
#'
getProbabilities = function(prev_f, probs, observations, neighbors) {
  tourist1 = observations[["t1"]]
  tourist2 = observations[["t2"]]
  if (sum(prev_f) == 0) {
    counter = 0
    # loop thorugh all nodes
    for (node in 1:40) {
      # check if both tourists are alive 
      if (!is.na(tourist1) && !is.na(tourist2)) {
        # if tourist is at waterhole and alive, probability = 0
        if (tourist1 == node || tourist2 == node) {
          prev_f[node] = 0
        } else {
          prev_f[node] = 1
          counter = counter + 1
        }
      }
    }
    # probabilities equal at every possible node
    prev_f = prev_f / counter
  }
  
  new_f = replicate(40, 0)
  # check if tourist 1 has been eaten this turn
  if (!is.na(tourist1) && tourist1 < 0) {
    crocNode = -1 * tourist1
    new_f[crocNode] = 1
  }
  # chekc if tourist 2 has been eaten this turn
  else if (!is.na(tourist2) && tourist2 < 0) {
    crocNode = -1 * tourist2
    new_f[crocNode] = 1
  } else {
    # compute the probabilities for each waterhole
    for (i in 1:length(new_f)) {
      
      new_f[i] = getNodeProbability(i, observations, prev_f, probs, neighbors)
    }
  }
  # Normalize probabilities
  new_f = new_f / sum(new_f)
  return (new_f)
}

#' myFunction
#'
#'
myFunction = function(moveInfo,readings,positions,edges,probs) {
  
  #neighbors is a list of list containing the neighbors for each waterhole
  neighbors = list()
  #Each waterhole is its own neighbor
  for (i in 1:40) {
    neighbors[i] = list(i)
  }
  for (edgeNum in 1:nrow(edges)) {
    firstPoint = edges[edgeNum,1]
    secondPoint = edges[edgeNum,2]
    neighbors[[firstPoint]][length(neighbors[[firstPoint]]) + 1] = secondPoint
    neighbors[[secondPoint]][length(neighbors[[secondPoint]]) + 1] <- firstPoint
  }
  
  observations = list()
  observations[['s']] = readings[[1]]
  observations[['p']] = readings[[2]]
  observations[['n']] = readings[[3]]
  observations[['t1']] = positions[[1]]
  observations[['t2']] = positions[[2]]
  observations[['me']] = positions[[3]]
  
  if (length(moveInfo[["mem"]][["prevProbabilities"]]) == 0) {
    moveInfo[["mem"]][["prevProbabilities"]] = replicate(40,0)
  }
  prev_f = moveInfo[["mem"]][["prevProbabilities"]]
  new_f = getProbabilities(prev_f, probs, observations, neighbors)
  # print('ojo')
   print(max(new_f))
   print(hej)
  #moveInfo[['moves']] = moveResult$move
  moveInfo[['mem']][["prev_f"]] = new_f
  return(moveInfo)
}

#' randomWC
#'
#' Control function for Where's Croc where moves are random.
#' @param moveInfo See runWheresCroc for details
#' @param readings See runWheresCroc for details
#' @param positions See runWheresCroc for details
#' @param edges See runWheresCroc for details
#' @param probs See runWheresCroc for details
#' @return See runWheresCroc for details
#' @export
randomWC=function(moveInfo,readings,positions,edges,probs) {
  moveInfo$moves=c(sample(getOptions(positions[3],edges),1),0)
  return(moveInfo)
}

#' manualWC
#'
#' Control function for Where's Croc that allows manual play using keyboard.
#' @param moveInfo See runWheresCroc for details
#' @param readings See runWheresCroc for details
#' @param positions See runWheresCroc for details
#' @param edges See runWheresCroc for details
#' @param probs See runWheresCroc for details
#' @return See runWheresCroc for details
#' @export
manualWC=function(moveInfo,readings,positions,edges,probs) {
  print('hej')
  print(edges)
  options=getOptions(positions[3],edges)
  print("Move 1 options (plus 0 for search):")
  print(options)
  mv1=readline("Move 1: ")
  if (mv1=="q") {stop()}
  if (!mv1 %in% options && mv1 != 0) {
    warning ("Invalid move. Search ('0') specified.")
    mv1=0
  }
  if (mv1!=0) {
    options=getOptions(mv1,edges)
  }
  print("Move 2 options (plus 0 for search):")
  print(options)
  mv2=readline("Move 2: ")
  if (mv2=="q") {stop()}
  if (!mv1 %in% options && mv1 != 0) {
    warning ("Invalid move. Search ('0') specified.")
    mv2=0
  }
  moveInfo$moves=c(mv1,mv2)
  return(moveInfo)
}

#' testWC
#'
#' Use this to debug under multiple circumstances and to see how your function compares with the par function
#' With the default seed of 21, the mean for the par function on this is 5.444, and the sd is approximately 3.853.
#'
#' Your final result will be based on how your function performs on a similar run of 500 games, though with
#' a different seed used to select them.
#'
#' While this is dependent on the machine used, we expect your function to be able to run the 500 evaluation games on
#' the evaluation machine in under 30 seconds. Note that you will need to reuse objects that do not change
#' from game to game (such as the transition matrix and routing information) in order to achieve this sort
#' of speed.
#'
#' The par function takes approximately 3.85 seconds on my laptop. If it takes longer than 30 seconds on the
#' evaluation machine, the time limit will be increased so as to be 25% slower than the par function.
#'
#' The mem (memory) object you use in the function you create (see the runWheresCroc documentation)
#' is passed from game to game. This is so you can reuse whatever you set up there to quickly work out
#' what moves to make in different situations. Note that it contains a status field that can be used to work out
#' when a game ends and a new game begins. See the runWheresCroc documentation for more details.
#'
#' @param myFunction Your function to be passed to runWheresCroc. See runWheresCroc documentation for details.
#' @param verbose Set to 0 for no output, 1 for a summary of the results of the games played (mean,
#' standard deviation and time taken), and 2 for the above plus written output detailing seeds used and the
#' runWheresCroc output of the result of each game.
#' @param returnVec See return value.
#' @param n The number of games to run. In the evaluation this will be 500.
#' @param seed The random seed to use. Pass NA to not set a random seed.
#' @param timeLimit The time limit. If this is breached, a NA is returned.
#' @return If your function is too slow, NA is returned. Otherwise if returnVec is TRUE, a vector containing
#' all results is returned. If returnVec is FALSE, the average performance is returned.
#' @export
testWC=function(myFunction,verbose=0,returnVec=FALSE,n=500,seed=21,timeLimit=300){
  set.seed(seed)
  seeds=sample(1:25000,n)
  startTime=Sys.time()
  mem=NA
  hmm=c()
  for (s in seeds) {
    midTime=Sys.time()
    if (as.numeric(midTime)-as.numeric(startTime)>timeLimit) {
      cat("\nRun terminated due to slowness.")
      return (NA)
    }
    set.seed(s)
    if (verbose==2)
      cat("\nNew game, seed",s)
    res=runWheresCroc(myFunction,doPlot=F,pause=0,verbose=verbose==2,returnMem=T,mem=mem)
    mem=res$mem
    hmm=c(hmm,res$move)
  }
  if (verbose>=1) {
    endTime=Sys.time()
    cat("\nMean moves:", mean(hmm))
    cat("\nSD moves:", sd(hmm))
    cat("\nTime taken:",as.numeric(endTime)-as.numeric(startTime),"seconds.")
  }
  if (returnVec)
    return (hmm)
  else
    return (mean(hmm))
}

#' Run Where's Croc
#'
#' Runs the Where's Croc game. In this game, you are a ranger in an Australian national park.
#' This park consists of a number of waterholes, some of which are connected to each other.
#' There is a crocodile in the park called 'Croc'. Croc has been fitted with sensors that record
#' the salinity, phosphate and nitrogen levels in the water where he is swimming. He was also
#' fitted with a sensor that records his position, but that has broken.
#' Your task is to find Croc using the available information. To aid in this you have information
#' about the probability distributions for different salinity, phosphate and nitrogen levels in
#' different waterholes.
#' There are also two tourists in the park. Both the tourists and Croc walk randomly, each turn
#' moving to one of the neighboring waterholes from where they are or staying still. All moves
#' are equally likely.
#' If Croc and a tourist end up on the same waterhole, Croc will eat the tourist. If you search
#' the waterhole you are on when Croc is there, you have found Croc and win the game.
#' Your score is the number of turns it takes to find Croc.
#' To play manually pass manualWC
#' as the makeMoves function and enter the appropriate numbers to make moves.
#' Note that the croc will move randomly, with a uniform distribution over moving to any adjancent waterholes
#' or staying still.
#' @param makeMoves Your function that takes five arguments: (1) A list of information for the move.
#' This has two fields. The first is a vector of numbers called 'moves', where you will enter
#' the moves you want to make. You should
#' enter two moves (so you can move to a neighboring waterhole and search). Valid moves are the
#' numbers of a neighboring or current waterhole or '0' which means you will search your current
#' waterhole for Croc. The second field is a list called
#' 'mem' that you can use to store information you want to remember from turn to turn. (2) A
#' vector giving the salinity, phosphate and nitrogen reading from Croc sensors at his current
#' location. (3) A vector giving the positions of the two tourists
#' (elements 1 and 2) and yourself (element 3). If a tourist
#' has just been eaten by Croc that turn, the position will be multiplied by -1. If a tourist
#' was eaten by Croc in a previous turn, then the position will be NA. (4) a two column matrix giving the
#' edges paths between waterholes (edges) present (the numbers are from and to numbers for
#' the waterholes). All edges can be crossed both ways, so are only given once.
#' (5) a list of three matrices giving the mean
#' and standard deviation of readings for salinity, phosphate and nitrogen respectively
#' at each waterhole.
#' Your function should return the first argument passed with an updated moves vector
#' and any changes to the 'mem' field you wish to access later on.
#' @param doPlot A Boolean stating if you want the gameboard to be plotted each turn
#' @param showCroc A Boolean value specifying whether you want Croc to be shown on the gameboard.
#' Note that you are not permitted to use this visual information when you are scored.
#' @param pause The pause period between moves. Designed to give time to plot game.
#' @param verbose Set to FALSE to stop any print output
#' @param returnMem Should the info$mem field be returned? If so, the output is a list consisting of
#' the move field, giving the number of moves in the game, and the mem field consisting of the mem
#' object
#' @param mem If you returned a mem object from a previous run, it can be passed here. It's status
#' will be set to 1. Otherwise a new mem list will be created with status set to 0. The idea is
#' to speed up multiple runs, such as the evaluation run of 500 games, by avoiding redoing
#' expensive initial setups of the transition matrix and routing informing.
#' @return A string describing the outcome of the game.
#' @export
runWheresCroc=function(makeMoves,doPlot=T,showCroc=F,pause=1,verbose=T,returnMem=F,mem=NA) {
  positions=sample(1:40,4) # Croc, BP1, BP2, Player
  points=getPoints()
  edges=getEdges()
  probs=getProbs()
  move=0
  moveInfo=list(moves=c(),mem=list(status=0))
  if (!all(is.na(mem)))
    moveInfo$mem=mem
  first=T
  while (!is.na(positions[1])) {
    move=move+1
    if (!first) {
      positions[1]=sample(getOptions(positions[1],edges),1)
      if (!is.na(positions[2])&&positions[2]>0) {
        positions[2]=sample(getOptions(positions[2],edges),1)
      } else if (!is.na(positions[2]) && positions[2]<0) {
        positions[2]=NA
      }
      if (!is.na(positions[3])&&positions[3]>0) {
        positions[3]=sample(getOptions(positions[3],edges),1)
      } else if (!is.na(positions[3]) && positions[3]<0) {
        positions[3]=NA
      }
      if (!is.na(positions[2]) && positions[2]==positions[1]) {
        positions[2]=-positions[2]
      }
      if (!is.na(positions[3]) && positions[3]==positions[1]) {
        positions[3]=-positions[3]
      }
    }
    else
      first=F

    if (doPlot)
      plotGameboard(points,edges,move,positions,showCroc)

    Sys.sleep(pause)

    readings=getReadings(positions[1],probs)
    moveInfo=makeMoves(moveInfo,readings,positions[2:4],edges,probs)
    if (length(moveInfo$moves)!=2) {
      stop("Error! Passed makeMoves function should return a vector of two elements.")
    }
    for (m in moveInfo$moves) {
      if (m==0) {
        if (positions[1]==positions[4]) {
          if (verbose)
            cat("\nCongratualations - You got croc at move ",move)
          if (returnMem) {
            mem=moveInfo$mem
            mem$status=1
            return (list(move=move,mem=mem))
          }
          return (move)
        }
      } else {
        if (m%in%getOptions(positions[4],edges)) {
          positions[4]=m
        } else {
          warning("Invalid move.")
        }
      }
    }
  }
}
#' @keywords internal
getPoints=function() {
  points=matrix(c(1,1),ncol=2)
  points=rbind(points,c(1,7))
  points=rbind(points,c(1,17))
  points=rbind(points,c(2,3))
  points=rbind(points,c(2,12))
  points=rbind(points,c(3,2))
  points=rbind(points,c(3,19))
  points=rbind(points,c(4,7))
  points=rbind(points,c(4,11))
  points=rbind(points,c(5,5))
  points=rbind(points,c(5,15))
  points=rbind(points,c(6,1))
  points=rbind(points,c(6,20))
  points=rbind(points,c(7,6))
  points=rbind(points,c(7,11))
  points=rbind(points,c(8,2))
  points=rbind(points,c(8,14))
  points=rbind(points,c(8,18))
  points=rbind(points,c(9,6))
  points=rbind(points,c(10,10))
  points=rbind(points,c(10,18))
  points=rbind(points,c(11,1))
  points=rbind(points,c(11,12))
  points=rbind(points,c(12,6))
  points=rbind(points,c(12,12))
  points=rbind(points,c(13,16))
  points=rbind(points,c(14,4))
  points=rbind(points,c(14,12))
  points=rbind(points,c(14,20))
  points=rbind(points,c(15,3))
  points=rbind(points,c(15,8))
  points=rbind(points,c(15,17))
  points=rbind(points,c(16,14))
  points=rbind(points,c(17,3))
  points=rbind(points,c(17,18))
  points=rbind(points,c(18,10))
  points=rbind(points,c(19,13))
  points=rbind(points,c(20,2))
  points=rbind(points,c(20,6))
  points=rbind(points,c(20,19))
  return (points)
}

#' @keywords internal
getEdges=function() {
  edges=matrix(c(1,2),ncol=2)
  edges=rbind(edges,c(1,4))
  edges=rbind(edges,c(1,6))
  edges=rbind(edges,c(2,4))
  edges=rbind(edges,c(2,5))
  edges=rbind(edges,c(3,5))
  edges=rbind(edges,c(3,7))
  edges=rbind(edges,c(4,6))
  edges=rbind(edges,c(4,8))
  edges=rbind(edges,c(5,7))
  edges=rbind(edges,c(5,9))
  edges=rbind(edges,c(6,12))
  edges=rbind(edges,c(7,11))
  edges=rbind(edges,c(7,13))
  edges=rbind(edges,c(8,9))
  edges=rbind(edges,c(8,10))
  edges=rbind(edges,c(9,11))
  edges=rbind(edges,c(10,12))
  edges=rbind(edges,c(10,14))
  edges=rbind(edges,c(11,13))
  edges=rbind(edges,c(11,15))
  edges=rbind(edges,c(12,16))
  edges=rbind(edges,c(13,18))
  edges=rbind(edges,c(14,15))
  edges=rbind(edges,c(14,16))
  edges=rbind(edges,c(15,17))
  edges=rbind(edges,c(16,19))
  edges=rbind(edges,c(16,22))
  edges=rbind(edges,c(17,18))
  edges=rbind(edges,c(17,19))
  edges=rbind(edges,c(17,20))
  edges=rbind(edges,c(18,21))
  edges=rbind(edges,c(19,20))
  edges=rbind(edges,c(19,22))
  edges=rbind(edges,c(20,23))
  edges=rbind(edges,c(21,23))
  edges=rbind(edges,c(21,29))
  edges=rbind(edges,c(22,24))
  edges=rbind(edges,c(22,27))
  edges=rbind(edges,c(23,24))
  edges=rbind(edges,c(23,25))
  edges=rbind(edges,c(24,25))
  edges=rbind(edges,c(24,27))
  edges=rbind(edges,c(25,26))
  edges=rbind(edges,c(25,27))
  edges=rbind(edges,c(25,28))
  edges=rbind(edges,c(26,28))
  edges=rbind(edges,c(26,29))
  edges=rbind(edges,c(27,30))
  edges=rbind(edges,c(27,31))
  edges=rbind(edges,c(28,31))
  edges=rbind(edges,c(28,32))
  edges=rbind(edges,c(29,32))
  edges=rbind(edges,c(29,35))
  edges=rbind(edges,c(30,31))
  edges=rbind(edges,c(30,34))
  edges=rbind(edges,c(31,33))
  edges=rbind(edges,c(31,34))
  edges=rbind(edges,c(32,33))
  edges=rbind(edges,c(32,35))
  edges=rbind(edges,c(33,35))
  edges=rbind(edges,c(33,36))
  edges=rbind(edges,c(33,37))
  edges=rbind(edges,c(34,36))
  edges=rbind(edges,c(34,38))
  edges=rbind(edges,c(35,40))
  edges=rbind(edges,c(36,37))
  edges=rbind(edges,c(36,39))
  edges=rbind(edges,c(37,39))
  edges=rbind(edges,c(37,40))
  edges=rbind(edges,c(38,39))

  return (edges)
}

#' @keywords internal
getProbs=function(){
  salinity=cbind(runif(40,100,200),runif(40,5,30))
  phosphate=cbind(runif(40,100,200),runif(40,5,30))
  nitrogen=cbind(runif(40,100,200),runif(40,5,30))
  list(salinity=salinity,phosphate=phosphate,nitrogen=nitrogen)
}

#' @keywords internal
getReadings=function(point,probs){
  c(
    rnorm(1,probs$salinity[as.numeric(point),1],probs$salinity[as.numeric(point),2]),
    rnorm(1,probs$phosphate[as.numeric(point),1],probs$phosphate[as.numeric(point),2]),
    rnorm(1,probs$nitrogen[as.numeric(point),1],probs$nitrogen[as.numeric(point),2])
  )
}


#' @keywords internal
plotGameboard=function(points,edges,move,positions,showCroc) {
  plot(points,pch=18,col="blue",cex=2,xlab="X",ylab="Y",main=paste("Where's Croc - Move",move))
  xFrom=points[edges[,1],1]
  yFrom=points[edges[,1],2]
  xTo=points[edges[,2],1]
  yTo=points[edges[,2],2]
  segments(xFrom,yFrom,xTo,yTo)
  for (bp in 2:3)
    if (!is.na(positions[bp])) {
      if (positions[bp]>0) {
        points(points[as.numeric(positions[bp]),1],points[as.numeric(positions[bp]),2],col="orange",pch=17,cex=4)
      } else {
        points(points[-as.numeric(positions[bp]),1],points[-as.numeric(positions[bp]),2],col="red",pch=17,cex=4)
      }
    }
  points(points[as.numeric(positions[4]),1],points[as.numeric(positions[4]),2],col="green",pch=15,cex=4)
  if (showCroc) {
    points(points[as.numeric(positions[1]),1],points[as.numeric(positions[1]),2],col="red",pch=15,cex=4)
  }
  text(points[,1]+.4, points[,2], labels=as.character(1:40))
}

#' @keywords internal
getOptions=function(point,edges) {
  c(edges[which(edges[,1]==point),2],edges[which(edges[,2]==point),1],point)
}
