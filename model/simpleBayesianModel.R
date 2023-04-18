get_init_priors = function(L, p) {
  mod_priors = c()
  for (model in L) {
    prior_prob = 1
    for (spec in model) {
      prior_prob = prior_prob*((1-p)^(1-spec))*(p^spec)
    }
    mod_priors = c(mod_priors, prior_prob)
  }
  return(mod_priors)
}

get_likelihoods = function(L, event) {
  placements = event[1:length(event)-1]
  outcome = event[length(event)]
  likelihoods = c()
  for (model in L) {
    activation = 0
    for (i in 1:length(placements)) {
      if (placements[i]==1 & model[i]==1) {
        activation = 1
      }
    }
    likelihoods = c(likelihoods, activation==outcome)
  }
  return(likelihoods)
}

compute_two_posts = function(L, priors, event1, event2) {
  likelihoods1 = get_likelihoods(L, event1)
  likelihoods2 = get_likelihoods(L, event2)
  numerators = likelihoods1*likelihoods2*priors
  norm_term = sum(numerators)
  return(numerators/norm_term)
}

get_blicket_probs = function(L, posts) {
  blicket_probs = c()
  for (i in 1:length(L[[1]])) {
    prob = 0
    for (j in 1:length(L)) {
      prob = prob + L[[j]][i]*posts[j]
    }
    blicket_probs = c(blicket_probs, prob)
  }
  return(blicket_probs)
}

MyIntToBit = function(x, dig) {
  i <- 0L
  string <- numeric(dig)
  while (x > 0) {
    string[dig - i] <- x %% 2L
    x <- x %/% 2L
    i <- i + 1L
  }
  string
}

get_model_list = function(num_blickets) {
  return(lapply(0:(2^num_blickets - 1), function(x) MyIntToBit(x, num_blickets)))
}


bayes_model_func = function(x,prob,event1,event2){
  L = get_model_list(x) # x is the number of candidate causes
  priors = get_init_priors(L, prob) # prob = probability that an object is a blicket
  event1 = event1 #event1/event2 are lists, in which the first n elements correspond to the # of candidate causes and teh last element corresponds to
                  # whether the machine activates
  event2 = event2
  posts = compute_two_posts(L, priors, event1, event2)
  blicket_probs = get_blicket_probs(L, posts)
  print(blicket_probs)
}

# Example use of the command:
bayes_model_func(x=3,0.5,event1=c(1,1,1,1),event2=c(1,0,0,1)) 

# event 1 corresponds to the ABC+ event. Event 2 corresponds to the AB+ event
