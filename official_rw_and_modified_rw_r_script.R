##################################################
####### TRADITIONAL RESCORLA WAGNER MODELS #######
##################################################

# TWO OBJECTS #
# AB+ A+ (Backwards-Blocking Event)
rw_model = function(alpha, beta){
  Va_vec = rep(0,40)
  Vb_vec = rep(0,40)
  Va = .5 # generally, VA is set to 0
  Vb = .5 # generally, VB is set to 0
  for(i in 1:(length(Va_vec)-20)){
    adjustment = alpha*beta*(1-(Va+Vb))
    Va = Va + adjustment
    Vb = Vb + adjustment
    Va_vec[i] = Va
    Vb_vec[i] = Vb
  }
  for(i in (length(Va_vec)-19):(length(Va_vec))){
    adjustment = alpha*beta*(1-(Va+Vb))
    Va = Va + alpha*beta*(1-(Va))
    Vb = Vb + 0*beta*(1-(Vb))
    Va_vec[i] = Va
    Vb_vec[i] = Vb
  }
  return(data.frame(Va = Va_vec, Vb = Vb_vec))
}
rw_model(.5,.5)


# THREE OBJECTS #
rw_model = function(alpha, beta){
  Va_vec = rep(0,40)
  Vb_vec = rep(0,40)
  Vc_vec = rep(0,40)
  Va = .5 # generally, VA is set to 0
  Vb = .5 # generally, VB is set to 0
  Vc = .5
  for(i in 1:(length(Va_vec)-20)){
    adjustment = alpha*beta*(1-(Va+Vb+Vc))
    Va = Va + adjustment
    Vb = Vb + adjustment
    Vc = Vc + adjustment
    Va_vec[i] = Va
    Vb_vec[i] = Vb
    Vc_vec[i] = Vc
  }
  for(i in (length(Va_vec)-19):(length(Va_vec))){
    adjustment = alpha*beta*(1-(Va+Vb))
    Va = Va + alpha*beta*(1-(Va))
    Vb = Vb + 0*beta*(1-(Vb))
    Vc = Vc + 0*beta*(1-(Vc))
    Va_vec[i] = Va
    Vb_vec[i] = Vb
    Vc_vec[i] = Vc
  }
  return(data.frame(Va = Va_vec, Vb = Vb_vec, Vc = Vc_vec))
}
rw_model(.5,.5)



# FOUR OBJECTS #
rw_model = function(alpha, beta){
  Va_vec = rep(0,40)
  Vb_vec = rep(0,40)
  Vc_vec = rep(0,40)
  Vd_vec = rep(0,40)
  Va = .5 # generally, VA is set to 0
  Vb = .5 # generally, VB is set to 0
  Vc = .5
  Vd = .5
  for(i in 1:(length(Va_vec)-20)){
    adjustment = alpha*beta*(1-(Va+Vb+Vc+Vd))
    Va = Va + adjustment
    Vb = Vb + adjustment
    Vc = Vc + adjustment
    Vd = Vd + adjustment
    Va_vec[i] = Va
    Vb_vec[i] = Vb
    Vc_vec[i] = Vc
    Vd_vec[i] = Vd
  }
  for(i in (length(Va_vec)-19):(length(Va_vec))){
    adjustment = alpha*beta*(1-(Va+Vb))
    Va = Va + alpha*beta*(1-(Va))
    Vb = Vb + 0*beta*(1-(Vb))
    Vc = Vc + 0*beta*(1-(Vc))
    Vd = Vd + 0*beta*(1-(Vd))
    Va_vec[i] = Va
    Vb_vec[i] = Vb
    Vc_vec[i] = Vc
    Vd_vec[i] = Vd
  }
  return(data.frame(Va = Va_vec, Vb = Vb_vec, Vc = Vc_vec, vd = Vd_vec))
}
rw_model(.5,.5)




###################################################
######### MODIFIED RESCORLA WAGNER MODELS #########
###################################################
# TWO OBJECTS #
rw_model = function(alpha, beta){
  Va_vec = rep(0,40)
  Vb_vec = rep(0,40)
  Va = .5 # generally, VA is set to 0
  Vb = .5 # generally, VB is set to 0
  for(i in 1:(length(Va_vec)-20)){
    adjustment = alpha*beta*(1-(Va+Vb))
    Va = Va + adjustment
    Vb = Vb + adjustment
    Va_vec[i] = Va
    Vb_vec[i] = Vb
  }
  for(i in (length(Va_vec)-19):(length(Va_vec))){
    adjustment = alpha*beta*(1-(Va+Vb))
    Va = Va + alpha*beta*(1-(Va))
    Vb = Vb + -1*beta*(1-(Vb))
    Va_vec[i] = Va
    Vb_vec[i] = Vb
  }
  return(data.frame(Va = Va_vec, Vb = Vb_vec))
}

rw_model(.5,.5)


# THREE OBJECTS #
rw_model = function(alpha, beta){
  Va_vec = rep(0,40)
  Vb_vec = rep(0,40)
  Vc_vec = rep(0,40)
  Va = .5 # generally, VA is set to 0
  Vb = .5 # generally, VB is set to 0
  Vc = .5 # generally, VB is set to 0
  for(i in 1:(length(Va_vec)-20)){
    adjustment = alpha*beta*(1-(Va+Vb+Vc))
    Va = Va + adjustment
    Vb = Vb + adjustment
    Vc = Vc + adjustment
    Va_vec[i] = Va
    Vb_vec[i] = Vb
    Vc_vec[i] = Vc
  }
  for(i in (length(Va_vec)-19):(length(Va_vec))){
    adjustment = alpha*beta*(1-(Va+Vb+Vc))
    Va = Va + alpha*beta*(1-(Va))
    Vb = Vb + -1*beta*(1-(Vb))
    Vc = Vc + -1*beta*(1-(Vc))
    Va_vec[i] = Va
    Vb_vec[i] = Vb
    Vc_vec[i] = Vc
  }
  return(data.frame(Va = Va_vec, Vb = Vb_vec, Vc = Vc_vec))
}

rw_model(.5,.5)


# FOUR OBJECTS #
rw_model = function(alpha, beta){
  Va_vec = rep(0,40)
  Vb_vec = rep(0,40)
  Vc_vec = rep(0,40)
  Vd_vec = rep(0,40)
  Va = .5 # generally, VA is set to 0
  Vb = .5 # generally, VB is set to 0
  Vc = .5 # generally, VB is set to 0
  Vd = .5 # generally, VB is set to 0
  for(i in 1:(length(Va_vec)-20)){
    adjustment = alpha*beta*(1-(Va+Vb+Vc))
    Va = Va + adjustment
    Vb = Vb + adjustment
    Vc = Vc + adjustment
    Vd = Vd + adjustment
    Va_vec[i] = Va
    Vb_vec[i] = Vb
    Vc_vec[i] = Vc
    Vd_vec[i] = Vd
  }
  for(i in (length(Va_vec)-19):(length(Va_vec))){
    adjustment = alpha*beta*(1-(Va+Vb+Vc))
    Va = Va + alpha*beta*(1-(Va))
    Vb = Vb + -1*beta*(1-(Vb))
    Vc = Vc + -1*beta*(1-(Vc))
    Vd = Vd + -1*beta*(1-(Vd))
    Va_vec[i] = Va
    Vb_vec[i] = Vb
    Vc_vec[i] = Vc
    Vd_vec[i] = Vd
  }
  return(data.frame(Va = Va_vec, Vb = Vb_vec, Vc = Vc_vec, Vd = Vd_vec))
}

rw_model(.5,.5)
