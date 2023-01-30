############
# BB Model #
############



#############
## OBJECTS ##
#############
objects_full = diag(4)
objects_full = as.data.frame(objects_full)


names(objects_full) = NULL
rownames(objects_full) = NULL
row.names(objects_full) = NULL



###################
## CAUSAL EFFECT ##
###################

effect = data.frame(x = c('0', '1'))

names(effect) = NULL
rownames(effect) = NULL
row.names(effect) = NULL


# activated: 1
# non-activated: 0



##############
## TRAINING ##
##############
sink('ABCplus.ex')
cat(paste("defI:-", "\n", sep=""))
cat(paste("defT:-", "\n", sep=""))
cat(paste(";", "\n", sep=""))
cat(paste("name: ABCplus", "\n", sep=""))
# INPUT
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(A)", sep="\t"))
print(objects_full[1,], sep = "\t", quote = FALSE, row.names = FALSE)

# Object B
cat(paste("(B)", sep="\t"))
print(objects_full[2,], sep = "\t", quote = FALSE, row.names = FALSE)

# Object C
cat(paste("(C)", sep="\t"))
print(objects_full[3,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
print(effect[2,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()



# A plus

sink('Aplus.ex')
cat(paste("defI:-", "\n", sep=""))
cat(paste("defT:-", "\n", sep=""))
cat(paste(";", "\n", sep=""))
cat(paste("name: Aplus", "\n", sep=""))
# INPUT
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(A)", sep="\t"))
print(objects_full[1,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
print(effect[2,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()

# A minus

sink('Aminus.ex')
cat(paste("defI:-", "\n", sep=""))
cat(paste("defT:-", "\n", sep=""))
cat(paste(";", "\n", sep=""))
cat(paste("name: Aminus", "\n", sep=""))
# INPUT
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(A)", sep="\t"))
print(objects_full[1,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
print(effect[1,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()


# D plus

sink('Dplus.ex')
cat(paste("defI:-", "\n", sep=""))
cat(paste("defT:-", "\n", sep=""))
cat(paste(";", "\n", sep=""))
cat(paste("name: Dplus", "\n", sep=""))
# INPUT
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(D)", sep="\t"))
print(objects_full[4,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
print(effect[2,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()



# D minus

sink('Dminus.ex')
cat(paste("defI:-", "\n", sep=""))
cat(paste("defT:-", "\n", sep=""))
cat(paste(";", "\n", sep=""))
cat(paste("name: Dminus", "\n", sep=""))
# INPUT
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(D)", sep="\t"))
print(objects_full[4,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
print(effect[1,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()



# D nothing

sink('Dnothing.ex')
cat(paste("defI:-", "\n", sep=""))
cat(paste("defT:-", "\n", sep=""))
cat(paste(";", "\n", sep=""))
cat(paste("name: Dnothing", "\n", sep=""))
# INPUT
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(D)", sep="\t"))
print(objects_full[4,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
cat(paste(";", sep="\t"))
cat("\n")
sink()



##########
## TEST ##
##########
sink('experimentalTest.ex')
cat(paste("defI:-", "\n", sep=""))
cat(paste("defT:-", "\n", sep=""))
cat(paste(";", "\n", sep=""))
cat(paste("name: A", "\n", sep=""))
# INPUT
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(A)", sep="\t"))
print(objects_full[1,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
print(effect[2,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")


cat(paste("name: B", "\n", sep=""))
# INPUT
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(B)", sep="\t"))
print(objects_full[2,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
print(effect[2,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")



cat(paste("name: C", "\n", sep=""))
# INPUT
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(C)", sep="\t"))
print(objects_full[3,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
print(effect[2,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()



sink('controlTest.ex')
cat(paste("defI:-", "\n", sep=""))
cat(paste("defT:-", "\n", sep=""))
cat(paste(";", "\n", sep=""))
cat(paste("name: A", "\n", sep=""))
# INPUT
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(A)", sep="\t"))
print(objects_full[1,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
print(effect[2,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")


cat(paste("name: B", "\n", sep=""))
# INPUT
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(B)", sep="\t"))
print(objects_full[2,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
print(effect[2,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")



cat(paste("name: C", "\n", sep=""))
# INPUT
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(C)", sep="\t"))
print(objects_full[3,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
print(effect[2,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")


cat(paste("name: D", "\n", sep=""))
# INPUT
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(D)", sep="\t"))
print(objects_full[4,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
print(effect[2,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()
