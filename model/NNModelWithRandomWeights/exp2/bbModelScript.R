############
# BB Model #
############

# set working directory
setwd("C:/Users/bentod2/Documents/projects/current/bbInChildren/model/NNModelWithRandomWeights/exp2")

#############
## OBJECTS ##
#############
objects_full = data.frame(x = c('1'))
names(objects_full) = NULL
rownames(objects_full)
row.names(objects_full)




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



cat(paste("name: Bplus", "\n", sep=""))
# INPUT
cat(paste("I:", "\n", sep="\t"))
# Object B
cat(paste("(B)", sep="\t"))
print(objects_full[1,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
print(effect[2,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")



cat(paste("name: Cplus", "\n", sep=""))
# INPUT
cat(paste("I:", "\n", sep="\t"))
# Object C
cat(paste("(C)", sep="\t"))
print(objects_full[1,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
print(effect[2,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()



## AB plus ##
sink('ABplus.ex')
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



cat(paste("name: Bplus", "\n", sep=""))
# INPUT
cat(paste("I:", "\n", sep="\t"))
# Object B
cat(paste("(B)", sep="\t"))
print(objects_full[1,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
print(effect[2,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()


## AB minus ##
sink('ABminus.ex')
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



cat(paste("name: Bminus", "\n", sep=""))
# INPUT
cat(paste("I:", "\n", sep="\t"))
# Object B
cat(paste("(B)", sep="\t"))
print(objects_full[1,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
print(effect[1,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()





## DE plus ##
sink('DEplus.ex')
cat(paste("defI:-", "\n", sep=""))
cat(paste("defT:-", "\n", sep=""))
cat(paste(";", "\n", sep=""))
cat(paste("name: Dplus", "\n", sep=""))
# INPUT
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(D)", sep="\t"))
print(objects_full[1,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
print(effect[2,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")



cat(paste("name: Eplus", "\n", sep=""))
# INPUT
cat(paste("I:", "\n", sep="\t"))
# Object B
cat(paste("(E)", sep="\t"))
print(objects_full[1,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
print(effect[2,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()






# DE minus
sink('DEminus.ex')
cat(paste("defI:-", "\n", sep=""))
cat(paste("defT:-", "\n", sep=""))
cat(paste(";", "\n", sep=""))
cat(paste("name: Dminus", "\n", sep=""))
# INPUT
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(D)", sep="\t"))
print(objects_full[1,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
print(effect[1,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")



cat(paste("name: Eminus", "\n", sep=""))
# INPUT
cat(paste("I:", "\n", sep="\t"))
# Object B
cat(paste("(E)", sep="\t"))
print(objects_full[1,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
print(effect[1,], sep = "\t", quote = FALSE, row.names = FALSE)
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
print(objects_full[1,], sep = "\t", quote = FALSE, row.names = FALSE)


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
print(objects_full[1,], sep = "\t", quote = FALSE, row.names = FALSE)


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
print(objects_full[1,], sep = "\t", quote = FALSE, row.names = FALSE)


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
print(objects_full[1,], sep = "\t", quote = FALSE, row.names = FALSE)


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
print(objects_full[1,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
print(effect[2,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")



cat(paste("name: E", "\n", sep=""))
# INPUT
cat(paste("I:", "\n", sep="\t"))
# Object A
cat(paste("(E)", sep="\t"))
print(objects_full[1,], sep = "\t", quote = FALSE, row.names = FALSE)


# OUTPUT
cat(paste("T:", "\n", sep="\t"))
cat(paste("(Effect)", sep="\t"))
print(effect[2,], sep = "\t", quote = FALSE, row.names = FALSE)
cat(paste(";", sep="\t"))
cat("\n")
sink()
