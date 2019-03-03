#' Find clades with similar ages to observed groups
#'
#' Take a tree and a distribution of clade crown ages, and find clades that have a similar
#' age distribution to those in the inputs, plus or minus some tolerance.
#'
#' @param ages Named vector of clade ages from the input tree.
#' @param tree An ape-style phylogenetic tree.
#' @param tolerance Numeric specifying the tolerance around which the function will accept
#' a clade as matching the input.
#' @param fixed Whether or not the tolerance refers to a relative or a fixed differnece.
#' For instance, given a clade with a crown age of 10 (mya), if tolerance is 5 then
#' if fixed is TRUE, clades of 5-15 myas will be considered matches; if tolerance is
#' 0.5 and fixed is FALSE, the same clades will be considered matches. 
#' 
#' @details Not sure whether fixed or relative tolerance is more useful. The idea is that
#' for older clades, whether a match is 105 mya or 100 mya is probably not that important,
#' but for a young clade, whether a match is 0 or 5 mya old probably matters more.
#'
#' @return A list with two elements, I think.
#'
#' @export
#'
#' @importFrom ape branching.times
#' @importFrom geiger tips
#'
#' @references Miller, E.T. & M.W. Pennell, unpublished.
#'
#' @examples
#' data(taxFile)
#' data(jetzMCC)
#'
#' #create a taxonomic table for use in looking species up
#' taxFile$genus  <- unlist(lapply(strsplit(x=as.character(taxFile[,"TipLabel"]),
#'   split="_"), "[", 1))
#'
#' #count up the members of each genus
#' genusCount <- plyr::count(taxFile$genus)
#'
#' #exclude monotypic genera
#' genera <- as.character(genusCount$x[genusCount$freq > 1])
#'
#' #coerce to list, mclapply cladeAge over these genera. this takes ~ 1 minute
#' #generaAges <- parallel::mclapply(as.list(genera), function(x) cladeAge(jetzMCC,
#' #   taxFile, "TipLabel", "genus", x, crown=TRUE), mc.cores=4)
#'
#' #generaAges <- unlist(generaAges)
#' #names(generaAges) <- genera
#'
#' #this takes 10-15 seconds
#' #system.time(test <- ageGenerator(generaAges, jetzMCC, 0.05, FALSE)) 
#'
#' #merge these datasets
#' #toMerge1 <- data.frame(genus=names(test$newAges), new.age=test$newAges)
#' #toMerge2 <- data.frame(genus=names(generaAges), orig.age=generaAges)
#' #merged <- merge(toMerge1, toMerge2)
#' #plot(merged$new.age~merged$orig.age)

ageGenerator <- function(ages, tree, tolerance, fixed)
{
	#randomize the order of the age vector. if ages is 1, don't sample from it
	if(length(ages) == 1)
	{
		ages <- ages
	}
	else
	{
		ages <- sample(ages)
	}

	#find the branching time of every node in the tree
	bts <- ape::branching.times(tree)

	#randomize the order of this too
	if(length(bts)==1)
	{
		bts <- 1
	}
	else
	{
		bts <- sample(bts)
	}

	#set up a blank vector to save new ages into
	output <- c()

	#also set up a blank list to save the tip names of each excised clade into
	newGenera <- list()

	#now take each named clade in turn
	for(i in 1:length(ages))
	{
		#figure out what the min and max acceptable ages are
		if(fixed)
		{
			minAcc <- ages[i]-tolerance
			maxAcc <- ages[i]+tolerance
		}
		else
		{
			minAcc <- ages[i]-ages[i]*tolerance
			maxAcc <- ages[i]+ages[i]*tolerance
		}

		#you already randomized the order of both of these things, so just take
		#the first node you find of appropriate age (if there is one)
		possNodes <- bts[bts >= minAcc & bts <= maxAcc]

		#if there are no such nodes, set output to NA, and skip to next clade
		if(length(possNodes) < 1)
		{
			output[i] <- NA
			newGenera[[i]] <- NA
			next()
		}

		#otherwise just set output to the first possible node
		else
		{
			output[i] <- possNodes[1]
			
			#use the name (it refers to a node) of possNodes[1] to identify all nodes that
			#descend from it. also, figure out what the ancestral nodes are from that node.
			#exclude these nodes from bts. if you don't do this, then you will have genera
			#nested in polyphyletic genera. also subtract all nodes (tips and internal
			#nodes) that descend from the chosen node, so that the same tips aren't chosen
			#in subsequent iterations, and also subtract the node in question. 
			
			#first find all descendants of node
			newGenera[[i]] <- geiger::tips(tree, as.numeric(names(possNodes)[1]))

			ancestors <- geiger:::.get.ancestors.of.node(
				node=as.numeric(names(possNodes)[1]), phy=tree)
			descendants <- geiger:::.get.descendants.of.node(
				node=as.numeric(names(possNodes)[1]), phy=tree, tips=FALSE)

			#note the call to convert to character here
			bts <- bts[!names(bts) %in% as.character(ancestors)]
			bts <- bts[!names(bts) %in% as.character(descendants)]
			bts <- bts[!names(bts) %in% as.character(names(possNodes)[1])]
		}

	}

	#set the names of output to the genera they are meant to correspond to
	names(output) <- names(ages)

	#same with the tips out
	names(newGenera) <- names(ages)

	#bind together and return
	results <- list(newAges=output, newGenera=newGenera)
	results
}
