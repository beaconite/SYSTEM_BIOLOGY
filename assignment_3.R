library(WGCNA)

# Load the expression data without setting row names
expressionData <- read.csv("HW03_expression.csv", check.names = FALSE)

# Remove duplicates based on the first column (gene names)
expressionData <- expressionData[!duplicated(expressionData[, 1]), ]

# Set the first column as row names and remove it from the data frame
rownames(expressionData) <- expressionData[, 1]
expressionData <- expressionData[, -1]

# Continue with the WGCNA analysis
powers <- c(1:10)
sft <- pickSoftThreshold(expressionData, powerVector = powers, verbose = 5)
softPower <- sft$powerEstimate

# Construct the network adjacency
adjacency <- adjacency(expressionData, power = softPower)

# Transform the adjacency into a topological overlap matrix (TOM)
TOM <- TOMsimilarity(adjacency)

# Perform hierarchical clustering using the TOM-based dissimilarity
geneTree <- hclust(as.dist(1 - TOM), method = "average")

# Cut the dendrogram using dynamic tree cut to identify modules
dynamicColors <- cutreeDynamic(dendro = geneTree, distM = TOM, deepSplit = 2, pamRespectsDendro = FALSE)

# Ensure dynamicColors has the same length as the number of genes
if (length(dynamicColors) < nrow(expressionData)) {
  dynamicColors <- c(dynamicColors, rep("grey", nrow(expressionData) - length(dynamicColors)))
}

# Create a data frame with gene names and their corresponding module labels
moduleAssignments <- data.frame(Gene = rownames(expressionData), Module = labels2colors(dynamicColors))

# Save the module assignments to a CSV file in the root directory
write.csv(moduleAssignments, "module_assignments.csv", row.names = FALSE)

# Print a message to indicate that the file has been saved
cat("Module assignments have been saved to 'module_assignments.csv'.\n")
