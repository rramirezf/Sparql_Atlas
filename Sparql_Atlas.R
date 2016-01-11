##
# Create a query
##
create_query<- function(ID){
  q_I<- "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  PREFIX atlas: <http://rdf.ebi.ac.uk/resource/atlas/>
  PREFIX atlasterms: <http://rdf.ebi.ac.uk/terms/atlas/>
  SELECT DISTINCT ?genename ?dbXref ?factorLabel ?tStat WHERE {
  atlas:"
  q_I <- paste(q_I,ID,sep="")
  q_II <- "atlasterms:hasAnalysis ?analysis . 
  ?analysis atlasterms:hasExpressionValue ?value . 
  ?value atlasterms:pValue ?pvalue . 
  ?value atlasterms:tStatistic ?tStat . 
  ?value atlasterms:hasFactorValue ?factor .
  ?factor atlasterms:propertyValue ?factorLabel .
  ?value atlasterms:isMeasurementOf ?probe . 
  ?probe atlasterms:dbXref ?dbXref .
  ?dbXref rdfs:label ?genename .
  } ORDER BY ?genename limit 10000"
  q <- paste(q_I,q_II,sep=" ")
  
  d <- SPARQL(url="http://www.ebi.ac.uk/rdf/services/atlas/sparql",
              query=q)
  
  # attach results to a data frame
  df <- data.frame(Genename=d$results$genename, Factor=factor(d$results$factorLabel), TStat=d$results$tStat, stringsAsFactors=FALSE)
  attach(df)
  genes <- unique(Genename)
  
  # create the matrix for the results and set the row and col names
  values <- matrix(0, nrow=length(genes), ncol=length(unique(Factor)))
  rownames(values) <- genes
  colnames(values) <- unique(Factor)
  
  i<-1
  while (i <= length(Genename)) {
    gn <-df$Genename[i]
    var <-df$Factor[i]
    tstat <-df$TStat[i]
    
    rowindex <- match(gn, rownames(values))
    colindex <- match(var, unique(Factor))
    values[rowindex,colindex] <- tstat
    i<-i+1
  }
  return(values)
}

analog_experiments <- read.table(file="/Users/admin/Documents/Mouse/Annotation_tables/Analog_experiments_filtered.txt",sep="\t")
colnames(analog_experiments) <- c("H_ID","H_comp","H_desc","H_Tissue","H_cell","M_ID","M_comp","M_desc","M_Tissue","M_cell","Analogs")

H_ID <- analog_experiments[1,1]
M_ID <- analog_experiments[1,"M_ID"]

H_mat <- create_query(H_ID)
M_mat <- create_query(M_ID)






