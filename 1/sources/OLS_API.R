queries <- read.table("DIME_nutritional_intake-1.csv",
                      header = T, row.names = 1, sep = ",") |>
  rownames()

queries <- structure(
	strsplit(queries, "[[:punct:]]+"),
	names = queries)

queryOLS <- function(term, n = 10){
  base.url <- "https://www.ebi.ac.uk/ols/api/suggest?q="
  q <- paste0(base.url, term, "&queryFields=label,synonym&type=class&local=true&rows=", n) |> URLencode()
  q.conn <- url(q)
  
  on.exit(close(q.conn))
  
  rjson::fromJSON(readLines(q.conn))
}

syntacticQueries <- function(queries, n = 10){
	results <- structure(
		vector("list", length = length(queries)),
		names = names(queries))
	
	pb <- txtProgressBar(min = 0, max = length(queries),
											 style = 3)
	
	on.exit(close(pb))
	
	for(i in seq_along(queries)){
		q <- queries[[i]]
		qs <- vapply(seq_along(q), function(i) paste(q[1:i], collapse = " "), 
								 FUN.VALUE = character(1))
		names(qs) <- qs
		results[[i]] <- lapply(qs, queryOLS, n = n)
		setTxtProgressBar(pb, i)
	}
	
	return(results)
}

results <- syntacticQueries(queries, n = 10)

formatted <- lapply(seq_along(results), function(i){
	res <- results[[i]]
	if(is.null(res)){
		return(NULL)
	}
	qs <- names(res)
	n <- vapply(res, function(x) x$response$numFound, numeric(1))
	best.res <- lapply(res, function(x) paste(unlist(x$response$docs), collapse = " | "))
	data.frame(raw = names(results)[i], query = qs, n = n, 
						 best.results = unlist(best.res), row.names = NULL)
})
formatted <- do.call(rbind, formatted)

write.table(formatted, "ols_results.tsv",
            sep = "\t", col.names = T, 
						row.names = F, quote = F)
