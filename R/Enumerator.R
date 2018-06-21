enumerator <- function(src) {
	
	imports("microsoft.visualbasic.language");
	
	type   <- GetType(src);
	types  <- primitiveTypes();
	
	#region "linq functions"
	
	.select <- function(project) {
		if (type == types$data.frame) {
			stop("Incompatible type!");
		} else if (type == types$list) {
			if (is.function(project)) {
				lapply(src, project);
			} else {
				microsoft.visualbasic.data()$list.project(src, project);
			}
		} else if (type == types$vector) {
			lapply(src, project);
		} else {
			stop("Incompatible type!");
		}
	}
	
	.where <- function(assert) {
		if (type == types$data.frame) {
			stop("Incompatible type!");
		} else if (type == types$list) {			
			
			names <- names(src)
			test  <- sapply(names, function(name) assert(src[[name]])) 
				%=>% which 
				%=>% as.integer;
			list  <- src[names[test]];  
					
			list;
			
		} else if (type == types$vector) {
			
			test <- sapply(src, function(x) assert(x)) 
				%=>% which 
				%=>% as.integer;
			list <- src[test];  
			
		} else {
			stop("Incompatible type!");
		}
	}
	
	#endregion
	
	list(src     = src, 
		 select  = function(project) enumerator(.select(project)), 
		 where   = function(assert) enumerator(.where(assert)),
		 toarray = function() src
	);
}