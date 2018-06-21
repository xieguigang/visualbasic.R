enumerator <- function(src) {
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
			
			test  <- which(assert(src));
			names <- names(src)[test];
			list  <- src[names];  
					
			list;
			
		} else if (type == types$vector) {
			
			
			
		} else {
			stop("Incompatible type!");
		}
	}
	
	#endregion
	
	list(src    = src, 
		 select = function(project) enumerator(.select(project)), 
		 where  = function(assert) enumerator(.where(assert))
	);
}