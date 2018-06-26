#' Common data operation helper function
Microsoft.VisualBasic.Data <- function() {

	# data.frame rows to list collection
	.as.list <- function(d) {

		.list <- list();
		list  <- .to.list(d);
		names <- colnames(d);

		for (i in 1:nrow(d)) {
			l <- list();

			for (name in names) {
				l[[name]] <- list[[name]][i];
			}

			.list[[i]] <- l;
		}

		.list;
	}

	.to.list <- function(d) {
		list  <- list();
		names <- colnames(d);

		for (name in names) {
			list[[name]] <- as.vector(unlist(d[, name]));
		}

		list;
	}

	# 有些时候dataframe取列的结果得到的是一个list？
	# 这是一个什么bug？？？
	# 尝试使用这个函数来消除这个需要调用unlist的bug
	.as.matrix <- function(d) {

		names   <- colnames(d);
		columns <- lapply(names, function(col) {
			col <- d[, col];

			if (is.list(col)) {
				as.vector(unlist(col));
			} else {
				as.vector(col);
			}
		});

				 d  <- as.data.frame(columns);
		colnames(d) <- names;
		rownames(d) <- as.character(1:nrow(d));

		d;
	}

	selectMany <- function(list, project) {
		v <- NULL;

		for(x in list) {
			v <- append(v, project(x));
		}

		v;
	}

	list.project <- function(list, fields) {
		l <- list();

		for (name in fields) {
			x <- list[[name]];

			if (is.null(x)) {
				l[[name]] <- NA;
			} else {
				l[[name]] <- x;
			}
		}

		l;
	}

	as.dataframe <- function(list) {
		d <- NULL;
		list.names <- names(list);
		all.prop <- selectMany(list, function(x) names(x));
		all.prop <- unique(all.prop);

		for (name in list.names) {
			x <- list[[name]];
			x <- list.project(x, all.prop);
			d <- rbind(d, x);
		}

		rownames(d) <- list.names;
		colnames(d) <- all.prop;
		d;
	}

	# register function for namespace export
    list(namespace = GetCurrentFunc(),
		 description = "Namespace contains some common data operation helpers.",
		 methods = list(
			 .as.list     = .as.list,
		 	 .to.list     = .to.list,
		 	 .as.matrix   = .as.matrix,
		 	 .selectMany  = selectMany,
		 	 list.project = list.project,
		 	 as.dataframe = as.dataframe
	));
}
