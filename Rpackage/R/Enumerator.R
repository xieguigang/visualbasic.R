#Region "Microsoft.ROpen::f8fa48fb9540f25761f96b97a3477437, Enumerator.R"

# Summaries:

# Enumerator <- function(src) {...
# .select <- function(project) {...
# .where <- function(assert) {...
# .orderBy <- function(key, key.numeric = function(x) as.numeric(x)) {...
# .orderByDescending <- function(key, key.numeric = function(x) as.numeric(x)) {...
# .count <- function(predicate) {...
# Select= function(project) .select(project) %=>% Enumerator,Where = function(assert) .where(assert)%=>% Enumerator,OrderBy = function(key, key.numeric = as.numeric) {...
# OrderByDescending = function(key, key.numeric = as.numeric) {...
# ToArray = function() src,Take= function(n) Linq$Take(src, n)%=>% Enumerator,Skip= function(n) Linq$Skip(src, n)%=>% Enumerator,GroupBy = function(key, type) Linq$GroupBy(src, key, type) %=>% Enumerator,# Logical predicatesAny = function(predicate = NULL) {...
# All= function(predicate) {...

#End Region

#' Enumerator object constructor
#'
#' @description Enumerator object constructor function for create a
#'     VB.NET Linq like data sequence wrapper. This constructor
#'     function will create a list which contains a sequence source 
#'     and sevral linq data helper extension functions
#'
#' @param src A generic type data sequence, which can be a 
#'     \code{dataframe}, \code{list}, or \code{vector}.
#'
#' @return A generic enumerator list object, which it contains:
#'         \enumerate{
#'         \item \code{src} The generic data sequence input
#'         \item \code{where} Extension function for select values 
#'                by test on condition for each element if they are 
#'                true
#'         \item \code{select} Project the input data sequence 
#'                source to another form.
#'         \item \code{toarray} Get the input source data sequence.
#'         \item \code{orderBy}
#'         \item \code{orderByDescending}
#'         }
Enumerator <- function(src) {

  Imports("Microsoft.VisualBasic.Data");
  Imports("Microsoft.VisualBasic.Language");

  type  <- GetType(src);
  types <- primitiveTypes();
  Linq  <- Microsoft.VisualBasic.Data.Linq()$methods;

  #region "linq functions"

  .select <- function(project) {
    if (type == types$data.frame) {
      stop("Incompatible type!");
    } else if (type == types$list) {
      if (is.function(project)) {
        lapply(src, project);
      } else {
        list.project(src, project);
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
      test  <- sapply(names, function(name) assert(src[[name]])) %=>% which %=>% as.integer;
      list  <- src[names[test]];

      list;

    } else if (type == types$vector) {

      test <- sapply(src, function(x) assert(x)) %=>% as.vector %=>% which %=>% as.integer;
      list <- src[test];

    } else {
      stop("Incompatible type!");
    }
  }

  #' Reorder the elements in the source sequence by a given \code{key}
  #'
  #' @param key Can be a string name which can select the property of
  #'            the list object or a lambda function for evaluate the
  #'            element object to numeric value.
  .orderBy <- function(key, key.numeric = function(x) as.numeric(x)) {
    sort.list(src, key, key.numeric, FALSE);
  }

  .orderByDescending <- function(key, key.numeric = function(x) as.numeric(x)) {
    sort.list(src, key, key.numeric, TRUE);
  }

  .count <- function(predicate) {
    if (predicate) {
      test <- sapply(src, predicate);
      sum(test);
    } else {
      length(src);
    }
  }

  #endregion

  list(src   = src,
       # source enumerator
       Select  = function(project) .select(project)               %=>% Enumerator,
       Where   = function(assert) .where(assert)                  %=>% Enumerator,
       OrderBy = function(key, key.numeric = as.numeric) {
         .orderBy(key, key.numeric)                              %=>% Enumerator
       },
       OrderByDescending = function(key, key.numeric = as.numeric) {
         .orderByDescending(key, key.numeric)                    %=>% Enumerator
       },
       ToArray = function() src,
       Take    = function(n) Linq$Take(src, n)                    %=>% Enumerator,
       Skip    = function(n) Linq$Skip(src, n)                    %=>% Enumerator,
       GroupBy = function(key, type) Linq$GroupBy(src, key, type) %=>% Enumerator,
       # Logical predicates
       Any     = function(predicate = NULL) {
         if (predicate) {
           Linq$Any(src, predicate);
         } else {
           !IsNothing(src);
         }
       },
       All     = function(predicate) {
         .count() == .count(predicate);
       }
  );
}
