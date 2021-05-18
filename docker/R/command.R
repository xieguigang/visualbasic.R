
#' docker images
#'
#' @description List images
#' @details The default docker images will show all top level images,
#' their repository and tags, and their size.
#'
#' Docker images have intermediate layers that increase reusability,
#' decrease disk usage, and speed up docker build by allowing each
#' step to be cached. These intermediate layers are not shown by
#' default.
#'
#' The \code{SIZE} is the cumulative space taken up by the image and all its
#' parent images. This is also the disk space used by the contents of
#' the Tar file created when you docker save an image.
#'
#' An image will be listed more than once if it has multiple repository
#' names or tags. This single image (identifiable by its matching \code{IMAGE
#' ID}) uses up the \code{SIZE} listed only once.
#'
#' @param all Show all images (default hides intermediate images)
#' @param digests Show digests
#' @param no_trunc Don’t truncate output
#'
images = function(all = FALSE, digests = FALSE, no_trunc = FALSE) {
  args = list(
    all      = list("--all"      = all),
    digests  = list("--digests"  = digests),
    no_trunc = list("--no-trunc" = no_trunc)
  );

  stdout = system(commandlineArgs("images", args));

}

#' docker ps
#'
#' @description List containers
#' @details list containers
#'
#' @param all Show all containers (default shows just running)
#' @param no_trunc Don’t truncate output
#' @param size Display total file sizes
#'
ps = function(all = FALSE, no_trunc = FALSE, size = FALSE) {
  args = list(
    all      = list("--all"      = all),
    no_trunc = list("--no-trunc" = no_trunc),
    size     = list("--size"     = size)
  );

  stdout = system(commandlineArgs("ps", args));
}
