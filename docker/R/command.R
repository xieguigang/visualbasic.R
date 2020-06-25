
#' docker images
#'
#' @description List images
#' @details The default docker images will show all top level images,
#' their repository and tags, and their size.
#' Docker images have intermediate layers that increase reusability,
#' decrease disk usage, and speed up docker build by allowing each
#' step to be cached. These intermediate layers are not shown by
#' default.
#' The SIZE is the cumulative space taken up by the image and all its
#' parent images. This is also the disk space used by the contents of
#' the Tar file created when you docker save an image.
#' An image will be listed more than once if it has multiple repository
#' names or tags. This single image (identifiable by its matching IMAGE
#' ID) uses up the SIZE listed only once.
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

#' docker run
#'
#' @description Run a command in a new container
#' @details The docker run command first creates a writeable container
#' layer over the specified image, and then starts it using the specified
#' command. That is, docker run is equivalent to the API
#' \code{/containers/create} then \code{/containers/(id)/start}. A stopped
#' container can be restarted with all its previous changes intact using
#' docker start. See docker ps -a to view a list of all containers.
#' The docker run command can be used in combination with docker commit to
#' change the command that a container runs. There is additional detailed
#' information about docker run in the Docker run reference.
#' For information on connecting a container to a network, see the
#' \code{Docker network overview}.
#'
#' @param workdir Working directory inside the container
#' @param name Assign a name to the container
#' @param volume Bind mount a volume
#'
run = function(container, commandline, workdir = "/", name = NULL, volume = NULL) {
  args = list(
    workdir = list("--workdir" = workdir),
    name    = list("--name"    = name),
    volume  = list("--volume"  = volumeBind(volume))
  );

  cli    = sprintf("%s %s %s", commandlineArgs("run", args), container, commandline);
  stdout = system(cli);
  stdout;
}


