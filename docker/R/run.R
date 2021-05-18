#' docker run
#'
#' @description Run a command in a new container
#' @details The docker run command first creates a writeable container
#' layer over the specified image, and then starts it using the specified
#' command. That is, docker run is equivalent to the API
#' \code{/containers/create} then \code{/containers/(id)/start}. A stopped
#' container can be restarted with all its previous changes intact using
#' docker start. See docker ps -a to view a list of all containers.
#'
#' The docker run command can be used in combination with docker commit to
#' change the command that a container runs. There is additional detailed
#' information about docker run in the Docker run reference.
#'
#' For information on connecting a container to a network, see the
#' \code{Docker network overview}.
#'
#' @param workdir Working directory inside the container
#' @param name Assign a name to the container
#' @param volume Bind mount a volume, see \link{volumeBind}.
#' @param lambda R function that running in \code{R#} environment. 
#'
#' @seealso \link{volumeBind}
#'
run = function(container, commandline,
               workdir   = "/",
               name      = NULL,
               volume    = NULL,
               tty       = FALSE,
			   lambda    = NULL,
               framework = c("bash", ".netcore5", "mono")) {

  if (is.null(volume)) {
    volume = list();
  }
  if (! dir.exists(workdir) ) {
	dir.create(workdir, recursive = TRUE);
  }

  volume$docker.sock = list(host = "/var/run/docker.sock", virtual = "/var/run/docker.sock");
  volume$docker      = list(host = "$(which docker)", virtual = "/bin/docker");

  args = list(
    name    = list("--name"    = name),
    volume  = list("--volume"  = volumeBind(volume)),
    workdir = list("--workdir" = normalizePath(workdir))
  );
  tty = ifelse(tty, "-t", "");

  cli    = "%s %s -i --privileged=true %s %s";
  cli    = sprintf(cli, commandlineArgs("run", args), tty, container, commandline);
  print(cli);
  
  # just for debug view
  writeLines(cli, con = sprintf("%s/docker.sh", workdir));

  stdout = system(cli);
  stdout;
}
