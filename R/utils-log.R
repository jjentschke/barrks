


.msg <- function(type = 0, .quiet = FALSE, ...) {

  if(.quiet) return()

  if(type == 0) message(...)
  else {

    code <- switch(type,
                   '1;39',
                   '1;39',
                   '39',
                   '3;39',
                   '39')

    prefix <- switch(type,
                     '\n=== ',
                     '\n-- ',
                     '',
                     '> ',
                     '[ storage ] ')

    postfix <- switch(type,
                     ' ===',
                     ' --',
                     '',
                     '',
                     '')

    message(prefix, paste0('\033[', code, 'm'), ..., '\033[0m', postfix)
  }
}




.get_pb <- function(.quiet = FALSE, name = '') {
  if(.quiet) return(FALSE)
  return(list(name = name,
              format = "{cli::pb_name} {cli::pb_bar} {cli::pb_percent} | {cli::pb_eta_str}"))
}
