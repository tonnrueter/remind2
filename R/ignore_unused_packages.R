# Add a dummy call to placate R CMD check to not bother about the import of the
# piamInterfaces package which is used for the tests.

ignore_unused_packages <- function() {
  if (FALSE)
    piamInterfaces::templateNames()
}
