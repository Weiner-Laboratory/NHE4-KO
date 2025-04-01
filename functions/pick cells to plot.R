pick.cells.to.plot <- function (data.set, how.many = 1) {
  source("/blue/weineid/version-controlled-software/functions/get input.R")
  # set up variables
  all.cells.unique <- unique(Idents(data.set))
  cells.to.plot <- NULL # start here for new set of cells
  get.cells <- TRUE
  # message user
  message("Select a cell number to plot, 0 for all cells or to exit")
  message("<Ctrl-2> to enter choices")
  while (get.cells) {
    if (how.many == 1) {
      new.cell <- select.list(all.cells.unique, multiple = F)
      if (new.cell == "") {
        get.cells <- F
      } else {
        cells.to.plot <- as.character(new.cell)
        get.cells <- F
      }
    }
    if (!(how.many == 1)) {
      new.cell <- select.list(all.cells.unique, multiple = T)
      if (length(new.cell) == 0) {
        get.cells <- F
      } else {
        for (counter in 1:length(new.cell)) {
          cells.to.plot <- c(cells.to.plot, as.character(new.cell[counter]))
        }
      }
    }
  }
if (is.null(cells.to.plot)) {
  cells.to.plot <- all.cells.unique
}
  message("<Ctrl-1> to continue step-by-step")
  return(cells.to.plot)
}
