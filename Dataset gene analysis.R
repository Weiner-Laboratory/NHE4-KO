# load libraries ----
  library(Seurat)
  library(ggplot2)
  library(beepr)
  library(berryFunctions)
  library(scCustomize)
  library(biomaRt)
  library(openxlsx) # used to create excel workbook of expression data
  library(officer)
  library(qs) # used for faster Seurat data save and read access
  library("stringr")
# load idw functions ----
  source("/blue/weineid/version-controlled-software/functions/Save plot.R")
  source("/blue/weineid/version-controlled-software/functions/Make experiment folder.R") # nolint
  source("/blue/weineid/version-controlled-software/functions/get input.R")
  source("/blue/weineid/version-controlled-software/functions/convert gene and ensembl.R") # nolint
  source("/blue/weineid/version-controlled-software/functions/word documentation.R") # nolint
  source("/blue/weineid/version-controlled-software/functions/pick a cell.R")
  source("/blue/weineid/version-controlled-software/functions/format ggplot.R")
  source("/blue/weineid/version-controlled-software/functions/pick cells to plot.R") # nolint
  source("/blue/weineid/version-controlled-software/functions/data set information.R")
  source("/blue/weineid/version-controlled-software/functions/output message.R")
  source("/blue/weineid/version-controlled-software/functions/message with delay.R")
  data.info <- get.data.info()
  message.delay <- 0.5
  plot.delay <- 3
# get experiment details ----
  experiment.title <- get.input("Enter experiment title")
  experiment.title.original <- experiment.title
  experiment.subtitle <- Sys.Date()
  folder.date.experiment <- make.experiment.folder(experiment.title)
  r.script.file <- rstudioapi::getSourceEditorContext()$path
# load data set ----
  choices <- rownames(data.info)
  kidney.choice <- get.input ("Which data set to use?", choices)
  ensembl.dataset <- data.info[kidney.choice, "ensembl.dataset"]
# create data file name and read the data and subset for normal if human data
  data.file.name <- paste0("/blue/weineid/datasets/", data.info[kidney.choice, "filename"])
  if (if.error(nchar(data.file.name.old),
              TRUE,
              !(data.file.name == data.file.name.old))) {
    message("Reading data")
    file.extension <- substr(data.file.name,
                            nchar(data.file.name) - 2,
                            nchar(data.file.name))
    if (file.extension == ".qs") {
      data.set <- qread(data.file.name) #uses qread for qs formatted files
    }
    if (file.extension == "rds") {
      data.set <- readRDS(data.file.name)
    }
    data.set.original <- data.set
  } else {
    message("Using same data file...")
    data.set <- data.set.original
  }
  data.file.name.old <- data.file.name
  Sys.sleep(message.delay)
  disease.subset <- data.info[kidney.choice, "disease.subset"]
  if (disease.subset == "yes") {
    choices <- unique(data.set$disease)
    kidney.subset <- get.input("Which subset to use?", choices)
    message("Subsetting for disease = ", kidney.subset)
    data.set <- subset(data.set, subset = (disease == kidney.subset))
  }
# set 'idents'
  cell.type.field <- data.info[kidney.choice, "cell.ID"]
  cell.type.idents <- data.set[[cell.type.field]]
  colnames(cell.type.idents) <- "celltype"
  Idents(data.set) <- cell.type.idents$celltype
# load BiomaRT data ----
  gene.type <- data.info[kidney.choice, "gene.type"]
  get.ensembl <- F
  if (gene.type == "ensembl") {
    if (!is.error(ensembl)) {
      if (!ensembl@dataset == ensembl.dataset) {
        get.ensembl <- T
      }
    } else {
      get.ensembl <- T
    }
    if (get.ensembl) {
      ensembl <- useEnsembl(biomart = "genes", dataset = ensembl.dataset)
    }
  }
# start Word documentation ----
  documentation.Word <- NULL # nolint
  documentation.Word <- read_docx() # nolint
  documentation.Word <- body_add_par(documentation.Word,
                                    # nolint
                                    paste("Experiment:", experiment.title),
                                    style = "heading 1")
  documentation.Word <- body_add_par(documentation.Word,
                                    # nolint
                                    paste("Analysis date:", experiment.subtitle),
                                    style = "heading 2")
# get genes to plot ----
  # all.genes.oldway <- data.set@assays[["RNA"]]@data@Dimnames[[1]]
  all.genes <- rownames(data.set)
  genes.to.plot <- NULL # start here to reset all genes ----
  gene.names <- NULL
  gene.next <- "" # start here to add genes
# get genes and convert to ensembl ID
  while (!(gene.next == "exit")) {
    gene.next <- get.input("Enter gene to search; enter 'exit' to end input", "", F) # nolint
    if (gene.next == "exit") {
      message ("Done entering genes to be plotted")
    } else {
      if (data.info[kidney.choice, "gene.case"] == "upper") {
        gene.next <- toupper(gene.next)
      }
      if (data.info[kidney.choice, "gene.case"] == "title") {
        gene.next <- str_to_title(gene.next)
      }
      if (data.info[kidney.choice, "gene.type"] == "gene") {
        if (is.element(gene.next, all.genes)) {
          genes.to.plot <- c(genes.to.plot, gene.next)
          gene.names <- c(gene.names, gene.next)
        } else {
          documentation <- paste(gene.next, "is not in data set")
          message(documentation)
          Sys.sleep(message.delay)
          documentation.Word <- body_add_par(documentation.Word, documentation) # nolint
        }
      } else {
        ensembl.ID <- getBM(
          # nolint
          filters = "external_gene_name",
          values = gene.next,
          attributes = "ensembl_gene_id",
          mart = ensembl
        )
        if (nrow(ensembl.ID) == 0) {
          message("Cannot match this gene in ensembl")
          documentation <- paste0("Gene '", gene.next, "' could not be matched to Ensembl") # nolint
          documentation.Word <- body_add_par(documentation.Word, documentation) # nolint
        } else {
          ensembl.ID.length <- nchar(ensembl.ID) # nolint
          for (counter in (1:nrow(ensembl.ID))) {
            ensembl.ID.internal <- ensembl.ID[counter, 1]
            message(paste(
              "Ensembl ID for",
              gene.next,
              "is",
              ensembl.ID.internal
            ))
            if (is.element(ensembl.ID.internal, all.genes)) {
              genes.to.plot <- c(genes.to.plot, ensembl.ID.internal)
              gene.names <- c(gene.names, gene.next)
              documentation <- NULL
              documentation <- paste0("Gene '",
                                      gene.next,
                                      ".' Ensembl is ",
                                      ensembl.ID.internal,
                                      ".")
              documentation.Word <- body_add_par(documentation.Word, documentation, style = "Normal")
            } else {
              message(paste(ensembl.ID.internal, "not in data set"))
              documentation <- paste0(
                "Gene '",
                gene.next,
                "', Ensembl ID, ",
                ensembl.ID.internal,
                ", is not in this data set"
              )
              documentation.Word <- body_add_par(documentation.Word, documentation, style = "Normal")
            }
          }
        }
      }
    }
  }
# convert list of genes to a single list variable
  gene.list <- paste(gene.names, collapse = ", ")
# if all genes entered were NOT in dataset, document and stop
  if (nchar(gene.list) == 0) {
    documentation <- "No genes entered were in dataset"
    message(documentation)
    Sys.sleep(message.delay)
    documentation.Word <- body_add_par(documentation.Word, documentation, style = "Normal")
    # document the variables used
    variables.used <- c("data.file.name", "kidney.choice", "r.script.file")
    if (data.info[kidney.choice, "disease.subset"] == "yes") {
      variables.used <- c(variables.used, "kidney.subset")
    }
    document.variables (documentation.Word, variables.used)
    print(documentation.Word,
          target = paste0(folder.date.experiment, experiment.title, ".docx"))
    data.file.name.old <- data.file.name
    stop("Analysis completed")
  }
# subset with which gene ----
  gene.select <- "all"
  choice.made <- TRUE # change to false to implement subsetting by gene
  while (!(choice.made)) {
    # while (!(is.element(gene.select.ensembl, all.genes))) {
    gene.select <- get.input("Gene to use to select cells ('all' to not subset)")
    if (gene.select == "all") {
      choice.made <- T
    } else {
      gene.select.ensembl <- convert.to.ensembl(gene.select)
      if (gene.select.ensembl == "") {
        choice.made <- F
      } else {
        gene.cutoff <- 0
        gene.cutoff <- get.input("Gene expression cutoff")
        choice.made = T
      }
    }
  }
# enter gene.select as subset criteria below
  if (gene.select == "all") {
    message("No subsetting performed")
  } else {
    data.set <- subset(data.set.original, subset = (ENSMUSG00000070645 > gene.cutoff))
    Idents(data.set) <- data.set$cell_type
    experiment.title <- paste(experiment.title.original, gene.select, gene.cutoff)
    documentation <- paste(
      "Select for gene",
      gene.select,
      gene.select.ensembl,
      "-",
      ncol(data.set),
      "cells selected from",
      ncol(data.set.original),
      "in original data set."
    )
    documentation.Word <- body_add_par(documentation.Word, documentation, style = "Normal")
    message(documentation)
    # reset 'idents'
    if (data.info[kidney.choice, "cell.ID"] == "subclass.l2") {
      Idents(data.set) <- data.set$subclass.l2
    }
    if (data.info[kidney.choice, "cell.ID"] == "cell_type") {
      Idents(data.set) <- data.set$cell_type
    }
  }
# identify cells to plot ----
  cells.to.plot <- pick.cells.to.plot(data.set)
# create violin plot ----
  if (length(genes.to.plot) == 1) {
    violin.1.plot <- VlnPlot(
      data.set,
      features = genes.to.plot,
      idents = cells.to.plot,
      flip = T,
  #    raster = F,
      alpha = 0.,
      sort = T
    )
  } else {
    violin.1.plot <- VlnPlot(
      data.set,
      features = genes.to.plot,
      idents = cells.to.plot,
      log = F,
      stack = T,
      flip = T,
      sort = F
    )
  }
  violin.1.plot <- violin.1.plot +
    scale_fill_discrete(breaks = genes.to.plot, labels = gene.names) +
    theme(axis.text.x =
            element_text(size = 8, angle = 60),
          plot.caption = element_text(size = 8)) +
    xlab("Cell type") +
    ylab("Expression")
  violin.1.plot <- format.ggplot(violin.1.plot)
  violin.1.plot
  Sys.sleep(plot.delay)
  plot.filename <- plot.file ("violin", experiment.title)
  save.plot(violin.1.plot, plot.filename)
  documentation.Word <- plot.to.word(documentation.Word, violin.1.plot)
# create dot plot ----
  log1p <- get.input("Log1P transform dot plots?",c("Y","N")) # let user determine whether to (ln(x+1) transform expression data
  dot.plot <- 
    DotPlot_scCustom(
      data.set, 
      features = genes.to.plot, 
      idents = cells.to.plot,
      scale = FALSE,
      scale.max = 100,
      cluster.idents = TRUE
      )
  if(log1p == "N"){dot.plot$data$avg.exp.scaled <- dot.plot$data$avg.exp} # undoes log1p transformation of expression
  dot.plot <- dot.plot +
    RotatedAxis() +
    scale_fill_discrete(breaks = genes.to.plot, labels = gene.names) +
    theme(
      plot.caption = element_text(size = 8),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8)
    ) +
    xlab("Gene") +
    ylab("Cell type")
  dot.plot <- format.ggplot(dot.plot, , , gene.list)
  dot.plot
  Sys.sleep(plot.delay)
  plot.filename <- plot.file ("dot plot", experiment.title)
  save.plot(dot.plot, plot.filename)
  documentation.Word <- plot.to.word(documentation.Word, dot.plot)
# determine mean expression ----
  expression.pattern <- dot.plot[["data"]]
  expression.pattern$gene <- expression.pattern$features.plot
  expression.pattern$features.plot <- NULL
  expression.pattern$cell <- expression.pattern$id
  expression.pattern$id <- NULL
  expression.pattern$pct.exp <- expression.pattern$pct.exp / 100
  column.order <-
    c("gene", "cell", "avg.exp", "avg.exp.scaled", "pct.exp")
  expression.pattern <- expression.pattern[, column.order]
# create gene name to match ensembl ID ----
# data frame of ensembl id's and gene names
  gene.set <- as.data.frame(genes.to.plot)
  gene.set$names <- gene.names
  gene.set$ensembl <- gene.set$genes.to.plot
  gene.set$genes.to.plot <- NULL
# get number of each cell type
  number.of.cells <- table(Idents(data.set))
  number.of.cells <- as.data.frame(number.of.cells)
  colnames(number.of.cells) <- c("Cell", "number")
  sheet.gene.name <- "cells"
# remove old and create a new workbook for results ----
  expression.workbook <- NULL
  expression.workbook <- createWorkbook()
  addWorksheet(expression.workbook, sheetName = sheet.gene.name)
  writeData(expression.workbook, sheet = sheet.gene.name, number.of.cells)
  genes <- unique(expression.pattern$gene)
# determine cutoff for % of cells in a cell type that express a specific gene ----
  expression.cutoff <- get.input("Enter % of cells with expression to be considered significant expression")
# test to see if value is a '%" and change to number if so
  if (substr(expression.cutoff,
            nchar(expression.cutoff),
            nchar(expression.cutoff)) == "%") {
    expression.cutoff <- substr(expression.cutoff, 1, nchar(expression.cutoff) - 1)
    expression.cutoff <- as.numeric(expression.cutoff) / 100
  }
# if value is greater than 1, assume that it was meant to be a percent
  expression.cutoff <- as.numeric(expression.cutoff)
  if (expression.cutoff > 1) {
    expression.cutoff <- expression.cutoff / 100
  }
# prepare to find cell types with significant gene expression ----
  all.cells.expressing.any.gene <- NULL
  for (individual.gene in levels(genes)) {
    gene.data <-
      subset(expression.pattern, subset = (genes == individual.gene))
    row.number.gene <-
      which(gene.set$ensembl == individual.gene)
    sheet.gene.name <- gene.set$names[row.number.gene]
    gene.data$gene.name <- sheet.gene.name
    column.order <-
      c(# "gene.name",
        "gene", "cell", "avg.exp", "avg.exp.scaled", "pct.exp")
    gene.data <- gene.data[, column.order]
    addWorksheet(expression.workbook, sheetName = sheet.gene.name)
    writeData(expression.workbook, sheet = sheet.gene.name, gene.data)
    rownames(gene.data) <- gene.data$cell
  # determine which cell type expresses gene in selected % of cells ----
    documentation.text <- paste("Which cells have",
                                sheet.gene.name,
                                "expression >",
                                expression.cutoff)
    message.with.delay(documentation.text)
    documentation.Word <- body_add_par(documentation.Word, documentation.text, style = "heading 2") # nolint
    for (cell.pointer in gene.data$cell) {
      if (gene.data[cell.pointer, "pct.exp"] > expression.cutoff) {
        all.cells.expressing.any.gene <- c(all.cells.expressing.any.gene, cell.pointer)
        pct.expression.text <- paste(cell.pointer, format(gene.data[cell.pointer, "pct.exp"], digits = 3), sep = ", ")
        pct.expression.text <- paste0(cell.pointer,
                                      ", ",
                                      round(100 * gene.data[cell.pointer, "pct.exp"], 0),
                                      "%"
        )
        message.with.delay(paste(" ", pct.expression.text), 1)
        documentation.Word <- body_add_par(documentation.Word, pct.expression.text)
      }
    }
    if (length(all.cells.expressing.any.gene) == 0) {
      documentation <- "  No cell types met % cellular expression criteria"
      message.with.delay(paste(documentation), 1)
      documentation.Word <- body_add_par(documentation.Word, documentation)
    }
  }
  Sys.sleep(message.delay)
  all.cells.expressing.any.gene <- unique(all.cells.expressing.any.gene)

# generate violin plot just of these cells
  if (! is.error(violin.2.plot)) {rm(violin.2.plot)}
  if (length(all.cells.expressing.any.gene) > 0) {
    if (length(genes.to.plot) == 1) {
      violin.2.plot <- VlnPlot(
        data.set,
        features = genes.to.plot,
        idents = all.cells.expressing.any.gene,
        flip = T,
        alpha = 0,
        # raster = F,
        sort = T
      )
    } else {
      violin.2.plot <- VlnPlot(
        data.set,
        features = genes.to.plot,
        idents = all.cells.expressing.any.gene,
        log = F,
        stack = T,
        flip = T,
        sort = F
      )
    }
    violin.2.plot <- violin.2.plot +
      scale_fill_discrete(breaks = genes.to.plot, labels = gene.names) +
      theme(axis.text.x =
              element_text(size = 8, angle = 60),
            plot.caption = element_text(size = 8)) +
      xlab("Cell type") +
      ylab("Expression")
    violin.2.plot <- format.ggplot(
      violin.2.plot,
      paste0(experiment.title, ": cell types with gene in more than ", round(100 * expression.cutoff,0), "% of cells")
    )
    violin.2.plot
    Sys.sleep(plot.delay)
    plot.filename <- plot.file ("violin - percent subset", experiment.title)
    save.plot(violin.2.plot, plot.filename)
    documentation.Word <- plot.to.word(documentation.Word, violin.2.plot)
  } else {
    documentation <- "No cell types met % expression criteria"
    message.with.delay (documentation)
  }
# display subsetted violin plots
  try(violin.2.plot)
  Sys.sleep(plot.delay)
# plots of avg expression and pct expression ----
  avg.exp.plot <-
    ggplot(expression.pattern, aes(x = gene, y = avg.exp, fill = cell)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    theme_ggprism_mod() +
    theme(
      plot.caption = element_text(size = 8),
      axis.text.x = element_text(
        angle = 60,
        vjust = 1,
        hjust = 1,
        size = 8
      ),
      axis.text.y = element_text(size = 8),
      legend.text = element_text(size = 8),
      legend.position = "top",
      legend.title = element_blank(),
      panel.grid.major.y = element_line(
        color = "grey",
        linewidth = 0.5,
        linetype = 2
      ),
      panel.background = element_rect(fill = "grey85")
    )
  avg.exp.plot <- format.ggplot(avg.exp.plot, , , gene.list)
  avg.exp.plot
  plot.filename <- paste0(experiment.title, " avg exp.pdf")
  save.plot(avg.exp.plot)
  documentation.Word <- plot.to.word(documentation.Word, avg.exp.plot)
# pct expression plot ----
  pct.exp.plot <-
    ggplot(expression.pattern, aes(x = gene, y = pct.exp, fill = cell)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    theme(
      plot.caption = element_text(size = 8),
      axis.text.x = element_text(
        angle = 60,
        size = 8,
        vjust = 1,
        hjust = 1
      ),
      axis.text.y = element_text(size = 8),
      legend.text = element_text(size = 8),
      legend.position = "top",
      legend.title = element_blank(),
      panel.grid.major.y = element_line(
        color = "grey",
        linewidth = 0.25,
        linetype = 2
      )
    ) +
    xlab("Gene") +
    ylab("% of cells with expression") +
    scale_y_continuous(labels = scales::percent, limits = 0:1)
  pct.exp.plot <- format.ggplot(pct.exp.plot, , , gene.list)
  pct.exp.plot
  plot.filename <- paste0(experiment.title, " pct.exp.pdf")
  save.plot(pct.exp.plot, plot.filename)
  documentation.Word <- plot.to.word(documentation.Word, pct.exp.plot)
  # create dot plot with minimum cells expressing ----
  cells.to.plot.min.exp <- subset(expression.pattern, subset = (pct.exp > expression.cutoff))
  cells.to.plot.min.exp <- cells.to.plot.min.exp$cell
  cells.to.plot.min.exp <- unique(cells.to.plot.min.exp)
  if (length(cells.to.plot.min.exp) > 0) {
  dot.plot.min.exp <- 
    DotPlot_scCustom(
      data.set, 
      features = genes.to.plot, 
      idents = cells.to.plot.min.exp,
      scale = FALSE,
      scale.max = 100,
      cluster.idents = TRUE
    )
  if(log1p == "N") {dot.plot.min.exp$data$avg.exp.scaled <- dot.plot.min.exp$data$avg.exp} # undoes log1p transformation of expression
  dot.plot.min.exp <- dot.plot.min.exp +
    RotatedAxis() +
    scale_fill_discrete(breaks = genes.to.plot, labels = gene.names) +
    theme(
      plot.caption = element_text(size = 8),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8)
    ) +
    xlab("Gene") +
    ylab("Cell type")
  dot.plot.min.exp <- format.ggplot(dot.plot.min.exp, , , gene.list)
  dot.plot.min.exp
  plot.filename <- plot.file ("dot plot min exp", experiment.title)
  save.plot(dot.plot.min.exp, plot.filename)
  documentation.Word <- plot.to.word(documentation.Word, dot.plot.min.exp)
  } else {
    documentation <- "No cell types met % expression criteria"
    message.with.delay (documentation)
  }
  try(dot.plot.min.exp)
# Document the work done in Word ----
  variables.used <- c(
    "data.file.name",
    "kidney.choice",
    "gene.list",
    "genes.to.plot",
    "gene.select",
    "r.script.file",
    "expression.cutoff",
    "log1p"
  )
  if (data.info[kidney.choice, "disease.subset"] == "yes") {
    variables.used <- c(variables.used, "kidney.subset")
  }
  nothing <- document.variables (documentation.Word, variables.used)
  print(documentation.Word,
        target = paste0(folder.date.experiment, experiment.title, ".docx"))
# save Excel documentation
  saveWorkbook(
    expression.workbook,
    paste0(folder.date.experiment, experiment.title, ".xlsx"),
    overwrite = T
  )
# Done ----
  data.file.name.old <- data.file.name
  message("Done")
