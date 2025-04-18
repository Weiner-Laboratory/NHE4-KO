get.data.info <- function() {
  # information on different datasets ----
  # load libraries
  library(Seurat)
  library(qs)
  # load dataset information
  mouse.cz <- "a8689831-a9b3-4915-9d62-5d85ce926224.rds.qs.qs2" # nolint
  human.sc.cz <- "51101a5a-7f7e-4559-b67a-5f5fd484569c.rds.qs.qs2" # nolint
  human.sn.cz <- "b2f1886e-a3aa-4d48-80c9-dcdd44e89e02.rds.qs.qs2" # nolint
  kpmp <- "13234428-0cb8-49b9-bb86-861a9289a1be_WashU-Altos_KPMP-Biopsy_10X-R_01082024.h5Seurat.qs.qs2" # nolint
  mouse.ah <- "all.data.azimuth.wt.rds.qs.qs2"
  mouse.ah.merge.etc <- "AH WT 20241113.qs.qs2"
  mouse.ah.wtko.merge.etc <- "all.data.combined.azimuth - merge, logNorm, Harmony, JoinLayers - WT-KO - 20241113.qs.qs2"
  mouse.ah.ko.etc <- "AR KO - merge, logNorm, Harmony, JoinLayers.qs.qs2"
  mouse.ah.3criteria <- "3criteria.qs.qs2"
  human.czi.cns <- "a97cd3b7-8083-4d4b-b2d8-681078d7c94f.rds"
  rat <- "rat from kah.rds.qs2"
  data.info <- NULL
  data.info <- rbind(
    data.info,
    c(
      "Mouse Kidney Atlas from CZI",
      mouse.cz,
      "mouse",
      "ensembl",
      "lower",
      "cell_type",
      "no",
      "mmusculus_gene_ensembl",
      "Origin"
    )
  )
  data.info <- as.data.frame(data.info)
  colnames(data.info) <- c(
    "dataset",
    "filename",
    "species",
    "gene.type",
    "gene.case",
    "cell.ID",
    "disease.subset",
    "ensembl.dataset",
    "group.by"
  )
  # data.info <- rbind(data.info,c("MKA - nomalized",paste0("normalized/", mouse.cz),"mouse","ensembl","lower","cell_type","no","mmusculus_gene_ensembl","Origin"))
  data.info <- rbind(
    data.info,
    c(
      "human.sc.cz",
      human.sc.cz,
      "human",
      "ensembl",
      "lower",
      "cell_type",
      "yes",
      "hsapiens_gene_ensembl",
      "Run"
    )
  )
  # data.info <- rbind(data.info,c("human.sc.cz - normalized",paste0("normalized/",human.sc.cz),"human","ensembl","lower","cell_type","yes","hsapiens_gene_ensembl","Run"))
  data.info <- rbind(
    data.info,
    c(
      "human.sn.cz",
      human.sn.cz,
      "human",
      "ensembl",
      "lower",
      "cell_type",
      "yes",
      "hsapiens_gene_ensembl",
      "experiment_id"
    )
  )
  
  # data.info <- rbind(data.info,c("human.sn.cz - normalized",paste0("normalized/", human.sn.cz),"human","ensembl","lower","azimuth.l2","yes","hsapiens_gene_ensembl","experiment_id"))
  
  data.info <- rbind(
    data.info,
    c(
      "kpmp",
      kpmp,
      "human",
      "gene",
      "upper",
      "subclass.l2",
      "no",
      "hsapiens_gene_ensembl",
      "experiment_id"
    )
  )
  # data.info <- rbind(data.info,c("kpmp - normalized",paste0("normalized/", kpmp),"human","gene","upper","azimuth.l2","no","hsapiens_gene_ensembl","experiment_id"))
  data.info <- rbind(
    data.info,
    c(
      "mouse - AH dataset - wt",
      mouse.ah,
      "mouse",
      "gene",
      "title",
      "azimuth.l3",
      "no",
      "mmusculus_gene_ensembl",
      "group.by"
    )
  )
  data.info <- rbind(
    data.info,
    c(
      "mouse - AH merge, LogN, Harmony, JL - wt",
      mouse.ah.merge.etc,
      "mouse",
      "gene",
      "title",
      "azimuth.l3",
      "no",
      "mmusculus_gene_ensembl",
      "group.by"
    )
  )
  data.info <- rbind(
    data.info,
    c(
      "mouse - AH merge, LogN, Harmony, JL - wt-KO",
      mouse.ah.wtko.merge.etc,
      "mouse",
      "gene",
      "title",
      "azimuth.l3",
      "no",
      "mmusculus_gene_ensembl",
      "group.by"
    )
  )
    data.info <- rbind(
      data.info,
      c(
        "AH AR KO merge, LogN, Harmony, JL",
        mouse.ah.ko.etc,
        "mouse",
        "gene",
        "title",
        "azimuth.l3",
        "no",
        "mmusculus_gene_ensembl",
        "group.by"
      )
  )
    data.info <- rbind(
      data.info,
      c(
        "mouse - AH dataset - wt - 3 criteria",
        mouse.ah.3criteria,
        "mouse",
        "gene",
        "title",
        "azimuth.l3",
        "no",
        "mmusculus_gene_ensembl",
        "group.by"
      )
    )
    data.info <- rbind(
      data.info,
      c(
        "rat from kah",
        rat,
        "rat",
        "gene",
        "title",
        "celltype",
        "no",
        "mmusculus_gene_ensembl",
        "group.by"
      )
    )
    # human CNS 
    data.info <- rbind(
      data.info,
      c(
        "Human CZI cns",
        human.czi.cns,
        "human",
        "ensembl",
        "lower",
        "cell_type",
        "no",
        "hsapiens_gene_ensembl",
        "group.by"
      )
    )
  rownames(data.info) <- data.info$dataset
  data.info$dataset <- NULL
  return(data.info)
}
