# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Set Resources for Clustermq or Future -----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#SBATCH --output=<%= resources$log_path %>   #./log/%x_%A-%a.out

plan <- tweak(
  batchtools_slurm,
  template = "/home/clz4002/batchtools.slurm.tmpl",
  resources =
    list(
      log_path = "./log/%x_%A-%a.out",
      ncpus = 1,
      memory = "16G",
      walltime = 600,
      partition = "scu-cpu",
      ntasks = 1
    )
)

plan_intensive <- tweak(
  batchtools_slurm,
  template = "/home/clz4002/batchtools.slurm.tmpl",
  resources =
    list(
      log_path = "./log/%x_%A-%a.out",
      ncpus = 1,
      memory = "160G",
      walltime = "48:00:00",
      partition = "scu-cpu",
      ntasks = 1
    )
)

plan_64G <- tweak(
  batchtools_slurm,
  template = "/athena/marchionnilab/scratch/clz4002/inghirami/atac_seq/210318_A00814_0381_AH5HYLDRXY/inghirami_atacseq/batchtools.slurm.tmpl",
  resources =
    list(
      log_path = "./log/%x_%A-%a.out",
      ncpus = 1,
      memory = "64G",
      walltime = "48:00:00",
      partition = "scu-cpu",
      ntasks = 1
    )
)



resources_all <- tar_resources(
  future = tar_resources_future(plan = plan)
)

resources_intensive <- tar_resources(
  future = tar_resources_future(plan = plan_intensive)
)

resources_64G <- tar_resources(
  future = tar_resources_future(plan = plan_64G)
)
