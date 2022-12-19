# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Set Resources for Clustermq or Future -----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#SBATCH --output=<%= resources$log_path %>   #./log/%x_%A-%a.out

plan_all <- tweak(
  batchtools_slurm,
  template = "/home/clz4002/batchtools.slurm.tmpl",
  resources =
    list(
      log_path = "./log/%x_%A-%a.out",
      ncpus = 1,
      memory = "4G",
      walltime = 600,
      partition = "scu-cpu",
      ntasks = 1
    )
)

resources_all <- tar_resources(
  future = tar_resources_future(plan = plan_all)
)


plan_align <- tweak(
  batchtools_slurm,
  template = "/home/clz4002/batchtools.slurm.tmpl",
  resources =
    list(
      log_path = "./log/%x_%A-%a.out",
      ncpus = 16,
      memory = "64G",
      walltime = "48:00:00",
      partition = "scu-cpu",
      ntasks = 1
    )
)


resources_align <- tar_resources(
  future = tar_resources_future(plan = plan_align)
)
