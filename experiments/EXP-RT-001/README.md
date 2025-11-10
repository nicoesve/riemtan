# EXP-RT-001: Batching Effect on Frechet Mean Computation

**Experiment ID**: EXP-RT-001
**Date**: 2025-10-24
**Project**: riemtan package development
**Agent**: Toby Esterhase
**Status**: Planned

## Hypothesis

Batching in `compute_frechet_mean()` speeds up computation when multiple cores are available because the `relocate()` function processes batches in parallel.

## Background

The `compute_frechet_mean()` function in riemtan has a `batch_size` parameter (default=32) that controls how samples are processed. Within each batch iteration, the `relocate()` function is called, which uses `parallel::mclapply` on Unix-like systems to parallelize the relocation of tangent images.

**Current implementation** (R/other_utils.R:175-199):
- Shuffles n samples
- Processes in batches of size `batch_size`
- For each batch, calls `relocate()` on all n tangent images
- `relocate()` uses `parallel::detectCores()` cores

**Research question**: How does batch_size interact with the number of cores to affect computation time?

## Experimental Design

### Factorial Design
- **Batch sizes**: [8, 16, 32, 64, 128, 256] (6 levels)
- **Core counts**: [1, 2, 4, 8, 16, 32] (6 levels)
- **Total conditions**: 36 combinations
- **Replications**: 10 per condition
- **Total Frechet mean computations**: 360

### Data Generation
- **Sample size**: n=500 SPD matrices
- **Matrix dimension**: p=10×10
- **Metric**: AIRM (Affine Invariant Riemannian Metric)
- **Generation**: `rspdnorm(n=500, refpt=I_10, disp=0.1*I_10, met=airm)`

### Response Variables
1. **Computation time** (seconds) - primary outcome
2. **Iterations to convergence**
3. **Final delta**
4. **CPU utilization**

## Phases

### Phase 0: Reconnaissance (1 hour)
**Status**: Planned

Review implementation in:
- `R/other_utils.R::compute_frechet_mean()` (lines 136-215)
- `R/other_utils.R::relocate()` (lines 84-107)

Document measurement strategy and current behavior.

### Phase 1: Minimal Local Test (1 hour)
**Status**: Planned
**Script**: `test_minimal.R`

Single condition test to verify timing measurement:
- 1 batch size (e.g., 32)
- 1 replication
- Local Windows machine (sequential processing)

### Phase 2: Local Subset Test (2 hours)
**Status**: Planned
**Script**: `test_local.R`

Reduced factorial:
- 3 batch sizes: [16, 64, 256]
- 3 replications
- Generates: `artifacts/local_results.csv`, diagnostic plots

### Phase 3: Cluster Infrastructure Test (2 hours)
**Status**: Planned
**Scripts**: `script_cluster_test.R`, `run_cluster_test.slurm`

Minimal cluster validation:
- 2 batch sizes: [32, 128]
- 2 core counts: [4, 16]
- 2 replications = 8 total runs
- Verifies: package installation, SLURM submission, artifact retrieval

### Phase 4: Full Cluster Experiment (6-8 hours)
**Status**: Planned
**Scripts**: `script_cluster_full.R`, `run_cluster_full.slurm`

Complete factorial design:
- All 6 batch sizes × 6 core counts
- 10 replications
- 360 total Frechet mean computations
- Estimated runtime: 1-2 hours on BigRed200

### Phase 5: Analysis and Reporting (3 hours)
**Status**: Planned
**Script**: `analyze_results.R`

Statistical analysis:
- ANOVA: batch_size × cores interaction
- Response surface analysis
- Speedup curves
- Recommendations for riemtan defaults

## Files

```
EXP-RT-001/
├── README.md (this file)
├── params.yaml (experimental parameters)
├── test_minimal.R (Phase 1)
├── test_local.R (Phase 2)
├── script_cluster_test.R (Phase 3)
├── run_cluster_test.slurm (Phase 3)
├── script_cluster_full.R (Phase 4)
├── run_cluster_full.slurm (Phase 4)
├── analyze_results.R (Phase 5)
└── artifacts/
    ├── minimal_results.csv
    ├── local_results.csv
    ├── cluster_test_results.csv
    ├── cluster_full_results.csv
    └── plots/
```

## Cluster Setup

**Cluster**: BigRed200 (bigred200.uits.iu.edu)
**Account**: r00582
**Home**: `/N/u/nescobar/BigRed200/riemtan-dev/`
**Experiments**: `/N/slate/nescoba/experiments/EXP-RT-001/`

**SSH Workflow** (per LOG-280):
1. Agent prepares scripts and SSH command guide
2. Control executes SSH commands manually (passphrase required)
3. Agent processes retrieved results

## Expected Outcomes

1. **Quantified speedup**: How much faster with optimal batching?
2. **Optimal batch_size**: Function of cores available?
3. **Recommendations**: Should default batch_size be adaptive?
4. **Algorithm insight**: Does batching overhead negate parallel benefits?

## Success Criteria

- ✅ All 360 conditions complete without errors
- ✅ Timing CV <5% within conditions
- ✅ Clear statistical evidence for/against batching effect
- ✅ Actionable recommendations for riemtan

## References

- **Package**: riemtan-dev (C:\Users\new user\Documents\GitHub\packages\riemtan-dev)
- **Notion**: [EXP-RT-001 page to be created]
- **GitHub Issue**: [Issue to be created in riemtan-dev]
- **Related experiments**: EXP-MP-001, EXP-MP-002 (methods_paper)
- **Infrastructure docs**: LOG-280 (SSH workflow), LOG-284 (cluster validation)
