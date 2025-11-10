# Phase 0 Reconnaissance Report: EXP-RT-001

**Date**: 2025-10-24
**Agent**: Toby Esterhase
**Objective**: Understand implementation of batching in `compute_frechet_mean()` and identify measurement strategy

## Code Analysis

### 1. `compute_frechet_mean()` Implementation

**Location**: `R/other_utils.R`, lines 136-215

**Key Parameters**:
- `sample`: CSample object containing SPD matrices
- `tol`: Convergence tolerance (default=0.05)
- `max_iter`: Maximum iterations (default=20)
- `batch_size`: Samples per batch (default=32) ← **EXPERIMENTAL PARAMETER**
- `lr`: Learning rate (default=0.2)

**Algorithm Flow**:

```r
while ((delta > tol) && (iter < max_iter)) {
  old_tan <- aux_sample$tangent_images  # n tangent images
  iter <- iter + 1
  old_ref_pt <- aux_sample$ref_point

  # BATCHING STARTS HERE (lines 169-172)
  n <- length(old_tan)
  idx <- sample(n)  # Shuffle for stochastic batching
  old_tan_shuffled <- old_tan[idx]

  # BATCH PROCESSING LOOP (lines 175-199)
  for (start in seq(1, n, by = batch_size)) {
    end <- min(start + batch_size - 1, n)
    batch <- old_tan_shuffled[start:end]  # Select batch

    # Compute batch step
    tan_step <- lr * Reduce(`+`, batch) / length(batch)
    tan_step <- tan_step |> Matrix::symmpart() |> Matrix::pack()
    new_ref_pt <- aux_sample$riem_metric$exp(old_ref_pt, tan_step)

    # CRITICAL: relocate() is called on ALL n images, not just batch
    # This is where parallelization happens (lines 187-189)
    new_tan_imgs <- relocate(
      old_ref_pt, new_ref_pt, old_tan,  # old_tan has n elements
      sample$riem_metric
    )

    aux_sample <- CSample$new(...)
    old_ref_pt <- new_ref_pt
    old_tan <- new_tan_imgs
  }

  # Compute delta after all batches in this iteration
  new_diff <- Matrix::norm(aux_sample$ref_point - sample$ref_point, "F")
  delta <- abs(new_diff - old_diff) / old_diff
  old_diff <- new_diff
}
```

**Key Insight**:
- Smaller `batch_size` → More iterations through batch loop
- Each batch iteration calls `relocate()` on **all n tangent images**
- More batch iterations = more calls to `relocate()` = more opportunities for parallelization

### 2. `relocate()` Parallelization

**Location**: `R/other_utils.R`, lines 84-107

**Implementation**:

```r
relocate <- function(old_ref, new_ref, images, met) {
  relocate_single <- function(tan) {
    met$exp(old_ref, tan) |> met$log(sigma = new_ref, lambda = _)
  }

  # Platform check (line 91)
  if (.Platform$OS.type == "windows") {
    return(lapply(images, relocate_single))  # Sequential on Windows
  }

  # Core count detection (lines 95-103)
  chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  if (nzchar(chk) && chk == "TRUE") {
    num_workers <- 2L  # CRAN testing
  } else {
    num_workers <- parallel::detectCores()  # Use all cores
  }

  # Parallel execution (line 106)
  parallel::mclapply(images, relocate_single, mc.cores = num_workers)
}
```

**Parallelization Details**:
- Uses `parallel::mclapply()` (forking, Unix-like systems only)
- `num_workers` determined by `parallel::detectCores()`
- Each image relocated independently → embarrassingly parallel
- No explicit `mc.cores` control from `compute_frechet_mean()`

**Current Limitation**: Cannot control number of cores from experiment

### 3. Batching Hypothesis

**Mechanism**:
```
Small batch_size → More batch loop iterations → More relocate() calls
More relocate() calls → More parallel work distributed across cores
Result: Better CPU utilization?
```

**Alternative Hypothesis**:
```
Small batch_size → More overhead from relocate() calls
Each relocate() call → fork overhead + communication overhead
Result: Slower due to overhead?
```

**The Tradeoff**:
- **Smaller batches**: More parallelization opportunities, but more overhead
- **Larger batches**: Less overhead, but fewer parallel opportunities

## Experimental Challenges

### Challenge 1: Core Count Control

**Problem**: `relocate()` uses `parallel::detectCores()` automatically.
**Impact**: Cannot test different core counts without modifying code.

**Solutions**:
1. **Modify `relocate()` temporarily** to accept `mc.cores` parameter
2. **Use environment variable** to limit cores: `options(mc.cores = X)`
3. **SLURM control**: Use `--cpus-per-task` and `OMP_NUM_THREADS`

**Recommendation**: Use Solution 3 with explicit `mc.cores` parameter in test scripts.

### Challenge 2: Windows Testing Limitation

**Problem**: Local machine is Windows → `mclapply` falls back to sequential `lapply`.
**Impact**: Cannot measure parallelization effect locally.

**Solutions**:
- Phase 1 & 2 (local): Verify code correctness only
- Phase 3 & 4 (cluster): Measure parallelization effect

### Challenge 3: Timing Measurement Points

**What to measure**:
1. **Total computation time**: `system.time(compute_frechet_mean(...))`
2. **Iterations to convergence**: Return value from algorithm
3. **Per-iteration time**: Add instrumentation
4. **relocate() time**: Detailed profiling (optional)

**Recommendation**: Focus on total time for simplicity, track iterations as secondary.

## Experimental Design Refinements

### Data Generation Strategy

```r
library(Matrix)
library(riemtan)
data(airm)

# Reference point: 10x10 identity
ref_pt <- diag(10) |>
  Matrix::nearPD() |>
  _$mat |>
  Matrix::pack()

# Dispersion: 0.1 * identity (moderate variability)
disp <- 0.1 * diag(10) |>
  Matrix::nearPD() |>
  _$mat |>
  Matrix::pack()

# Generate n=500 samples
set.seed(20251024)  # Reproducibility
samples <- rspdnorm(n = 500, refpt = ref_pt, disp = disp, met = airm)
```

### Timing Measurement Template

```r
# Control cores explicitly
library(parallel)
options(mc.cores = num_cores)  # Set before running

# Measure timing
timing <- system.time({
  fmean <- compute_frechet_mean(
    sample = samples,
    tol = 0.05,
    max_iter = 20,
    lr = 0.2,
    batch_size = bs  # Experimental condition
  )
})

# Record results
results <- data.frame(
  batch_size = bs,
  cores = num_cores,
  elapsed_time = timing["elapsed"],
  user_time = timing["user"],
  system_time = timing["system"],
  replication = rep_num
)
```

### Modified `relocate()` for Experiments

**Approach**: Create wrapper that controls `mc.cores` explicitly.

```r
# In experiment scripts
relocate_controlled <- function(old_ref, new_ref, images, met, cores) {
  relocate_single <- function(tan) {
    met$exp(old_ref, tan) |> met$log(sigma = new_ref, lambda = _)
  }

  if (cores == 1 || .Platform$OS.type == "windows") {
    return(lapply(images, relocate_single))
  }

  parallel::mclapply(images, relocate_single, mc.cores = cores)
}

# Modify compute_frechet_mean call to use relocate_controlled
# OR: Set environment before calling
Sys.setenv(MC_CORES = cores)
options(mc.cores = cores)
```

## Measurement Strategy

### Phase 1: Minimal Test
- **Purpose**: Verify timing code works
- **Conditions**: 1
- **What to check**: Code runs without errors, timing captured

### Phase 2: Local Subset
- **Purpose**: Validate data collection structure
- **Conditions**: 3 batch sizes × 3 reps = 9
- **What to check**:
  - Consistent timing measurement
  - CSV output format
  - Basic visualization

### Phase 3: Cluster Infrastructure
- **Purpose**: Validate parallel timing measurement
- **Conditions**: 2 batch sizes × 2 cores × 2 reps = 8
- **What to check**:
  - Core control works
  - Parallel speedup observed
  - Artifact retrieval successful

### Phase 4: Full Experiment
- **Purpose**: Complete factorial design
- **Conditions**: 6 batch sizes × 6 cores × 10 reps = 360
- **What to analyze**:
  - Main effect of batch_size
  - Main effect of cores
  - Interaction: batch_size × cores
  - Optimal batch_size per core count

## Expected Patterns

### If Hypothesis is Correct (Batching Helps):
```
Small batch_size + Many cores → Fastest
Large batch_size + Many cores → Slower (underutilized cores)
Small batch_size + Few cores → Similar to large batch
```

### If Null Hypothesis (No Effect):
```
Computation time independent of batch_size
Only depends on cores
```

### If Overhead Dominates:
```
Small batch_size → Slower (overhead)
Large batch_size → Faster (less overhead)
Effect independent of cores
```

## Implementation Plan for Phases 1-4

### Phase 1 Script Structure
```r
# test_minimal.R
library(Matrix)
library(riemtan)

# Generate data
set.seed(20251024)
n <- 500; p <- 10
# ... data generation ...

# Single timing measurement
batch_size <- 32
timing <- system.time({
  fmean <- compute_frechet_mean(sample, batch_size = batch_size)
})

# Print result
cat("Batch size:", batch_size, "\n")
cat("Elapsed time:", timing["elapsed"], "seconds\n")
```

### Phase 2 Script Structure
```r
# test_local.R
# Loop over batch sizes, collect results
batch_sizes <- c(16, 64, 256)
results <- data.frame()

for (bs in batch_sizes) {
  for (rep in 1:3) {
    # ... timing measurement ...
    results <- rbind(results, ...)
  }
}

write.csv(results, "artifacts/local_results.csv")
```

### Phase 3/4 Cluster Script Structure
```r
# script_cluster_full.R
library(parallel)

# Read parameters
batch_sizes <- c(8, 16, 32, 64, 128, 256)
core_counts <- c(1, 2, 4, 8, 16, 32)

# Detect available cores from SLURM
slurm_cpus <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "32"))

# Loop over conditions
for (bs in batch_sizes) {
  for (cores in core_counts) {
    if (cores > slurm_cpus) next  # Skip if not enough cores

    # Set core limit
    options(mc.cores = cores)
    Sys.setenv(MC_CORES = cores)

    for (rep in 1:10) {
      # ... timing measurement ...
    }
  }
}
```

## Key Findings from Reconnaissance

1. **Batching mechanism confirmed**: Smaller batches → more `relocate()` calls
2. **Parallelization location**: In `relocate()` via `mclapply`
3. **Core control needed**: Must modify experiment scripts to control cores
4. **Windows limitation**: Local testing limited to correctness verification
5. **Measurement straightforward**: `system.time()` sufficient

## Recommendations for Phase 1

1. **Start simple**: Single batch_size, verify timing works
2. **Use default parameters**: tol=0.05, max_iter=20, lr=0.2
3. **Set fixed seed**: Reproducibility across phases
4. **Record all metrics**: time, iterations, convergence delta

## Next Steps

- ✅ Phase 0 complete: Code understood, strategy defined
- → Phase 1: Create `test_minimal.R` and verify locally
- → Phase 2: Expand to subset testing
- → Modify scripts for core control before cluster phases

## References

- `R/other_utils.R::compute_frechet_mean()` (lines 136-215)
- `R/other_utils.R::relocate()` (lines 84-107)
- `parallel::mclapply` documentation
- EXP-MP-001 cluster infrastructure patterns
