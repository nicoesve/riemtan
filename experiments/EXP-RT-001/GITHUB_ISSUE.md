# Parallelization Does Not Improve Performance in `compute_frechet_mean()`

## Summary

**Finding**: The `parallel::mclapply()` parallelization in `relocate()` provides **no speedup** for Frechet mean computation across tested conditions. Increasing core count from 1 to 32 results in 0.98-1.04× speedup (essentially identical or worse performance).

**Recommendation**: Remove or deprecate parallel processing code, set default `cores=1`, and focus optimization efforts elsewhere.

## Evidence

Comprehensive experimental study (EXP-RT-001) with 39 runs across 6 phases tested parallelization effects:

### Phase 3: Small Matrices (p=10×10, n=500)
- **4 cores**: 84.62 sec (mean)
- **16 cores**: 81.53 sec (mean)
- **Speedup**: 1.04× (essentially none)
- **Parallel efficiency**: 1% (should be 400% for linear scaling)

### Phase 4: Large Matrices (p=100×100, n=500)

| Cores | Mean Time (sec) | Speedup vs 1 core | Parallel Efficiency |
|-------|----------------|-------------------|---------------------|
| 1     | 400.30         | 1.00×             | 100%                |
| 4     | 404.98         | 0.99×             | 25%                 |
| 16    | 409.37         | 0.98×             | 6%                  |
| 32    | 403.11         | 0.99×             | 3%                  |

**Result**: No benefit from parallelization even at larger matrix sizes. Slight performance degradation with more cores due to parallel overhead.

### What DOES Help: Batch Size

While parallelization provides no benefit, **batch size does improve performance** through overhead reduction:

- Windows (sequential): `batch_size=256` is 6.3× faster than `batch_size=16`
- Cluster: `batch_size=256` is 22% faster than `batch_size=128`
- Mechanism: Fewer gradient descent iterations, NOT parallelization

## Root Cause Analysis

The `relocate()` function uses `parallel::mclapply()` to process batches:

```r
# From R/other_utils.R, lines 84-107
relocate <- function(sample, target, met) {
  mapfn <- if (getOption("mc.cores", 2L) > 1) {
    parallel::mclapply
  } else {
    lapply
  }

  result <- mapfn(1:length(sample), function(i) {
    met$exp(met$log(sample[[i]], target))
  })
  # ...
}
```

**Why parallelization fails:**

1. **Small per-item workload**: Each `met$log()` and `met$exp()` operation is relatively fast, even for p=100 matrices
2. **Parallel overhead dominates**: Process forking, data copying, and synchronization costs exceed computational savings
3. **Memory bandwidth bottleneck**: Multiple cores competing for memory access may degrade performance
4. **No data dependencies to exploit**: Operations are embarrassingly parallel but too small to benefit

## Recommendations

### 1. Immediate Actions

**Update default parameters in `compute_frechet_mean()`:**

```r
compute_frechet_mean <- function(sample,
                                  tol = 0.05,
                                  max_iter = 50,
                                  lr = 0.05,
                                  batch_size = 128,  # Keep this default
                                  cores = 1) {       # Change from getOption("mc.cores", 2L)
  # ...
}
```

**Add warning when `cores > 1`:**

```r
if (cores > 1) {
  warning(
    "Parallelization provides no performance benefit for Frechet mean computation. ",
    "Using cores > 1 may actually degrade performance due to overhead. ",
    "See https://github.com/nicoesve/riemtan-dev/issues/[THIS_ISSUE] for details.",
    call. = FALSE
  )
}
```

### 2. Documentation Updates

Update function documentation and vignettes:

**In `?compute_frechet_mean`:**

```r
#' @param batch_size Integer. Number of samples per batch in gradient descent.
#'   Larger batches reduce iteration overhead but increase memory usage.
#'   Recommended: 128-256. Default: 128.
#'
#' @param cores Integer. Number of CPU cores for parallel processing.
#'   **Note**: Experimental evidence shows parallelization provides no benefit
#'   and may degrade performance. Recommended: 1. Default: 1.
```

**Add performance vignette section:**

```markdown
## Performance Optimization

### Batch Size
The `batch_size` parameter significantly affects computation time by reducing
gradient descent iteration overhead. Larger batches (128-256) are recommended.

### Parallelization
Despite using `parallel::mclapply()` internally, **increasing cores does not
improve performance**. Parallel overhead exceeds computational benefit even for
large matrices (100×100). Always use `cores=1` (the default).

See EXP-RT-001 experimental report for details:
[Notion Documentation](https://www.notion.so/297cd823851981c59d31ca974c56ac0e)
```

### 3. Code Simplification (Optional)

Consider removing parallel code entirely to simplify maintenance:

```r
# Simplified relocate() function
relocate <- function(sample, target, met) {
  result <- lapply(1:length(sample), function(i) {
    met$exp(met$log(sample[[i]], target))
  })
  # ...
}
```

This removes dependency on `parallel` package and eliminates user confusion about the `cores` parameter.

### 4. Future Research

Investigate why parallelization fails and explore alternatives:

- **Algorithmic redesign**: Can gradient descent be parallelized at a different level?
- **C++ parallelization**: Would OpenMP-based parallelization in C++ layer help?
- **GPU acceleration**: Are GPU implementations viable for larger matrices (p > 1000)?
- **Batch processing**: Can multiple Frechet mean computations be parallelized across datasets?

## Experimental Details

**Full experimental documentation**: [EXP-RT-001 in Notion](https://www.notion.so/297cd823851981c59d31ca974c56ac0e)

**Experiment logs**:
- [LOG-292](https://www.notion.so/297cd823851981a18f06fe867d94252b): Phase 3 submission (p=10)
- [LOG-293](https://www.notion.so/297cd82385198182a3cbfcfc26cb5f35): Phase 3 completion (no parallel benefit)
- [LOG-294](https://www.notion.so/297cd82385198137969bc6c072a2d977): Phase 4a calibration (p=100)
- [LOG-295](https://www.notion.so/297cd8238519813f8b5bde7b22f2e9fa): Phase 4a results (excellent BLAS scaling)
- [LOG-296](https://www.notion.so/297cd82385198169b928ff8915dda44b): Phase 4 submission (final test)
- [LOG-297](https://www.notion.so/297cd8238519812e9713c04ca78bd457): Phase 4 completion (hypothesis rejected)

**Raw data**: `experiments/EXP-RT-001/artifacts/`
- `phase3_cluster_results.csv` (8 runs, p=10)
- `calibration_p100_results.csv` (1 run, p=100)
- `phase4_minimal_results.csv` (20 runs, p=100, cores=[1,4,16,32])

**Platform**: Indiana University BigRed200 cluster
- CPUs: AMD EPYC 7763 (up to 32 cores tested)
- Memory: 32GB allocated
- R version: 4.5.1
- SLURM jobs: 5846428 (Phase 3), 5846622 (Phase 4a), 5846682 (Phase 4)

## Statistical Rigor

All results show high consistency:

**Phase 4 (p=100) Coefficient of Variation**:
- 1 core: CV = 1.8%
- 4 cores: CV = 1.7%
- 16 cores: CV = 1.9%
- 32 cores: CV = 1.6%

Low variance confirms that lack of speedup is real, not measurement noise.

## Impact

**User benefit**:
- Clear guidance prevents wasted computational resources
- Simplified parameter choices (always use `cores=1`)
- Focus on effective optimization (batch size)

**Scientific value**:
- Documents when/why parallelization fails
- Provides evidence-based recommendations
- Foundation for future algorithmic improvements

**Package quality**:
- Removes misleading parameters
- Simplifies code maintenance
- Aligns documentation with reality

## Related Issues

- #[XX]: Performance optimization for large matrices (if exists)
- #[XX]: Memory usage in batch processing (if exists)

## Checklist

- [ ] Update `compute_frechet_mean()` default `cores=1`
- [ ] Add warning message for `cores > 1`
- [ ] Update function documentation (`?compute_frechet_mean`)
- [ ] Add performance vignette section
- [ ] Update README performance claims
- [ ] Consider removing parallel code entirely
- [ ] Add unit tests for new warnings
- [ ] Update CRAN submission documentation

---

**Experiment ID**: EXP-RT-001
**Date**: 2025-10-24 to 2025-10-25
**Agent**: Toby Esterhase (R package specialist)
**Total runs**: 39 across 6 experimental phases
**Conclusion**: Hypothesis definitively **REJECTED**
