#!/bin/bash
#SBATCH --job-name=EXP-RT-001-Phase4-Full
#SBATCH --output=/N/slate/nescoba/experiments/EXP-RT-001/logs/slurm_%j.out
#SBATCH --error=/N/slate/nescoba/experiments/EXP-RT-001/logs/slurm_%j.err
#SBATCH --time=02:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32
#SBATCH --mem=32G
#SBATCH --partition=general
#SBATCH --account=r00582
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=nescoba@iu.edu

# =============================================================================
# EXP-RT-001 Phase 4: Full Cluster Experiment
# =============================================================================
# Experiment: EXP-RT-001
# Project: riemtan package development
# GitHub Issue: [To be created in riemtan-dev]
# Notion: https://www.notion.so/297cd823851981c59d31ca974c56ac0e
# Purpose: Execute complete factorial design to quantify batching effect on
#          Frechet mean computation time across different core counts
# =============================================================================

# Load R module
module load r/4.5.1

# Print job information
echo "==============================================================================="
echo "EXP-RT-001 Phase 4: Full Factorial Experiment"
echo "==============================================================================="
echo "Job started on $(hostname) at $(date)"
echo "Job ID: $SLURM_JOB_ID"
echo "Number of CPUs: $SLURM_CPUS_PER_TASK"
echo "Memory: $SLURM_MEM_PER_NODE MB"
echo "Partition: $SLURM_JOB_PARTITION"
echo "Account: $SLURM_JOB_ACCOUNT"
echo "Working directory: $(pwd)"
echo "==============================================================================="
echo ""

# Change to experiment directory
cd $SLURM_SUBMIT_DIR
echo "Experiment directory: $(pwd)"
echo ""

# Print R and package versions
echo "Software versions:"
Rscript --version
echo ""
Rscript -e "cat('riemtan version:', as.character(packageVersion('riemtan')), '\n')"
Rscript -e "cat('Matrix version:', as.character(packageVersion('Matrix')), '\n')"
Rscript -e "cat('parallel version:', as.character(packageVersion('parallel')), '\n')"
echo ""

# Create/verify cluster experiment directory structure
CLUSTER_EXP_DIR="/N/slate/nescoba/experiments/EXP-RT-001"
mkdir -p "$CLUSTER_EXP_DIR/raw_results"
mkdir -p "$CLUSTER_EXP_DIR/artifacts"
mkdir -p "$CLUSTER_EXP_DIR/logs"

echo "Cluster directory structure verified:"
echo "  - Base: $CLUSTER_EXP_DIR/"
echo "  - Raw results: $CLUSTER_EXP_DIR/raw_results/"
echo "  - Artifacts: $CLUSTER_EXP_DIR/artifacts/"
echo "  - Logs: $CLUSTER_EXP_DIR/logs/"
echo ""

# Copy params.yaml to cluster for archival
if [ -f "params.yaml" ]; then
    cp params.yaml "$CLUSTER_EXP_DIR/"
    echo "Parameters archived: $CLUSTER_EXP_DIR/params.yaml"
fi
echo ""

# Print experimental design
echo "EXPERIMENTAL DESIGN:"
echo "==============================================================================="
echo "  Batch sizes: [8, 16, 32, 64, 128, 256] (6 levels)"
echo "  Core counts: [1, 2, 4, 8, 16, 32] (6 levels)"
echo "  Replications: 10 per condition"
echo "  Total conditions: 6 × 6 = 36"
echo "  Total runs: 36 × 10 = 360 Frechet mean computations"
echo ""
echo "  Sample size (n): 500 SPD matrices"
echo "  Matrix dimension (p): 10×10"
echo "  Metric: AIRM (Affine Invariant Riemannian Metric)"
echo ""
echo "  Estimated runtime: 1-2 hours"
echo "==============================================================================="
echo ""

# Run the full experiment
echo "Starting Phase 4 full factorial experiment..."
echo "Timestamp: $(date)"
echo ""

Rscript script_cluster_full.R 2>&1 | tee "$CLUSTER_EXP_DIR/logs/phase4_execution.log"

# Capture exit code
EXIT_CODE=$?

echo ""
echo "==============================================================================="
echo "EXECUTION SUMMARY"
echo "==============================================================================="
echo "Job finished at: $(date)"
echo ""

if [ $EXIT_CODE -eq 0 ]; then
    echo "STATUS: SUCCESS ✓"
    echo ""
    echo "All 360 experimental conditions completed successfully."
    echo ""
else
    echo "STATUS: FAILED ✗"
    echo "Exit code: $EXIT_CODE"
    echo ""
    echo "Check error log for details:"
    echo "  $CLUSTER_EXP_DIR/logs/slurm_$SLURM_JOB_ID.err"
    echo ""
fi

echo "==============================================================================="
echo "RESULTS LOCATIONS"
echo "==============================================================================="
echo "  Main results: $CLUSTER_EXP_DIR/artifacts/cluster_full_results.csv"
echo "  Raw results: $CLUSTER_EXP_DIR/raw_results/results_*.csv"
echo "  Parameters: $CLUSTER_EXP_DIR/params.yaml"
echo "  Execution log: $CLUSTER_EXP_DIR/logs/phase4_execution.log"
echo "  SLURM output: $CLUSTER_EXP_DIR/logs/slurm_$SLURM_JOB_ID.out"
echo "  SLURM errors: $CLUSTER_EXP_DIR/logs/slurm_$SLURM_JOB_ID.err"
echo ""

echo "==============================================================================="
echo "NEXT STEPS FOR CONTROL"
echo "==============================================================================="
echo ""
echo "1. Retrieve results from cluster:"
echo "   scp -r nescoba@bigred200.uits.iu.edu:$CLUSTER_EXP_DIR/artifacts/ \\"
echo "     'C:/Users/new user/Documents/GitHub/packages/riemtan-dev/experiments/EXP-RT-001/artifacts/'"
echo ""
echo "2. Update Notion EXP-RT-001 page:"
echo "   URL: https://www.notion.so/297cd823851981c59d31ca974c56ac0e"
echo "   - SLURM Job ID: $SLURM_JOB_ID"
echo "   - Status: Phase 4 Complete"
echo "   - Results Summary: [Add from analyze_results.R]"
echo ""
echo "3. Run Phase 5 analysis locally:"
echo "   Rscript experiments/EXP-RT-001/analyze_results.R"
echo ""
echo "4. Create LOG entry documenting completion"
echo ""
echo "5. Update GitHub issue with findings"
echo ""
echo "6. Review recommendations for riemtan package defaults"
echo ""

echo "==============================================================================="
echo "VALIDATION CHECKLIST"
echo "==============================================================================="
echo "  [ ] All 360 runs completed (check results CSV row count)"
echo "  [ ] No error messages in SLURM error log"
echo "  [ ] Timing measurements reasonable (not suspiciously fast/slow)"
echo "  [ ] Results show variation across conditions"
echo "  [ ] Files successfully retrieved from cluster"
echo "  [ ] Ready for statistical analysis (Phase 5)"
echo ""

echo "==============================================================================="
echo "Job ID: $SLURM_JOB_ID"
echo "Exit Code: $EXIT_CODE"
echo "==============================================================================="

exit $EXIT_CODE
