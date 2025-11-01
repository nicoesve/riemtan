#!/bin/bash
#SBATCH --job-name=EXP-RT-001-Phase3
#SBATCH --output=/N/slate/nescoba/experiments/EXP-RT-001/logs/slurm_%j.out
#SBATCH --error=/N/slate/nescoba/experiments/EXP-RT-001/logs/slurm_%j.err
#SBATCH --time=01:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=16G
#SBATCH --partition=general
#SBATCH --account=r00582
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=nescoba@iu.edu

# =============================================================================
# EXP-RT-001 Phase 3: Cluster Infrastructure Test
# =============================================================================
# Experiment: EXP-RT-001
# Project: riemtan package development
# GitHub Issue: [To be created in riemtan-dev]
# Notion: https://www.notion.so/297cd823851981c59d31ca974c56ac0e
# Purpose: Validate BigRed200 cluster setup and parallel processing for batching experiment
# =============================================================================

# Load R module
module load r/4.5.1

# Print job information
echo "==============================================================================="
echo "EXP-RT-001 Phase 3: Cluster Infrastructure Test"
echo "==============================================================================="
echo "Job started on $(hostname) at $(date)"
echo "Job ID: $SLURM_JOB_ID"
echo "Number of CPUs: $SLURM_CPUS_PER_TASK"
echo "Memory: $SLURM_MEM_PER_NODE MB"
echo "Working directory: $(pwd)"
echo "==============================================================================="
echo ""

# Change to experiment directory
cd $SLURM_SUBMIT_DIR
echo "Experiment directory: $(pwd)"
echo ""

# Print R version and package info
echo "R version:"
Rscript --version
echo ""

echo "Checking riemtan installation:"
Rscript -e "cat('riemtan version:', as.character(packageVersion('riemtan')), '\n')"
echo ""

# Create cluster experiment directory structure
CLUSTER_EXP_DIR="/N/slate/nescoba/experiments/EXP-RT-001"
mkdir -p "$CLUSTER_EXP_DIR/raw_results"
mkdir -p "$CLUSTER_EXP_DIR/artifacts"
mkdir -p "$CLUSTER_EXP_DIR/logs"

echo "Cluster directories created:"
echo "  - Raw results: $CLUSTER_EXP_DIR/raw_results/"
echo "  - Artifacts: $CLUSTER_EXP_DIR/artifacts/"
echo "  - Logs: $CLUSTER_EXP_DIR/logs/"
echo ""

# Run the infrastructure test
echo "Starting Phase 3 infrastructure test..."
echo "Testing conditions:"
echo "  - Batch sizes: [32, 128]"
echo "  - Core counts: [4, 16]"
echo "  - Replications: 2 per condition"
echo "  - Total runs: 8"
echo ""

Rscript script_cluster_test.R

# Capture exit code
EXIT_CODE=$?

echo ""
echo "==============================================================================="
if [ $EXIT_CODE -eq 0 ]; then
    echo "INFRASTRUCTURE TEST: SUCCESS"
    echo "Phase 3 completed successfully at $(date)"
else
    echo "INFRASTRUCTURE TEST: FAILED"
    echo "Phase 3 failed with exit code $EXIT_CODE at $(date)"
fi
echo "==============================================================================="
echo ""

# Print location of results
echo "Results locations:"
echo "  - Cluster artifacts: $CLUSTER_EXP_DIR/artifacts/cluster_test_results.csv"
echo "  - Logs: $CLUSTER_EXP_DIR/logs/slurm_$SLURM_JOB_ID.out"
echo ""

# Print next steps
echo "Next steps for Control:"
echo "  1. Check this log file for completion status"
echo "  2. Retrieve artifacts:"
echo "     scp -r nescoba@bigred200.uits.iu.edu:$CLUSTER_EXP_DIR/artifacts/ \\"
echo "       'C:/Users/new user/Documents/GitHub/packages/riemtan-dev/experiments/EXP-RT-001/artifacts/'"
echo "  3. Update Notion EXP-RT-001 page:"
echo "     - SLURM Job ID: $SLURM_JOB_ID"
echo "     - Status: Phase 3 Complete"
echo "     - Results Summary: [From cluster_test_results.csv]"
echo "  4. If successful, proceed to Phase 4 (full experiment)"
echo ""

# Print validation checklist
echo "Validation Checklist:"
echo "  [ ] Parallel processing functional (speedup with more cores)"
echo "  [ ] All 8 experimental conditions completed"
echo "  [ ] Results CSV file created successfully"
echo "  [ ] Timing measurements reasonable (not suspiciously fast/slow)"
echo "  [ ] Ready to proceed to full factorial design"
echo ""

exit $EXIT_CODE
