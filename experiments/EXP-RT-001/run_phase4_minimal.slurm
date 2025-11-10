#!/bin/bash
#SBATCH --job-name=EXP-RT-001-Phase4
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=nescoba@iu.edu
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32
#SBATCH --mem=32G
#SBATCH --time=04:00:00
#SBATCH --partition=general
#SBATCH --account=r00582
#SBATCH --output=/N/slate/nescoba/experiments/EXP-RT-001/logs/slurm_%j.out
#SBATCH --error=/N/slate/nescoba/experiments/EXP-RT-001/logs/slurm_%j.err

# EXP-RT-001 Phase 4: Minimal Parallelization Study (p=100)
# Purpose: Test whether parallelization helps for larger matrices
# Date: 2025-10-25
# Agent: Toby Esterhase

echo "========================================"
echo "EXP-RT-001 Phase 4: Parallelization Study"
echo "========================================"
echo ""
echo "Job started at: $(date)"
echo "Job ID: $SLURM_JOB_ID"
echo "Running on node: $SLURMD_NODENAME"
echo "Allocated CPUs: $SLURM_CPUS_PER_TASK"
echo "Working directory: $(pwd)"
echo ""

# Load R module
echo "Loading R module..."
module load r/4.5.1
module list
echo ""

# Verify riemtan package
echo "Checking riemtan package..."
Rscript -e "cat('riemtan version:', as.character(packageVersion('riemtan')), '\n')"
echo ""

# Navigate to experiment directory
cd ~/riemtan-dev/experiments/EXP-RT-001

echo "Experiment directory contents:"
ls -lh
echo ""

# Run Phase 4 experiment
echo "========================================"
echo "Starting Phase 4 experiment..."
echo "Expected runtime: ~3 hours"
echo "Total runs: 20 (4 core counts × 5 reps)"
echo "========================================"
echo ""

# Run with time tracking
/usr/bin/time -v Rscript script_phase4_minimal.R

EXIT_CODE=$?

echo ""
echo "========================================"
echo "Job completed at: $(date)"
echo "Exit code: $EXIT_CODE"
echo ""

if [ $EXIT_CODE -eq 0 ]; then
    echo "Status: SUCCESS ✓"
    echo ""
    echo "Results saved to:"
    echo "  /N/slate/nescoba/experiments/EXP-RT-001/artifacts/phase4_minimal_results.csv"
    echo ""
    echo "To retrieve:"
    echo "  scp -r nescoba@bigred200.uits.iu.edu:/N/slate/nescoba/experiments/EXP-RT-001/artifacts/phase4_minimal_results.csv ./"
    echo ""
    echo "Expected results: 20 runs + 1 header = 21 lines"
else
    echo "Status: FAILED ✗"
    echo ""
    echo "Check error log:"
    echo "  /N/slate/nescoba/experiments/EXP-RT-001/logs/slurm_${SLURM_JOB_ID}.err"
fi

echo "========================================"
