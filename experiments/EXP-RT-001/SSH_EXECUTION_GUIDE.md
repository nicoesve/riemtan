# SSH Execution Guide: EXP-RT-001
**For**: Control (manual execution)
**From**: Toby Esterhase
**Date**: 2025-10-24

Following the SSH workflow pattern established in LOG-280, this guide provides step-by-step commands for executing the cluster experiments.

## Overview

**SSH Authentication**: Requires passphrase-protected SSH key (cannot be automated)
**Workflow**: Agent-prepared scripts, user-executed SSH commands
**Cluster**: BigRed200 (bigred200.uits.iu.edu)

## Prerequisites

- [ ] WSL or Git Bash installed (for SSH on Windows)
- [ ] SSH key configured for bigred200.uits.iu.edu
- [ ] Passphrase for SSH key available

## Phase 3: Cluster Infrastructure Test (Execute First)

### Purpose
Validate that:
- riemtan-dev is installed correctly on BigRed200
- SLURM job submission works
- Parallel processing is functional
- Artifact retrieval works

### Estimated Time
- Setup: 10 minutes
- Job runtime: 15-30 minutes
- Retrieval: 5 minutes
- **Total**: ~45 minutes

### Step-by-Step Commands

#### 1. Create Cluster Directories

```bash
# Connect to BigRed200
ssh nescoba@bigred200.uits.iu.edu
# [Enter passphrase when prompted]

# Create experiment directories
mkdir -p ~/riemtan-dev/experiments/EXP-RT-001
mkdir -p /N/slate/nescoba/experiments/EXP-RT-001/{logs,raw_results,artifacts}

# Verify
ls -la /N/slate/nescoba/experiments/EXP-RT-001/

# Logout
exit
```

#### 2. Copy Scripts to Cluster

```bash
# From local machine (Windows)
# Navigate to experiment directory
cd "C:/Users/new user/Documents/GitHub/packages/riemtan-dev/experiments/EXP-RT-001"

# Copy Phase 3 scripts
scp script_cluster_test.R nescoba@bigred200.uits.iu.edu:~/riemtan-dev/experiments/EXP-RT-001/
scp run_cluster_test.slurm nescoba@bigred200.uits.iu.edu:~/riemtan-dev/experiments/EXP-RT-001/
```

#### 3. Submit SLURM Job

```bash
# Connect to BigRed200
ssh nescoba@bigred200.uits.iu.edu

# Navigate to experiment directory
cd ~/riemtan-dev/experiments/EXP-RT-001

# Verify scripts are present
ls -l

# Submit job
sbatch run_cluster_test.slurm

# Note the Job ID returned (e.g., "Submitted batch job 5845678")
```

#### 4. Monitor Job

```bash
# While still connected to BigRed200

# Check job status
squeue -u nescoba

# View live output (optional)
tail -f /N/slate/nescoba/experiments/EXP-RT-001/logs/slurm_*.out

# Check if job is complete
squeue -u nescoba | grep EXP-RT-001
# (No output means job finished)

# Logout
exit
```

#### 5. Retrieve Results

```bash
# From local machine
# Retrieve artifacts
scp -r nescoba@bigred200.uits.iu.edu:/N/slate/nescoba/experiments/EXP-RT-001/artifacts/ \
  "C:/Users/new user/Documents/GitHub/packages/riemtan-dev/experiments/EXP-RT-001/"

# Verify retrieval
ls -l "C:/Users/new user/Documents/GitHub/packages/riemtan-dev/experiments/EXP-RT-001/artifacts/"
```

#### 6. Report Results to Agent

After retrieval, check:
- `artifacts/cluster_test_results.csv` exists and has 8 rows
- Results show parallelization effect (faster with 16 cores vs 4 cores)

Report to Toby Esterhase:
- SLURM Job ID
- Status (SUCCESS/FAILED)
- Speedup observed (if successful)

### Troubleshooting Phase 3

**If job fails:**
```bash
ssh nescoba@bigred200.uits.iu.edu
cat /N/slate/nescoba/experiments/EXP-RT-001/logs/slurm_*.err
```

**If riemtan not found:**
```bash
ssh nescoba@bigred200.uits.iu.edu
module load r/4.5.1
R -e "packageVersion('riemtan')"
```

---

## Phase 4: Full Cluster Experiment (Execute After Phase 3 Success)

### Purpose
Execute the complete factorial design:
- 6 batch sizes × 6 core counts × 10 reps = 360 runs
- Approximately 1-2 hours runtime

### Estimated Time
- Setup: 5 minutes (scripts already on cluster)
- Job runtime: 60-120 minutes
- Retrieval: 5 minutes
- **Total**: ~1.5-2.5 hours

### Step-by-Step Commands

#### 1. Copy Full Experiment Scripts

```bash
# From local machine
cd "C:/Users/new user/Documents/GitHub/packages/riemtan-dev/experiments/EXP-RT-001"

# Copy Phase 4 scripts
scp script_cluster_full.R nescoba@bigred200.uits.iu.edu:~/riemtan-dev/experiments/EXP-RT-001/
scp run_cluster_full.slurm nescoba@bigred200.uits.iu.edu:~/riemtan-dev/experiments/EXP-RT-001/
scp params.yaml nescoba@bigred200.uits.iu.edu:~/riemtan-dev/experiments/EXP-RT-001/
```

#### 2. Submit Full Experiment

```bash
# Connect to BigRed200
ssh nescoba@bigred200.uits.iu.edu

# Navigate to experiment directory
cd ~/riemtan-dev/experiments/EXP-RT-001

# Double-check scripts are present
ls -l script_cluster_full.R run_cluster_full.slurm params.yaml

# Submit job
sbatch run_cluster_full.slurm

# Note the Job ID (CRITICAL - update Notion with this)
```

#### 3. Monitor Progress

```bash
# While still connected

# Check job status
squeue -u nescoba

# View progress (optional - job updates every 10 runs)
tail -f /N/slate/nescoba/experiments/EXP-RT-001/logs/slurm_*.out

# Estimate time remaining from output:
# Look for lines like: [Progress: 50/360 (13.9%) | Elapsed: 15.2 min | ETA: 94.3 min]

# Logout (job continues running)
exit
```

#### 4. Check Completion

```bash
# From local machine, periodically check:
ssh nescoba@bigred200.uits.iu.edu "squeue -u nescoba | grep EXP-RT-001"

# No output = job finished
```

#### 5. Retrieve Full Results

```bash
# From local machine (after job completion)

# Retrieve all artifacts
scp -r nescoba@bigred200.uits.iu.edu:/N/slate/nescoba/experiments/EXP-RT-001/artifacts/ \
  "C:/Users/new user/Documents/GitHub/packages/riemtan-dev/experiments/EXP-RT-001/"

# Also retrieve logs for documentation
scp -r nescoba@bigred200.uits.iu.edu:/N/slate/nescoba/experiments/EXP-RT-001/logs/ \
  "C:/Users/new user/Documents/GitHub/packages/riemtan-dev/experiments/EXP-RT-001/"
```

#### 6. Verify Results

```bash
# Check CSV has 360 rows (36 conditions × 10 reps)
wc -l "C:/Users/new user/Documents/GitHub/packages/riemtan-dev/experiments/EXP-RT-001/artifacts/cluster_full_results.csv"
# Should show: 361 (360 data rows + 1 header)
```

### Troubleshooting Phase 4

**If job fails or times out:**
```bash
# Check error log
ssh nescoba@bigred200.uits.iu.edu
cat /N/slate/nescoba/experiments/EXP-RT-001/logs/slurm_*.err

# Check how many runs completed
wc -l /N/slate/nescoba/experiments/EXP-RT-001/artifacts/cluster_full_results.csv
```

**If need to extend walltime:**
- Modify `run_cluster_full.slurm`, line 6: `#SBATCH --time=03:00:00`
- Resubmit

---

## Post-Execution Tasks

After Phase 4 completes successfully:

### 1. Update Notion
Visit: https://www.notion.so/297cd823851981c59d31ca974c56ac0e

Update:
- **Status**: Phase 4 Complete
- **SLURM Job ID**: [from sbatch output]
- **Results Summary**: "360/360 runs completed successfully"

### 2. Run Local Analysis

```bash
# From local machine
cd "C:/Users/new user/Documents/GitHub/packages/riemtan-dev/experiments/EXP-RT-001"

# Run Phase 5 analysis
Rscript analyze_results.R

# This generates:
# - artifacts/analysis_summary.txt
# - artifacts/plots/heatmap_computation_time.pdf
# - artifacts/plots/lines_batch_effect.pdf
# - artifacts/plots/speedup_curves.pdf
```

### 3. Review Results

Check plots and analysis summary to understand:
- Effect of batch size on computation time
- Effect of core count on speedup
- Optimal batch_size recommendations

### 4. Create LOG Entry

Document completion in Notion daily log:
- Title: "EXP-RT-001 Complete - Batching Effect Quantified"
- Include key findings from analysis
- Reference Notion experiment page and GitHub issue

---

## Quick Reference

### SSH Connection
```bash
ssh nescoba@bigred200.uits.iu.edu
```

### Check Job Status
```bash
squeue -u nescoba
```

### View Recent Jobs
```bash
sacct -u nescoba --starttime=today
```

### Cancel Job (if needed)
```bash
scancel <JOB_ID>
```

### Copy Files TO Cluster
```bash
scp <local_file> nescoba@bigred200.uits.iu.edu:<remote_path>
```

### Copy Files FROM Cluster
```bash
scp nescoba@bigred200.uits.iu.edu:<remote_path> <local_path>
```

---

## Notes

1. **Passphrase Required**: Every SSH command requires passphrase entry
2. **Job Emails**: You'll receive emails when jobs complete (or fail)
3. **SLURM Output**: Real-time job output in logs/slurm_*.out
4. **Results Safety**: Results saved to both /N/slate (permanent) and job directory
5. **Reproducibility**: All scripts use set.seed(20251024) for reproducibility

---

## Support

If you encounter issues:
1. Check SLURM error logs first
2. Report to Toby Esterhase with:
   - SLURM Job ID
   - Error messages from logs
   - Stage where failure occurred
3. Agent will investigate and provide fixes

---

**Agent**: Toby Esterhase (R Package Development)
**Supervisor**: Bill Haydon (Statistics Research)
**Documentation**: Following LOG-280 SSH workflow pattern
