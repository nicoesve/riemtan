# Phase 3 Quick Command Reference
**EXP-RT-001 Cluster Infrastructure Test**

Copy-paste these commands in sequence. You'll be prompted for your SSH passphrase multiple times.

---

## Step 1: Create Cluster Directories (2 min)

```bash
ssh nescoba@bigred200.uits.iu.edu
```
*[Enter passphrase]*

```bash
mkdir -p ~/riemtan-dev/experiments/EXP-RT-001
mkdir -p /N/slate/nescoba/experiments/EXP-RT-001/{logs,raw_results,artifacts}
ls -la /N/slate/nescoba/experiments/EXP-RT-001/
exit
```

---

## Step 2: Copy Scripts to Cluster (2 min)

Open **Git Bash** or **WSL** on your local Windows machine:

```bash
cd "C:/Users/new user/Documents/GitHub/packages/riemtan-dev/experiments/EXP-RT-001"

# Copy Phase 3 R script
scp script_cluster_test.R nescoba@bigred200.uits.iu.edu:~/riemtan-dev/experiments/EXP-RT-001/
```
*[Enter passphrase]*

```bash
# Copy Phase 3 SLURM script
scp run_cluster_test.slurm nescoba@bigred200.uits.iu.edu:~/riemtan-dev/experiments/EXP-RT-001/
```
*[Enter passphrase]*

---

## Step 3: Submit SLURM Job (1 min)

```bash
ssh nescoba@bigred200.uits.iu.edu
```
*[Enter passphrase]*

```bash
cd ~/riemtan-dev/experiments/EXP-RT-001
ls -l  # Verify scripts are there
sbatch run_cluster_test.slurm
```

**IMPORTANT**: Note the Job ID from output (e.g., "Submitted batch job 1234567")

**Job ID: _____________** ← Write it down!

---

## Step 4: Monitor Job (30-60 min)

While still connected to cluster:

```bash
# Check if job is running
squeue -u nescoba

# Watch live output (Ctrl+C to exit)
tail -f /N/slate/nescoba/experiments/EXP-RT-001/logs/slurm_*.out

# Or check periodically
watch -n 30 'squeue -u nescoba'
```

Job is complete when `squeue -u nescoba` shows no output.

```bash
exit  # Logout from cluster
```

---

## Step 5: Retrieve Results (2 min)

From your local machine (Git Bash/WSL):

```bash
cd "C:/Users/new user/Documents/GitHub/packages/riemtan-dev/experiments/EXP-RT-001"

# Retrieve artifacts
scp -r nescoba@bigred200.uits.iu.edu:/N/slate/nescoba/experiments/EXP-RT-001/artifacts/ ./
```
*[Enter passphrase]*

```bash
# Verify retrieval
ls -l artifacts/cluster_test_results.csv
```

---

## Step 6: Verify Results (1 min)

Check that the results file has 8 rows (+ 1 header):

```bash
wc -l artifacts/cluster_test_results.csv
# Should show: 9 (8 data rows + 1 header)
```

Look at the speedup (compare cores=16 vs cores=4):

```bash
cat artifacts/cluster_test_results.csv
```

---

## Step 7: Report to Agent (1 min)

Report these to Toby Esterhase:

1. **SLURM Job ID**: _____________
2. **Status**: SUCCESS or FAILED
3. **Speedup observed**: (time with 4 cores) / (time with 16 cores) = ___x

Example: "Job 1234567 completed. Speedup from 4→16 cores: ~3.2x"

---

## Troubleshooting

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

**Check job history:**
```bash
ssh nescoba@bigred200.uits.iu.edu
sacct -u nescoba --starttime=today
```

---

## Expected Results

**Successful Phase 3 should show:**
- ✅ All 8 runs completed
- ✅ Faster times with 16 cores vs 4 cores
- ✅ Speedup of 2-4x (may not be linear due to overhead)
- ✅ No errors in SLURM error log

**Next**: If Phase 3 succeeds → Proceed to Phase 4 (full factorial, 360 runs)

---

## Quick SSH Commands Reference

```bash
# Connect
ssh nescoba@bigred200.uits.iu.edu

# Check job status
squeue -u nescoba

# Cancel job (if needed)
scancel <JOB_ID>

# Copy TO cluster
scp <local_file> nescoba@bigred200.uits.iu.edu:<remote_path>

# Copy FROM cluster
scp nescoba@bigred200.uits.iu.edu:<remote_path> <local_path>
```

---

**Total Time Estimate**: ~1-2 hours
- Active time: ~15 minutes
- Waiting time: ~30-60 minutes (job runtime)

**Good luck!** 🚀
