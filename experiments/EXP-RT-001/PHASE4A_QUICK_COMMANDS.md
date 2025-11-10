# Phase 4a Quick Command Reference
**EXP-RT-001 Calibration Test (p=100)**

Copy-paste these commands to run the calibration test.

---

## Step 1: Upload Scripts to Cluster (2 min)

Open **Git Bash** or **WSL**:

```bash
cd "C:/Users/new user/Documents/GitHub/packages/riemtan-dev/experiments/EXP-RT-001"

# Copy Phase 4a R script
scp script_calibration_p100.R nescoba@bigred200.uits.iu.edu:~/riemtan-dev/experiments/EXP-RT-001/
```
*[Enter passphrase]*

```bash
# Copy Phase 4a SLURM script
scp run_calibration_p100.slurm nescoba@bigred200.uits.iu.edu:~/riemtan-dev/experiments/EXP-RT-001/
```
*[Enter passphrase]*

---

## Step 2: Submit SLURM Job (1 min)

```bash
ssh nescoba@bigred200.uits.iu.edu
```
*[Enter passphrase]*

```bash
cd ~/riemtan-dev/experiments/EXP-RT-001
ls -l  # Verify scripts are there
sbatch run_calibration_p100.slurm
```

**IMPORTANT**: Note the Job ID from output (e.g., "Submitted batch job 1234567")

**Job ID: _____________** ← Write it down!

---

## Step 3: Monitor Job (? minutes - UNKNOWN!)

While still connected to cluster:

```bash
# Check if job is running
squeue -u nescoba

# Watch live output (Ctrl+C to exit)
tail -f /N/slate/nescoba/experiments/EXP-RT-001/logs/slurm_*.out

# Or check periodically
watch -n 30 'squeue -u nescoba'
```

**Expected runtime:** UNKNOWN (calibration run!)
- Best case: ~1-5 minutes
- Expected: ~10-30 minutes
- Worst case: 1-2 hours (will timeout if > 2 hours)

Job is complete when `squeue -u nescoba` shows no output.

```bash
exit  # Logout from cluster
```

---

## Step 4: Retrieve Results (1 min)

From your local machine (Git Bash/WSL):

```bash
cd "C:/Users/new user/Documents/GitHub/packages/riemtan-dev/experiments/EXP-RT-001"

# Retrieve calibration results
scp nescoba@bigred200.uits.iu.edu:/N/slate/nescoba/experiments/EXP-RT-001/artifacts/calibration_p100_results.csv ./artifacts/
```
*[Enter passphrase]*

```bash
# Verify retrieval
cat artifacts/calibration_p100_results.csv
```

---

## Step 5: Analyze Results (1 min)

Check the timing:

```bash
# Quick look at elapsed time
cat artifacts/calibration_p100_results.csv | tail -1 | cut -d',' -f10
```

This will show the `elapsed_seconds` value.

**Decision matrix:**
- **< 60 sec**: Full factorial (360 runs) is feasible! (~6 hours total)
- **60-300 sec**: Reduced factorial recommended (27-50 runs)
- **> 300 sec**: Minimal comparison only (4-20 runs)

---

## Step 6: Report to Agent

Report these to Toby Esterhase:

1. **SLURM Job ID**: _____________
2. **Status**: SUCCESS or FAILED
3. **Elapsed time**: _______ seconds (_______ minutes)
4. **Scaling factor**: (p=100 time) / (p=10 time of ~66 sec) = ___×
5. **Recommendation**: Full / Reduced / Minimal factorial

Example: "Job 5846500 completed. p=100 took 245 seconds (~4 min). Scaling: 3.7×. Recommend reduced factorial."

---

## Troubleshooting

**If job fails:**
```bash
ssh nescoba@bigred200.uits.iu.edu
cat /N/slate/nescoba/experiments/EXP-RT-001/logs/slurm_*.err
```

**If job times out (> 2 hours):**
- p=100 may be too expensive for this approach
- Consider p=50 or p=30 instead
- Or use sparse matrix methods

**Check job history:**
```bash
ssh nescoba@bigred200.uits.iu.edu
sacct -u nescoba --starttime=today
```

---

## What This Test Will Tell Us

1. **Actual computation time** for p=100 (vs theoretical O(p³) estimate)
2. **Whether parallelization helps** at this scale (16 cores vs Phase 3's 4 cores)
3. **Feasibility of full Phase 4** factorial design
4. **Data generation cost** (may be significant for p=100)

---

**Total Time Estimate**: ~1-2 hours (mostly waiting for cluster)
- Active time: ~5 minutes
- Waiting time: Unknown (calibration!)

**Next Step**: Based on timing, design appropriate Phase 4 experiment

Good luck! 🚀
