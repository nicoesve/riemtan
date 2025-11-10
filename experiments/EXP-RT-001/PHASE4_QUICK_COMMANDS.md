# Phase 4 Quick Command Reference
**EXP-RT-001 Minimal Parallelization Study (p=100)**

Copy-paste these commands to run the Phase 4 experiment.

---

## Experiment Overview

**Design:** Minimal targeted study on parallelization effect
- **Fixed:** n=500, p=100, batch_size=128
- **Vary:** cores = [1, 4, 16, 32]
- **Reps:** 5 per core count
- **Total:** 20 runs
- **Expected time:** ~3.2 hours

**Question:** Does parallelization help for p=100 matrices?

---

## Step 1: Upload Scripts to Cluster (2 min)

Open **Git Bash** or **WSL**:

```bash
cd "C:/Users/new user/Documents/GitHub/packages/riemtan-dev/experiments/EXP-RT-001"

# Copy Phase 4 R script
scp script_phase4_minimal.R nescoba@bigred200.uits.iu.edu:~/riemtan-dev/experiments/EXP-RT-001/
```
*[Enter passphrase]*

```bash
# Copy Phase 4 SLURM script
scp run_phase4_minimal.slurm nescoba@bigred200.uits.iu.edu:~/riemtan-dev/experiments/EXP-RT-001/
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
sbatch run_phase4_minimal.slurm
```

**IMPORTANT**: Note the Job ID from output (e.g., "Submitted batch job 1234567")

**Job ID: _____________** ← Write it down!

---

## Step 3: Monitor Job (~3.2 hours)

While still connected to cluster:

```bash
# Check if job is running
squeue -u nescoba

# Watch live output (Ctrl+C to exit)
tail -f /N/slate/nescoba/experiments/EXP-RT-001/logs/slurm_*.out

# Or check periodically
watch -n 60 'squeue -u nescoba'
```

**Expected runtime:** ~3.2 hours (based on calibration)
- cores=1: ~10 min each × 5 reps = 50 min
- cores=4: ~10 min each × 5 reps = 50 min
- cores=16: ~10 min each × 5 reps = 50 min
- cores=32: ~10 min each × 5 reps = 50 min (if it helps!)

Job is complete when `squeue -u nescoba` shows no output.

```bash
exit  # Logout from cluster
```

---

## Step 4: Retrieve Results (1 min)

From your local machine (Git Bash/WSL):

```bash
cd "C:/Users/new user/Documents/GitHub/packages/riemtan-dev/experiments/EXP-RT-001"

# Retrieve Phase 4 results
scp nescoba@bigred200.uits.iu.edu:/N/slate/nescoba/experiments/EXP-RT-001/artifacts/phase4_minimal_results.csv ./artifacts/
```
*[Enter passphrase]*

```bash
# Verify retrieval
wc -l artifacts/phase4_minimal_results.csv
# Should show: 21 (20 data rows + 1 header)
```

---

## Step 5: Quick Analysis

Check the speedup:

```bash
cat artifacts/phase4_minimal_results.csv
```

**Look for:**
- Does time decrease with more cores?
- Compare cores=1 vs cores=16 vs cores=32
- Is there a speedup pattern?

---

## Step 6: Report to Agent

Report these to Toby Esterhase:

1. **SLURM Job ID**: _____________
2. **Status**: SUCCESS or FAILED
3. **All 20 runs completed**: YES or NO
4. **Speedup observed**:
   - 1 core: _____ sec (baseline)
   - 4 cores: _____ sec (___× speedup)
   - 16 cores: _____ sec (___× speedup)
   - 32 cores: _____ sec (___× speedup)
5. **Does parallelization help for p=100?**: YES or NO

Example: "Job 5846700 completed. All 20 runs successful. Speedup: 1 core (580s) → 16 cores (145s) = 4× speedup. YES, parallelization helps!"

---

## Troubleshooting

**If job fails:**
```bash
ssh nescoba@bigred200.uits.iu.edu
cat /N/slate/nescoba/experiments/EXP-RT-001/logs/slurm_*.err
```

**If some runs complete but not all:**
- Check which core counts completed
- Partial results still valuable
- May need to rerun missing conditions

**Check job history:**
```bash
ssh nescoba@bigred200.uits.iu.edu
sacct -u nescoba --starttime=today
```

---

## Expected Results

### Scenario A: Parallelization Helps (HOPE!)

**Pattern:**
- 1 core: ~580 sec
- 4 cores: ~290 sec (2× speedup)
- 16 cores: ~145 sec (4× speedup)
- 32 cores: ~100 sec (5-6× speedup)

**Interpretation:**
- p=100 is large enough for parallel benefit
- Threshold between p=10 and p=100
- Provides guidance for users

### Scenario B: Parallelization Still Doesn't Help

**Pattern:**
- 1 core: ~580 sec
- 4 cores: ~580 sec (no speedup)
- 16 cores: ~580 sec (no speedup)
- 32 cores: ~580 sec (no speedup)

**Interpretation:**
- Algorithm doesn't parallelize well
- Or need even larger p (p > 100)
- Or implementation issue in riemtan

### Scenario C: Partial Benefit

**Pattern:**
- 1 core: ~580 sec
- 4 cores: ~450 sec (1.3× speedup)
- 16 cores: ~400 sec (1.5× speedup)
- 32 cores: ~400 sec (no additional benefit)

**Interpretation:**
- Some benefit but diminishing returns
- Optimal core count identified
- Practical guidance for users

---

## What This Experiment Answers

### Primary Question
**Does parallelization help for p=100 matrices?**
- Phase 3 (p=10): NO
- Phase 4 (p=100): ???

### Secondary Questions
- At what core count does benefit plateau?
- Is speedup linear or sub-linear?
- What's the efficiency (speedup / cores)?
- Should riemtan recommend different cores for different p?

### Practical Impact
- Guidance for package users
- Default parameter recommendations
- Resource allocation advice
- When to use cluster vs laptop

---

**Total Time Estimate**: ~3.5-4 hours
- Active time: ~10 minutes
- Waiting time: ~3.2 hours (job runtime)
- Analysis time: ~15 minutes

**Good luck!** 🚀

**This is the final phase of the experiment - we'll finally know if/when parallelization helps!**
