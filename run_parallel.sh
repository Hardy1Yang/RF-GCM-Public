#!/bin/bash
# run_parallel.sh
# Launches main.R in parallel batches, balanced by dataset size.
# Skips already-completed runs (prediction.rds exists).
# R3C4 MCMC comparison: controlled by config.R RUN_MCMC_COMPARISON flag.
#
# Dataset index (from description.csv):
#  1 breast_cancer(683)   7 fertility(100)    13 satimage(6435)
#  2 heart_failure(299)   8 wholesale(440)    14 letter_A(20000)
#  3 hepatitis(155)       9 abalone19(4177)   15 ozone_level(1847)
#  4 thyroid_diff(383)   10 page_blocks(5473) 16 arrhythmia(420)
#  5 oil_spill(937)      11 mammography(11183)17 wine_quality(6497)
#  6 yeast5(1484)        12 sick(1947)

cd "$(dirname "$0")"
mkdir -p logs

echo "Starting parallel batches at $(date)"
echo ""

# Large datasets: 1 each (bottleneck)
Rscript main.R 14 14 > logs/letter_A.log 2>&1 &
Rscript main.R 11 11 > logs/mammography.log 2>&1 &
Rscript main.R 13 13 > logs/satimage.log 2>&1 &
Rscript main.R 17 17 > logs/wine_quality.log 2>&1 &
Rscript main.R 10 10 > logs/page_blocks.log 2>&1 &
Rscript main.R 9  9  > logs/abalone19.log 2>&1 &

# Medium + small datasets grouped
Rscript main.R 5 6   > logs/medium1.log 2>&1 &   # oil_spill + yeast5
Rscript main.R 12 12 > logs/sick.log 2>&1 &       # sick
Rscript main.R 15 16 > logs/medium2.log 2>&1 &    # ozone_level + arrhythmia
Rscript main.R 1 4   > logs/small1.log 2>&1 &     # breast_cancer..thyroid_diff
Rscript main.R 7 8   > logs/small2.log 2>&1 &     # fertility + wholesale

echo "Launched 11 parallel R processes"
echo "Monitor: ls -lt results/*/seed=219/ntree=500/prediction.rds"
echo ""

wait
echo "All batches complete at $(date)"
echo "Now run: Rscript get_performance_summary.R"
