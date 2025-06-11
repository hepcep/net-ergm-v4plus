# net-ergm-v4plus

## 🛠 Launching R in VS Code (Remotely on Oscar, @khanna7)

To work with this project in a **VS Code Remote SSH session**:

1. Open the workspace file:  
   `File → Open Workspace from File…` → `net-ergm-v4plus.code-workspace`

2. From the Command Palette (`Ctrl+Shift+P`), select:  
   **"Create R Terminal (new session)"**  
   > If the default "Create R Terminal" option does not work, try the one below it. This is about getting the right R loaded from the `renv`. 

3. Confirm the correct R environment is active:
   ```r
   .libPaths()
   renv::status()
   ```

**Note**: 
The VS Code workspace is configured to use `launch_R.sh` to start R with the correct environment:

- Loads the required R module on Oscar (`R/4.4.0`)
- Ensures `renv` activates correctly
- Avoids issues with missing libraries or misconfigured R paths

You don't need to run this script manually — it's automatically used when launching R via **"Create R Terminal (new session)"**.

To modify the setup (e.g., use a different R version or load additional modules), edit the `launch_R.sh` script at the project root.

Make sure it remains an executable:
```
chmod +x launch_R.sh
```




## 🧩 Modeling Steps 

#### Data Processing

Update the degree distributions, mixing parameters, distance metrics in:

```
fit-ergms/process-data.R
```

This produces a data output file in:

```
fit-ergms/out/processed_data.rds
```

Submission script to run the data processing code on a compute node is at
`fit-ergms/process-data-jobscript.sh`.

#### ERGM Fitting

Then run the stepwise ERGM fitting code from:

```
fit-ergms/fit-stepwise-ergms.R
```

- Remember to update the `run_label` [near the top](https://github.com/hepcep/net-ergm-v4plus/blob/60bb27461392556ac56834b7dedf82961e236edc/fit-ergms/fit-stepwise-ergms.R#L9).

- Code follows a checkpointing structure to enable increasingly complex ERGMs.

- The fitted output is stored via:

```
save.image(file = file.path(out_dir, paste0(run_label, ".RData")))
```

Submission script to run the ERGM fitting code on a compute node is at
`fit-ergms/jobscript.sh`.

#### Simulation

Network simulation file at:

```
simulate-from-ergms/simulate-networks-from-meta-data-ergm-fit.R
```

Check the [`run_label`](https://github.com/hepcep/net-ergm-v4plus/blob/60bb27461392556ac56834b7dedf82961e236edc/simulate-from-ergms/simulate-networks-from-meta-data-ergm-fit.R#L38C1-L40) 
to ensure it matches the correct output from the fitting process. 

By default, the simulation code 
is written to simulate 100 networks, 
and save 2 `qs` objects, one comprising 10 networks, and the other 100 networks. 

Submission script at `simulate-from-ergms/sim-jobscript.sh`.

#### Statistical/Visual Summary of Simulated Networks relative to Mean Target Parameters

Then check the summaries across the simulated networks via:
```
simulate-from-ergms/summaries-across-simulated-distributions.R
```

Again, confirm that the [`run_label`](https://github.com/hepcep/net-ergm-v4plus/blob/60bb27461392556ac56834b7dedf82961e236edc/simulate-from-ergms/summaries-across-simulated-distributions.R#L39-L41) is consistent.

This file produces numerical summaries and violin plots for the 
mean target parameter values relative to the distribution of the simulated outcomes.

Violin plots saved via: 
```
    ggsave(here("simulate-from-ergms", "out", "*.png"), width = 8, height = 6)
```

#### Extract Networks for HepCep4Py

Output extraction to HepCep4Py at:

```
simulate-from-ergms/extract-vertex-atts.R
simulate-from-ergms/unpack-edgelist.R
simulate-from-ergms/unpack-vertex-atts.R
```

#### Uncertainty in Target Parameters 
Compute uncertainty in input parameters:

```
targets-uncertainty-computation/process-uncertainty-inputs.R
```

I didn't plot the uncertainty in the inputs because almost 
all targets were included in the simulation 'violins'.


## Network Visualization
Protoptying:

```
viz-simulated-nets/compute-layout.R
viz-simulated-nets/convert-to-json-lite.R
```

Built off of the packages used in 

> Clipman SJ, Mehta SH, Mohapatra S, et al. Deep learning and social network analysis elucidate drivers of HIV transmission in a high-incidence cohort of people who inject drugs. Sci Adv. 2022;8(42):eabf0158.


