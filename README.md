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



#### Uncertainty Analysis

#### Plotting