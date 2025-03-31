# net-ergm-v4plus

### 🛠 Launching R in VS Code (Remotely on Oscar, @khanna7)

To work with this project in a **VS Code Remote SSH session**:

1. Open the workspace file:  
   `File → Open Workspace from File…` → `net-ergm-v4plus.code-workspace`

2. From the Command Palette (`Ctrl+Shift+P`), select:  
   **"Create R Terminal (new session)"**  
   > _Do **not** use the default "Create R Terminal" option — it may fail._

3. Confirm the correct R environment is active:
   ```r
   .libPaths()
   renv::status()
   ```


