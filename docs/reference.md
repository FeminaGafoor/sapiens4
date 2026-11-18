# Usage

Choose a corpora in the header. **Pause** and **Activate** control its execution.
Chat with ChiefSapi to delegate work, manage specialists or schedule routines.
**Stop** cannot undo completed external actions; inspect results before retrying.

## Models

Open **Workspace & settings → Models** for defaults and per-Sapi choices.
Use **AI Connections** to sign in to an installed provider CLI.
Provider definitions are in `corpora/providers.json`.

| Variable | Purpose |
| --- | --- |
| `SAPIENS_DATA_DIR` | Data folder |
| `SAPIENS_CONFIG_DIR` | Configuration folder |
| `SAPIENS_<ID>_BINARY` | Provider executable, such as `SAPIENS_CODEX_BINARY` |
| `SAPIENS_BLINDLY_BINARY` | Computer-control executable |

## Data

Choose an empty folder for new data or a complete Data Root to open saved data.
Defaults: `~/Library/Application Support/Sapiens4/data` on macOS,
`%LOCALAPPDATA%\Sapiens4\data` on Windows, `.sapiens4` for source CLI runs.

Stop the host before backing up the **whole data folder**. Keep backups private.
Never run two hosts against the same folder.
Notes and files live in `workspaces/<id>/`; link media from `Notes.html` rather
than embedding it. Closing a workspace tab does not delete its file.

## Integrations

MCP: `.build/bin/sapiens4 mcp --url http://127.0.0.1:4174`.
HTTP: `/api/health`, `/api/state`, `/api/model-settings`, `/api/providers`.
Local writes require JSON and `X-Sapiens-Local: 1`.

[Remote control](remote.md) · [Tests](testing.md)
