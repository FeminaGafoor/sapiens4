# Append4 storage

Sapiens embeds the Haskell library in `submodules/append4`. There is one host
clock at 120 BPM (500 ms), shared by local Corpora, flushes, execution,
transcription, filesystem observation and telemetry scheduling. Append4 has a
writer worker, but no independent clock or service. Network timeouts and retry
backoff are transport concerns, not additional application pulses.

A Corpora owns `corpora.append4`. API operations and execution transitions stage
KV changes within the existing service guard. Exceptions discard staged changes.
The writer syncs before publishing or returning success; provider workers launch
through post-commit callbacks. A paused Corpora still commits administrative
changes; it does not admit scheduled work.

The same store holds agents, state, turns, messages, groups, routines, attachments,
preferences, settings, directory relationships, audit records, remote receipts,
pairing credentials and the local/remote Corpora registry. `local/` keys are
device-bound: a future replication protocol must explicitly exclude/rebind them.
The standalone relay also uses Append4 (`relay.append4`) and its own process's
single 120 BPM clock. SQLite and its schema/SQL adapters have been removed.

Bootstrap files select and identify the data directory. Installer transaction
state, consent/configuration outside the data directory, user workspace files,
media, and generated provider manifests remain files; they are not parallel
Corpora state stores. Update backup discovers registered children from Append4
while writers are stopped and copies the entire data tree.

This change uses fresh temporary fixtures. Existing SQLite/JSON Corpora data are
rejected without conversion. Use a separate empty data directory. No migration
runner or dual-write bridge is included.

`NoChange` rejects an exact logical KV state round-trip inside one transaction.
Separate reversing transactions are retained, including within one flush.
Snapshots are immutable; history reads identify the end of an append. No complex
netting, replication, failover, checkpointing or compaction is implemented here.
All historical data remain resident; relay expiry removes delivery eligibility,
not bytes from journal history. External action outcomes still require review
when an interruption leaves them uncertain; remote retries never blindly replay
an incomplete receipt.

Tests assert public behavior with temporary directories, fixture providers and
fixture-owned remote credentials. They no longer repair internal JSON files or
read audit/credential persistence formats. Low-level Append4 format and durability
checks live with Append4 and exercise the Haskell implementation from Lean.
