# Repository Guidelines

## Project Structure & Module Organization
Erlang sources live in `src/`, with shared headers in `include/`. Native C++ NIF code and build scripts sit in `c_src/`, and compiled artifacts land under `_build/`. Common Test suites and configs reside in `test/` (for example `integrity_test_SUITE.erl`), while Cassandra benchmarks and configs are in `benchmarks/`. Runtime assets such as shared libraries are staged in `priv/`. Keep generated logs in `log/` out of version control.

## Build, Test, and Development Commands
Use `make compile` (default) or `rebar3 compile` to build Erlang modules; both invoke the `rebar3` pipeline and will regenerate `c_src/env.mk` automatically. Run native driver builds with `make nif_compile` and clean them with `make nif_clean`. Execute the Common Test suite via `make ct`, which wraps `ct_run` with the correct `_build` paths and `test/sys.config`. For high-load validation, run `make setup_benchmark` once, then `make benchmark MODULE=erlcass PROCS=100 REQ=100000`.

## Coding Style & Naming Conventions
Follow OTP-style indentation (four spaces) and prefer snake_case for modules, functions, and variables (`erlcass_session`). Export only what is needed; `rebar.config` enforces `warnings_as_errors` and flags full exports, so clear Dialyzer and compiler warnings before pushing. Macro names stay upper snake case (`?CASS_LOG_WARN`). For C++ code in `c_src/`, keep includes sorted and run `make cpplint` or `make cppcheck` when touching native sources.

## Testing Guidelines
Add or update Common Test suites named `*_SUITE.erl`, grouping related cases with descriptive test case names such as `reconnect_after_network_drop/1`. Place shared fixtures in `test/sys.config` or helper modules in `src/`. Run `rebar3 as test ct` locally before opening a PR; ensure new behaviour is covered and that `log/` remains clean after a run.

## Native Driver & Security Notes
`build_deps.sh` fetches the DataStax C++ driver revision pinned in the Makefile; verify checksums when bumping `CPP_DRIVER_REV`. Avoid committing credentials—use environment variables or `test/sys.config` overrides when sharing cluster settings. For production builds, confirm OpenSSL paths in `cppcheck` arguments align with your target hosts.
