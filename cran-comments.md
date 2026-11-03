## Resubmission of archived package

This release addresses the historical citation issues by replacing
old-style citation syntax and the T.B.C. placeholder with complete
published references.

The repmis dependency has been removed. On-demand model downloads
are retained using utils::download.file(), with models cached for
the current R session. Users can alternatively install pollimetrydata
(>= 1.0.1) for offline predictions. Public installation and the
download fallback have been tested.

The archived check results also list an additional OpenBLAS issue.
The linked diagnostic report now returns HTTP 404, so we have not
been able to identify or reproduce that specific historical failure.
We do not claim that it has been resolved and would welcome details
if it persists in the current submission.

## Current checks

- macOS Sequoia 15.7.9, R 4.6.1: 0 errors, 0 warnings, 0 notes.
- Windows Server 2022, R 4.6.2: 0 errors, 0 warnings, 1 NOTE.
- Windows Server 2022, R-devel (2026-11-02 r90827):
  0 errors, 0 warnings, 1 NOTE.

The Windows incoming-check NOTE identifies the previous archival,
the optional GitHub-hosted pollimetrydata package and spelling flags
(et al., bodysize and hoverflies). The spelling is intentional.

Both Windows checks completed without pollimetrydata installed;
Bayesian examples are conditional on its availability.