# Bundle builder (copy)

A copy of `bundle-builder/` from the HealthScope Mobile project, used by
`.github/workflows/refresh-climate-data.yml` to rebuild the app's Tanzania data
from the climate API every week. Do not edit it here: change the builder in the
project, run its tests, and copy it again with
`bundle-builder/ci/install_to_data_repo.ps1`.