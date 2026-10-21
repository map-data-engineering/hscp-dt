<#
.SYNOPSIS
  Publishes a bundle build (out\publish) as GitHub Releases, which serve the app's data.

.DESCRIPTION
  Needs a build made with --github-repo OWNER/REPO. Each folder in out\publish is one release:
    tanzania-<bundle_version>\        data bundle + summaries   (tag tanzania-<bundle_version>)
    tanzania-basemap-<sha12>\         offline map, only when built with --basemap
  The app reads https://github.com/OWNER/REPO/releases/latest/download/manifest.json, so
  manifest.json is attached to the newest data release, and that release is created last
  and marked "Latest". Basemap and older releases are created with --latest=false.
  `gh release create` uploads the files to a draft and publishes it only when every upload
  has finished, so phones never see a manifest that points at files not yet uploaded.
  Releases that already exist are left alone (files are immutable); missing files are added.

  Needs: the GitHub CLI (`winget install GitHub.cli`), then `gh auth login` once, with write
  access to the repository.

.EXAMPLE
  .\publish_github.ps1 -Repo map-data-engineering/hscp-dt -DryRun
  .\publish_github.ps1 -Repo map-data-engineering/hscp-dt
#>
param(
  [Parameter(Mandatory = $true)] [string] $Repo,
  [string] $PublishDir,
  [switch] $DryRun
)

$ErrorActionPreference = 'Stop'
# Resolved here, not as a param default: Windows PowerShell 5.1 leaves $PSScriptRoot empty there under -File.
$here = if ($PSScriptRoot) { $PSScriptRoot } else { Split-Path -Parent $MyInvocation.MyCommand.Path }
if (-not $PublishDir) { $PublishDir = Join-Path $here "out\publish" }
$manifestPath = Join-Path $PublishDir "manifest.json"
if (-not (Test-Path $manifestPath)) { throw "No manifest.json in $PublishDir. Build a bundle first." }
$manifest = Get-Content $manifestPath -Raw | ConvertFrom-Json
if (-not $DryRun -and -not (Get-Command gh -ErrorAction SilentlyContinue)) {
  throw "The GitHub CLI (gh) is not installed. Run: winget install GitHub.cli   then: gh auth login"
}

$prefix = "https://github.com/$Repo/releases/download/"
$releases = [ordered]@{}   # tag -> @{ Files; Title; Notes }
$latestTag = $null; $latestVersion = [long]0

# PowerShell 7's ConvertFrom-Json turns ISO date strings into DateTime values
# (5.1 keeps the text), so dates are written back explicitly.
function DateText($value, [string] $format) {
  if ($value -is [datetime]) { return $value.ToUniversalTime().ToString($format) }
  return [string]$value
}

function Get-ReleaseAssets([string] $tag) {
  # "release not found" on stderr is the normal answer for a new release; under
  # 'Stop', Windows PowerShell 5.1 would turn it into a terminating error.
  $ErrorActionPreference = 'Continue'
  $json = & gh release view $tag --repo $Repo --json assets 2>$null
  if ($LASTEXITCODE -ne 0) { return $null }   # release doesn't exist
  return @(($json | ConvertFrom-Json).assets | ForEach-Object { $_.name })
}

foreach ($c in $manifest.countries) {
  foreach ($name in @('bundle', 'summaries', 'basemap')) {
    $f = $c.files.$name
    if ($null -eq $f) { continue }
    if (-not $f.url.StartsWith($prefix)) {
      throw "$($c.country_iso3) $name URL '$($f.url)' is not a release of $Repo. Rebuild with --github-repo $Repo (and --basemap for the map)."
    }
    $tag, $file = $f.url.Substring($prefix.Length).Split('/')
    $local = Join-Path (Join-Path $PublishDir $tag) $file
    if (-not (Test-Path $local)) {
      # Not part of this build (the offline map, kept from an earlier one, as in
      # the scheduled climate refresh): fine when its release already has it.
      if ($DryRun) { Write-Host "$tag/$file is not in this build; it must already be published."; continue }
      $assets = Get-ReleaseAssets $tag
      if ($null -ne $assets -and $assets -contains $file) { Write-Host "$tag/$file already published"; continue }
      throw "Missing $local (referenced by the manifest, and not published yet)."
    }
    if ((Get-Item $local).Length -ne $f.bytes) { throw "$local has a different size than the manifest says." }
    if (-not $releases.Contains($tag)) {
      $country = (Get-Culture).TextInfo.ToTitleCase($c.country_slug)
      if ($name -eq 'basemap') {
        $builtOn = DateText $f.built_on 'yyyy-MM-dd'
        $title = "$country offline map ($builtOn)"
        $notes = "Offline basemap for the HealthScope app: max zoom $($f.maxzoom), OpenStreetMap data of $builtOn. " +
                 "Map data (c) OpenStreetMap contributors, ODbL; tiles built with Protomaps."
      } else {
        $title = "$country data $($c.bundle_version)"
        $notes = "HealthScope Mobile data bundle for $country, version $($c.bundle_version). " +
                 "ETL source date $(DateText $c.etl_source_date 'yyyy-MM-dd'), ETL commit $($c.etl_commit), " +
                 "built $(DateText $c.built_at "yyyy-MM-dd'T'HH:mm:ss'Z'")."
      }
      $releases[$tag] = @{ Files = @(); Title = $title; Notes = $notes }
    }
    $releases[$tag].Files += $local
    # bundle_version (YYYYMMDDNN) only grows, so the highest is the newest bundle.
    if ($name -eq 'bundle' -and [long]$c.bundle_version -gt $latestVersion) { $latestVersion = [long]$c.bundle_version; $latestTag = $tag }
  }
}
if ($null -eq $latestTag) { throw "The manifest lists no data bundles." }

if (-not $DryRun) {
  # A release tag must point at a commit, so an empty repository can't hold releases.
  $ErrorActionPreference = 'Continue'
  $heads = & git ls-remote --heads "https://github.com/$Repo.git" 2>$null
  $ErrorActionPreference = 'Stop'
  if (-not $heads) {
    throw "$Repo has no commits yet, and GitHub releases need one. On github.com open the repository, choose 'Add a README' (or 'creating a new file'), commit it, then run this script again."
  }
}

# Newest data release last, so it's only created (and becomes Latest) after the others exist.
$order = @($releases.Keys | Where-Object { $_ -ne $latestTag }) + $latestTag
foreach ($tag in $order) {
  $r = $releases[$tag]
  $isLatest = $tag -eq $latestTag
  $files = @($r.Files)
  if ($isLatest) { $files += $manifestPath }
  $mb = [math]::Round((($files | ForEach-Object { (Get-Item $_).Length }) | Measure-Object -Sum).Sum / 1MB, 2)
  Write-Host ("{0}{1}  ({2} files, {3} MB)" -f $tag, $(if ($isLatest) { '  [Latest, with manifest.json]' } else { '' }), $files.Count, $mb)
  if ($DryRun) { $files | ForEach-Object { Write-Host "    $(Split-Path $_ -Leaf)" }; continue }

  $existing = Get-ReleaseAssets $tag
  if ($null -eq $existing) {
    & gh release create $tag @files --repo $Repo --title $r.Title --notes $r.Notes "--latest=$($isLatest.ToString().ToLower())"
    if ($LASTEXITCODE -ne 0) { throw "Creating release $tag failed. Nothing after it was published." }
    continue
  }
  $missing = @($r.Files | Where-Object { $existing -notcontains (Split-Path $_ -Leaf) })
  if ($missing.Count) {
    & gh release upload $tag @missing --repo $Repo
    if ($LASTEXITCODE -ne 0) { throw "Uploading to release $tag failed." }
  }
  if ($isLatest) {
    & gh release upload $tag $manifestPath --repo $Repo --clobber
    if ($LASTEXITCODE -ne 0) { throw "Uploading manifest.json to $tag failed." }
    & gh release edit $tag --repo $Repo --latest
    if ($LASTEXITCODE -ne 0) { throw "Marking $tag as Latest failed." }
  } else {
    Write-Host "    already published"
  }
}

if ($DryRun) { Write-Host "Dry run: nothing uploaded." -ForegroundColor Yellow }
else {
  Write-Host "Published. App manifest URL: https://github.com/$Repo/releases/latest/download/manifest.json" -ForegroundColor Green
  Write-Host "Phones pick it up at their next update check (within 12 hours)." -ForegroundColor Green
}
