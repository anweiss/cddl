param(
  [string]$token,
  [string]$tag,
  # [string]$name,
  # [string]$descr,
  [string]$user,
  [string]$project,
  [string]$file
)

$auth = @{"Authorization" = "token $token" }
$files = $file.Split("|")

function UploadAsset([int]$rel_id, [string]$fullpath) {
  $type_exe = "application/x-msdownload"

  $fname = Split-Path $fullpath -Leaf

  if ([System.IO.Path]::GetExtension($fname) -eq ".exe") {
    $content_type = $type_exe
  }

  $rel_arg = $auth + @{"Content-Type" = $content_type; "name" = $fname; }
  # $rel_arg_js = ConvertTo-Json $rel_arg

  Write-Host "Loading contents of '$fullpath'"
  $body = [System.IO.File]::ReadAllBytes($fullpath)

  Write-Host "  -Uri 'https://uploads.github.com/repos/$user/$project/releases/$rel_id/assets?name=$fname'"
  Write-Host -NoNewLine "  -Headers "
  $rel_arg

  Write-Host "  Uploading to github"
  $rel = Invoke-WebRequest -Headers $rel_arg -Method POST -Body $body -Uri https://uploads.github.com/repos/$user/$project/releases/$rel_id/assets?name=$fname

  Write-Host "  Upload finished, checking result"
  if (($null -eq $rel) -Or ($rel.StatusCode -ne 201)) {
    $rel
    $host.SetShouldExit(101)
    exit
  }

  $rel_js = ConvertFrom-Json $rel.Content
  # $rel_js
  Write-Host ("  Upload is ready for download: " + $rel_js.browser_download_url)
}

function FindAsset([int]$rel_id, [string]$fullpath) {
  $fname = Split-Path $fullpath -Leaf
  $a_id = 0
  $rel = Invoke-WebRequest -Headers $auth -Uri https://api.github.com/repos/$user/$project/releases/$rel_id/assets
  if ($null -eq $rel) {
    return 0
  }
  $rel_js = ConvertFrom-Json $rel.Content
  $rel_js | ? { $_.name -eq $fname } | % {
    $a_id = $_.id
    Write-Host "Asset $fname was already uploaded, id: $a_id"
    Write-Host ("  Upload is ready for download: " + $_.browser_download_url)
  }
  return $a_id
}

function FindRelease([string]$tag_name) {
  $rel_id = 0
  $rel = Invoke-WebRequest -Headers $auth -Uri https://api.github.com/repos/$user/$project/releases
  if ($null -eq $rel) {
    return 0
  }
  $rel_js = ConvertFrom-Json $rel.Content
  $rel_js | ? { $_.tag_name -eq $tag_name } | % {
    $rel_id = $_.id
    Write-Host ("Release found, upload_url=" + $_.upload_url)
  }
  return $rel_id
}

# function CreateRelease([string]$tag,[string]$name,[string]$descr)
# {
#   #  "target_commitish"="60b20ba"; `
#   #  "target_commitish"="master"; `
#   #  "target_commitish"="daily"; `

#   #$tag_name = ("v"+$build.Substring(0,2)+"."+$build.Substring(2,2)+"."+$build.Substring(4))
#   #$tag_name = $tag

#   #Write-Host "$tag $name $descr"

#   $rel_arg = @{ `
#     "tag_name"=$tag; `
#     "name"=$name; `
#     "body"=$descr; `
#     "draft"=$FALSE; `
#     "prerelease"=$FALSE
#   }

#   $rel_arg_js = ConvertTo-Json $rel_arg
#   #$rel_arg_js

#   $rel = Invoke-WebRequest -Headers $auth -Method POST -Body $rel_arg_js -Uri https://api.github.com/repos/$user/$project/releases
#   if ($null -ne $rel)
#   {
#     $rel_js = ConvertFrom-Json $rel.Content
#     return $rel_js.id
#   }

#   $host.SetShouldExit(101)
#   exit
# }

Write-Host "Trying to find release $tag"
$rel_id = FindRelease $tag
if ($rel_id -eq 0) {
  Write-Host "Release for tag $tag does not exist"
  exit
}

$files | % {
  $a_id = FindAsset $rel_id $_
  if ($a_id -ne 0) {
    Write-Host "Asset $_ was already uploaded, id: $a_id"
  }
  else {
    UploadAsset $rel_id $_
  }
}