#!/bin/sh

repo='Aripiprazole%2FOwl'
url="https://gitlab.com/api/v4/projects/${repo}/jobs/artifacts/${ReleaseVer}/download?job=deploy"
header="Private-Token: ${GitLab_Token}"

eval "curl -L -o artifacts.zip -H "'"$header"'" $url"
unzip artifacts.zip
mv Owl.keter /opt/keter/incoming