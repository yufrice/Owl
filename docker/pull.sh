#!/bin/bash

repo = Aripiprazole/Owl
url = https://gitlab.com/$repo/-/jobs/artifacts/master/raw/Owl.keter
header = "private-Token: {$GitLab_Token}"
curl -O --header $header $url
chmod a+x Owl.keter