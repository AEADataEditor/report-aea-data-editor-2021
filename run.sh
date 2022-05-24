#!/bin/bash
baseURL=http://localhost:8787
openURL=${baseURL}

docker run -e PASSWORD=testing -v $HOME/Workspace/git:/home/rstudio --rm -p 8787:8787 rocker/verse
