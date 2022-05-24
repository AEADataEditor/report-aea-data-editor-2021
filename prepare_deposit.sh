#!/bin/bash

outfile=deposit-repository-$(date +%F).zip

zip -rp $outfile README.* tables/ programs/ images/
zip -rp $outfile data/jira/anon  data/*txt data/icpsr data/manual data/scholarone
zip -rp $outfile $(find data -name README.\*)
