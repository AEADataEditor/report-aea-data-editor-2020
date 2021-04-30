#!/bin/bash

outfile=deposit-repository-$(date +%F).zip

zip -rp $outfile README* tables/ programs/ images/
zip -rp $outfile data/jira/anon  data/*txt data/scholarone
zip -rp $outfile $(find data -name README\*)
