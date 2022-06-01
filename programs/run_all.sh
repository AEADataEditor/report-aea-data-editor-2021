#!/bin/bash
R --vanilla < 01_lab_members.R > 01_lab_members.Rlog 2>&1
R --vanilla < 03_jira_dataprep.R > 03_jira_dataprep.Rlog 2>&1
R --vanilla < 04_table1_compliance.R > 04_table1_compliance.Rlog 2>&1
R --vanilla < 05_table2_stats.R > 05_table2_stats.Rlog 2>&1
R --vanilla < 06_table3_response_options.R > 06_table3_response_options.Rlog 2>&1
R --vanilla < 07_table4.R > 07_table4.Rlog 2>&1
R --vanilla < 08_figure1_filesize.R > 08_figure1_filesize.Rlog 2>&1
R --vanilla < 09_write_nums.R > 09_write_nums.Rlog 2>&1
