Hi Amanda,

Please find some example code in the directory with this note. 

SampleAnalysisCode -> SampleAnalysisCode.Rmd
	This R notebook file should run on your machine with the provided input data. 

	You should only need to edit lines 73 & 74 to get the directories set up properly for the directory structure on your machine. 

	You can also edit any of the parameters in the control block (lines 60-70). The n.tree parameter will impact run time for the script. I have it set to run across just 10 trees currently (should run in just a few minutes), but you can step this value up to 1000 (will take at least an hour to run, potentially longer depending on the power of your machine). 



LeastCostPathSampleCode.R
	This R script is an example of a batch file that I would pass to the computational cluster to calculate the least cost paths between sets of species ranges. I don't include all of the necessary data to run this script as there is quite a bit, but the file is fully commented so you should be able to get a good sense of what the script is doing. 