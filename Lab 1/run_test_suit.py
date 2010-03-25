import glob 
import sys 
import os 
 
for file in glob.glob(sys.argv[1] + "/*.jl"): 
        print file 
        os.system("./main " + file)