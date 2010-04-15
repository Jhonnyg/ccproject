import glob 
import sys 
import os 
 
for file in glob.glob(sys.argv[1] + "/*"): 
        print file 
        os.system("./cumpiler.sh " + file)