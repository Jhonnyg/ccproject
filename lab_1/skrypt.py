import glob
import sys
import subprocess

jl = glob.glob(sys.argv[1] + "/*.jl")
outs = [ f[:-2] + "output" for f in jl ]

for (jl_file, out_file) in zip(jl, outs):
	print("{0} - {1}".format(jl_file, out_file))
	output = subprocess.getoutput("./runtest.sh " + jl_file) + "\n"
	output = output[24:]
	expected_output = open(out_file).read()
	if output != expected_output:
		print("{0} failed. Got:".format(jl_file))
		print(output)
		print("Expected:")
		print(expected_output)
		print()

