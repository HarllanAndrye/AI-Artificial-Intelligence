import sys

# python filterData.py inputFile outputFile
def main(args):
    inFilename = args[0]
    outFilename = args[1]
    inFile = open(inFilename,"r")
    lines = inFile.readlines()
    inFile.close()
    outFile = open(outFilename,"w")
    for line in lines:
        currentLine = line.split(",")
        #r1=currentLine[1]
	#r1=currentLine[1].replace("T", " ")
	#r1=r1.replace(".000Z", "")
	#r2=currentLine[2]
	r2=currentLine[2].replace("T", " ")
	r2=r2.replace(".000Z", "")
	r3=currentLine[5]
	r4=currentLine[6]
	outFile.write(",".join([str(repr(r2)),str(r3),str(r4)]))
	#outFile.write(",".join([str(repr(r1)),str(repr(r2)),str(r3),str(r4)]))
    outFile.close()

if __name__ == "__main__":
    main(sys.argv[1:])
