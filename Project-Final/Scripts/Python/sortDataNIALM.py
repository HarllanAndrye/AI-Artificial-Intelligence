# http://code.activestate.com/recipes/440612/
import sys
# python sortDataNIALM.py file.txt > newFile.txt
map(sys.stdout.write, sorted(file(sys.argv[1]).readlines()))
