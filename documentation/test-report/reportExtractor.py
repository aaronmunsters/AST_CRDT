#!/usr/bin/env python3
import os

directory_path = os.path.dirname(os.path.abspath(__file__))

# open and read report, close afterwards
report = open(directory_path + "/test-report.txt")
lines = report.readlines()
report.close()

testing_amount = -1
success_amount = -1

for line in lines:
    if "Total number of tests run: " in line:
        testing_amount = line.split(":")[1].strip()
    if "Tests: succeeded" in line:
        success_amount = line.split(",")[0].split(" ")[-1]
        if testing_amount == success_amount:
            success_amount = "all"

if(int(testing_amount) > 0):
    f = open(directory_path + "/test-report.tex", "w")
    report = ''.join([
        "The project includes a test suite containing ",
        testing_amount,
        " tests, of which ",
        success_amount,
        " succeed.",
    ])
    f.write(report)
    f.close()

# sources for this little script:
# src1: https://pythonguides.com/get-current-directory-python/
# src2: https://pythonspot.com/read-file/
