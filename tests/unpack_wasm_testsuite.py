
import subprocess
import sys
import os
import shutil

from . import wadze

WASM_TESTSUITE = 'tests/wasm_testsuite'
WASM_TESTSUITE_UNPACKED = 'tests/wasm_testsuite_unpacked'

def unpackWast(wastFname):
    wastPath = os.path.join(WASM_TESTSUITE, wastFname)
    noExt = os.path.splitext(wastFname)[0]
    unpackedDir = os.path.join(WASM_TESTSUITE_UNPACKED, noExt)
    if os.path.exists(unpackedDir): shutil.rmtree(unpackedDir)
    os.mkdir(unpackedDir)

    wastUnpackedPath = os.path.join(unpackedDir, wastFname)
    shutil.copyfile(wastPath, wastUnpackedPath)
    subprocess.run(['wast2json', wastFname], check=True, cwd=unpackedDir)

def unpackWastDir():
    for wastFname in os.listdir(WASM_TESTSUITE):
        if os.path.splitext(wastFname)[1] != 'wast': continue
        unpackWast(wastFname)


def main():
    unpackWastDir()

if __name__ == '__main__':
    main()
