import json
import os
from .wadze import parse_module, parse_code
from fnpy.machine import w, Env, ENV, run

SUITE = 'tools/wasm_testsuite_unpacked'

tyMap = {
    'f32': F32,
    'f64': F64,
    'i32': I32,
    'i64': I64,
}

def parseWasm(wasmPath):
    with open(wasmPath, 'rb') as f:
        module = parse_module(f.read())
    module['code'] = [parse_code(c) for c in module['code']]
    return module


def runTest(wasmPath, expected=None, expectedTy=None):
    wasm = parseWasm(wasmPath)
    e = ENV.copyForTest()


def runTests(wasmDir):
    errors = []
    passed = 0
    dirName = os.path.split(wasmDir)[1]
    jsonFile = os.path.join(wasmDir, dirName + '.json')
    with open(jsonFile) as f: j = json.load(f)
    for test in j['commands']:
        testTy = test['type']
        if testTy not in {'module', 'assert_return'}:
            continue
        wasmPath = os.path.join(wasmDir, test['filename'])
        expected, expectedTy = None, None
        if testTy == 'assert_return':
            action = test['action']
            assert action['type'] == 'invoke'
            assert action['args'] == [], "TODO"
            actionExpected = action['expected']
            assert len(actionExpected) == 1, "TODO"
            expectedTy = tyMap[actionExpected[0]['type']]
            expectedValue = float(actionExpected[0]['value'])
        runTest(wasmPath, expected, expectedTy)


def testParse():
    wasm = parseWasm('tools/wasm_testsuite_unpacked/const/const.0.wasm')
    assert False
