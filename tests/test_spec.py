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


def runTest(wasmPath, inp, out):
    wasm = parseWasm(wasmPath)
    e = ENV.copyForTest()


#   {"type": "f64", "value": "18442240474082181119"}
def _convertJsonValue(json):
    jty, jval = json['type'], json['value']
    val = int(jval) if jty.startsWith('i') else float(jval)
    return tyMap[jty](val)


# {
#   "type": "assert_return", "line": 1057,
#   "action": {"type": "invoke", "field": "f", "args": []},
#   "expected": [{"type": "f64", "value": "18442240474082181119"}]}, 
def _assertReturnInOut(json) -> Tuple[List[any], List[any]]
    """Get the inputs/outputs of an assert return."""
    assert json['type'] == 'assert_return'
    jaction = json['action']
    assert jaction['type'] == 'invoke'
    assert jaction['field'] == 'f'
    inp = [_convertJsonValue(j) for j in jaction['args']]
    out = [_convertJsonValue(j) for j in json['expected']]

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
        inp, out = [], []
        if testTy == 'assert_return':
            inp, out = _assertReturnInOut(test)
        runTest(wasmPath, inp, out)


def testParse():
    wasm = parseWasm('tools/wasm_testsuite_unpacked/const/const.0.wasm')
    assert False
