import json
import os
from .wadze import parse_module, parse_code
from fnpy.machine import w, Env, ENV, runWasm
from typing import List, Tuple
from fnpy.wasm import *

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


def runTest(wasmPath, inp, expected):
    wasm = parseWasm(wasmPath)
    e = ENV.copyForTest()
    for value in inp: e.ds.push(value)

    if len(wasm['code']) == 0: raise ValueError('no code')
    assert len(wasm['code']) == 1, "not implemented"
    wasmStrCode = wasm['code'][0]
    wCode = []
    for strInstr in wasmStrCode.instructions:
        wiStr, *args = strInstr
        wCode.append([wasmCode[wiStr]] + args)
    runWasm(e, wCode)

    result = []
    expectedCopy = expected[:]

    while len(e.ds):
        ty = type(expectedCopy.pop())
        result.append(e.ds.popv(ty))

    expectedValues = [v.value for v in expected]
    assert expectedValues == result



#   {"type": "f64", "value": "18442240474082181119"}
def _convertJsonValue(json):
    jty, jval = json['type'], json['value']
    val = int(jval) if jty.startswith('i') else float(jval)
    return tyMap[jty](val)


# {
#   "type": "assert_return", "line": 1057,
#   "action": {"type": "invoke", "field": "f", "args": []},
#   "expected": [{"type": "f64", "value": "18442240474082181119"}]}, 
def _assertReturnInOut(json) -> Tuple[List[any], List[any]]:
    """Get the inputs/outputs of an assert return."""
    assert json['type'] == 'assert_return'
    jaction = json['action']
    assert jaction['type'] == 'invoke'
    assert jaction['field'] == 'f'
    inp = [_convertJsonValue(j) for j in jaction['args']]
    out = [_convertJsonValue(j) for j in json['expected']]
    return inp, out

def runTests(wasmDir):
    errors = []
    passed = 0
    dirName = os.path.split(wasmDir)[1]
    jsonFile = os.path.join(wasmDir, dirName + '.json')
    with open(jsonFile) as f: j = json.load(f)

    # Note: assert_return does NOT have the module info in it.
    # {"type": "module", "line": 440, "filename": "const.178.wasm"}
    # {"type": "assert_return", "line": 441, "action": {... }, "expected": [...]}
    modulePath = None
    ranAssertions = False
    for test in j['commands']:
        testTy = test['type']
        if testTy == 'module':
            previousModulePath = modulePath
            modulePath = os.path.join(wasmDir, test['filename'])

            # If we already ran assertions on the prevoius module path don't
            # run the code.
            if not previousModulePath or ranAssertions: continue

            try:
                runTest(previousModulePath, [], [])
                passed += 1
            except Exception as e:
                errors.append(f'{previousModulePath}: {e}')

        elif testTy == 'assert_return':
            ranAssertions = True
            inp, out = _assertReturnInOut(test)

            try:
                runTest(modulePath, inp, out)
                passed += 1
            except Exception as e:
                raise
                errors.append(f'{modulePath}: {e}')

        elif testTy in {'assert_malformed'}: pass
        else: errors.append(f'{modulePath}: Unkown testTy {testTy}')

    assert [] == errors, f"Num failed={len(errors)} passed={passed}"


def testConst0():
    runTest('tools/wasm_testsuite_unpacked/const/const.0.wasm', [], [])

def testConstAll():
    runTests('tools/wasm_testsuite_unpacked/const')
