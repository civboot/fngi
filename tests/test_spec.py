import os
from .wadze import parse_module, parse_code

SUITE = 'tools/wasm_testsuite_unpacked'

def parseWasm(wasmPath):
    with open(wasmPath, 'rb') as f:
        module = parse_module(f.read())
    module['code'] = [parse_code(c) for c in module['code']]
    return module



def testParse():
    wasm = parseWasm('tools/wasm_testsuite_unpacked/const/const.0.wasm')
    assert False
