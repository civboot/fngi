from .imports import *
from .mem import Mem

@dataclass
class FuEnv(object):
    mem: Mem
