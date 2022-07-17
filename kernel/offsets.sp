\ @file kernel/offsets.sp
\ DO NOT EDIT MANUALLY! THIS FILE WAS GENERATED BY: etc/make.py
\
\ @brief Defines global variable offsets.

\ struct Buf { ... }
#00   #0=Buf_dat               \ Ref: data
#04   #0=Buf_len               \ U2: length of buffer
#06   #0=Buf_cap               \ U2: cap of buffer

\ struct Kernel { ... }
#04   #0=K_memTop              \ Ref: highest address in memory
#08   #0=K_ba                  \ BA struct: kernel BA
#14   #0=K_bbaPub              \ BBA struct: kernel BBA
#20   #0=K_bbaPriv             \ BBA struct: private BBA
#2C   #0=K_dict                \ &Dict: kernel dictionary

\ struct Globals { ... }
#00   #90=G_glen                \ U2
#02   #90=G_gcap                \ U2
#04   #A0=G_fb                  \ &Fiber: current fiber
#0A   #90=G_cstate              \ U2: compiler state
#0C   #80=G_fnState             \ U1: function compilation state
#0D   #80=G_localOffset         \ U1: current offset of locals
#0E   #80=G_logLvlSys           \ U1
#0F   #80=G_logLvlUsr           \ U1
#08   #90=G_metaNext            \ U2: next function's meta
#10   #A0=G_curNode             \ Ref: current compiling dict node
#14   #A0=G_compFn              \ Ref: function used for compiling
#18   #A0=G_bbaLocal            \ BBA: local BBA
#24   #A0=G_dictLocal           \ &DNode: local dict
#28   #A0=G_bbaPub              \ &BBA: current public bba
#30   #A0=G_bbaPriv             \ &BBA: current private bba
#38   #A0=G_srcM                \ &FileMethods: src file methods
#3C   #A0=G_src                 \ &File: src File

\ struct Fiber { ... }
#0C   #0=Fb_ws                 \ Stk struct: working stack
#2C   #0=Fb_gb                 \ &Globals: globals base pointer
#30   #0=Fb_err                \ U2: panic error

\ struct File { ... }
#08   #0=Fs_buf                \ Buf: buffer
#10   #0=Fs_plc                \ U2: plc in buffer
#12   #0=Fs_code               \ U2: file code

\ BA { Ref nodes; Ref blocks; U1 rooti; U1 cap; }
#00   #0=BA_nodes              \ &Node: start of nodes len cap*2
#04   #0=BA_blocks             \ &Block: start of 4k blocks len cap
#08   #0=BA_rooti              \ U1: root index
#09   #0=BA_cap                \ U1: number of nodes and blocks

\ BBA { Ref ba; U1 rooti; U2 len; U2 cap; }
#00   #0=BBA_ba                \ &BA
#04   #0=BBA_rooti             \ U1: owned block root index
#06   #0=BBA_len               \ U2: unsigned heap
#08   #0=BBA_cap               \ U2: signed topheap
#00   #0=BBAm_bump             \ method index
#01   #0=BBAm_newBlock         \ method index
#02   #0=BBAm_drop             \ method index

\ struct DNode { ... }
#00   #0=DN_l                  \ Ref: left
#04   #0=DN_r                  \ Ref: right
#08   #0=DN_ckey               \ Ref: counted data key
#10   #0=DN_m                  \ U2: meta
#0C   #0=DN_v                  \ Ref: value, which may be a constant
#14   #0=DN_ty                 \ Ref?: only exists if C_TYPED
