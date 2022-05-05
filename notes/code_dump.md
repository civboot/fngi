# Code Dump: where code goes to die

## fetch/store local offset

This was replaced by FTO and SRO


In C:
```
FTLO: case SzI2 + FTLO:
    l = fetch(mem, LS_SP + popLit(SzI1), SzIA); // &local
    return WS_PUSH(fetch(mem, l + popLit(SzI1), szI)); // fetch offset
    GOTO_SZ(FTLO, SzI1)
    GOTO_SZ(FTLO, SzI4)
SRLO: case SzI2 + SRLO:
    l = fetch(mem, LS_SP + popLit(SzI1), SzIA); // &local
    return store(mem, l + popLit(SzI1), WS_POP(), szI);
    GOTO_SZ(SRLO, SzI1)
    GOTO_SZ(SRLO, SzI4)
```

In spor test:
```
\ Test FTLO

#1234 @SZ4 $GLOBAL gStruct
#67    @SZ1 $GLOBAL gStruct1
$dictGetK gStruct @SZ4 $GLOBAL gStructR  \ &gStruct

$FN testFTSRLocalOffset \ () -> ()
  @SZ4 $LOCAL _a \ just to test that local offset works
  @SZ4 $LOCAL r $END_LOCALS

  \ Test fetch local offset
  $REF gStruct $_SET r
  .4%FTLO $ldictGet r $h1 #0$h1    #1234$L2 $xsl tAssertEq
  .1%FTLO $ldictGet r $h1 #4$h1    #67$L2 $xsl tAssertEq

  \ Test store local offset
  #4242$L2 .4%SRLO $ldictGet r $h1 #0$h1
  .4%FTLO $ldictGet r $h1 #0$h1    #4242$L2 $xsl tAssertEq

  #15$L2   .1%SRLO $ldictGet r $h1 #4$h1
  .1%FTLO $ldictGet r $h1 #4$h1    #15$L2 $xsl tAssertEq
  %RET
$testFTSRLocalOffset

```

# Dot
```
\ # Dot Compiler (.compiler)
\ The below is the initial compiler for below cases:
\   .var               \ variable fetch
\   .var = <token>     \ variable store
\   .@var              \ variable dereference
\   .@var = <token>    \ variable dereference store
\   .&var              \ variable/function reference
\
\ These are built to be extended by future dot compiler implementations for
\ (i.e.) structs, modules, roles, etc.
\ note: @4:1 [= <token>] syntax is a separate word (not .compiler)


\ fn c_countChr [c -> count]      : count and consume matching characters
$SFN c_countChr $PRE \ { chr -> count } count and consume matching chrs
:wa
  #0$L0 $LOOP l0 \ {chr count}
    %OVR $xsl c_peekChr \ {chr count chr c}
    %NEQ $IF \ {chr count}
      %SWP %DRP %RET $END
    %INC \ inc count. Next line inc tokenLen
    .1%FTGL@c_tokenLen$h2 %INC  .1%SRGL@c_tokenLen$h2
  $AGAIN l0

$SFN c_dotRefs \ { -> dotMeta } get dot meta for de/refs.
  \ Get the dotMeta for preceeding & or @
  $xsl c_peekChr %DUP #26$L0 %EQ $IF \ Reference (&) case
    $xsl c_countChr %DUP #4$L0 %LT_U @E_cBadRefs$L2 $xsl assert
    @DOT_REF$L0 %ADD
  $ELSE %DUP #40$L1 %EQ $IF \ Dereference (@) case
      $xsl c_countChr %DUP #4$L0 %LT_U @E_cBadRefs$L2 $xsl assert
      @DOT_DEREF$L0 %ADD
  $ELSE %DRP #0$L0 \ no meta
  $END $END %RET \ {dotMeta}


#26 $c_countChr &&& #3 $tAssertEq
#26 $c_countChr     #0 $tAssertEq
$c_dotRefs &&       #2 @DOT_REF   ^ADD $tAssertEq
$c_dotRefs @@       #2 @DOT_DEREF ^ADD $tAssertEq
$c_dotRefs          #0                 $tAssertEq
```

# Zoa in spor
```
\ **********
\ * [10] Zoa strings and logging zoab
\ See ./harness.md for design reasoning.
\
\ fn $loc <name> |zoa string literal|    : define a zoa string
\
\ fn comzStart []                 : zoab start
\ fn comzArr [len join]           : start an array of len and join
\ fn comzLogStart [lvl extraLen]  : start a log arr of data len exraLen
\ fn print [len &raw]             : print raw data to the user (LOG_USER)
\ fn _printz [&z]                 : print zoab bytes (<=63) to the user
\ fn TODO: not finished

\ |zoa string literal|
\ Creates a zoa string in the heap.
$FN |
  $SYN $xsl assertNoNow
  $GET heap
  \ maxLen: (topHeap - heap)
  %DUP .A%FTGL@topHeap$h2 %SWP %SUB
  @D_zoa$L0 %DVFT .A%SRGL @heap$h2 %RET

$FN c_logAll $PRE \ {&writeStruct len &buf } Write raw data to log output
  $LARGE $declEnd \ no locals, but used in XW.
  %DRP
  $LOOP l0
    %OVR %OVR
    @D_com$L0 %DVFT
    %NOT $IF %DRP %DRP %RET $END
  $AGAIN l0

$FN ft4BE $PRE \ {&a -> U4} fetch4 unaligned big-endian
  %DUP%INC %DUP%INC %DUP%INC .1%FT \ {&a &a+1 &a+2                 a@3 }
  %SWP .1%FT #08$L0%SHL %ADD       \ {&a &a+1            (a@2<<8 + a@3)}
  %SWP .1%FT #10$L0%SHL %ADD       \ {&a       (a@1<<16 + a@2<<8 + a@3)}
  %SWP .1%FT #18$L0%SHL %ADD       \ {a@0<<24 + a@1<<16 + a@2<<8 + a@3 }
  %RET

$loc LOG_ZOAB_START  #80$h1 #03$h1
$FN comzStart  #2$L0 @LOG_ZOAB_START$LA  @D_com$L0 %DVSR %RET

$loc TODO #0$h1
$FN comzArr  $PRE \ {len join}
  $declL b0  @SZ1  #1 $declVar $declEnd \ b0 is used as an array for com
  \ $IF @ZOAB_JOIN$L1 $ELSE #0$L0 $END %SWP \ join -> joinTy       {joinTy len}
  \ %DUP #40$L1 %LT_U @E_cZoab$L2 $xsl assert \ assert len <= 0x3F {joinTy len}
  \ %JN  $_SET b0
  \ #33$L0 $REF b0 $jmpl com \ send via com

  %DRP \ ignore join TODO
  @ZOAB_ARR$L1 %JN  \ len->arrLen
  @TODO$L2 .1%SR \ store len @TODO
  #1$L0 @TODO$L2 @D_com$L0 %DVSR %RET \ {len &raw} communicate directly

$FN comzLogStart  $PRE \ {lvl extraLen}  extraLen is in addition to sending the lvl
  $xsl comzStart \ TODO: check return code once it's added
  %INC @FALSE$L0 $xl comzArr
  @D_comZoab$L0 %DVFT %RET \ send lvl

$FN print  $PRE \ {len &raw}: print data to user
  @LOG_USER$L1 #1$L0 $xsl comzLogStart
  @FALSE$L0 @D_comZoab$L0 %DVSR %RET

$FN _printz  $PRE \ {&z}: print zoab bytes to user. (single segment)
  %DUP .1%FT %DUP #40$L1 %LT_U @E_cZoab$L2 $xsl assert \ {len}
  %SWP %INC  $xsl print
  @D_comDone$L0 %DVFT %RET
```

# Block Allocator
```
\ **********
\ * Block Allocator (BA)
\ The Block Allocator (BA) is used to allocate and free 4KiB blocks. It can
\ store up to 255 of them (almost a full MiB).
\
\ It's implementation architecture is built on the Byte Singly Linked List
\ (BSLL). The structure of a BSLL is an array of bytes, up to length 255.
\ Each byte can contain either 0xFF (BA_iNull) or the index of the next node.
\ At initialization, the root points to the first index and each index
\ points to the next index. Allocation is then simply popping indexes
\ from the SLL.
\
\ STRUCT BA [ \ size=10
\   indexes: &U1;   \ 0: pointer to indexes (up to 255 bytes)
\   blocks: APtr;   \ 4: pointer to data blocks, each of 4KiB
\   len: U1;        \ 5: number of indexes and blocks
\   root: U1;       \ 6: root free block, may be 0xFF if empty
\ ]

CONST NULL = 0;
CONST BA_iNull = 0xFF;
CONST BA_blockPo2 = 12;  \ 4KiB == 2^12
CONST BA_halfBlock = 2048;

\ STRUCT BA
\     BA_indexesOfs = 0;  \ indexes: APtr
CONST BA_blocksOfs  = 4;  \ blocks: APtr
CONST BA_lenOfs     = 8;  \ len: U1
CONST BA_rootOfs    = 9; \ root: U1

LFN BA_iToPtr PRE \ {bi &ba -> &block}
  $declVar(declL ba, TY_VAR_INPUT+SZA, ASIZE) declEnd
  IF(dup\bi == BA_iNull) drp; ret NULL;  END \ {bi}
  assert(dup\bi < ft1(GET ba + BA_lenOfs), E_iBlock) \ {bi}
  ft4(GET ba + BA_blocksOfs); swp; \ {&firstBlk bi}
  ret(\firstBlk + (\bi << BA_blockPo2))

LFN BA_isPtrValid PRE \ {&block &ba -> bool}
  $declVar(declL blk, TY_VAR_INPUT+SZA, ASIZE)
  $declVar(declL ba,    TY_VAR_INPUT+SZA, ASIZE)
  declEnd
  ret between( \ blk between [&firstBlk, &lastBlkEnd)
    GET blk,
    ft4(GET ba + BA_blocksOfs), \ {&firstBlk}
    dup\firstBlk + (ft1(GET ba + BA_lenOfs) << BA_blockPo2),
  );

LFN BA_ptrToI PRE \ {&blk &ba -> bi}
  $declVar(declL blk, TY_VAR_INPUT+SZA, ASIZE)
  $declVar(declL ba,    TY_VAR_INPUT+SZA, ASIZE)
  \ $SZ4 INPUT blk  $SZ4 INPUT ba
  declEnd
  IF(not GET blk)  ret BA_iNull;  END
  assert(BA_isPtrValid(GET blk, GET ba), E_ptrBlk)
  ret((GET blk - ft4(GET ba + BA_blocksOfs)) >> BA_blockPo2);
```


```
SFN BA_iGet  PRE ret ft1(_ + ft4(_)) \ {bi &ba -> bi}
SFN BA_iSet  PRE  \ {value bi &ba}  set ba@bi = value
  _ + ft4(_) \ {value &index}
  ret sr1(_, _)

LFN BA_init PRE \ {&ba}
  $declVar(declL ba, TY_VAR_INPUT+SZA, ASIZE)
  $declVar(declL iLast,           SZ1, 1)
  declEnd
  SET iLast = dec(ft1(GET ba + BA_lenOfs));
  sr1(0, GET ba + BA_rootOfs); \ initialze root as index=0
  0 LOOP l0 \ {bi}
    IF(dup == GET iLast)
      BA_iNull; swp; ret BA_iSet(\BA_iNull, \bi, GET ba)
    END \ {bi}
    dup; inc(\bi); ovr; \(bi bi+1 bi)
    BA_iSet(\(bi+1), \bi, GET ba)
    inc(\bi) \ {bi+1}
  AGAIN l0

\ $setSysLogLvl(LOG_EXECUTE)
$BA_init(REF BA_kernel)

\ { &ba -> bi} alocate a block index
\ (returning a) root->a->b ===> root->b
LFN BA_iAlloc PRE
  $declVar(declL ba, TY_VAR_INPUT+SZA, ASIZE) declEnd
  ft1(GET ba + BA_rootOfs) \ {biRoot}
  reteq(dup, BA_iNull) \ {biRoot} return BA_iNull if biRoot=BA_iNull
  sr1(BA_iGet(dup, GET ba), GET ba + BA_rootOfs); \ {biRoot} root=next node
  ret _;

\ {bi &ba} free a block index
\ (freeing bi=a) root->b  ===>  root->a->b
LFN BA_iFree PRE
  $declVar(declL bi, TY_VAR_INPUT+SZ1, 1)
  $declVar(declL biRoot,          SZ1, 1)
  $declVar(declL ba, TY_VAR_INPUT+SZA, ASIZE)
  declEnd
  assert(BA_iNull != GET bi, E_iBlock)
  SET biRoot = ft1(GET ba + BA_rootOfs)
  BA_iSet(GET biRoot, GET bi, GET ba)  \ a -> b  (note: may be BA_iNull)
  ret sr1(GET bi, GET ba + BA_rootOfs) \ root -> a

SFN BA_alloc PRE \ {&ba -> &blockNullable}E
  BA_iAlloc(dup\ba); swp;
  ret BA_iToPtr(\bi, \ba);

SFN BA_free  PRE \ {&block &ba}
  swp; ovr; \ {&ba &block &ba}
  BA_ptrToI(\ba); swp; ret BA_iFree(\bi, \ba);
```

```
\ **********
\ * Singly Linked List (SLL)
\ Singly linked lists are used throughout fngi because of their simplicity
\ and extensibility. A singly linked list is a data structure that looks like
\ below, with how the operations affect it as well.
\
\ start    : root -> b -> NULL
\ push a   : root -> a -> b -> NULL
\ now pop a: root -> b -> NULL (return a)
\
\ STRUCT SLL [ next: &SLL ]
\ Note: root is just a pointer to a SLL with no data (&SLL)

LFN SLL_pop  PRE \ {&root -> &a}: Pop a node from the SLL
  $declVar(declL root, TY_VAR_INPUT+SZA, ASIZE) declEnd
  ft4(GET root); \ {&a}
  retz(dup); \ {&a}
  ret sr4(ft4(dup), GET root); \ {&a} root->b

SFN SLL_push PRE \ {&a &root}: Push a node onto the SLL
  ovr; ovr;           \ {&a &root &a &root}
  ft4(\root); swp;    \ {&a &root &b &a}
  sr4(\b, \a);        \ a->b
  ret sr4(\a, \root); \ root->a
```

```
\ **********
\ * Arena Allocator (AA)
\ The arena budy allocator is built on top of the BA. It allows allocations of
\ power of 2 blocks of sizes 2^2 (4 bytes) to 2^12 (4KiB).
\
\ The AA keeps track of blocks it owns by using the BA's indexes (the same way
\ the BA keeps track of free blocks). When the arena is dropped, all (4KiB)
\ blocks are returned to the BA.
\
\ The AA does a best-effort job of trying to join freed blocks to prevent
\ fragmentation. However, it prioritizes known speed over reduction of
\ fragmentation. It's performance requires a maximum of 12 loops for any
\ request, except for freeing blocks which can require a BA index scan (rare).
\
\ If further fragmenetation reduction is required, a library can implement methods
\ to sort the freed areas and join conjoining ones. However the proper method to
\ eliminate fragmentation is to move any still-needed data to a new arena and
\ drop the fragmented one (obviously requiring careful programming).
\
\ ## Design:
\ The allocator has an array of pointers, each pointing to a larger po2.
\ Multiple free blocks in a single power of 2 are stored by creating a
\ singly-linked-list in the first 4 bytes of the free memory. Any freed (4KiB)
\ blocks are returned to the BA.
\
\ STRUCT AA [
\   bi: U1;  \ block index for allocated block.
\   _reserved1: U1;
\   _reserved2: U2;
\   ba: &BA;
\   \ Po2 2 - 11 Clever: &aa@2 is the 0th index (2nd power), &aa@11 is 11th
\   roots: arr[10; APtr];
\ ]

SFN sort2 PRE \ { a b -> _ _ } sort two values on the stack
  ovr; ovr; retlt(_, _) swp; ret;

\ {&a &b po2 -> bool} return whether blocks can be joined.
LFN canBlocksMerge PRE
  $declVar(declL po2, TY_VAR_INPUT+SZ1, 1) declEnd
  ovr\a != align(dup\a, 1 << inc(GET po2)) \ {&a, &b, &a is on po2 boundary}
  IF(_) drp\b; drp\a; ret FALSE;  END
  ret(\a == (\b - (1 << GET po2))) \ return whether a is next to b

CONST AA_size     = 48;
CONST AA_baOfs    = 4;
CONST AA_rootsOfs = 8;

SFN AA_isValidPo2  PRE ret betweenIncl(_, 2, 12); \ {po2 -> bool}
SFN AA_assertPo2   PRE (AA_isValidPo2(_), E_aaPo2)$jmpl assert

LFN AA_init PRE \ {&ba &aa}
  $declVar(declL aa, TY_VAR_INPUT+SZA, ASIZE) declEnd \ {&ba}
  sr4(\ba, GET aa + AA_baOfs); \ init ba
  (GET aa + AA_rootsOfs, 10 << 2)$jmpl memClear \ all roots=NULL

$declVar(declG AA_kernel, SZA, AA_size)
$AA_init(REF BA_kernel, REF AA_kernel)

LFN AA_findPo2 PRE \ {po2 &aa -> &free gotPo2}: find the closest available po2
  $declVar(declL aa, TY_VAR_INPUT+SZA, ASIZE) declEnd \ {&ba}
  AA_assertPo2(dup\po2);
  LOOP l0  \ Find a free block of the appropriate size
    reteq(dup\po2, 12)                  \ maximum size
    retif(ft4((dup\po2 << 2) + GET aa)) \ non-null size
    inc(\po2)
  AGAIN l0

SFN AA_allocExactPo2 PRE \ {po2 &aa -> &free}: get an exact po2
  swp; \ {&aa po2}
  IF(dup\po2 == 12) drp\po2; BA_alloc(\aa + AA_baOfs);
  ELSE SLL_pop(\aa + (\po2 << 2))  END ret;

LFN AA_allocPo2 PRE \ {po2 &aa -> &free} allocate memory of size po2
  $declVar(declL po2,    TY_VAR_INPUT+SZ1, 1)
  $declVar(declL gotPo2,              SZ1, 1)
  $declVar(declL aa,     TY_VAR_INPUT+SZA, ASIZE)
  declEnd
  SET gotPo2 = AA_findPo2(GET po2, GET aa);
  LOOP l1 \ Break the block until it is the right size.
    AA_allocExactPo2(GET gotPo2, GET aa); \ {&free}
    retz(dup) \ return if NULL
    reteq(GET po2, GET gotPo2) \ {&free}: return if correct po2

    SET gotPo2 = dec(GET gotPo2); \ {&free}: reduce gotPo2 since we're spliting in half
    \ The next cycle will return or divide the lowest free block
    SLL_push(dup\free + (1 << GET gotPo2), GET aa + (GET gotPo2 << 2))
    SLL_push(\free                       , GET aa + (GET gotPo2 << 2))
  AGAIN l1

\ LFN AA_freePo2 PRE \ {&free po2 &aa}
\   $SZ1 INPUT po2  $SZ1 LOCAL gotPo2  $SZ4 INPUT aa  declEnd \ {&free}
\   AA_assertPo2(GET po2); \ {&free}
\   LOOP l0 \ {&free} Merge free blocks while they are consecutive
\     IF(GET po2 == 12)  ret BA_free(GET aa + AA_baOfs);  END \ if block, just free that.
\     SLL_pop(dup, GET aa + (GET po2 << 2)) \ {&f, &fp} f=free fp=freePrev
\     IF(not dup) \ check null case of existing free
\       drp\fp; ret SLL_push(\f, GET aa + (GET po2 << 2));
\     END sort2(_, _); \ {&a &b} sort them for next checks.
\     IF(canBlocksMerge(ovr\a, ovr\b, GET po2))
\       drp\b; SET po2 = inc(GET po2); AGAIN l0 \ drop &b and treat as one
\     END \ cannot merge, just push both back and return.
\     SLL_push(\b, GET aa + (GET po2 << 2));
\     ret SLL_push(\a, GET aa + (GET po2 << 2));
\   \ END LOOP l0

$c_dictDump
```

```
\ **********
\ ** Test Block Allocator
$tAssertEq(0xFF, BA_iNull)

$declVar(declG BA_fake, SZ4, 12)
  $sr4(0, gRef BA_fake)
$declVar(declG BA_fakeIndexes, SZ4, 4)
  $sr4(0, gRef BA_fakeIndexes)

SFN BA_fakeInitForTest PRE \ {&indexes, &blocks, len) Initialize some fake data
  sr1(\len    , REF BA_fake + BA_lenOfs)
  sr4(\blocks , REF BA_fake + BA_blocksOfs)
  sr4(\indexes, REF BA_fake)
  ret sr1(0, REF BA_fake + BA_rootOfs)

$BA_fakeInitForTest(REF BA_fakeIndexes, 0x1000, 4)
$tAssertNot(BA_isPtrValid(0, REF BA_fake))      \ null block
$tAssertNot(BA_isPtrValid(0x500, REF BA_fake))
$tAssert   (BA_isPtrValid(0x1000, REF BA_fake)) \ 1st block
$tAssert   (BA_isPtrValid(0x2000, REF BA_fake)) \ 2nd block
$tAssert   (BA_isPtrValid(0x3000, REF BA_fake)) \ 3rd block
$tAssert   (BA_isPtrValid(0x4000, REF BA_fake)) \ 4th block
$tAssertNot(BA_isPtrValid(0x5000, REF BA_fake)) \ 5th block

$tAssertEq(BA_iNull, BA_ptrToI(0, REF BA_fake)) \ hard-coded
$tAssertEq(0,        BA_ptrToI(0x1000, REF BA_fake))
$tAssertEq(1,        BA_ptrToI(0x2000, REF BA_fake))
$tAssertEq(2,        BA_ptrToI(0x3000, REF BA_fake))

$tAssertEq(0,        BA_iToPtr(BA_iNull, REF BA_fake)) \ hard-coded
$tAssertEq(0x1000,   BA_iToPtr(0, REF BA_fake))
$tAssertEq(0x2000,   BA_iToPtr(1, REF BA_fake))
$tAssertEq(0x3000,   BA_iToPtr(2, REF BA_fake))
$tAssertEq(0x4000,   BA_iToPtr(3, REF BA_fake))
\ $tAssertEq(0x5000,   BA_iToPtr(4, REF BA_fake)) \ panics

$BA_init(REF BA_kernel) \ TODO: make this the fake block
LFN testBASimple
  $declVar(declL blk, SZ4, 4)
  $declVar(declL iBlk, SZ1, 1) declEnd
  tAssert(ft1(REF BA_kernel + BA_lenOfs))
  tAssertEq(0, ft1(REF BA_kernel + BA_rootOfs));
  tAssertEq(1, BA_iGet(0, REF BA_kernel));
  tAssertEq(2, BA_iGet(1, REF BA_kernel));

  SET iBlk = BA_iAlloc(REF BA_kernel);
  tAssertEq(0, GET iBlk)
  tAssertEq(1, ft1(REF BA_kernel + BA_rootOfs)); \ root changed
  SET blk = BA_iToPtr(GET iBlk, REF BA_kernel);
  tAssertEq(ft4(REF BA_kernel + BA_blocksOfs), GET blk)
  tAssertEq(GET blk,  BA_iToPtr(GET iBlk, REF BA_kernel));
  tAssertEq(ft4(REF BA_kernel + BA_blocksOfs), GET blk);
  tAssertEq(GET iBlk, BA_ptrToI(GET blk,  REF BA_kernel));
  BA_free(GET blk, REF BA_kernel)
  tAssertEq(0, ft1(REF BA_kernel + BA_rootOfs)); \ root changed
  tAssertEq(1, BA_iGet(0, REF BA_kernel));
  tAssertEq(2, BA_iGet(1, REF BA_kernel));
  ret;

$testBASimple

\ **********
\ ** Test SLL

$declVar(declG sll_c,    SZ4, 4)
$declVar(declG sll_b,    SZ4, 4)
$declVar(declG sll_a,    SZ4, 4)
$declVar(declG sll_root, SZ4, 4)
  $sr4(gRef sll_a, gRef sll_root) \ root->a->b->c->NULL
  $sr4(gRef sll_b, gRef sll_a)
  $sr4(gRef sll_c, gRef sll_b)
  $sr4(NULL, gRef sll_c)

$tAssertEq(REF sll_a, SLL_pop(REF sll_root)) \ root->b->c
$tAssertEq(REF sll_b, SLL_pop(REF sll_root)) \ root->c
 $SLL_push(REF sll_a, REF sll_root)          \ root->a->c
$tAssertEq(REF sll_a, SLL_pop(REF sll_root)) \ root->c
$tAssertEq(REF sll_c, SLL_pop(REF sll_root)) \ root->NULL
$tAssertEq(NULL     , SLL_pop(REF sll_root))
$tAssertEq(NULL     , SLL_pop(REF sll_root))

\ **********
\ ** Test AA
$sort2(5 10) $tAssertEq(_, 10) $tAssertEq(_, 5)
$sort2(10 5) $tAssertEq(_, 10) $tAssertEq(_, 5)

$tAssert   (canBlocksMerge(16, 24, 3))
$tAssertNot(canBlocksMerge( 8, 16, 3))
$tAssertNot(canBlocksMerge( 8, 24, 3))
$tAssertNot(canBlocksMerge( 4, 12, 3))
$tAssertNot(canBlocksMerge(12, 20, 3))
$tAssertNot(canBlocksMerge(32, 64, 4))
$tAssert   (canBlocksMerge(64, 96, 5))
$tAssertNot(canBlocksMerge(64, 96, 4))

$tAssert(AA_isValidPo2(2))
$tAssert(AA_isValidPo2(3))
$tAssert(AA_isValidPo2(11))
$tAssert(AA_isValidPo2(12))
$tAssertNot(AA_isValidPo2(0))
$tAssertNot(AA_isValidPo2(1))
$tAssertNot(AA_isValidPo2(13))

$declVar(declG AA_fake, SZ4, AA_size)
$AA_init(REF BA_kernel, REF AA_fake)

\ Populate some fake data in freePo2 array
$sr4(2, REF AA_fake +  (2<<2))
$sr4(5, REF AA_fake +  (5<<2))
$sr4(10, REF AA_fake + (10<<2))

\ test AA_findPo2
$tAssertEq( 2, AA_findPo2(2, REF AA_fake))
$tAssertEq( 5, AA_findPo2(3, REF AA_fake))
$tAssertEq( 5, AA_findPo2(5, REF AA_fake))
$tAssertEq(10, AA_findPo2(6, REF AA_fake))
$tAssertEq(10, AA_findPo2(9, REF AA_fake))
$tAssertEq(10, AA_findPo2(10, REF AA_fake))
$tAssertEq(12, AA_findPo2(11, REF AA_fake))
$tAssertEq(12, AA_findPo2(12, REF AA_fake))

\ test AA_allocExactPo2 (except for po2=12)

$declVar(declG AA_blk0, SZA, ASIZE)
  $srA(BA_alloc(REF BA_kernel), gRef AA_blk0)

\ Populate some "real fake" data.
SFN AA_initForTest PRE \ {&ba}
  AA_init(_, REF AA_fake)

  GET AA_blk0 + (1<<2); \ po2=2
  sr4(NULL, ovr)  sr4(_, REF AA_fake + (2<<2))

  GET AA_blk0 + (1<<5); \ po2=5
  sr4(NULL, ovr)  sr4(_, REF AA_fake + (5<<2))

  GET AA_blk0 + (1<<10); \ po2=10
  sr4(NULL, ovr)  sr4(_, REF AA_fake + (10<<2))
  ret;

$AA_initForTest(NULL);
$tAssertEq(GET AA_blk0 + (1<<2) , AA_allocExactPo2(2, REF AA_fake))
$tAssertEq(NULL                 , AA_allocExactPo2(3, REF AA_fake))
$tAssertEq(NULL                 , AA_allocExactPo2(4, REF AA_fake))
$tAssertEq(GET AA_blk0 + (1<<5) , AA_allocExactPo2(5, REF AA_fake))
$tAssertEq(NULL                 , AA_allocExactPo2(6, REF AA_fake))
$tAssertEq(GET AA_blk0 + (1<<10), AA_allocExactPo2(10, REF AA_fake))
$tAssertEq(NULL                 , AA_allocExactPo2(11, REF AA_fake))
```

```
\ All roots are now null
$tAssertEq(NULL, ft4(REF AA_fake + (2<<2)))
$tAssertEq(NULL, ft4(REF AA_fake + (3<<2)))
$tAssertEq(NULL, ft4(REF AA_fake + (5<<2)))
$tAssertEq(NULL, ft4(REF AA_fake + (10<<2)))

$AA_initForTest(NULL);
\ First let's do a freebie
$tAssertEq(GET AA_blk0 + (1<<2) ,          AA_allocPo2(2, REF AA_fake))

\ Now, we want to allocate 2^9, but that will require spliting 2^10
$tAssertEq(GET AA_blk0 + (1<<10),          AA_allocPo2(9, REF AA_fake))
$tAssertEq(GET AA_blk0 + (1<<10) + (1<<9), AA_allocPo2(9, REF AA_fake))
\ They should now both be exhausted
$tAssertEq(NULL, ft4(REF AA_fake + ( 9<<2)))
$tAssertEq(NULL, ft4(REF AA_fake + (10<<2)))

\ Let's do that again, but this time for 2^8
$AA_initForTest(NULL);
$tAssertEq(GET AA_blk0 + (1<<10),          AA_allocPo2(8, REF AA_fake))
$tAssertEq(GET AA_blk0 + (1<<10) + (1<<8), AA_allocPo2(8, REF AA_fake))
$tAssertEq(GET AA_blk0 + (1<<10) + (2<<8), AA_allocPo2(8, REF AA_fake))
$tAssertEq(GET AA_blk0 + (1<<10) + (3<<8), AA_allocPo2(8, REF AA_fake))
\ They are now all exhausted
$tAssertEq(NULL, ft4(REF AA_fake + ( 8<<2)))
$tAssertEq(NULL, ft4(REF AA_fake + ( 9<<2)))
$tAssertEq(NULL, ft4(REF AA_fake + (10<<2)))

\ Now we need to test that we can reserve an actual block
$BA_fakeInitForTest(REF BA_fakeIndexes, \blocks GET AA_blk0, 1)
$AA_initForTest(REF BA_fake)

\ TODO: write a test which reserves a ba block... it should just work :D

\ TODO: write and test FREEDOM

$BA_free(GET AA_blk0, REF BA_kernel)
```
