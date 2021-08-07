// Below is copy/pasted uxn source code from: https://git.sr.ht/~rabbits/uxn/tree/master/item/src/

/*
 * Copyright (c) 2021 Devine Lu Linvega
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE.
 * */


// ##################### uxn.h
typedef unsigned char Uint8;
typedef signed char Sint8;
typedef unsigned short Uint16;
typedef signed short Sint16;

#define PAGE_PROGRAM 0x0100

typedef struct {
	Uint8 ptr, kptr, error;
	Uint8 dat[256];
} Stack;

typedef struct {
	Uint16 ptr;
	Uint8 dat[65536];
} Memory;

typedef struct Device {
	struct Uxn *u;
	Uint8 addr, dat[16], *mem;
	void (*talk)(struct Device *d, Uint8, Uint8);
} Device;

typedef struct Uxn {
	Stack wst, rst, *src, *dst;
	Memory ram;
	Device dev[16];
} Uxn;

struct Uxn;

void mempoke16(Uint8 *m, Uint16 a, Uint16 b);
Uint16 mempeek16(Uint8 *m, Uint16 a);

int uxn_boot(Uxn *c);
int uxn_eval(Uxn *u, Uint16 vec);
int uxn_halt(Uxn *u, Uint8 error, char *name, int id);
Device *uxn_port(Uxn *u, Uint8 id, char *name, void (*talkfn)(Device *, Uint8, Uint8));

// ##################### uxn.c


#define MODE_RETURN 0x40
#define MODE_KEEP 0x80

#pragma mark - Operations

/* clang-format off */
static void   push8(Stack *s, Uint8 a) { if(s->ptr == 0xff) { s->error = 2; return; } s->dat[s->ptr++] = a; }
static Uint8  pop8_keep(Stack *s) { if(s->kptr == 0) { s->error = 1; return 0; } return s->dat[--s->kptr]; }
static Uint8  pop8_nokeep(Stack *s) { if(s->ptr == 0) { s->error = 1; return 0; } return s->dat[--s->ptr]; }
static Uint8 (*pop8)(Stack *s);
static void   mempoke8(Uint8 *m, Uint16 a, Uint8 b) { m[a] = b; }
static Uint8  mempeek8(Uint8 *m, Uint16 a) { return m[a]; }
static void   devpoke8(Device *d, Uint8 a, Uint8 b) { d->dat[a & 0xf] = b; d->talk(d, a & 0x0f, 1); }
static Uint8  devpeek8(Device *d, Uint8 a) { d->talk(d, a & 0x0f, 0); return d->dat[a & 0xf];  }
static void   push16(Stack *s, Uint16 a) { push8(s, a >> 8); push8(s, a); }
static Uint16 pop16(Stack *s) { Uint8 a = pop8(s), b = pop8(s); return a + (b << 8); }
void   mempoke16(Uint8 *m, Uint16 a, Uint16 b) { mempoke8(m, a, b >> 8); mempoke8(m, a + 1, b); }
Uint16 mempeek16(Uint8 *m, Uint16 a) { return (mempeek8(m, a) << 8) + mempeek8(m, a + 1); }
static void   devpoke16(Device *d, Uint8 a, Uint16 b) { devpoke8(d, a, b >> 8); devpoke8(d, a + 1, b); }
static Uint16 devpeek16(Device *d, Uint16 a) { return (devpeek8(d, a) << 8) + devpeek8(d, a + 1); }
/* Stack */
static void op_brk(Uxn *u) { u->ram.ptr = 0; }
static void op_nop(Uxn *u) { (void)u; }
static void op_lit(Uxn *u) { push8(u->src, mempeek8(u->ram.dat, u->ram.ptr++)); }
static void op_pop(Uxn *u) { pop8(u->src); }
static void op_dup(Uxn *u) { Uint8 a = pop8(u->src); push8(u->src, a); push8(u->src, a); }
static void op_swp(Uxn *u) { Uint8 a = pop8(u->src), b = pop8(u->src); push8(u->src, a); push8(u->src, b); }
static void op_ovr(Uxn *u) { Uint8 a = pop8(u->src), b = pop8(u->src); push8(u->src, b); push8(u->src, a); push8(u->src, b); }
static void op_rot(Uxn *u) { Uint8 a = pop8(u->src), b = pop8(u->src), c = pop8(u->src); push8(u->src, b); push8(u->src, a); push8(u->src, c); }
/* Logic */
static void op_equ(Uxn *u) { Uint8 a = pop8(u->src), b = pop8(u->src); push8(u->src, b == a); }
static void op_neq(Uxn *u) { Uint8 a = pop8(u->src), b = pop8(u->src); push8(u->src, b != a); }
static void op_gth(Uxn *u) { Uint8 a = pop8(u->src), b = pop8(u->src); push8(u->src, b > a); }
static void op_lth(Uxn *u) { Uint8 a = pop8(u->src), b = pop8(u->src); push8(u->src, b < a); }
static void op_jmp(Uxn *u) { Uint8 a = pop8(u->src); u->ram.ptr += (Sint8)a; }
static void op_jnz(Uxn *u) { Uint8 a = pop8(u->src); if(pop8(u->src)) u->ram.ptr += (Sint8)a; }
static void op_jsr(Uxn *u) { Uint8 a = pop8(u->src); push16(u->dst, u->ram.ptr); u->ram.ptr += (Sint8)a; }
static void op_sth(Uxn *u) { Uint8 a = pop8(u->src); push8(u->dst, a); }
/* Memory */
static void op_pek(Uxn *u) { Uint8 a = pop8(u->src); push8(u->src, mempeek8(u->ram.dat, a)); }
static void op_pok(Uxn *u) { Uint8 a = pop8(u->src); Uint8 b = pop8(u->src); mempoke8(u->ram.dat, a, b); }
static void op_ldr(Uxn *u) { Uint8 a = pop8(u->src); push8(u->src, mempeek8(u->ram.dat, u->ram.ptr + (Sint8)a)); }
static void op_str(Uxn *u) { Uint8 a = pop8(u->src); Uint8 b = pop8(u->src); mempoke8(u->ram.dat, u->ram.ptr + (Sint8)a, b); }
static void op_lda(Uxn *u) { Uint16 a = pop16(u->src); push8(u->src, mempeek8(u->ram.dat, a)); }
static void op_sta(Uxn *u) { Uint16 a = pop16(u->src); Uint8 b = pop8(u->src); mempoke8(u->ram.dat, a, b); }
static void op_dei(Uxn *u) { Uint8 a = pop8(u->src); push8(u->src, devpeek8(&u->dev[a >> 4], a)); }
static void op_deo(Uxn *u) { Uint8 a = pop8(u->src), b = pop8(u->src); devpoke8(&u->dev[a >> 4], a, b); }
/* Arithmetic */
static void op_add(Uxn *u) { Uint8 a = pop8(u->src), b = pop8(u->src); push8(u->src, b + a); }
static void op_sub(Uxn *u) { Uint8 a = pop8(u->src), b = pop8(u->src); push8(u->src, b - a); }
static void op_mul(Uxn *u) { Uint8 a = pop8(u->src), b = pop8(u->src); push8(u->src, b * a); }
static void op_div(Uxn *u) { Uint8 a = pop8(u->src), b = pop8(u->src); if(a == 0) { u->src->error = 3; a = 1; } push8(u->src, b / a); }
static void op_and(Uxn *u) { Uint8 a = pop8(u->src), b = pop8(u->src); push8(u->src, b & a); }
static void op_ora(Uxn *u) { Uint8 a = pop8(u->src), b = pop8(u->src); push8(u->src, b | a); }
static void op_eor(Uxn *u) { Uint8 a = pop8(u->src), b = pop8(u->src); push8(u->src, b ^ a); }
static void op_sft(Uxn *u) { Uint8 a = pop8(u->src), b = pop8(u->src); push8(u->src, b >> (a & 0x07) << ((a & 0x70) >> 4)); }
/* Stack */
static void op_lit16(Uxn *u) { push16(u->src, mempeek16(u->ram.dat, u->ram.ptr++)); u->ram.ptr++; }
static void op_pop16(Uxn *u) { pop16(u->src); }
static void op_dup16(Uxn *u) { Uint16 a = pop16(u->src); push16(u->src, a); push16(u->src, a); }
static void op_swp16(Uxn *u) { Uint16 a = pop16(u->src), b = pop16(u->src); push16(u->src, a); push16(u->src, b); }
static void op_ovr16(Uxn *u) { Uint16 a = pop16(u->src), b = pop16(u->src); push16(u->src, b); push16(u->src, a); push16(u->src, b); }
static void op_rot16(Uxn *u) { Uint16 a = pop16(u->src), b = pop16(u->src), c = pop16(u->src); push16(u->src, b); push16(u->src, a); push16(u->src, c); }
/* Logic(16-bits) */
static void op_equ16(Uxn *u) { Uint16 a = pop16(u->src), b = pop16(u->src); push8(u->src, b == a); }
static void op_neq16(Uxn *u) { Uint16 a = pop16(u->src), b = pop16(u->src); push8(u->src, b != a); }
static void op_gth16(Uxn *u) { Uint16 a = pop16(u->src), b = pop16(u->src); push8(u->src, b > a); }
static void op_lth16(Uxn *u) { Uint16 a = pop16(u->src), b = pop16(u->src); push8(u->src, b < a); }
static void op_jmp16(Uxn *u) { u->ram.ptr = pop16(u->src); }
static void op_jnz16(Uxn *u) { Uint16 a = pop16(u->src); if(pop8(u->src)) u->ram.ptr = a; }
static void op_jsr16(Uxn *u) { push16(u->dst, u->ram.ptr); u->ram.ptr = pop16(u->src); }
static void op_sth16(Uxn *u) { Uint16 a = pop16(u->src); push16(u->dst, a); }
/* Memory(16-bits) */
static void op_pek16(Uxn *u) { Uint8 a = pop8(u->src); push16(u->src, mempeek16(u->ram.dat, a)); }
static void op_pok16(Uxn *u) { Uint8 a = pop8(u->src); Uint16 b = pop16(u->src); mempoke16(u->ram.dat, a, b); }
static void op_ldr16(Uxn *u) { Uint8 a = pop8(u->src); push16(u->src, mempeek16(u->ram.dat, u->ram.ptr + (Sint8)a)); }
static void op_str16(Uxn *u) { Uint8 a = pop8(u->src); Uint16 b = pop16(u->src); mempoke16(u->ram.dat, u->ram.ptr + (Sint8)a, b); }
static void op_lda16(Uxn *u) { Uint16 a = pop16(u->src); push16(u->src, mempeek16(u->ram.dat, a)); }
static void op_sta16(Uxn *u) { Uint16 a = pop16(u->src); Uint16 b = pop16(u->src); mempoke16(u->ram.dat, a, b); }
static void op_dei16(Uxn *u) { Uint8 a = pop8(u->src); push16(u->src, devpeek16(&u->dev[a >> 4], a)); }
static void op_deo16(Uxn *u) { Uint8 a = pop8(u->src); Uint16 b = pop16(u->src); devpoke16(&u->dev[a >> 4], a, b); }
/* Arithmetic(16-bits) */
static void op_add16(Uxn *u) { Uint16 a = pop16(u->src), b = pop16(u->src); push16(u->src, b + a); }
static void op_sub16(Uxn *u) { Uint16 a = pop16(u->src), b = pop16(u->src); push16(u->src, b - a); }
static void op_mul16(Uxn *u) { Uint16 a = pop16(u->src), b = pop16(u->src); push16(u->src, b * a); }
static void op_div16(Uxn *u) { Uint16 a = pop16(u->src), b = pop16(u->src); if(a == 0) { u->src->error = 3; a = 1; } push16(u->src, b / a); }
static void op_and16(Uxn *u) { Uint16 a = pop16(u->src), b = pop16(u->src); push16(u->src, b & a); }
static void op_ora16(Uxn *u) { Uint16 a = pop16(u->src), b = pop16(u->src); push16(u->src, b | a); }
static void op_eor16(Uxn *u) { Uint16 a = pop16(u->src), b = pop16(u->src); push16(u->src, b ^ a); }
static void op_sft16(Uxn *u) { Uint8 a = pop8(u->src); Uint16 b = pop16(u->src); push16(u->src, b >> (a & 0x0f) << ((a & 0xf0) >> 4)); }

static void (*ops[])(Uxn *u) = {
	op_brk, op_lit, op_nop, op_pop, op_dup, op_swp, op_ovr, op_rot,
	op_equ, op_neq, op_gth, op_lth, op_jmp, op_jnz, op_jsr, op_sth, 
	op_pek, op_pok, op_ldr, op_str, op_lda, op_sta, op_dei, op_deo,
	op_add, op_sub, op_mul, op_div, op_and, op_ora, op_eor, op_sft,
	/* 16-bit */
	op_brk,   op_lit16, op_nop,   op_pop16, op_dup16, op_swp16, op_ovr16, op_rot16,
	op_equ16, op_neq16, op_gth16, op_lth16, op_jmp16, op_jnz16, op_jsr16, op_sth16, 
	op_pek16, op_pok16, op_ldr16, op_str16, op_lda16, op_sta16, op_dei16, op_deo16, 
	op_add16, op_sub16, op_mul16, op_div16, op_and16, op_ora16, op_eor16, op_sft16
};

/* clang-format on */

#pragma mark - Core

int
uxn_eval(Uxn *u, Uint16 vec)
{
	if(u->dev[0].dat[0xf])
		return 0;
	u->ram.ptr = vec;
	if(u->wst.ptr > 0xf8) u->wst.ptr = 0xf8;
	while(u->ram.ptr) {
		Uint8 instr = u->ram.dat[u->ram.ptr++];
		/* Return Mode */
		if(instr & MODE_RETURN) {
			u->src = &u->rst;
			u->dst = &u->wst;
		} else {
			u->src = &u->wst;
			u->dst = &u->rst;
		}
		/* Keep Mode */
		if(instr & MODE_KEEP) {
			pop8 = pop8_keep;
			u->src->kptr = u->src->ptr;
		} else {
			pop8 = pop8_nokeep;
		}
		(*ops[instr & 0x3f])(u);
		if(u->wst.error)
			return uxn_halt(u, u->wst.error, "Working-stack", instr);
		if(u->rst.error)
			return uxn_halt(u, u->rst.error, "Return-stack", instr);
	}
	return 1;
}

int
uxn_boot(Uxn *u)
{
	unsigned int i;
	char *cptr = (char *)u;
	for(i = 0; i < sizeof(*u); i++)
		cptr[i] = 0;
	return 1;
}

Device *
uxn_port(Uxn *u, Uint8 id, char *name, void (*talkfn)(Device *d, Uint8 b0, Uint8 w))
{
	Device *d = &u->dev[id];
	d->addr = id * 0x10;
	d->u = u;
	d->mem = u->ram.dat;
	d->talk = talkfn;
	(void)name;
	return d;
}
