
/* All function prototypes are stored here */

/* Functions used for board representation */
void BoardInit();
void HTableInit();
int hash_rand();
void set_hash();
BOOL in_check(int s);
BOOL attack(int sq, int s);
void gen();
void gen_caps();
void gen_push(int from, int to, int bits);
void gen_promote(int from, int to, int bits);
BOOL makemove(move_bytes m);
void takeback();

/* Functions used to access move book(s) */
void open_book();
void close_book();
int book_move();
BOOL book_match(char *s1, char *s2);

/* Functions for the alpha-beta and quiescence search */
void think(int output);
int search(int alpha, int beta, int depth);
int quiesce(int alpha, int beta);
int reps();
void sort_pv();
void sort(int from);
void checkup();

/* Functions for score evaluation*/
int eval();
int eval_light_pawn(int sq);
int eval_dark_pawn(int sq);
int eval_light_king(int sq);
int eval_lkp(int f);
int eval_dark_king(int sq);
int eval_dkp(int f);

/* The main. The main, here, plays a middle-man role. It is simply used to access the xboard() function, which implements the engine written above in the Xboard GUI that is included with most Ubuntu distributions.*/

int get_ms();
int main();
int parse_move(char *s);
char *move_str(move_bytes m);
void print_board();
void xboard();
void print_result();
void bench();

#define _CRT_SECURE_NO_DEPRECATE
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#if defined(_WIN32) || defined(_WIN64)
#include <windows.h>
#include <conio.h>
#include <sys/timeb.h>
struct _timeb tv;
#else
#include <sys/time.h>
struct timeval tv;
struct timezone tz;
#endif
typedef unsigned long long u64;
typedef unsigned long u32;
typedef int Move;

#define PAWN 1
#define KNIGHT 2
#define KING 3
#define ENP 4
#define BISHOP 5
#define ROOK 6
#define QUEEN 7

#define CNODES 0xFFFF
const int pval[] = {0, 100, 290, 0, 100, 310, 500, 950};
const int pawnrun[] = {0, 0, 1, 8, 16, 32, 64, 128};

#define FROM(x) ((x) & 63)
#define TO(x) (((x) >> 6) & 63)
#define PROM(x) (((x) >> 12) & 7)
#define PIECE(x) (((x) >> 15) & 7)  
#define ONMV(x) (((x) >> 18) & 1)
#define CAP(x) (((x) >> 19) & 7)

#define _TO(x) ((x) << 6)
#define _PROM(x) ((x) << 12)
#define _PIECE(x) ((x) << 15)
#define _ONMV(x) ((x) << 18)
#define _CAP(x) ((x) << 19)
#define PREMOVE(f, p) ((f) | _ONMV(c) | _PIECE(p))

#define RATT1(f) rays[((f) << 7) | key000(BOARD, f)]
#define RATT2(f) rays[((f) << 7) | key090(BOARD, f) | 0x2000]
#define BATT3(f) rays[((f) << 7) | key045(BOARD, f) | 0x4000]
#define BATT4(f) rays[((f) << 7) | key135(BOARD, f) | 0x6000]
#define RXRAY1(f) rays[((f) << 7) | key000(BOARD, f) | 0x8000]
#define RXRAY2(f) rays[((f) << 7) | key090(BOARD, f) | 0xA000]
#define BXRAY3(f) rays[((f) << 7) | key045(BOARD, f) | 0xC000]
#define BXRAY4(f) rays[((f) << 7) | key135(BOARD, f) | 0xE000]

#define ROCC1(f) (RATT1(f) & BOARD)
#define ROCC2(f) (RATT2(f) & BOARD)
#define BOCC3(f) (BATT3(f) & BOARD)
#define BOCC4(f) (BATT4(f) & BOARD)
#define RMOVE1(f) (RATT1(f) & (~BOARD))
#define RMOVE2(f) (RATT2(f) & (~BOARD))
#define BMOVE3(f) (BATT3(f) & (~BOARD))
#define BMOVE4(f) (BATT4(f) & (~BOARD))
#define RCAP1(f, c) (RATT1(f) & colorb[(c)^1])
#define RCAP2(f, c) (RATT2(f) & colorb[(c)^1])
#define BCAP3(f, c) (BATT3(f) & colorb[(c)^1])
#define BCAP4(f, c) (BATT4(f) & colorb[(c)^1])
#define ROCC(f) (ROCC1(f) | ROCC2(f))
#define BOCC(f) (BOCC3(f) | BOCC4(f))
#define RMOVE(f) (RMOVE1(f) | RMOVE2(f))
#define BMOVE(f) (BMOVE3(f) | BMOVE4(f))
#define RCAP(f, c) (ROCC(f) & colorb[(c)^1])
#define BCAP(f, c) (BOCC(f) & colorb[(c)^1])

#define SHORTMOVE(x) ((x) & ((x)^BOARD))
#define SHORTOCC(x) ((x) & BOARD)
#define SHORTCAP(x, c) ((x) & colorb[(c)^1])

#define NMOVE(x) (SHORTMOVE(nmoves[x]))
#define KMOVE(x) (SHORTMOVE(kmoves[x]))
#define PMOVE(x, c) (pmoves[(x) | ((c)<<6)] & (~BOARD))
#define NOCC(x) (SHORTOCC(nmoves[x]))
#define KOCC(x) (SHORTOCC(kmoves[x]))
#define POCC(x, c) (pcaps[(x) | ((c)<<6)] & BOARD)
#define NCAP(x, c) (SHORTCAP(nmoves[x], (c)))
#define KCAP(x, c) (SHORTCAP(kmoves[x], (c)))
#define PCAP(x, c) (pcaps[(x) | ((c)<<6)] & colorb[(c)^1])
#define PCA3(x, c) (pcaps[(x) | ((c)<<6) | 128] & (colorb[(c)^1] | ((BIT[ENPASS]) & (c ? 0xFF0000LL : 0xFF0000000000LL))))
#define PCA4(x, c) (pcaps[(x) | ((c)<<6) | 256] & (colorb[(c)^1] | ((BIT[ENPASS]) & (c ? 0xFF0000LL : 0xFF0000000000LL))))

#define RANK(x, y) (((x) & 0x38) == (y))
#define TEST(f, b) (BIT[f] & (b))
#define ENPASS (flags & 63)
#define CASTLE (flags & 960)
#define COUNT (count & 0x3FF)

#define HSIZEB 0x200000
#define HMASKB 0x1FFFFF
#define HINVB 0xFFFFFFFFFFE00000LL

#define HSIZEP 0x400000
#define HMASKP 0x3FFFFF
#define HINVP 0xFFFFFFFFFFC00000LL

#ifndef DEFS_H
#define DEFS_H

typedef unsigned long long Sp64;

#define NAME "Treasure Chess v0.1a"
#define BRD_SQ_NUM 120
#define MAXGAMEMOVES 2048

enum { EMPTY, wP, wN, wB, wR, wQ, wK, bP, bN, bB, bR, bQ, bK };
enum { C_A, C_B, _C, C_D, C_E, C_F, C_H, C_NONE };
enum { R1, R2, R3, R4, R5, R6, R7, R8, RIn };
enum { WHITE, BLACK, BOTH };
enum {FALSE, TRUE}; 
enum { WKCA=1, WQCA=2, BKCA=4, BQCA=8};
enum {
	A1=21, B1, C1, D1, E1, F1, G1, H1,
	A2=31, B2, C2, D2, E2, F2, G2, H2,
	A3=41, B3, C3, D3, E3, F3, G3, H3,
	A4=51, B4, C4, D4, E4, F4, G4, H4, 
	A5=61, B5, C5, D5, E5, F5, G5, H5, 
	A6=71, B6, C6, D6, E6, F6, G6, H6, 
	A7=81, B7, C7, D7, E7, F7, G7, H7, 
	A8=91, B8, C8, D8, E8, F8, G8, H8, OFF_BOARD};
	
typedef struct {
	int move;
	int CasPerm;
	int enPas;
	int count;
	Sp64 posKey;
} S_UNDO;

typedef struct {
	int pieces [BRD_SQ_NUM];
	Sp64 pawns[3];
	
	int KingSq[2];
	int side;
	int enPas;
	int count;
	int ply;
	int hisPly;
	Sp64 posKey;
	int pcenum[13];
	int bigPce[3];
	int majPce[3];
	int smlPce[3];
	int CasPerm;
	S_UNDO history[MAXGAMEMOVES];
} S_BOARD;


#endif 
