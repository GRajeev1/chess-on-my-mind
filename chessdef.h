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
