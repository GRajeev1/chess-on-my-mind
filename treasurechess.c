#include <stdio.h>
#include <string.h>
#include "chessdefs.h"

#include <setjmp.h>
jmp_buf env;
BOOL SearchEnd;


*/ ONLY THE NEGAMAX ALGORITHM IS DONE SO FAR. BOARD REPRESENTATION STILL UNDERWAY */

void check(int output)
{
	int i, j, x;

	pv[0][0].u = Bmove();
	if (pv[0][0].u != -1)
		return;

	SearchEnd = FALSE;
	setjmp(env);
	if (SearchEnd) {
		
		while (ply)
			*/ takeback() function to be implemented here. It hasn't been written yet */ ;
		return;
	}

	start_time = get_ms();
	stop_time = start_time + max_time;

	ply = 0;
	nodes = 0;

	memset(pv, 0, sizeof(pv));
	memset(history, 0, sizeof(history));
	for (i = 1; i <= max_depth; ++i) {
		isPV = TRUE;
		x = search(-10000, 10000, i);
		printf("%d %d %d %d",
					i, x, (get_ms() - start_time) / 10, nodes);
		if (output) {
			for (j = 0; j < pv_length[0]; ++j)
				printf(" %s", move_str(pv[0][j].b));
			printf("\n");
			fflush(stdout);
		}
		if (x > 9000 || x < -9000)
			break;
	}
}

*/ GENERIC NEGAMAX ALGORITHM IMPLEMENTATION IN C. */
int search(int alpha, int beta, int depth)
{
	int i, j, x;
	BOOL c, f;
	if (!depth)
		return quiesce(alpha,beta);
	++nodes;

	if ((nodes & 1023) == 0)
		checkup();

	pv_length[ply] = ply;

	if (ply && reps())
		return 0;

	if (ply >= PlyLim - 1)
		return eval();
	if (hply >= MovRecord - 1)
		return eval();
      */ Eval function hasn't been written yet */
	c = in_check(side);
	if (c)
		++depth;
	gen();
	if (isPV) 
		sort_pv();
	f = FALSE;

	for (i = first_move[ply]; i < first_move[ply + 1]; ++i) {
		sort(i);
		if (!makemove(gen_dat[i].m.b))
			continue;
		f = TRUE;
		x = -search(-beta, -alpha, depth - 1);
		takeback();
		if (x > alpha) {

			history[(int)gen_dat[i].m.b.from][(int)gen_dat[i].m.b.to] += depth;
			if (x >= beta)
				return beta;
			alpha = x;

			/* update the PV */
			pv[ply][ply] = gen_dat[i].m;
			for (j = ply + 1; j < pv_length[ply + 1]; ++j)
				pv[ply][j] = pv[ply + 1][j];
			pv_length[ply] = pv_length[ply + 1];
		}
	}

	/* no legal moves? then we're in checkmate or stalemate */
	if (!f) {
		if (c)
			return -10000 + ply;
		else
			return 0;
	}

	/* fifty move draw rule */
	if (fifty >= 100)
		return 0;
	return alpha;
}

int quiesce(int alpha,int beta)
{
	int i, j, x;

	++nodes;

	if ((nodes & 1023) == 0)
		checkup();

	pv_length[ply] = ply;

	
	if (ply >= PlyLim - 1)
		return eval();

	if (hply >= MovRecord - 1)
		return eval();

	/* Evaluation function not written yet.*/
	x = eval();
	if (x >= beta)
		return beta;
	if (x > alpha)
		alpha = x;

	gen_caps();
	if (isPV)  /* are we following the PV? */
		sort_pv();

	/* loop through the moves */
	for (i = first_move[ply]; i < first_move[ply + 1]; ++i) {
		sort(i);
		if (!makemove(gen_dat[i].m.b))
			continue;
		x = -quiesce(-beta, -alpha);
		takeback();
		if (x > alpha) {
			if (x >= beta)
				return beta;
			alpha = x;

			/* update the PV */
			pv[ply][ply] = gen_dat[i].m;
			for (j = ply + 1; j < pv_length[ply + 1]; ++j)
				pv[ply][j] = pv[ply + 1][j];
			pv_length[ply] = pv_length[ply + 1];
		}
	}
	return alpha;
}


/* reps() returns the number of times the current position
   has been repeated. It compares the current value of hash
   to previous values. */

int reps()
{
	int i;
	int r = 0;

	for (i = hply - fifty; i < hply; ++i)
		if (hist_dat[i].hash == hash)
			++r;
	return r;
}

/* Checks for a Principal Variation move. i.e : A move that is the best possible move in maximum number of best case scenarios in the next few rounds of the search tree */

void sort_pv()
{
	int i;

	isPV = FALSE;
	for(i = first_move[ply]; i < first_move[ply + 1]; ++i)
		if (gen_dat[i].m.u == pv[0][ply].u) {
			isPV = TRUE;
			gen_dat[i].score += 10000000;
			return;
		}
}


void sort(int from)
{
	int i;
	int bs;  /* best score */
	int bi;  /* best i */
	gen_t g;

	bs = -1;
	bi = from;
	for (i = from; i < first_move[ply + 1]; ++i)
		if (gen_dat[i].score > bs) {
			bs = gen_dat[i].score;
			bi = i;
		}
	g = gen_dat[from];
	gen_dat[from] = gen_dat[bi];
	gen_dat[bi] = g;
}


/* checkup() is called once in a while during the search. */

void checkup()
{
	/
	if (get_ms() >= stop_time) {
		SearchEnd = TRUE;
		longjmp(env, 0);
	}
}

