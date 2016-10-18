#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include "chessdefs.h"



FILE *Book;



void Bopen()
{
	srand(time(NULL));
	Book = fopen("book.txt", "r");
	if (!Book)
		printf("Opening book missing.\n");
}


void Bclose()
{
	if (Book)
		fclose(Book);
	Book = NULL;
}


int Bmove()
{
	char movecet[256];
	char book_movecet[256];
	int i, j, m, moves[50], occ[50], movec=0, TotCount=0;
	if (!Book || hply > 25)
		return -1;

	movecet[0] = '\0';
	j = 0;
	for (i = 0; i < hply; ++i)
		j += sprintf(movecet + j, "%s ", moves_str(hist_dat[i].m.b));

	fseek(Book, 0, SEEK_SET);
	while (fgets(book_movecet, 256, Book)) {
		if (BCheck(movecet, book_movecet)) {
			m = parse_moves(&book_movecet[strlen(movecet)]);
			if (m == -1)
				continue;
			m = gen_dat[m].m.u;
			for (j = 0; j < movec; ++j)
				if (moves[j] == m) {
					++count[j];
					break;
				}
			if (j == movec) {
				moves[movec] = m;
				count[movec] = 1;
				++movec;
			}
			++TotCount;
		}
	}
	if (movec == 0)
		return -1;

	j = rand() % TotCount;
	for (i = 0; i < movec; ++i) {
		j -= count[i];
		if (j < 0)
			return moves[i];
	}
	return -1; 
}




BOOL BCheck(char *s1, char *s2)
{
	int i;

	for (i = 0; i < (signed int)strlen(s1); ++i)
		if (s2[i] == '\0' || s2[i] != s1[i])
			return FALSE;
	return TRUE;
}
