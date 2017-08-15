	switch (t->back) {
	default: Uerror("bad return move");
	case  0: goto R999; /* nothing to undo */

		 /* PROC :init: */

	case 3: /* STATE 1 */
		;
		;
		delproc(0, now._nr_pr-1);
		;
		goto R999;

	case 4: /* STATE 2 */
		;
		p_restor(II);
		;
		;
		goto R999;

		 /* PROC supervisor */

	case 5: /* STATE 1 */
		;
		((P0 *)this)->i = trpt->bup.oval;
		;
		goto R999;

	case 6: /* STATE 2 */
		;
		((P0 *)this)->random = trpt->bup.oval;
		;
		goto R999;

	case 7: /* STATE 4 */
		;
		((P0 *)this)->random = trpt->bup.oval;
		;
		goto R999;

	case 8: /* STATE 6 */
		;
		((P0 *)this)->random = trpt->bup.oval;
		;
		goto R999;

	case 9: /* STATE 8 */
		;
		((P0 *)this)->random = trpt->bup.oval;
		;
		goto R999;

	case 10: /* STATE 10 */
		;
		((P0 *)this)->random = trpt->bup.oval;
		;
		goto R999;

	case 11: /* STATE 15 */
		;
		now.moleposition = trpt->bup.oval;
		;
		goto R999;
;
		;
		
	case 13: /* STATE 17 */
		;
		((P0 *)this)->_1_1_1_target = trpt->bup.oval;
		;
		goto R999;

	case 14: /* STATE 19 */
		;
		((P0 *)this)->_1_1_1_target = trpt->bup.oval;
		;
		goto R999;

	case 15: /* STATE 21 */
		;
		((P0 *)this)->_1_1_1_target = trpt->bup.oval;
		;
		goto R999;

	case 16: /* STATE 23 */
		;
		((P0 *)this)->_1_1_1_target = trpt->bup.oval;
		;
		goto R999;

	case 17: /* STATE 25 */
		;
		((P0 *)this)->_1_1_1_target = trpt->bup.oval;
		;
		goto R999;

	case 18: /* STATE 27 */
		;
		((P0 *)this)->_1_1_1_target = trpt->bup.oval;
		;
		goto R999;

	case 19: /* STATE 29 */
		;
		((P0 *)this)->_1_1_1_target = trpt->bup.oval;
		;
		goto R999;

	case 20: /* STATE 33 */
		;
		now.molecaught = trpt->bup.ovals[1];
	/* 0 */	((P0 *)this)->_1_1_1_target = trpt->bup.ovals[0];
		;
		;
		ungrab_ints(trpt->bup.ovals, 2);
		goto R999;

	case 21: /* STATE 35 */
		;
		now.molecaught = trpt->bup.ovals[1];
	/* 0 */	((P0 *)this)->_1_1_1_target = trpt->bup.ovals[0];
		;
		;
		ungrab_ints(trpt->bup.ovals, 2);
		goto R999;

	case 22: /* STATE 37 */
		;
		now.molecaught = trpt->bup.oval;
		;
		goto R999;

	case 23: /* STATE 56 */
		;
		((P0 *)this)->i = trpt->bup.ovals[1];
		now.moleposition = trpt->bup.ovals[0];
		;
		ungrab_ints(trpt->bup.ovals, 2);
		goto R999;

	case 24: /* STATE 56 */
		;
		((P0 *)this)->i = trpt->bup.oval;
		;
		goto R999;

	case 25: /* STATE 56 */
		;
		((P0 *)this)->i = trpt->bup.ovals[1];
		now.moleposition = trpt->bup.ovals[0];
		;
		ungrab_ints(trpt->bup.ovals, 2);
		goto R999;

	case 26: /* STATE 56 */
		;
		((P0 *)this)->i = trpt->bup.ovals[1];
		now.moleposition = trpt->bup.ovals[0];
		;
		ungrab_ints(trpt->bup.ovals, 2);
		goto R999;

	case 27: /* STATE 56 */
		;
		((P0 *)this)->i = trpt->bup.ovals[1];
		now.moleposition = trpt->bup.ovals[0];
		;
		ungrab_ints(trpt->bup.ovals, 2);
		goto R999;
;
		;
		;
		;
		
	case 30: /* STATE 63 */
		;
		p_restor(II);
		;
		;
		goto R999;
	}

