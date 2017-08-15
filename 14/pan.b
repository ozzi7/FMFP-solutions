	switch (t->back) {
	default: Uerror("bad return move");
	case  0: goto R999; /* nothing to undo */

		 /* CLAIM ltl_0 */
;
		;
		;
		;
		
	case 5: /* STATE 11 */
		;
		p_restor(II);
		;
		;
		goto R999;

		 /* PROC philosopher */
;
		;
		;
		;
		
	case 8: /* STATE 5 */
		;
		now.fork[ Index((((P1 *)this)->p%5), 5) ] = trpt->bup.oval;
		;
		goto R999;

	case 9: /* STATE 10 */
		;
		now.fork[ Index(((((P1 *)this)->p+1)%5), 5) ] = trpt->bup.oval;
		;
		goto R999;
;
		;
		
	case 11: /* STATE 16 */
		;
		now.fork[ Index(((((P1 *)this)->p+1)%5), 5) ] = trpt->bup.oval;
		;
		goto R999;

	case 12: /* STATE 21 */
		;
		now.fork[ Index((((P1 *)this)->p%5), 5) ] = trpt->bup.oval;
		;
		goto R999;

	case 13: /* STATE 27 */
		;
		now.eats[ Index(((P1 *)this)->p, 5) ] = trpt->bup.oval;
		;
		goto R999;
;
		;
		
	case 15: /* STATE 29 */
		;
		now.eats[ Index(((P1 *)this)->p, 5) ] = trpt->bup.oval;
		;
		goto R999;
;
		;
		
	case 17: /* STATE 32 */
		;
		now.fork[ Index((((P1 *)this)->p%5), 5) ] = trpt->bup.oval;
		;
		goto R999;

	case 18: /* STATE 36 */
		;
		now.fork[ Index(((((P1 *)this)->p+1)%5), 5) ] = trpt->bup.oval;
		;
		goto R999;
;
		;
		
	case 20: /* STATE 41 */
		;
		now.fork[ Index(((((P1 *)this)->p+1)%5), 5) ] = trpt->bup.oval;
		;
		goto R999;

	case 21: /* STATE 45 */
		;
		now.fork[ Index((((P1 *)this)->p%5), 5) ] = trpt->bup.oval;
		;
		goto R999;

	case 22: /* STATE 54 */
		;
		p_restor(II);
		;
		;
		goto R999;

		 /* PROC :init: */
;
		;
		;
		;
		
	case 25: /* STATE 4 */
		;
		((P0 *)this)->i = trpt->bup.oval;
		;
		delproc(0, now._nr_pr-1);
		;
		goto R999;

	case 26: /* STATE 6 */
		;
	/* 0 */	((P0 *)this)->i = trpt->bup.oval;
		;
		;
		goto R999;

	case 27: /* STATE 11 */
		;
		p_restor(II);
		;
		;
		goto R999;
	}

