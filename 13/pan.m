#define rand	pan_rand
#if defined(HAS_CODE) && defined(VERBOSE)
	#ifdef BFS_PAR
		bfs_printf("Pr: %d Tr: %d\n", II, t->forw);
	#else
		cpu_printf("Pr: %d Tr: %d\n", II, t->forw);
	#endif
#endif
	switch (t->forw) {
	default: Uerror("bad forward move");
	case 0:	/* if without executable clauses */
		continue;
	case 1: /* generic 'goto' or 'skip' */
		IfNotBlocked
		_m = 3; goto P999;
	case 2: /* generic 'else' */
		IfNotBlocked
		if (trpt->o_pm&1) continue;
		_m = 3; goto P999;

		 /* PROC :init: */
	case 3: /* STATE 1 - mole.pml:56 - [(run supervisor())] (0:0:0 - 1) */
		IfNotBlocked
		reached[1][1] = 1;
		if (!(addproc(II, 0)))
			continue;
		_m = 3; goto P999; /* 0 */
	case 4: /* STATE 2 - mole.pml:57 - [-end-] (0:0:0 - 1) */
		IfNotBlocked
		reached[1][2] = 1;
		if (!delproc(1, II)) continue;
		_m = 3; goto P999; /* 0 */

		 /* PROC supervisor */
	case 5: /* STATE 1 - mole.pml:37 - [i = 0] (0:0:1 - 1) */
		IfNotBlocked
		reached[0][1] = 1;
		(trpt+1)->bup.oval = ((P0 *)this)->i;
		((P0 *)this)->i = 0;
#ifdef VAR_RANGES
		logval("supervisor:i", ((P0 *)this)->i);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 6: /* STATE 2 - mole.pml:39 - [random = 1] (0:0:1 - 1) */
		IfNotBlocked
		reached[0][2] = 1;
		(trpt+1)->bup.oval = ((P0 *)this)->random;
		((P0 *)this)->random = 1;
#ifdef VAR_RANGES
		logval("supervisor:random", ((P0 *)this)->random);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 7: /* STATE 4 - mole.pml:40 - [random = 2] (0:0:1 - 1) */
		IfNotBlocked
		reached[0][4] = 1;
		(trpt+1)->bup.oval = ((P0 *)this)->random;
		((P0 *)this)->random = 2;
#ifdef VAR_RANGES
		logval("supervisor:random", ((P0 *)this)->random);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 8: /* STATE 6 - mole.pml:41 - [random = 3] (0:0:1 - 1) */
		IfNotBlocked
		reached[0][6] = 1;
		(trpt+1)->bup.oval = ((P0 *)this)->random;
		((P0 *)this)->random = 3;
#ifdef VAR_RANGES
		logval("supervisor:random", ((P0 *)this)->random);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 9: /* STATE 8 - mole.pml:42 - [random = 4] (0:0:1 - 1) */
		IfNotBlocked
		reached[0][8] = 1;
		(trpt+1)->bup.oval = ((P0 *)this)->random;
		((P0 *)this)->random = 4;
#ifdef VAR_RANGES
		logval("supervisor:random", ((P0 *)this)->random);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 10: /* STATE 10 - mole.pml:43 - [random = 5] (0:0:1 - 1) */
		IfNotBlocked
		reached[0][10] = 1;
		(trpt+1)->bup.oval = ((P0 *)this)->random;
		((P0 *)this)->random = 5;
#ifdef VAR_RANGES
		logval("supervisor:random", ((P0 *)this)->random);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 11: /* STATE 15 - mole.pml:45 - [moleposition = random] (0:0:1 - 11) */
		IfNotBlocked
		reached[0][15] = 1;
		(trpt+1)->bup.oval = now.moleposition;
		now.moleposition = ((P0 *)this)->random;
#ifdef VAR_RANGES
		logval("moleposition", now.moleposition);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 12: /* STATE 16 - mole.pml:47 - [((i<6))] (0:0:0 - 1) */
		IfNotBlocked
		reached[0][16] = 1;
		if (!((((P0 *)this)->i<6)))
			continue;
		_m = 3; goto P999; /* 0 */
	case 13: /* STATE 17 - mole.pml:7 - [target = 0] (0:0:1 - 1) */
		IfNotBlocked
		reached[0][17] = 1;
		(trpt+1)->bup.oval = ((P0 *)this)->_1_1_1_target;
		((P0 *)this)->_1_1_1_target = 0;
#ifdef VAR_RANGES
		logval("supervisor:target", ((P0 *)this)->_1_1_1_target);
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 14: /* STATE 18 - mole.pml:8 - [((i==0))] (38:0:1 - 1) */
		IfNotBlocked
		reached[0][18] = 1;
		if (!((((P0 *)this)->i==0)))
			continue;
		/* merge: target = 2(0, 19, 38) */
		reached[0][19] = 1;
		(trpt+1)->bup.oval = ((P0 *)this)->_1_1_1_target;
		((P0 *)this)->_1_1_1_target = 2;
#ifdef VAR_RANGES
		logval("supervisor:target", ((P0 *)this)->_1_1_1_target);
#endif
		;
		/* merge: .(goto)(0, 31, 38) */
		reached[0][31] = 1;
		;
		_m = 3; goto P999; /* 2 */
	case 15: /* STATE 20 - mole.pml:9 - [((i==1))] (38:0:1 - 1) */
		IfNotBlocked
		reached[0][20] = 1;
		if (!((((P0 *)this)->i==1)))
			continue;
		/* merge: target = 3(0, 21, 38) */
		reached[0][21] = 1;
		(trpt+1)->bup.oval = ((P0 *)this)->_1_1_1_target;
		((P0 *)this)->_1_1_1_target = 3;
#ifdef VAR_RANGES
		logval("supervisor:target", ((P0 *)this)->_1_1_1_target);
#endif
		;
		/* merge: .(goto)(0, 31, 38) */
		reached[0][31] = 1;
		;
		_m = 3; goto P999; /* 2 */
	case 16: /* STATE 22 - mole.pml:10 - [((i==2))] (38:0:1 - 1) */
		IfNotBlocked
		reached[0][22] = 1;
		if (!((((P0 *)this)->i==2)))
			continue;
		/* merge: target = 4(0, 23, 38) */
		reached[0][23] = 1;
		(trpt+1)->bup.oval = ((P0 *)this)->_1_1_1_target;
		((P0 *)this)->_1_1_1_target = 4;
#ifdef VAR_RANGES
		logval("supervisor:target", ((P0 *)this)->_1_1_1_target);
#endif
		;
		/* merge: .(goto)(0, 31, 38) */
		reached[0][31] = 1;
		;
		_m = 3; goto P999; /* 2 */
	case 17: /* STATE 24 - mole.pml:11 - [((i==3))] (38:0:1 - 1) */
		IfNotBlocked
		reached[0][24] = 1;
		if (!((((P0 *)this)->i==3)))
			continue;
		/* merge: target = 2(0, 25, 38) */
		reached[0][25] = 1;
		(trpt+1)->bup.oval = ((P0 *)this)->_1_1_1_target;
		((P0 *)this)->_1_1_1_target = 2;
#ifdef VAR_RANGES
		logval("supervisor:target", ((P0 *)this)->_1_1_1_target);
#endif
		;
		/* merge: .(goto)(0, 31, 38) */
		reached[0][31] = 1;
		;
		_m = 3; goto P999; /* 2 */
	case 18: /* STATE 26 - mole.pml:12 - [((i==4))] (38:0:1 - 1) */
		IfNotBlocked
		reached[0][26] = 1;
		if (!((((P0 *)this)->i==4)))
			continue;
		/* merge: target = 3(0, 27, 38) */
		reached[0][27] = 1;
		(trpt+1)->bup.oval = ((P0 *)this)->_1_1_1_target;
		((P0 *)this)->_1_1_1_target = 3;
#ifdef VAR_RANGES
		logval("supervisor:target", ((P0 *)this)->_1_1_1_target);
#endif
		;
		/* merge: .(goto)(0, 31, 38) */
		reached[0][31] = 1;
		;
		_m = 3; goto P999; /* 2 */
	case 19: /* STATE 28 - mole.pml:13 - [((i==5))] (38:0:1 - 1) */
		IfNotBlocked
		reached[0][28] = 1;
		if (!((((P0 *)this)->i==5)))
			continue;
		/* merge: target = 4(0, 29, 38) */
		reached[0][29] = 1;
		(trpt+1)->bup.oval = ((P0 *)this)->_1_1_1_target;
		((P0 *)this)->_1_1_1_target = 4;
#ifdef VAR_RANGES
		logval("supervisor:target", ((P0 *)this)->_1_1_1_target);
#endif
		;
		/* merge: .(goto)(0, 31, 38) */
		reached[0][31] = 1;
		;
		_m = 3; goto P999; /* 2 */
	case 20: /* STATE 32 - mole.pml:16 - [((moleposition==target))] (55:0:2 - 1) */
		IfNotBlocked
		reached[0][32] = 1;
		if (!((now.moleposition==((P0 *)this)->_1_1_1_target)))
			continue;
		/* dead 1: _1_1_1_target */  (trpt+1)->bup.ovals = grab_ints(2);
		(trpt+1)->bup.ovals[0] = ((P0 *)this)->_1_1_1_target;
#ifdef HAS_CODE
		if (!readtrail)
#endif
			((P0 *)this)->_1_1_1_target = 0;
		/* merge: molecaught = 1(0, 33, 55) */
		reached[0][33] = 1;
		(trpt+1)->bup.ovals[1] = ((int)now.molecaught);
		now.molecaught = 1;
#ifdef VAR_RANGES
		logval("molecaught", ((int)now.molecaught));
#endif
		;
		/* merge: .(goto)(0, 39, 55) */
		reached[0][39] = 1;
		;
		_m = 3; goto P999; /* 2 */
	case 21: /* STATE 34 - mole.pml:17 - [(((moleposition!=target)&&(molecaught==0)))] (55:0:2 - 1) */
		IfNotBlocked
		reached[0][34] = 1;
		if (!(((now.moleposition!=((P0 *)this)->_1_1_1_target)&&(((int)now.molecaught)==0))))
			continue;
		/* dead 1: _1_1_1_target */  (trpt+1)->bup.ovals = grab_ints(2);
		(trpt+1)->bup.ovals[0] = ((P0 *)this)->_1_1_1_target;
#ifdef HAS_CODE
		if (!readtrail)
#endif
			((P0 *)this)->_1_1_1_target = 0;
		/* merge: molecaught = 0(0, 35, 55) */
		reached[0][35] = 1;
		(trpt+1)->bup.ovals[1] = ((int)now.molecaught);
		now.molecaught = 0;
#ifdef VAR_RANGES
		logval("molecaught", ((int)now.molecaught));
#endif
		;
		/* merge: .(goto)(0, 39, 55) */
		reached[0][39] = 1;
		;
		_m = 3; goto P999; /* 2 */
	case 22: /* STATE 37 - mole.pml:18 - [molecaught = 1] (0:0:1 - 1) */
		IfNotBlocked
		reached[0][37] = 1;
		(trpt+1)->bup.oval = ((int)now.molecaught);
		now.molecaught = 1;
#ifdef VAR_RANGES
		logval("molecaught", ((int)now.molecaught));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 23: /* STATE 41 - mole.pml:24 - [((moleposition==1))] (60:0:2 - 1) */
		IfNotBlocked
		reached[0][41] = 1;
		if (!((now.moleposition==1)))
			continue;
		/* merge: moleposition = 2(60, 42, 60) */
		reached[0][42] = 1;
		(trpt+1)->bup.ovals = grab_ints(2);
		(trpt+1)->bup.ovals[0] = now.moleposition;
		now.moleposition = 2;
#ifdef VAR_RANGES
		logval("moleposition", now.moleposition);
#endif
		;
		/* merge: .(goto)(60, 54, 60) */
		reached[0][54] = 1;
		;
		/* merge: i = (i+1)(60, 56, 60) */
		reached[0][56] = 1;
		(trpt+1)->bup.ovals[1] = ((P0 *)this)->i;
		((P0 *)this)->i = (((P0 *)this)->i+1);
#ifdef VAR_RANGES
		logval("supervisor:i", ((P0 *)this)->i);
#endif
		;
		/* merge: .(goto)(0, 61, 60) */
		reached[0][61] = 1;
		;
		_m = 3; goto P999; /* 4 */
	case 24: /* STATE 54 - mole.pml:32 - [.(goto)] (0:60:1 - 7) */
		IfNotBlocked
		reached[0][54] = 1;
		;
		/* merge: i = (i+1)(60, 56, 60) */
		reached[0][56] = 1;
		(trpt+1)->bup.oval = ((P0 *)this)->i;
		((P0 *)this)->i = (((P0 *)this)->i+1);
#ifdef VAR_RANGES
		logval("supervisor:i", ((P0 *)this)->i);
#endif
		;
		/* merge: .(goto)(0, 61, 60) */
		reached[0][61] = 1;
		;
		_m = 3; goto P999; /* 2 */
	case 25: /* STATE 43 - mole.pml:25 - [((moleposition==5))] (60:0:2 - 1) */
		IfNotBlocked
		reached[0][43] = 1;
		if (!((now.moleposition==5)))
			continue;
		/* merge: moleposition = 4(60, 44, 60) */
		reached[0][44] = 1;
		(trpt+1)->bup.ovals = grab_ints(2);
		(trpt+1)->bup.ovals[0] = now.moleposition;
		now.moleposition = 4;
#ifdef VAR_RANGES
		logval("moleposition", now.moleposition);
#endif
		;
		/* merge: .(goto)(60, 54, 60) */
		reached[0][54] = 1;
		;
		/* merge: i = (i+1)(60, 56, 60) */
		reached[0][56] = 1;
		(trpt+1)->bup.ovals[1] = ((P0 *)this)->i;
		((P0 *)this)->i = (((P0 *)this)->i+1);
#ifdef VAR_RANGES
		logval("supervisor:i", ((P0 *)this)->i);
#endif
		;
		/* merge: .(goto)(0, 61, 60) */
		reached[0][61] = 1;
		;
		_m = 3; goto P999; /* 4 */
	case 26: /* STATE 46 - mole.pml:28 - [moleposition = (moleposition+1)] (0:60:2 - 1) */
		IfNotBlocked
		reached[0][46] = 1;
		(trpt+1)->bup.ovals = grab_ints(2);
		(trpt+1)->bup.ovals[0] = now.moleposition;
		now.moleposition = (now.moleposition+1);
#ifdef VAR_RANGES
		logval("moleposition", now.moleposition);
#endif
		;
		/* merge: goto :b2(60, 47, 60) */
		reached[0][47] = 1;
		;
		/* merge: .(goto)(60, 54, 60) */
		reached[0][54] = 1;
		;
		/* merge: i = (i+1)(60, 56, 60) */
		reached[0][56] = 1;
		(trpt+1)->bup.ovals[1] = ((P0 *)this)->i;
		((P0 *)this)->i = (((P0 *)this)->i+1);
#ifdef VAR_RANGES
		logval("supervisor:i", ((P0 *)this)->i);
#endif
		;
		/* merge: .(goto)(0, 61, 60) */
		reached[0][61] = 1;
		;
		_m = 3; goto P999; /* 4 */
	case 27: /* STATE 48 - mole.pml:29 - [moleposition = (moleposition-1)] (0:60:2 - 1) */
		IfNotBlocked
		reached[0][48] = 1;
		(trpt+1)->bup.ovals = grab_ints(2);
		(trpt+1)->bup.ovals[0] = now.moleposition;
		now.moleposition = (now.moleposition-1);
#ifdef VAR_RANGES
		logval("moleposition", now.moleposition);
#endif
		;
		/* merge: goto :b2(60, 49, 60) */
		reached[0][49] = 1;
		;
		/* merge: .(goto)(60, 54, 60) */
		reached[0][54] = 1;
		;
		/* merge: i = (i+1)(60, 56, 60) */
		reached[0][56] = 1;
		(trpt+1)->bup.ovals[1] = ((P0 *)this)->i;
		((P0 *)this)->i = (((P0 *)this)->i+1);
#ifdef VAR_RANGES
		logval("supervisor:i", ((P0 *)this)->i);
#endif
		;
		/* merge: .(goto)(0, 61, 60) */
		reached[0][61] = 1;
		;
		_m = 3; goto P999; /* 4 */
	case 28: /* STATE 58 - mole.pml:52 - [((i==6))] (0:0:0 - 1) */
		IfNotBlocked
		reached[0][58] = 1;
		if (!((((P0 *)this)->i==6)))
			continue;
		_m = 3; goto P999; /* 0 */
	case 29: /* STATE 59 - mole.pml:52 - [assert((molecaught==1))] (0:0:0 - 1) */
		IfNotBlocked
		reached[0][59] = 1;
		spin_assert((((int)now.molecaught)==1), "(molecaught==1)", II, tt, t);
		_m = 3; goto P999; /* 0 */
	case 30: /* STATE 63 - mole.pml:54 - [-end-] (0:0:0 - 1) */
		IfNotBlocked
		reached[0][63] = 1;
		if (!delproc(1, II)) continue;
		_m = 3; goto P999; /* 0 */
	case  _T5:	/* np_ */
		if (!((!(trpt->o_pm&4) && !(trpt->tau&128))))
			continue;
		/* else fall through */
	case  _T2:	/* true */
		_m = 3; goto P999;
#undef rand
	}

