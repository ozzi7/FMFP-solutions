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

		 /* CLAIM ltl_0 */
	case 3: /* STATE 1 - _spin_nvr.tmp:3 - [(!((eats[0]==1)))] (0:0:0 - 1) */
		
#if defined(VERI) && !defined(NP)
#if NCLAIMS>1
		{	static int reported1 = 0;
			if (verbose && !reported1)
			{	int nn = (int) ((Pclaim *)pptr(0))->_n;
				printf("depth %ld: Claim %s (%d), state %d (line %d)\n",
					depth, procname[spin_c_typ[nn]], nn, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported1 = 1;
				fflush(stdout);
		}	}
#else
		{	static int reported1 = 0;
			if (verbose && !reported1)
			{	printf("depth %d: Claim, state %d (line %d)\n",
					(int) depth, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported1 = 1;
				fflush(stdout);
		}	}
#endif
#endif
		reached[2][1] = 1;
		if (!( !((((int)now.eats[0])==1))))
			continue;
		_m = 3; goto P999; /* 0 */
	case 4: /* STATE 7 - _spin_nvr.tmp:8 - [(!((eats[0]==1)))] (0:0:0 - 1) */
		
#if defined(VERI) && !defined(NP)
#if NCLAIMS>1
		{	static int reported7 = 0;
			if (verbose && !reported7)
			{	int nn = (int) ((Pclaim *)pptr(0))->_n;
				printf("depth %ld: Claim %s (%d), state %d (line %d)\n",
					depth, procname[spin_c_typ[nn]], nn, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported7 = 1;
				fflush(stdout);
		}	}
#else
		{	static int reported7 = 0;
			if (verbose && !reported7)
			{	printf("depth %d: Claim, state %d (line %d)\n",
					(int) depth, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported7 = 1;
				fflush(stdout);
		}	}
#endif
#endif
		reached[2][7] = 1;
		if (!( !((((int)now.eats[0])==1))))
			continue;
		_m = 3; goto P999; /* 0 */
	case 5: /* STATE 11 - _spin_nvr.tmp:10 - [-end-] (0:0:0 - 1) */
		
#if defined(VERI) && !defined(NP)
#if NCLAIMS>1
		{	static int reported11 = 0;
			if (verbose && !reported11)
			{	int nn = (int) ((Pclaim *)pptr(0))->_n;
				printf("depth %ld: Claim %s (%d), state %d (line %d)\n",
					depth, procname[spin_c_typ[nn]], nn, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported11 = 1;
				fflush(stdout);
		}	}
#else
		{	static int reported11 = 0;
			if (verbose && !reported11)
			{	printf("depth %d: Claim, state %d (line %d)\n",
					(int) depth, (int) ((Pclaim *)pptr(0))->_p, src_claim[ (int) ((Pclaim *)pptr(0))->_p ]);
				reported11 = 1;
				fflush(stdout);
		}	}
#endif
#endif
		reached[2][11] = 1;
		if (!delproc(1, II)) continue;
		_m = 3; goto P999; /* 0 */

		 /* PROC philosopher */
	case 6: /* STATE 1 - philosophers_no_deadlock_starvation.pml.txt:65 - [printf('Philosopher %d is thinking.\\n',p)] (0:0:0 - 1) */
		IfNotBlocked
		reached[1][1] = 1;
		Printf("Philosopher %d is thinking.\n", ((P1 *)this)->p);
		_m = 3; goto P999; /* 0 */
	case 7: /* STATE 3 - philosophers_no_deadlock_starvation.pml.txt:79 - [(leftFirst)] (0:0:0 - 1) */
		IfNotBlocked
		reached[1][3] = 1;
		if (!(((int)((P1 *)this)->leftFirst)))
			continue;
		_m = 3; goto P999; /* 0 */
	case 8: /* STATE 4 - philosophers_no_deadlock_starvation.pml.txt:32 - [((fork[(p%5)]==0))] (13:0:1 - 1) */
		IfNotBlocked
		reached[1][4] = 1;
		if (!((((int)now.fork[ Index((((P1 *)this)->p%5), 5) ])==0)))
			continue;
		/* merge: fork[(p%5)] = 1(13, 5, 13) */
		reached[1][5] = 1;
		(trpt+1)->bup.oval = ((int)now.fork[ Index((((P1 *)this)->p%5), 5) ]);
		now.fork[ Index((((P1 *)this)->p%5), 5) ] = 1;
#ifdef VAR_RANGES
		logval("fork[(philosopher:p%5)]", ((int)now.fork[ Index((((P1 *)this)->p%5), 5) ]));
#endif
		;
		/* merge: printf('Philosopher %d picks up the left fork (Fork %d)\\n',p,(p%5))(13, 6, 13) */
		reached[1][6] = 1;
		Printf("Philosopher %d picks up the left fork (Fork %d)\n", ((P1 *)this)->p, (((P1 *)this)->p%5));
		_m = 3; goto P999; /* 2 */
	case 9: /* STATE 9 - philosophers_no_deadlock_starvation.pml.txt:41 - [((fork[((p+1)%5)]==0))] (30:0:1 - 1) */
		IfNotBlocked
		reached[1][9] = 1;
		if (!((((int)now.fork[ Index(((((P1 *)this)->p+1)%5), 5) ])==0)))
			continue;
		/* merge: fork[((p+1)%5)] = 1(30, 10, 30) */
		reached[1][10] = 1;
		(trpt+1)->bup.oval = ((int)now.fork[ Index(((((P1 *)this)->p+1)%5), 5) ]);
		now.fork[ Index(((((P1 *)this)->p+1)%5), 5) ] = 1;
#ifdef VAR_RANGES
		logval("fork[((philosopher:p+1)%5)]", ((int)now.fork[ Index(((((P1 *)this)->p+1)%5), 5) ]));
#endif
		;
		/* merge: printf('Philosopher %d picks up the right fork (Fork %d).\\n',p,((p+1)%5))(30, 11, 30) */
		reached[1][11] = 1;
		Printf("Philosopher %d picks up the right fork (Fork %d).\n", ((P1 *)this)->p, ((((P1 *)this)->p+1)%5));
		/* merge: .(goto)(0, 26, 30) */
		reached[1][26] = 1;
		;
		_m = 3; goto P999; /* 3 */
	case 10: /* STATE 14 - philosophers_no_deadlock_starvation.pml.txt:80 - [(!(leftFirst))] (0:0:0 - 1) */
		IfNotBlocked
		reached[1][14] = 1;
		if (!( !(((int)((P1 *)this)->leftFirst))))
			continue;
		_m = 3; goto P999; /* 0 */
	case 11: /* STATE 15 - philosophers_no_deadlock_starvation.pml.txt:41 - [((fork[((p+1)%5)]==0))] (24:0:1 - 1) */
		IfNotBlocked
		reached[1][15] = 1;
		if (!((((int)now.fork[ Index(((((P1 *)this)->p+1)%5), 5) ])==0)))
			continue;
		/* merge: fork[((p+1)%5)] = 1(24, 16, 24) */
		reached[1][16] = 1;
		(trpt+1)->bup.oval = ((int)now.fork[ Index(((((P1 *)this)->p+1)%5), 5) ]);
		now.fork[ Index(((((P1 *)this)->p+1)%5), 5) ] = 1;
#ifdef VAR_RANGES
		logval("fork[((philosopher:p+1)%5)]", ((int)now.fork[ Index(((((P1 *)this)->p+1)%5), 5) ]));
#endif
		;
		/* merge: printf('Philosopher %d picks up the right fork (Fork %d).\\n',p,((p+1)%5))(24, 17, 24) */
		reached[1][17] = 1;
		Printf("Philosopher %d picks up the right fork (Fork %d).\n", ((P1 *)this)->p, ((((P1 *)this)->p+1)%5));
		_m = 3; goto P999; /* 2 */
	case 12: /* STATE 20 - philosophers_no_deadlock_starvation.pml.txt:32 - [((fork[(p%5)]==0))] (30:0:1 - 1) */
		IfNotBlocked
		reached[1][20] = 1;
		if (!((((int)now.fork[ Index((((P1 *)this)->p%5), 5) ])==0)))
			continue;
		/* merge: fork[(p%5)] = 1(30, 21, 30) */
		reached[1][21] = 1;
		(trpt+1)->bup.oval = ((int)now.fork[ Index((((P1 *)this)->p%5), 5) ]);
		now.fork[ Index((((P1 *)this)->p%5), 5) ] = 1;
#ifdef VAR_RANGES
		logval("fork[(philosopher:p%5)]", ((int)now.fork[ Index((((P1 *)this)->p%5), 5) ]));
#endif
		;
		/* merge: printf('Philosopher %d picks up the left fork (Fork %d)\\n',p,(p%5))(30, 22, 30) */
		reached[1][22] = 1;
		Printf("Philosopher %d picks up the left fork (Fork %d)\n", ((P1 *)this)->p, (((P1 *)this)->p%5));
		/* merge: .(goto)(0, 26, 30) */
		reached[1][26] = 1;
		;
		_m = 3; goto P999; /* 3 */
	case 13: /* STATE 27 - philosophers_no_deadlock_starvation.pml.txt:69 - [eats[p] = 1] (0:0:1 - 1) */
		IfNotBlocked
		reached[1][27] = 1;
		(trpt+1)->bup.oval = ((int)now.eats[ Index(((P1 *)this)->p, 5) ]);
		now.eats[ Index(((P1 *)this)->p, 5) ] = 1;
#ifdef VAR_RANGES
		logval("eats[philosopher:p]", ((int)now.eats[ Index(((P1 *)this)->p, 5) ]));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 14: /* STATE 28 - philosophers_no_deadlock_starvation.pml.txt:70 - [printf('Philosopher %d is eating.\\n',p)] (0:0:0 - 1) */
		IfNotBlocked
		reached[1][28] = 1;
		Printf("Philosopher %d is eating.\n", ((P1 *)this)->p);
		_m = 3; goto P999; /* 0 */
	case 15: /* STATE 29 - philosophers_no_deadlock_starvation.pml.txt:71 - [eats[p] = 0] (0:0:1 - 1) */
		IfNotBlocked
		reached[1][29] = 1;
		(trpt+1)->bup.oval = ((int)now.eats[ Index(((P1 *)this)->p, 5) ]);
		now.eats[ Index(((P1 *)this)->p, 5) ] = 0;
#ifdef VAR_RANGES
		logval("eats[philosopher:p]", ((int)now.eats[ Index(((P1 *)this)->p, 5) ]));
#endif
		;
		_m = 3; goto P999; /* 0 */
	case 16: /* STATE 31 - philosophers_no_deadlock_starvation.pml.txt:86 - [(leftFirst)] (0:0:0 - 1) */
		IfNotBlocked
		reached[1][31] = 1;
		if (!(((int)((P1 *)this)->leftFirst)))
			continue;
		_m = 3; goto P999; /* 0 */
	case 17: /* STATE 32 - philosophers_no_deadlock_starvation.pml.txt:50 - [fork[(p%5)] = 0] (0:39:1 - 1) */
		IfNotBlocked
		reached[1][32] = 1;
		(trpt+1)->bup.oval = ((int)now.fork[ Index((((P1 *)this)->p%5), 5) ]);
		now.fork[ Index((((P1 *)this)->p%5), 5) ] = 0;
#ifdef VAR_RANGES
		logval("fork[(philosopher:p%5)]", ((int)now.fork[ Index((((P1 *)this)->p%5), 5) ]));
#endif
		;
		/* merge: printf('Philosopher %d puts down the left fork (Fork %d).\\n',p,(p%5))(39, 33, 39) */
		reached[1][33] = 1;
		Printf("Philosopher %d puts down the left fork (Fork %d).\n", ((P1 *)this)->p, (((P1 *)this)->p%5));
		_m = 3; goto P999; /* 1 */
	case 18: /* STATE 36 - philosophers_no_deadlock_starvation.pml.txt:58 - [fork[((p+1)%5)] = 0] (0:51:1 - 1) */
		IfNotBlocked
		reached[1][36] = 1;
		(trpt+1)->bup.oval = ((int)now.fork[ Index(((((P1 *)this)->p+1)%5), 5) ]);
		now.fork[ Index(((((P1 *)this)->p+1)%5), 5) ] = 0;
#ifdef VAR_RANGES
		logval("fork[((philosopher:p+1)%5)]", ((int)now.fork[ Index(((((P1 *)this)->p+1)%5), 5) ]));
#endif
		;
		/* merge: printf('Philosopher %d puts down the right fork (Fork %d).\\n',p,((p+1)%5))(51, 37, 51) */
		reached[1][37] = 1;
		Printf("Philosopher %d puts down the right fork (Fork %d).\n", ((P1 *)this)->p, ((((P1 *)this)->p+1)%5));
		/* merge: .(goto)(0, 50, 51) */
		reached[1][50] = 1;
		;
		/* merge: .(goto)(0, 52, 51) */
		reached[1][52] = 1;
		;
		_m = 3; goto P999; /* 3 */
	case 19: /* STATE 40 - philosophers_no_deadlock_starvation.pml.txt:87 - [(!(leftFirst))] (0:0:0 - 1) */
		IfNotBlocked
		reached[1][40] = 1;
		if (!( !(((int)((P1 *)this)->leftFirst))))
			continue;
		_m = 3; goto P999; /* 0 */
	case 20: /* STATE 41 - philosophers_no_deadlock_starvation.pml.txt:58 - [fork[((p+1)%5)] = 0] (0:48:1 - 1) */
		IfNotBlocked
		reached[1][41] = 1;
		(trpt+1)->bup.oval = ((int)now.fork[ Index(((((P1 *)this)->p+1)%5), 5) ]);
		now.fork[ Index(((((P1 *)this)->p+1)%5), 5) ] = 0;
#ifdef VAR_RANGES
		logval("fork[((philosopher:p+1)%5)]", ((int)now.fork[ Index(((((P1 *)this)->p+1)%5), 5) ]));
#endif
		;
		/* merge: printf('Philosopher %d puts down the right fork (Fork %d).\\n',p,((p+1)%5))(48, 42, 48) */
		reached[1][42] = 1;
		Printf("Philosopher %d puts down the right fork (Fork %d).\n", ((P1 *)this)->p, ((((P1 *)this)->p+1)%5));
		_m = 3; goto P999; /* 1 */
	case 21: /* STATE 45 - philosophers_no_deadlock_starvation.pml.txt:50 - [fork[(p%5)] = 0] (0:51:1 - 1) */
		IfNotBlocked
		reached[1][45] = 1;
		(trpt+1)->bup.oval = ((int)now.fork[ Index((((P1 *)this)->p%5), 5) ]);
		now.fork[ Index((((P1 *)this)->p%5), 5) ] = 0;
#ifdef VAR_RANGES
		logval("fork[(philosopher:p%5)]", ((int)now.fork[ Index((((P1 *)this)->p%5), 5) ]));
#endif
		;
		/* merge: printf('Philosopher %d puts down the left fork (Fork %d).\\n',p,(p%5))(51, 46, 51) */
		reached[1][46] = 1;
		Printf("Philosopher %d puts down the left fork (Fork %d).\n", ((P1 *)this)->p, (((P1 *)this)->p%5));
		/* merge: .(goto)(0, 50, 51) */
		reached[1][50] = 1;
		;
		/* merge: .(goto)(0, 52, 51) */
		reached[1][52] = 1;
		;
		_m = 3; goto P999; /* 3 */
	case 22: /* STATE 54 - philosophers_no_deadlock_starvation.pml.txt:90 - [-end-] (0:0:0 - 1) */
		IfNotBlocked
		reached[1][54] = 1;
		if (!delproc(1, II)) continue;
		_m = 3; goto P999; /* 0 */

		 /* PROC :init: */
	case 23: /* STATE 1 - philosophers_no_deadlock_starvation.pml.txt:16 - [((i<5))] (0:0:0 - 1) */
		IfNotBlocked
		reached[0][1] = 1;
		if (!((((P0 *)this)->i<5)))
			continue;
		_m = 3; goto P999; /* 0 */
	case 24: /* STATE 2 - philosophers_no_deadlock_starvation.pml.txt:17 - [printf('Philosopher %d takes a seat,\\n',i)] (0:0:0 - 1) */
		IfNotBlocked
		reached[0][2] = 1;
		Printf("Philosopher %d takes a seat,\n", ((P0 *)this)->i);
		_m = 3; goto P999; /* 0 */
	case 25: /* STATE 3 - philosophers_no_deadlock_starvation.pml.txt:18 - [(run philosopher(i,(i==0)))] (8:0:1 - 1) */
		IfNotBlocked
		reached[0][3] = 1;
		if (!(addproc(II, 1, ((P0 *)this)->i, (((P0 *)this)->i==0))))
			continue;
		/* merge: i = (i+1)(0, 4, 8) */
		reached[0][4] = 1;
		(trpt+1)->bup.oval = ((P0 *)this)->i;
		((P0 *)this)->i = (((P0 *)this)->i+1);
#ifdef VAR_RANGES
		logval(":init::i", ((P0 *)this)->i);
#endif
		;
		/* merge: .(goto)(0, 9, 8) */
		reached[0][9] = 1;
		;
		_m = 3; goto P999; /* 2 */
	case 26: /* STATE 6 - philosophers_no_deadlock_starvation.pml.txt:21 - [((i==5))] (0:0:1 - 1) */
		IfNotBlocked
		reached[0][6] = 1;
		if (!((((P0 *)this)->i==5)))
			continue;
		/* dead 1: i */  (trpt+1)->bup.oval = ((P0 *)this)->i;
#ifdef HAS_CODE
		if (!readtrail)
#endif
			((P0 *)this)->i = 0;
		_m = 3; goto P999; /* 0 */
	case 27: /* STATE 11 - philosophers_no_deadlock_starvation.pml.txt:23 - [-end-] (0:0:0 - 3) */
		IfNotBlocked
		reached[0][11] = 1;
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

