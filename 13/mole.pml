bit molecaught;
int moleposition;


inline huntermove(i) {
	int target;
	if
	:: i == 0 -> target = 2;
	:: i == 1 -> target = 3;
	:: i == 2 -> target = 4;
	:: i == 3 -> target = 2;
	:: i == 4 -> target = 3;
	:: i == 5 -> target = 4;
	fi;
	if
	:: (moleposition == target) -> molecaught = 1;
	:: (moleposition != target) && (molecaught ==0) -> molecaught = 0;
	:: else -> molecaught = 1;
	fi;
}

inline littlebitchmove() {
if
:: (moleposition == 1) -> moleposition = 2;
:: (moleposition == 5) -> moleposition = 4;
:: else -> 
   do
   :: moleposition = moleposition + 1; break;
   :: moleposition = moleposition - 1; break;
   od; 
fi;
}

proctype supervisor() {
  int i;
  int random;
  i = 0;
  do
  :: random = 1; break;
  :: random = 2; break;
  :: random = 3; break;
  :: random = 4; break;
  :: random = 5; break;
  od;
  moleposition = random;
  do
  :: i < 6 -> atomic {  
		   huntermove(i);
		   littlebitchmove();
              i = i + 1;
                     }
  :: i == 6 -> assert (molecaught == 1);
  od
}
init {
   run supervisor();
}

