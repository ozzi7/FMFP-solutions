#define N 5

bit fork[N];


init {
  int i = 0;
  do
  :: i < N -> atomic { 
                printf ("Philosopher %d takes a seat.\n", i);
                run philosopher(i); 
                i = i + 1
              }
  :: i == N -> break
  od
}


#define leftFork(p) p % N
#define rightFork(p) (p + 1) % N


inline pickUpLeft(p) {
  /* ... complete ... */
}

inline pickUpRight(p) {
  /* ... complete ... */
}

inline putDownLeft(p) {
  /* ... complete ... */
}

inline putDownRight(p) {
  /* ... complete ... */
}


inline think(p) {
  printf("Philosopher %d is thinking.\n", p)
}
	
inline eat(p) {
  printf("Philosopher %d is eating.\n", p)
}


proctype philosopher(int p) {
  do
  :: think(p) /* philosopher decides to think */
  :: /* philosopher decides to eat */
     /* pick up forks */
     /* ... complete ... */

     eat(p);  /* eventually eat some spaghetti from the platter */

     /* put down forks */
     /* ... complete ... */
  od
}


