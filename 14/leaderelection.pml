#define N 5
#define L 1

chan c[N] = [L] of {byte};

init {
 atomic {
  int count;
  byte i = 1;
  do
  :: count <= N -> run pnode(c[i-1], c[i%N], (N+3-i) % (N+1)); i++;
  :: else -> break;
  od;
 }
}

proctype pnode (chan _in, out; byte id) {
 byte msg;
 do
 ::  out ! id;  _in ? msg
 od;
 do
 :: msg == id -> break;
 :: msg > id -> out ! id;
 :: else -> skip;
 od;
}



