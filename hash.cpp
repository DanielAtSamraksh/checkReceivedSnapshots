#include <cstdlib>
#include <stdio.h>

typedef long long unsigned internal_hash_t;

internal_hash_t _hash ( internal_hash_t seed ) {
  internal_hash_t m = 1; m <<= 24; // 2^24
  internal_hash_t a = 1140671485;
  internal_hash_t c = 12820163;
  seed = ( a * seed + c ) & (m-1);
  return seed;
};

int main ( int argc, char* argv[] ) {
  internal_hash_t seed = 0;
  for ( int i = 1; i < argc; i++ ) {
    printf ( "%llu\n", seed );
    seed = _hash ( seed + atoi ( argv [ i ]));
  }
  printf ( "%llu\n", seed );
}
    
