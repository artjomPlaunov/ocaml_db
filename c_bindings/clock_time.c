#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <time.h>

CAMLprim value clock_gettime_ocaml() {
  struct timespec ts;

  if (clock_gettime(CLOCK_REALTIME, &ts) != 0) {
    caml_failwith("clock_gettime failed");
  }

  return Val_int(ts.tv_nsec);
}
