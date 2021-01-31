#include "setsid.h"
#include <R_ext/Rdynload.h>



static const R_CallMethodDef callMethods[] = {
  {"_restbench_spawn_process", (DL_FUNC) &spawn_process, 1},
  {NULL, NULL, 0}
};

void R_init_restbench(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
