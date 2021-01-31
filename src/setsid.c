#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include "setsid.h"

SEXP spawn_process(SEXP args) {

  int spawn = 1;
  int fork_flag = 0;

  if (getpgrp() == getpid()) {

    fork_flag = fork();

    // 0: child process
    // -1: parent process and child creation failed
    // others: parent process
    if( fork_flag != 0 ){
      spawn = 0;
    }
  }

  // Create a new session and make this setsid process the session
  // leader for the new session; This setsid process also becomes
  // the process group leader of a new process group and has no
  // controlling terminal...
  if (setsid() < 0) {
    spawn = 0;
  }

  // Execute the requested command with the given arguments,
  // replacing our current process image (which now has its own
  // happy session and group) with a new process image...
  if( spawn ){
    int argc = LENGTH(args);

    if(argc){
      char* cmd = (char*)CHAR(STRING_ELT(args, 0));
      char* cargs[argc + 1];

      // char** cargs = (char**)(malloc(sizeof(char*) * argc));
      for(int i = 0; i < argc; i++){
        *(cargs + i) = (char*)CHAR(STRING_ELT(args, i));
      }
      *(cargs + argc) = NULL;

      execvp(cmd, cargs);
      // free(cargs);
      // cargs = NULL;
    }

    exit(0);

  }

  return(R_NilValue);
}


