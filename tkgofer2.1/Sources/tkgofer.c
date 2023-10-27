/* ------------------------------------------------------------
 * tkgofer.c --
 *
 *	This file contains the interface for the "gofer-tcl" link.
 *	It is based on tkMain.c - the main program of wish.
 *
 *	It supports a new tcl-command
 *	- toGofer n 	: to write the string n into the event buffer 
 *	
 *	The provided gofer primitives are
 *	- c_init_tcl 	: to initialze tcl/tk, returns 1 if successful
 *	- c_run_tcl	: to start the eventloop of tcl/tk
 *	- c_get_tcl     : to read the event buffer
 *                        buffer contains event identification
 *                        plus bind arguments
 *      - c_setvar_tcl  : write user output into tcl variables
 *      - c_execute_tcl : to evaluate an event by the tcl interpreter
 *     
------------------------------------------------------------ */

#include <stdio.h>
#include <tcl.h>
#include <tk.h>



extern void		exit _ANSI_ARGS_((int status));

/* ------------------------------------------------------------
* Declaration for debug information
------------------------------------------------------------ */

extern Bool tk_debug;   /* TRUE  => show debug information */


/* ------------------------------------------------------------
 * Declaration for window and interpreter variables 
------------------------------------------------------------ */

static Tcl_Interp *interp;	/* Interpreter for this application. */

#define BUFFER_SIZE 1000  	/* Buffers for communication 
                                   Contains identifier and
                                   bind arguments
                                */

static char to_gofer[BUFFER_SIZE+1];	/* events: tk to gofer */
static int  to_gofer_head=0;
static int  to_gofer_tail=0;

static int stopTkMain = 1;      /* 0 = False, 1 = True */

static int tclRunning   = 0;      /* 0 = False, 1 = True */

/* ------------------------------------------------------------
 * Declaration for new Tcl command procedures 
------------------------------------------------------------ */


/* ------------------------------------------------------------
 * addCmd: add command p to event-queue (if not full) and 
 *         return 1 (ok) or 0 (error)
 ----------------------------------------------------------- */

int addCmd(char *p) {
 int len; 
 int freeSpace;
 
 len=strlen(p);
 if (to_gofer_tail>=to_gofer_head) 
  freeSpace = BUFFER_SIZE-to_gofer_tail+to_gofer_head;
 else freeSpace = to_gofer_head-to_gofer_tail-1;

 if (freeSpace>len) {   /* enough bytes free */
  while (*p) {
   to_gofer[to_gofer_tail++] = *p;
   to_gofer_tail %= BUFFER_SIZE;
   p++;
  }; 
  to_gofer[to_gofer_tail++] = '\n';
  to_gofer_tail %= BUFFER_SIZE;
  stopTkMain = 1;
  return 1;
 }
 else {   /* queue is full */
  fprintf(stderr,"too many events\n");
  return 0;
 }
 
}

extern int EventCmd _ANSI_ARGS_((ClientData clientData,
	Tcl_Interp *interp, int argc, char *argv[]));


/* ------------------------------------------------------------
 *  EventCmd: 
 *    puts the argument string into the to_gofer buffer,
 *    resets the to_gofer_head and to_gofer_tail (see c_get_tcl),
 *    terminates the MainLoop - can be restarted using c_run_tcl
------------------------------------------------------------ */

int EventCmd(dummy, interp, argc, argv)
    ClientData dummy;                   /* Not used. */
    Tcl_Interp *interp;                 /* Current interpreter. */
    int argc;                           /* Number of arguments. */
    char **argv;                        /* Argument strings. */
{

    Bool breakStat = breakOn(False);      /* disable break checking          */
    if (argc!=2){
        Tcl_AppendResult(interp, "wrong # args: should be 1", (char *) NULL);
        breakOn(breakStat);              /* restore break trapping if nec.  */
        return TCL_ERROR;
    }
    addCmd(argv[1]);
    breakOn(breakStat);                /* restore break trapping if nec.  */
    return TCL_OK;
}



/* ------------------------------------------------------------
 *  Implementation of Gofer Primitives 
------------------------------------------------------------ */


/* ------------------------------------------------------------
 *  c_init_tcl: 
 *    initialize tcl/tk, i.e. creates the main window ,
 *    initializes the standard tcl/tk package 
 *    and the new new command `toGofer',
 *    load extra tcl/tk sources
 *    return 1 if successful, 0 otherwise
------------------------------------------------------------ */

int c_init_tcl () {

    Bool breakStat = breakOn(False);      /* disable break checking          */
    if (tk_debug) {
      fprintf(stderr, "[Initialize Tk]\n",to_gofer);
    }

    /* Reset commuication buffers */
    to_gofer_head = 0;
    to_gofer_tail = 0;

    interp     = Tcl_CreateInterp();    /* create interpreter  */
    tclRunning = 1;                     /* interpreter created */

    Tcl_SetVar(interp, "tcl_interactive", "0", TCL_GLOBAL_ONLY);

    if (Tcl_Init(interp) == TCL_ERROR) { /* Tcl Initialization Failed? */
	fprintf(stderr, "%s\n", interp->result);
        breakOn(breakStat);              /* restore break trapping if nec.  */
	return(0);
    }

    if (Tk_Init(interp) == TCL_ERROR) {  /* Tk Initialization Failed? */
	fprintf(stderr, "%s\n", interp->result);
        breakOn(breakStat);              /* restore break trapping if nec.  */
	return(0);
    }


    /* Extensions for tcl: */

    Tcl_CreateCommand(interp, "toGofer", EventCmd, (ClientData) NULL,
	    (void (*)()) NULL);

    /* Load extra commands using tcl commando `source */

    {
    char *tkextra;
    char tclcmd[80];

    tkextra = getenv("TKGOFER");
    if (tkextra) {
    	strcpy(tclcmd, "source ");
        strcat(tclcmd, tkextra);

        if (Tcl_Eval(interp,tclcmd) != TCL_OK) {
           fprintf(stderr, "%s\n", interp->result);
           breakOn(breakStat);     /* restore break trapping if nec.  */
           return(0);
        }
    }
    else fprintf(stderr, "environment variable TKGOFER undefined");
    } 


    breakOn(breakStat);       /* restore break trapping if nec.  */
    return(1);
}



/* ------------------------------------------------------------
 *  c_setvar_tcl:
 *    write user output in tcl variable
 *    in this way, special tcl characters 
 *    like [, $, } etc. are irrelevant for tcl.
 *  IMPORTANT: this special variable is called `tmp'
------------------------------------------------------------ */

void c_setvar_tcl (inp) 
    char * inp; 
{
    Bool breakStat = breakOn(False);   /* disable break checking          */
    if (tk_debug) {
       fprintf(stderr, "set tmp %s\n", inp);
    }
    Tcl_SetVar(interp, "tmp", inp, TCL_GLOBAL_ONLY); 
    breakOn(breakStat);                /* restore break trapping if nec.  */
}


/* ------------------------------------------------------------
 *  c_execute_tcl:
 *    perform action and return result to gofer
------------------------------------------------------------ */

char * c_execute_tcl (cmd) 
   char * cmd; 
{
   char errmsg[200];
   Bool breakStat = breakOn(False);      /* disable break checking          */
   if (tk_debug) fprintf(stderr, "  %s\n", cmd);

   if (Tcl_Eval(interp,cmd) != TCL_OK) {
       fprintf(stderr, "%s\n", interp->result);
       strcpy(errmsg,"internalError {");
       strcat (errmsg,interp -> result);
       strcat (errmsg, " }");
       if (Tcl_Eval(interp,errmsg) != TCL_OK){
             fprintf(stderr, "%s\n", interp->result);
             breakOn(breakStat);      /* restore break trapping if nec.  */
             exit(1);
       }   
   }
   breakOn(breakStat);                /* restore break trapping if nec.  */
   return (interp -> result);
} 


/* ------------------------------------------------------------
 *  c_get_tcl:
 *    return the next character of the event buffer to gofer
 *    interrupts possible !
------------------------------------------------------------ */

char c_get_tcl () {
     char c;

     if (to_gofer_tail!=to_gofer_head) {
      c = to_gofer[to_gofer_head++];
      to_gofer_head %= BUFFER_SIZE;
      if (tk_debug) 
       fprintf (stderr, "%c", c=='\0'?'+':(c=='\n'?' ':c));
      return c;
     }
     else return '\n';
}



/* ------------------------------------------------------------
 *  c_run_tcl:
 *    starts tk to handle events
 *    terminates if user interface is destroyed
 *    (Tk_GetNumMainWinows == 0) or after an event is
 *    handled  (stopTkMain == 1)
------------------------------------------------------------ */

void c_run_tcl () {

    if (tk_debug) {
   	fprintf(stderr, "[Tk is waiting for an event...]\n");
    }
    if (to_gofer_tail==to_gofer_head) stopTkMain = 0;
    while (/* FIX: (Tk_GetNumMainWindows() > 0) && */ (stopTkMain == 0)) {
      Tk_DoOneEvent(0);
    }
}


/* ------------------------------------------------------------
 *  c_quit_tcl:
 *    quit and destroy the actual interpreter
------------------------------------------------------------ */


int c_quit_tcl () 
 { if (tclRunning) {
     Bool breakStat = breakOn(False); /* disable break checking          */
     if (tk_debug) {
       fprintf(stderr, "[Quit] \n");
     }
     Tcl_Eval(interp,"update idletasks");
     Tcl_Eval(interp,"destroy ."); 
     Tcl_DeleteInterp(interp);
     tclRunning=0;
     breakOn(breakStat);  /* restore break trapping if nec.  */
 }}
