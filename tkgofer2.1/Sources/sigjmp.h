/* implementation of sigsetjmp/siglongjmp */

#include <setjmp.h>
#include <signal.h>

struct _sigjmp_buf {
  int mask_saved;
  sigset_t sigmask;
  jmp_buf registers;
};

#define _SJBLEN (sizeof(struct _sigjmp_buf))
typedef unsigned char sigjmp_buf[_SJBLEN];

#define _ENV(buf)       ((struct _sigjmp_buf *) &buf[0])

/* must be a macro */
#define sigsetjmp(buf, savemask)                                \
    (_ENV(buf)->mask_saved = savemask,                          \
    ((savemask) && sigprocmask(0, 0, &_ENV(buf)->sigmask)),     \
    setjmp(_ENV(buf)->registers))

#define siglongjmp(buf, res)                                    \
    ((_ENV(buf)->mask_saved &&                                  \
      sigprocmask(SIG_SETMASK, &_ENV(buf)->sigmask, 0)),        \
    longjmp(_ENV(buf)->registers, res))

