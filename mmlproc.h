/*
 * mmlproc.h
 */

/* Prologue */
#ifndef GAUCHE_MMLPROC_H
#define GAUCHE_MMLPROC_H

#include <gauche.h>
#include <gauche/extend.h>

SCM_DECL_BEGIN

/*
 * The following entry is a dummy one.
 * Replace it for your declarations.
 */

extern ScmObj test_mmlproc(void);
extern int calc_pcmdata_sub(short* pcmdata, int pcmdata_len, int sample_rate, int max_ch,
                            int note, double nlength1, double nlength2, int prog, double volume,
                            double rtime1, double rtime2);

/* Epilogue */
SCM_DECL_END

#endif  /* GAUCHE_MMLPROC_H */
