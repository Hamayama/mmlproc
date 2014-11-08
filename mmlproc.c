/*
 * mmlproc.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mmlproc.h"

/*
 * The following function is a dummy one; replace it for
 * your C function definitions.
 */

ScmObj test_mmlproc(void)
{
    return SCM_MAKE_STR("mmlproc is working");
}

int calc_pcmdata_sub(short* pcmdata, int pcmdata_len, int sample_rate, int max_ch,
                     int note, double nlength1, double nlength2, int prog, double volume,
                     double rtime1, double rtime2) {
    double nlen1, nlen2, freq, phase_c, amp_c;
    int i, pos_int, len_int;
    double t, phase, wave, fade;

    // ***** 音長計算 *****
    // (音符の途中でテンポが変わることを考慮する)
    nlen1 = sample_rate * (rtime2 - rtime1);
    // ***** 発音長計算 *****
    if (nlength1 > 0) {
        nlen2 = nlen1 * nlength2 / nlength1;
    } else {
        nlen2 = 0;
    }
    // ***** 音符の周波数計算 *****
    freq = 13.75 * pow(2, (double)(note - 9) / 12);
    // ***** 定数を先に計算しておく *****
    phase_c = 2 * M_PI * freq;
    amp_c = 32767 * volume / 127 / max_ch;
    pos_int = (int)(sample_rate * rtime1);
    len_int = (int)nlen1;
    // ***** データサイズチェック *****
    if (pos_int + len_int > pcmdata_len) {
        printf("pcmdata buffer size error!\n");
        printf(" pcmdata_len = %d\n", pcmdata_len);
        printf(" pos_int + len_int = %d\n", (pos_int + len_int));
        return 1;
    }
    // ***** 音声データの値を計算 *****
    for (i = 0; i < len_int; i++) {
        t = (double)i / sample_rate;
        phase = phase_c * t;
        switch (prog) {
            case 0:   // 方形波
                wave = (sin(phase) > 0) ? 1 : -1;
                break;
            case 1:   // 正弦波
                wave = sin(phase);
                break;
            case 2:   // のこぎり波
                wave = fmod(phase, (M_PI * 2)) / M_PI - 1;
                break;
            case 3:   // 三角波
                wave = 2 * asin(sin(phase)) / M_PI;
                break;
            case 4:   // ホワイトノイズ
                wave = (double)rand() / (RAND_MAX + 1) * 2 - 1;
                break;
            case 500: // ピアノ(仮)
                wave = 1.3 * ((sin(phase) > 0) ? 1 : -1) * exp(-5 * t);
                break;
            case 501: // オルガン(仮)
                wave = ((sin(phase) > 0) ? 1 : -1) * 13 * t * exp(-5 * t);
                break;
            case 502: // ギター(仮)
                wave = 5 * cos(phase + cos(phase / 2) + cos(phase * 2)) * exp(-5 * t);
                break;
            default:  // 方形波
                wave = (sin(phase) > 0) ? 1 : -1;
                break;
        }
        if (wave < -1) { wave = -1; }
        if (wave >  1) { wave =  1; }
        if (nlen2 == 0)           { fade = 1; }
        else if (i < 0.8 * nlen2) { fade = 1; }
        else if (i < nlen2)       { fade = 5 * (1 - (i / nlen2)); }
        else                      { fade = 0; }
        pcmdata[pos_int + i] += (short)(amp_c * wave * fade);
    }
    return 0;
}

/*
 * Module initialization function.
 */
extern void Scm_Init_mmlproclib(ScmModule*);

void Scm_Init_mmlproc(void)
{
    ScmModule *mod;

    /* Register this DSO to Gauche */
    SCM_INIT_EXTENSION(mmlproc);

    /* Create the module if it doesn't exist yet. */
    mod = SCM_MODULE(SCM_FIND_MODULE("mmlproc", TRUE));

    /* Register stub-generated procedures */
    Scm_Init_mmlproclib(mod);
}
