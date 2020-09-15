#ifndef WRAPPER_H
#define WRAPPER_H
#ifdef __cplusplus
extern "C" {
#endif

#include "rtaudio_c.h"

void wrap_rtaudio_get_device_info(rtaudio_t audio, int i, rtaudio_device_info_t* out);

#ifdef __cplusplus
}
#endif
#endif
