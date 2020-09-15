#include <utility>
#include "rtaudio_c.h"
#include "wrapper.h"

void wrap_rtaudio_get_device_info(rtaudio_t audio, int i, rtaudio_device_info_t* out) {
  *out = std::move(rtaudio_get_device_info(audio, i));
}
