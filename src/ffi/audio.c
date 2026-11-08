#pragma gcc diagnostic push
#pragma gcc diagnostic ignored "-Wunused-result"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-result"

#include <stdio.h>
#define MINIAUDIO_IMPLEMENTATION
#include "miniaudio.h"
#include <pthread.h>
#include <signal.h>
#include <stdbool.h>
#include <time.h>

#pragma clang diagnostic pop
#pragma gcc diagnostic pop

static bool engineInitialised = false;
static ma_engine engine;

void data_callback(ma_device *pDevice, void *pOutput, const void *pInput,
                   ma_uint32 frameCount) {
  ma_decoder *pDecoder = (ma_decoder *)pDevice->pUserData;
  if (pDecoder == NULL) {
    return;
  }

  ma_decoder_read_pcm_frames(pDecoder, pOutput, frameCount, NULL);

  (void)pInput;
}

void playAudio(char *filePath) {
  ma_result result;

  if (!engineInitialised) {
    result = ma_engine_init(NULL, &engine);
    if (result != MA_SUCCESS) {
      printf("Failed to initialize audio engine.");
      exit(1);
    }
    engineInitialised = true;
  }

  ma_engine_set_time_in_pcm_frames(&engine, 60 * 10 * 42000);
  ma_engine_stop(&engine);
  ma_engine_get_node_graph(&engine);

  ma_engine_play_sound(&engine, filePath, NULL);
  ma_engine_start(&engine);
}

int tPlayAudio(char filePath[]) {
  playAudio(filePath);
  return 0;
}

#ifdef TEST
#include <string.h>
int main() {
  tPlayAudio("foxy-scream-fnaf-1.mp3");
  pause();
  return 0;
}
#endif
