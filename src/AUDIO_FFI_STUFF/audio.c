#pragma gcc diagnostic push
#pragma gcc diagnostic ignored "-Wunused-result"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-result"

#include <stdio.h>
#define MINIAUDIO_IMPLEMENTATION
#include "miniaudio.h"
#include <pthread.h>
#include <signal.h>
#include <time.h>

#pragma clang diagnostic pop
#pragma gcc diagnostic pop

void data_callback(ma_device *pDevice, void *pOutput, const void *pInput,
                   ma_uint32 frameCount) {
  ma_decoder *pDecoder = (ma_decoder *)pDevice->pUserData;
  if (pDecoder == NULL) {
    return;
  }

  ma_decoder_read_pcm_frames(pDecoder, pOutput, frameCount, NULL);

  (void)pInput;
}

void *playAudio(void *data) {
  char filePath[1024] = {0};
  snprintf(filePath, sizeof(filePath), "%s", (char *)data);
  free(data);

  ma_result result;
  ma_engine engine;

  result = ma_engine_init(NULL, &engine);
  if (result != MA_SUCCESS) {
    printf("Failed to initialize audio engine.");
    exit(1);
  }

  ma_sound sound;
  result = ma_sound_init_from_file(&engine, filePath, 0, NULL, NULL, &sound);
  if (result != MA_SUCCESS) {
    printf("Failed to load sound file.");
    exit(1);
  }

  ma_sound_start(&sound);
  while (ma_sound_at_end(&sound) == 0) {
    sleep(1);
  }

  ma_sound_stop(&sound);
  ma_engine_uninit(&engine);
  return NULL;
}

int tPlayAudio(char filePath[]) {
  char *dupedFilePath = malloc(strlen(filePath) + 1);
  if (dupedFilePath == NULL) {
    abort();
  }

  strcpy(dupedFilePath, filePath);

  pthread_t thread;
  pthread_create(&thread, NULL, playAudio, dupedFilePath);
  pthread_detach(thread);
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
