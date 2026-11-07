#pragma gcc diagnostic push
#pragma gcc diagnostic ignored "-Wunused-result"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-result"

#include <stdio.h>
#define MINIAUDIO_IMPLEMENTATION
#include "miniaudio.h"
#include <pthread.h>
#include <signal.h>

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

void *play_audio(void *data) {
  char filePath[1024] = {0};
  snprintf(filePath, sizeof(filePath), "%s", (char *)data);
  free(data);
  ma_result result;
  ma_decoder decoder;
  ma_device_config deviceConfig;
  ma_device device;

  result = ma_decoder_init_file(filePath, NULL, &decoder);
  if (result != MA_SUCCESS) {
    fprintf(stderr, "Could not load file %s\n", filePath);
    return NULL;
  }
  deviceConfig = ma_device_config_init(ma_device_type_playback);
  deviceConfig.playback.format = decoder.outputFormat;
  deviceConfig.playback.channels = decoder.outputChannels;
  deviceConfig.sampleRate = decoder.outputSampleRate;
  deviceConfig.dataCallback = data_callback;
  deviceConfig.pUserData = &decoder;
  if (ma_device_init(NULL, &deviceConfig, &device) != MA_SUCCESS) {
    fprintf(stderr, "Failed to initialise audio device\n");
    ma_decoder_uninit(&decoder);
    return NULL;
  }
  if (ma_device_start(&device) != MA_SUCCESS) {
    fprintf(stderr, "Failed to start audio device\n");
    ma_decoder_uninit(&decoder);
    ma_device_uninit(&device);
    return NULL;
  }
  ma_device_uninit(&device);
  ma_decoder_uninit(&decoder);
  return NULL;
}

int tPlayAudio(char filePath[]) {
  pthread_t thread;
  char *dupedFilePath = malloc(strlen(filePath) + 1);
  if (dupedFilePath == NULL) {
    abort();
  }
  strcpy(dupedFilePath, filePath);
  pthread_create(&thread, NULL, play_audio, dupedFilePath);
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
