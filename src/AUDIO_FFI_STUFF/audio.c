#pragma gcc diagnostic push
#pragma gcc diagnostic ignored "-Wunused-result"

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunused-result"

#include <stdio.h>
#define MINIAUDIO_IMPLEMENTATION
#include "miniaudio.h"

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

int play_audio(char filePath[]) {
  ma_result result;
  ma_decoder decoder;
  ma_device_config deviceConfig;
  ma_device device;

  result = ma_decoder_init_file(filePath, NULL, &decoder);
  if (result != MA_SUCCESS) {
    printf("Could not load file %s\n", filePath);
    return -1;
  }
  deviceConfig = ma_device_config_init(ma_device_type_playback);
  deviceConfig.playback.format = decoder.outputFormat;
  deviceConfig.playback.channels = decoder.outputChannels;
  deviceConfig.sampleRate = decoder.outputSampleRate;
  deviceConfig.dataCallback = data_callback;
  deviceConfig.pUserData = &decoder;
  if (ma_device_init(NULL, &deviceConfig, &device) != MA_SUCCESS) {
    printf("Failed to initialise audio device\n");
    ma_decoder_uninit(&decoder);
    return -1;
  }
  if (ma_device_start(&device) != MA_SUCCESS) {
    printf("Failed to start audio device\n");
    ma_decoder_uninit(&decoder);
    ma_device_uninit(&device);
    return -1;
  }
  printf("press enter to quit\n");
  getchar();
  ma_device_uninit(&device);
  ma_decoder_uninit(&decoder);
  return 0;
}
#ifdef test
int main() {
  play_audio("foxy-scream-fnaf-1.mp3");
  return 0;
}
#endif
