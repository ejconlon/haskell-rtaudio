include Makefile.base

RT_AUDIO_VERSION := 5.1.0
RT_AUDIO_URL := https://raw.githubusercontent.com/thestk/rtaudio/$(RT_AUDIO_VERSION)

.PHONY: update-sources
update-sources:
	# Update RtAudio sources from upstream
	curl --output rtaudio/RtAudio.cpp $(RT_AUDIO_URL)/RtAudio.cpp
	curl --output rtaudio/RtAudio.h $(RT_AUDIO_URL)/RtAudio.h
	curl --output rtaudio/rtaudio_c.cpp $(RT_AUDIO_URL)/rtaudio_c.cpp
	curl --output rtaudio/rtaudio_c.h $(RT_AUDIO_URL)/rtaudio_c.h

.PHONY: citest
citest: build
	# All we can do on CI is build.
