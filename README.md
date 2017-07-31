# About

Tetris written in 256 bytes or less. Currently it is at 253 bytes; complete with rotation, collision, all different tetraminos, lines being removed and dropping pieces.

To my knowledge it is the first complete Tetris implementation in 256 bytes or less. There are other implementations, some significantly smaller than 256 bytes, that are extremely impressive in their own way. However, I never found one that doesn't alter the original concept in some way (most commonly by removing or altering tetraminos).

# Code walk-through

write me.

# Assumptions

These should ideally be removed, but I still consider it to be within the limits of completing the challenge.

- DS=CS=SS @ entry-point. The code assumes that the stack-, data- and code-selector all have the same value at the entry-point.

- SS:SP is writable for BOARD_BYTES worth of data.

- Graphics card is currently running in text-mode (ax=1; int 0x10)

# Further improvements

Stuff that is really trash in the current implementation.

- SPEED of dropping blocks depends on the CPU it is being ran on. Using a timer instead would be more portable :)

- Random logic is based on the PIT timer or rdtsc (#ifdef'ed), which for obvious reasons isn't ideal :)

