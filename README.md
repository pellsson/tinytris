# About

Tetris written in 256 bytes or less. Currently it is at 255 bytes; complete with rotation, collision, all different tetraminos, lines being removed and dropping pieces.

There are other implementations of Tetis in 256 bytes or less, but I have yet to find one that doesn't "cheat". Most commonly, they lack multiple tetraminos, or the tetraminos have been altered. Using a 3-length "long"-piece for instance significantly reduces rotation complexity.

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

- Random logic is based on the PIT timer, which for obvious reasons isn't ideal :)

