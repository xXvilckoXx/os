nasm -O0 -f bin -o bootload.bin bootload.asm

PARTCOPY.EXE bootload.bin 0 400 -f0

pause