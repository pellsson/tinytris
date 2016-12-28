yasm -DBOOT_SECTOR -f bin -o tinytris.img tinytris.asm
yasm -f bin -o tinytris.com tinytris.asm && hexdump -C tinytris.com

