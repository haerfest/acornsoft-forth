forth.rom: forth.asm
	@beebasm -i forth.asm
	@md5 forth-assembled.rom
