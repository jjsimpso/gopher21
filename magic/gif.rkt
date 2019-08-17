#lang magic

# Magic taken or derived from Release 5.x of Ian Darwin's file(1) command.

# GIF
# Strength set up to beat 0x55AA DOS/MBR signature word lookups (+65)
0	string		GIF8		GIF image data
>4	string		7a		\b, version 8%s,
>4	string		9a		\b, version 8%s,
>6	leshort		>0		%d x
>8	leshort		>0		%d
