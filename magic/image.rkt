#lang magic

# Magic taken or derived from Release 5.x of Ian Darwin's file(1) command.
# Currently detects: png, jpg, bmp, 

# Standard PNG image.
0	string		\x89PNG\x0d\x0a\x1a\x0a\x00\x00\x00\x0DIHDR	PNG image data

# JPEG
0	beshort		0xffd8		JPEG image data

# PC bitmaps (OS/2, Windows BMP files)  (Greg Roelofs, newt@uchicago.edu)
# http://en.wikipedia.org/wiki/BMP_file_format#DIB_header_.\
# 28bitmap_information_header.29
0	string		BM
>14	leshort		12		PC bitmap, OS/2 1.x format
>14	leshort		64		PC bitmap, OS/2 2.x format
>14	leshort		40		PC bitmap, Windows 3.x format
>14	leshort		124		PC bitmap, Windows 98/2000 and newer format
>14	leshort		108		PC bitmap, Windows 95/NT4 and newer format
>14	leshort		128		PC bitmap, Windows NT/2000 format
