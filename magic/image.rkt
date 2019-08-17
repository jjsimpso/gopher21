#lang magic

# Magic taken or derived from Release 5.x of Ian Darwin's file(1) command.
# Currently detects: png, jpg, bmp, 

#
# 137 P N G \r \n ^Z \n [4-byte length] I H D R [HEAD data] [HEAD crc] ...
#

# IHDR parser
0	name		png-ihdr
>0	belong		x		\b, %d x
>4	belong		x		%d,
>8	byte		x		%d-bit
>9	byte		0		grayscale,
>9	byte		2		\b/color RGB,
>9	byte		3		colormap,
>9	byte		4		gray+alpha,
>9	byte		6		\b/color RGBA,
#>10	byte		0		deflate/32K,
>12	byte		0		non-interlaced
>12	byte		1		interlaced

# Standard PNG image.
0	string		\x89PNG\x0d\x0a\x1a\x0a\x00\x00\x00\x0DIHDR	PNG image data
>16	use		png-ihdr

# JPEG
0	beshort		0xffd8		JPEG image data
!:mime	image/jpeg
!:apple	8BIMJPEG
!:strength *3
!:ext jpeg/jpg/jpe/jfif
>6	string		JFIF		\b, JFIF standard
>6	string		Exif		\b, Exif standard: [
>>12	default		x		\b]

# PC bitmaps (OS/2, Windows BMP files)  (Greg Roelofs, newt@uchicago.edu)
# http://en.wikipedia.org/wiki/BMP_file_format#DIB_header_.\
# 28bitmap_information_header.29
0	string		BM
>14	leshort		12		PC bitmap, OS/2 1.x format
>>18	leshort		x		\b, %d x
>>20	leshort		x		%d
>14	leshort		64		PC bitmap, OS/2 2.x format
>>18	leshort		x		\b, %d x
>>20	leshort		x		%d
>14	leshort		40		PC bitmap, Windows 3.x format
>>18	lelong		x		\b, %d x
>>22	lelong		x		%d x
>>28	leshort		x		%d
>14	leshort		124		PC bitmap, Windows 98/2000 and newer format
>>18	lelong		x		\b, %d x
>>22	lelong		x		%d x
>>28	leshort		x		%d
>14	leshort		108		PC bitmap, Windows 95/NT4 and newer format
>>18	lelong		x		\b, %d x
>>22	lelong		x		%d x
>>28	leshort		x		%d
>14	leshort		128		PC bitmap, Windows NT/2000 format
>>18	lelong		x		\b, %d x
>>22	lelong		x		%d x
>>28	leshort		x		%d
# Too simple - MPi
#0	string		IC		PC icon data
#0	string		PI		PC pointer image data
#0	string		CI		PC color icon data
#0	string		CP		PC color pointer image data
# Conflicts with other entries [BABYL]
#0	string		BA		PC bitmap array data


# PCX image files
# From: Dan Fandrich <dan@coneharvesters.com>
# updated by Joerg Jenderek at Feb 2013 by http://de.wikipedia.org/wiki/PCX
# http://web.archive.org/web/20100206055706/http://www.qzx.com/pc-gpe/pcx.txt
# GRR: original test was still too general as it catches xbase examples T5.DBT,T6.DBT with 0xa000000
# test for bytes 0x0a,version byte (0,2,3,4,5),compression byte flag(0,1), bit depth (>0) of PCX or T5.DBT,T6.DBT
0	ubelong&0xffF8fe00	0x0a000000
# for PCX bit depth > 0
>3	ubyte		>0
# test for valid versions
>>1	ubyte		<6
>>>1	ubyte		!1	PCX
>>>>1	ubyte		0	ver. 2.5 image data
>>>>1	ubyte		2	ver. 2.8 image data, with palette
>>>>1	ubyte		3	ver. 2.8 image data, without palette
>>>>1	ubyte		4	for Windows image data
>>>>1	ubyte		5	ver. 3.0 image data
>>>>4	uleshort	x	bounding box [%d,
>>>>6	uleshort	x	%d] -
>>>>8	uleshort	x	[%d,
>>>>10	uleshort	x	%d],
>>>>65	ubyte		>1	%d planes each of
>>>>3	ubyte		x	%d-bit
>>>>68	byte		1	colour,
>>>>68	byte		2	grayscale,
# this should not happen
>>>>68	default		x	image,
>>>>12	leshort		>0	%d x
>>>>>14	uleshort	x	%d dpi,
>>>>2	byte		0	uncompressed
>>>>2	byte		1	RLE compressed
