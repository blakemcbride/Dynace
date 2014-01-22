/*
  Copyright (c) 1996 Blake McBride
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

  1. Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/



// color.h

#define CLR_Max       19

#define CLR_Aqua      0
#define CLR_Black     1
#define CLR_Blue      2
#define CLR_Fuchsia   3
#define CLR_Gray      4
#define CLR_Green     5
#define CLR_Lime      6
#define CLR_Magenta   7
#define CLR_Maroon    8
#define CLR_Navy      9
#define CLR_Olive     10
#define CLR_Orange    11
#define CLR_Purple    12
#define CLR_Red       13
#define CLR_Silver    14
#define CLR_Teal      15
#define CLR_Violet    16
#define CLR_White     17
#define CLR_Yellow    18
#define CLR_None      19


static COLORREF CLR_Array[20] = {
RGB(0, 255, 255),    // Aqua
RGB(0, 0, 0),        // Black
RGB(0, 0, 255),      // Blue
RGB(255, 0, 255),    // Fuchsia
RGB(128, 128, 128),  // Gray
RGB(0, 128, 0),      // Green
RGB(0, 255, 0),      // Lime
RGB(255, 0, 255),    // Magenta
RGB(128, 0, 0),      // Maroon
RGB(0, 0, 128),      // Navy
RGB(128, 128, 0),    // Olive
RGB(255, 128, 0),    // Orange
RGB(128, 0, 128),    // Purple
RGB(255, 0, 0),      // Red
RGB(192, 192, 192),  // Silver
RGB(0, 128, 128),    // Teal
RGB(128, 0, 128),    // Violet
RGB(255, 255, 255),  // White
RGB(255, 255, 0),    // Yellow
RGB(192, 192, 192)   // CLR_None dummy 
};


// frame styles
#define FRAME_Null     0x0000
#define FRAME_Left     0x0001
#define FRAME_Top      0x0002
#define FRAME_Right    0x0004
#define FRAME_Bottom   0x0008
#define FRAME_Full     (FRAME_Left | FRAME_Top | FRAME_Right | FRAME_Bottom)


// line styles
#define LS_Max          11
#define LS_Separator    6

#define LS_Solid        0
#define LS_Dot1         1
#define LS_Dot2         2
#define LS_Dot3         3
#define LS_Dot4         4
#define LS_Dot5         5
#define LS_Dot6         6

#define LS_Dash1        7
#define LS_Dash2        8
#define LS_Dash3        9
#define LS_Dash4        10
#define LS_Dash5        11




