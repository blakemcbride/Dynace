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

defclass Restart {
	iCond;		// Condition for which Restart was inst'd
};

private	imeth	init(cond)
{
	iCond = cond;
	return self;
}

// Create a new Restart-- but Restart itself is an abstract class,
// so this must be called with a subclass.

cvmeth	vNew(object cond)
{
	if (ClassOf(self) == CLASS)
		gShouldNotImplement(self, "New");
	ChkArgTypNul(cond, 2, Condition);
	return init(gNew(super), cond);
}

imeth	gDeepDispose()
{
	if (iCond != NULL)
		gDeepDispose(iCond);
	return gDispose(super self);
}

imeth	gCopy, gDeepCopy()
{
    return gShouldNotImplement(self, "Copy/DeepCopy");
}

imeth	gValue()
{
    return iCond;
}

