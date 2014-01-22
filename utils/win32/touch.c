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





#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

static	int	touch(char *f);

main(int argc, char *argv[])
{
	int	i, err=0;

	for (i=1 ; --argc ; ++i)
		err |= touch(argv[i]);
	return err;
}

static	int	touch(char *f)
{
	FILE	*fp;
	int	c, err=0;
	struct	stat	sb;

	if (!stat(f, &sb)  &&  (sb.st_mode & S_IFDIR))
		return 0;  /*  is directory  */
	if (NULL != (fp = fopen(f,"r+b")))  {
		fseek(fp, 0L, 2);
		if (ftell(fp) == 0L)  {
			fclose(fp);
			unlink(f);
			if (NULL == (fp = fopen(f, "w")))  {
				fprintf(stderr, "touch:  can't create %s", f);
				err = 1;
			}
			fclose(fp);
		}  else  {
			fseek(fp, 0L, 0);
			c = fgetc(fp);
			fseek(fp, 0L, 0);
			fputc(c, fp);
			fclose(fp);
		}
	}  else  {
		if (NULL == (fp = fopen(f, "w")))  {
			fprintf(stderr, "touch:  can't create %s", f);
			err = 1;
		}
		fclose(fp);
	}
	return err;
}



