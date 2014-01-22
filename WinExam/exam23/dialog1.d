

#include "resource.h"

defclass Dialog1 : DialogTask {
	iDialog;	//  this dialog
};


private	imeth	void	init_controls(object self);


cmeth	gNewDialog1(tl)
{
	object	obj = gNewTask(super, tl);
	accessIVsOf(obj);

	return obj;
}

imeth	gRunRun()
{
	iDialog = mNewDialog(self, IDD_DIALOG1, gGetParent(super));

	init_controls(self);
	gPerform(iDialog);
	return self;
}

private	imeth	void	init_controls()
{
	object	ctl;
	int	r;
	
}

imeth	gRunClean()
{
	gMessage(Application, "RunClean");
	return self;
}

imeth	gRunSave()
{
	gMessage(Application, "RunSave");
	return self;
}

imeth	gRunClose()
{
	gMessage(Application, "RunClose");
	return gRunClose(super);
}

imeth	gDispose, gDeepDispose ()
{
	gMessage(Application, "Dispose");
	return gDispose(super);
}

