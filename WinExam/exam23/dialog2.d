

#include "resource.h"

defclass Dialog2 : DialogTask {
	iDialog;	//  this dialog
};


private	imeth	void	init_controls(object self);


cmeth	gNewDialog2(tl)
{
	object	obj = gNewTask(super, tl);
	accessIVsOf(obj);

	return obj;
}

imeth	gRunRun()
{
	iDialog = mNewDialog(self, IDD_DIALOG2, gGetParent(super));

	init_controls(self);
	gPerform(iDialog);
	return self;
}

private	imeth	void	init_controls()
{
	object	ctl;
	int	r;
	
}

