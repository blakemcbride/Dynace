

#include "generics.h"
#include "resource.h"


defclass Dialogs : TaskList {
	int	iDataMode;
};


cmeth	void	gPerformDialogs(object wind, int dm)
{
	object	tl = gNewTaskList(super, (unsigned) IDD_SELECT_PROCESS, (unsigned) IDC_PROCESS_LIST, wind, NULL);
	accessIVsOf(tl);
	object	proc;

	iDataMode = dm;

	proc = gNewDialog1(Dialog1, tl);
	gAddTask(tl, proc, "Dialog One");

	proc = gNewDialog2(Dialog2, tl);
	gAddTask(tl, proc, "Dialog Two");

	proc = gNewDialog3(Dialog3, tl);
	gAddTask(tl, proc, "Dialog Three");

	gPerform(tl);
}

imeth	int	gGetDataMode()
{
	return iDataMode;
}

imeth	int	gSetDataMode(int dm)
{
	iDataMode = dm;
	return iDataMode;
}

