
#include "generics.h"

void	signalWarning()
{
	object rstrt;
    
	fprintf(stderr, "Before gSignal(<SimpleWarning>)\n");
	rstrt = gSignal(vNew(SimpleWarning, Dynace, "You have been warned!"));
	if (gIsKindOf(rstrt, Abort))
		gSignal(rstrt);
	else
		gDeepDispose(rstrt);
	fprintf(stderr, "After gSignal(<SimpleWarning>)\n");
}

void	unwindMePlease()
{
	withUnwindProtect
		signalWarning();
	onUnwind
		fprintf(stderr, "It's good just to unwind sometimes.\n");
	endUnwind;
}

int feeling_cranky = 0;

object	maybeAbort(object cond)
{
	object result;
    
	if (feeling_cranky)
		result = vNew(Abort, cond);
	else
		result = vNew(SimpleRestart, cond);
	return result;
}

void	driver()
{
	withRestartHandling(maybeAbort) {
		fprintf(stderr, "Here we go...\n");
		signalWarning();
		fprintf(stderr, "Ignored first warning...\n");
		unwindMePlease();
		fprintf(stderr, "Ignored second warning...\n");
		feeling_cranky = 1;
		unwindMePlease();
		fprintf(stderr, "This should not print!\n");
	}
	onError {
		object cond;

		if (cond = catchKind(Abort)) {
			fprintf(stderr, "driver caught Abort-- declining to handle!\n");
			declineHandling(cond);
		}
	}
	endHandling;
}

int	main(int argc, char **argv)
{
	InitDynace(&argc);
	withHandling
		driver();
	onError {
		object cond;
	
		if (cond = catchKind(Error)) {
			fprintf(stderr, "main caught Abort!\n");
			gDeepDispose(cond);
		}
	}
	endHandling;
	return 0;
}

