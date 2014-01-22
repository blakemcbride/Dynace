



#include <stdio.h>


int	calc_nipib(int size)
{
	int	i;
	static struct {
		int	size;
		int	nipib;
	}  v[] = {
		{8, 50},
		{16, 40},
		{32, 30},
		{64, 20},
		{128, 10},
		{256, 6},
		{512, 6},
		{1024, 5},
		{2048, 5}
	};
	for (i=0 ; i < (sizeof(v)/sizeof(*v)) ; i++)
		if (size <= v[i].size)
			return v[i].nipib;
	return 1;
}


main(int argc, char *argv[])
{
	int	size = atoi(argv[1]);
	int	nipib = calc_nipib(size);
	printf("%d %d %d\n", size, nipib, size*nipib);
	return 0;
}
