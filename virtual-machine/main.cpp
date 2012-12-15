#include <iostream>
#include <fstream>

void init_memory(int * const memory, const char * const path)
{
	std::ifstream data(path);
	int *ptr = memory;

	char buffer[4];
	int count = 0;
	while (count = data.readsome(buffer, 4))
	{
		std::memmove(ptr, buffer, count);
		++ptr;
	}
}

int main(int argc, char* argv[])
{
	if (argc < 2)
	{
		std::cout << "Usage: virtual-machine path-to-datafile" << std::endl;
		return 1;
	}

	const char * const path = argv[1];

	int * const memory = new int[13371111];
	init_memory(memory, path);
}
