#include <cstdint>
#include <fstream>
#include <iostream>

void init_memory(int32_t * const memory, const char * const path)
{
	std::ifstream data(path);
	int32_t *ptr = memory;

	char buffer[4];
	int32_t count = 0;
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

	int32_t * const memory = new int32_t[13371111];
	init_memory(memory, path);

	size_t ip = 32;
	while (true)
	{
		int32_t instruction = memory[ip];
		int16_t command = instruction >> 16;
		int16_t shift = instruction;
		switch (command)
		{
		case 1:
			memory[ip + memory[ip + 1]] = memory[ip + memory[ip + 2]] + memory[ip + 3];
			break;
		case 2:
			memory[ip + memory[ip + 1]] = memory[ip + memory[ip + 2]] - memory[ip + 3];
			break;
		case 3:
			memory[ip + memory[ip + 1]] = memory[ip + memory[ip + 2]] * memory[ip + 3];
			break;
		case 4:
			memory[ip + memory[ip + 1]] = memory[ip + memory[ip + 2]] / memory[ip + 3];
			break;
		case 5:
			memory[ip + memory[ip + 1]] = memory[ip + memory[ip + 2]] & memory[ip + 3];
			break;
		case 6:
			memory[ip + memory[ip + 1]] = memory[ip + memory[ip + 2]] | memory[ip + 3];
			break;
		case 7:
			memory[ip + memory[ip + 1]] = memory[ip + memory[ip + 2]] << memory[ip + 3];
			break;
		case 8:
			memory[ip + memory[ip + 1]] = memory[ip + memory[ip + 2]] >> memory[ip + 3];
			break;
		case 9:
			memory[ip + memory[ip + 1]] = ~memory[ip + 2];
			break;
		case 10:
			if (memory[ip + 2] < memory[ip + 3])
			{
				ip += memory[ip + 1];
			}
			break;
		case 11:
			std::cout << (char)memory[ip + 1];
			break;
		default:
			std::cerr << "Bad command " << command << " at " << ip << std::endl;
			return 2;
		}

		ip += shift;
	}
}
