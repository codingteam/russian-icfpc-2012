#include <cstdint>
#include <fstream>
#include <iostream>

void init_memory(uint32_t * const memory, const char * const path)
{
	std::ifstream data(path, std::fstream::in);
	uint32_t *ptr = memory;

	std::cout << "Reading memory image file... ";
	uint32_t buffer;
	while (data >> buffer)
	{
		*ptr = buffer;
		ptr++;
	}
	
	std::cout << (ptr - memory) << " words read" << std::endl;
}

int main(int argc, char* argv[])
{
	if (argc < 2)
	{
		std::cout << "Usage: virtual-machine path-to-datafile" << std::endl;
		return 1;
	}

	const char * const path = argv[1];

	uint32_t * const memory = new uint32_t[13371111];
	init_memory(memory, path);

	size_t ip = 32;
	while (true)
	{
		uint32_t instruction = memory[ip];
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
