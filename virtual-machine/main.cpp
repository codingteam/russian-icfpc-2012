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

void check_address(uint32_t addr)
{
	if (addr >= 13371111)
	{
		throw std::exception("invalid address");
	}
}

void emulate(uint32_t ip, uint32_t * const memory)
{
	int counter = 0;
	while (counter < 10000)
	{
		check_address(ip);
		uint32_t instruction = memory[ip];
		int16_t command = instruction >> 16;
		int16_t shift = instruction;

		switch (command)
		{
		case 1:
			check_address(ip + 1);
			check_address(ip + 2);
			check_address(ip + 3);
			check_address(ip + memory[ip + 1]);
			check_address(ip + memory[ip + 2]);
			check_address(ip + memory[ip + 3]);
			memory[ip + memory[ip + 1]] = memory[ip + memory[ip + 2]] + memory[ip + 3];
			break;
		case 2:
			check_address(ip + 1);
			check_address(ip + 2);
			check_address(ip + 3);
			check_address(ip + memory[ip + 1]);
			check_address(ip + memory[ip + 2]);
			check_address(ip + memory[ip + 3]);
			memory[ip + memory[ip + 1]] = memory[ip + memory[ip + 2]] - memory[ip + 3];
			break;
		case 3:
			check_address(ip + 1);
			check_address(ip + 2);
			check_address(ip + 3);
			check_address(ip + memory[ip + 1]);
			check_address(ip + memory[ip + 2]);
			check_address(ip + memory[ip + 3]);
			memory[ip + memory[ip + 1]] = memory[ip + memory[ip + 2]] * memory[ip + 3];
			break;
		case 4:
			check_address(ip + 1);
			check_address(ip + 2);
			check_address(ip + 3);
			check_address(ip + memory[ip + 1]);
			check_address(ip + memory[ip + 2]);
			check_address(ip + memory[ip + 3]);
			if (memory[ip + 3] == 0)
			{
				throw std::exception("division by zero");
			}
			memory[ip + memory[ip + 1]] = memory[ip + memory[ip + 2]] / memory[ip + 3];
			break;
		case 5:
			check_address(ip + 1);
			check_address(ip + 2);
			check_address(ip + 3);
			check_address(ip + memory[ip + 1]);
			check_address(ip + memory[ip + 2]);
			check_address(ip + memory[ip + 3]);
			memory[ip + memory[ip + 1]] = memory[ip + memory[ip + 2]] & memory[ip + 3];
			break;
		case 6:
			check_address(ip + 1);
			check_address(ip + 2);
			check_address(ip + 3);
			check_address(ip + memory[ip + 1]);
			check_address(ip + memory[ip + 2]);
			check_address(ip + memory[ip + 3]);
			memory[ip + memory[ip + 1]] = memory[ip + memory[ip + 2]] | memory[ip + 3];
			break;
		case 7:
			check_address(ip + 1);
			check_address(ip + 2);
			check_address(ip + 3);
			check_address(ip + memory[ip + 1]);
			check_address(ip + memory[ip + 2]);
			check_address(ip + memory[ip + 3]);
			memory[ip + memory[ip + 1]] = memory[ip + memory[ip + 2]] << memory[ip + 3];
			break;
		case 8:
			check_address(ip + 1);
			check_address(ip + 2);
			check_address(ip + 3);
			check_address(ip + memory[ip + 1]);
			check_address(ip + memory[ip + 2]);
			check_address(ip + memory[ip + 3]);
			memory[ip + memory[ip + 1]] = memory[ip + memory[ip + 2]] >> memory[ip + 3];
			break;
		case 9:
			check_address(ip + 1);
			check_address(ip + 2);
			check_address(ip + memory[ip + 1]);
			memory[ip + memory[ip + 1]] = ~memory[ip + 2];
			break;
		case 10:
			check_address(ip + 2);
			check_address(ip + 3);
			if (memory[ip + 2] < memory[ip + 3])
			{
				check_address(ip + 1);
				ip += memory[ip + 1];
			}
			break;
		case 11:
			check_address(ip + 1);
			std::cout << (char)memory[ip + 1];
			break;
		default:
			//std::cerr << "Bad command " << command << " at " << ip << "(commands: " << counter << ")" << std::endl;
			return;
		}

		ip += shift;
		++counter;
	}

	if (counter >= 10000)
	{
		//std::cout << "10000 commands executed." << std::endl;
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

	uint32_t * const memory = new uint32_t[13371111];
	for (int i = 0; i < 13371111; ++i)
	{
		memory[i] = 0;
	}

	init_memory(memory, path);

	for (uint32_t ip = 0; ip < 60000; ++ip)
	{
		try
		{
			std::cout << /*"IP = " << ip <<*/ std::endl;
			emulate(ip, memory);
		}
		catch (...)
		{
			//std::cout << "exception" << std::endl;
		}
	}
}
