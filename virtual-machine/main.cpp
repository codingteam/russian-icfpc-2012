#include <cstdint>
#include <exception>
#include <fstream>
#include <iostream>

void init_memory(int32_t * const memory, const char * const path)
{
	//for (int i = 0; i < 13371111; ++i)
	//{
	//	memory[i] = 0;
	//}

	std::ifstream data(path);
	int32_t *ptr = memory;

	std::cerr << "Reading memory image file... ";
	uint32_t buffer;
	while (data >> buffer)
	{
		*ptr = buffer;
		ptr++;
	}
	
	std::cerr << (ptr - memory) << " words read" << std::endl;
}

void check_address(int32_t addr)
{
	if (addr < 0 || addr >= 13371111)
	{
		throw std::exception("invalid address");
	}
}

void emulate(int32_t ip, int32_t * const memory)
{
	int counter = 0;
	while (counter < 10000)
	{
		check_address(ip);
		int32_t instruction = memory[ip];
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
			memory[ip + memory[ip + 1]] = memory[ip + memory[ip + 2]] + memory[ip + 3];
			break;
		case 2:
			check_address(ip + 1);
			check_address(ip + 2);
			check_address(ip + 3);
			check_address(ip + memory[ip + 1]);
			check_address(ip + memory[ip + 2]);
			memory[ip + memory[ip + 1]] = memory[ip + memory[ip + 2]] - memory[ip + 3];
			break;
		case 3:
			check_address(ip + 1);
			check_address(ip + 2);
			check_address(ip + 3);
			check_address(ip + memory[ip + 1]);
			check_address(ip + memory[ip + 2]);
			memory[ip + memory[ip + 1]] = memory[ip + memory[ip + 2]] * memory[ip + 3];
			break;
		case 4:
			check_address(ip + 1);
			check_address(ip + 2);
			check_address(ip + 3);
			check_address(ip + memory[ip + 1]);
			check_address(ip + memory[ip + 2]);
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
			memory[ip + memory[ip + 1]] = memory[ip + memory[ip + 2]] & memory[ip + 3];
			break;
		case 6:
			check_address(ip + 1);
			check_address(ip + 2);
			check_address(ip + 3);
			check_address(ip + memory[ip + 1]);
			check_address(ip + memory[ip + 2]);
			memory[ip + memory[ip + 1]] = memory[ip + memory[ip + 2]] | memory[ip + 3];
			break;
		case 7:
			check_address(ip + 1);
			check_address(ip + 2);
			check_address(ip + 3);
			check_address(ip + memory[ip + 1]);
			check_address(ip + memory[ip + 2]);
			memory[ip + memory[ip + 1]] = memory[ip + memory[ip + 2]] << memory[ip + 3];
			break;
		case 8:
			check_address(ip + 1);
			check_address(ip + 2);
			check_address(ip + 3);
			check_address(ip + memory[ip + 1]);
			check_address(ip + memory[ip + 2]);
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
			else
			{
				ip += shift;
			}
			break;
		case 11:
			check_address(ip + 1);
			std::cout << (char)memory[ip + 1];
			break;
		default:
			std::cerr << "Bad command " << command << " at " << ip << "(commands: " << counter << ")" << std::endl;
			return;
		}

		if (command != 10)
		{
			ip += shift;
		}
		++counter;
	}

	if (counter >= 10000)
	{
		std::cerr << "10000 commands executed." << std::endl;
	}
}

int main(int argc, char* argv[])
{
	if (argc < 3)
	{
		std::cout << "Usage: virtual-machine path-to-datafile start-ip final-ip" << std::endl;
		return 1;
	}

	const char * const path = argv[1];
	int32_t first_ip = std::atoi(argv[2]);
	int32_t last_ip = std::atoi(argv[3]);

	int32_t * const memory = new int32_t[13371111];

	for (int32_t ip = first_ip; ip < last_ip; ++ip)
	{
		try
		{
			std::cerr << "IP = " << ip << std::endl;
			init_memory(memory, path);
			std::cout << std::endl;
			emulate(ip, memory);
		}
		catch (...)
		{
			std::cerr << "exception" << std::endl;
		}
	}
}
