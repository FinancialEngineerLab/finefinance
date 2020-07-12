#include "date.h"
#include <ctime>

int daysBetween(Date d1, Date d2)
{
	std::tm a = { 0,0,0, d1.day(), d1.month() - 1, d1.year() - 1900 };
	std::tm b = { 0,0,0, d2.day(), d2.month() - 1, d2.year() - 1900 };
	std::time_t x = std::mktime(&a);
	std::time_t y = std::mktime(&b);

	int difference = std::difftime(y, x) / (60 * 60 * 24);
	return difference;
}